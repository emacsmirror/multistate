;;; multistate.el --- Multistate mode -*- lexical-binding: t -*-

;; Author: Matsievskiy S.V.
;; Maintainer: Matsievskiy S.V.
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (ht "2.3"))
;; Homepage: https://gitlab.com/matsievskiysv/multistate


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Multistate mode is basically an evil-mode without vi.
;; It allows to define states for modal editing.
;; Unlike evil-mode it doesn't come with predefined states and key bindings, doesn't set hooks but
;; allows you to configure your system from the ground up.
;;
;; See README for more details.
;;
;; The following example recreates some of evil-mode states keybindings
;;
;; *Note:* in this example ` key is used instead of ESC to return to normal state.
;; (use-package multistate
;;   :demand
;;   :hook
;;   ;; enable selection is Visual state
;;   (multistate-visual-state-enter . (lambda () (set-mark (point))))
;;   (multistate-visual-state-exit .  deactivate-mark)
;;   ;; enable overwrite-mode in Replace state
;;   (multistate-replace-state-enter . overwrite-mode)
;;   (multistate-replace-state-exit .  (lambda () (overwrite-mode 0)))
;;   :config
;;   ;; Emacs/Insert state
;;   (multistate-define-state 'Emacs :lighter "E")
;;   ;; Normal state
;;   (multistate-define-state
;;    'normal
;;    :lighter "N"
;;    :cursor 'hollow
;;    :parent 'multistate-suppress-map)
;;   ;; Replace state
;;   (multistate-define-state
;;    'replace
;;    :lighter "R"
;;    :cursor 'hbar)
;;   ;; Motion state
;;   (multistate-define-state
;;    'motion
;;    :lighter "M"
;;    :cursor 'hollow
;;    :parent 'multistate-suppress-map)
;;   ;; Visual state
;;   (multistate-define-state
;;    'visual
;;    :lighter "V"
;;    :cursor 'hollow
;;    :parent 'multistate-motion-state-map)
;;   ;; Enable Normal state at startup
;;   (multistate-normal-state)
;;   ;; Enable multistate-mode globally
;;   (multistate-global-mode 1)
;;   :bind
;;   (:map multistate-emacs-state-map
;;         ("C-z" . multistate-normal-state))
;;   (:map multistate-normal-state-map
;;         ("C-z" . multistate-emacs-state)
;;         ("i" . multistate-emacs-state)
;;         ("R" . multistate-replace-state)
;;         ("v" . multistate-visual-state)
;;         ("m" . multistate-motion-state)
;;         ("/" . search-forward)
;;         ("?" . search-backward)
;;         ("x" . delete-char)
;;         ("X" . backward-delete-char))
;;   (:map multistate-motion-state-map
;;         ("`" . multistate-normal-state)
;;         ("h" . backward-char)
;;         ("j" . next-line)
;;         ("k" . previous-line)
;;         ("l" . forward-char)
;;         ("^" . move-beginning-of-line)
;;         ("$" . move-end-of-line)
;;         ("gg" . beginning-of-buffer)
;;         ("G" . end-of-buffer))
;;   (:map multistate-replace-state-map
;;         ("`" . multistate-normal-state)))


;;; Code:

(require 'cl-lib)
(require 'ht)

(defgroup multistate nil
  "Multiple state modal bindings."
  :group  'editing
  :tag    "Multistate"
  :prefix "multistate-"
  :link   '(url-link :tag "gitlab" "https://gitlab.com/matsievskiysv/multistate"))

(defcustom multistate-lighter-indicator " ꟽ"
  "Mutistate lighter will be shown when mode is active."
  :tag  "Multistate lighter indicator."
  :type 'string)

(defcustom multistate-lighter-format "❲%s❳"
  "Format of state indicator which will be appended to the lighter."
  :tag  "Multistate lighter format."
  :type 'string)

(defcustom multistate-manage-cursor t
  "Allow multistate to manage cursor style."
  :tag  "Multistate manage cursor."
  :type 'boolean)

(defcustom multistate-run-deferred-hooks t
  "Run current state enter and exit hooks when entering and exiting multistate mode."
  :tag  "Run deferred hooks."
  :type 'boolean)

(defcustom multistate-suppress-no-digits t
  "Do not use digits and minus sign as prefix arguments in `multistate-suppress-map'."
  :tag  "Suppress digits in `multistate-suppress-map'."
  :type 'boolean)

(defvar multistate--state nil "Current multistate state.")

(defvar multistate--state-list (ht-create) "Multistate state registry.")

(defvar multistate-suppress-map (make-keymap)
  "Mutistate suppress map may be used as a parent for new states in order to suppress global keybindings.")
(suppress-keymap multistate-suppress-map
                 multistate-suppress-no-digits)

(defun multistate--new-name (state &optional suffix)
  "Create name from STATE and SUFFIX."
  (intern (concat "multistate-" (symbol-name state) "-state"
		  (when suffix (concat "-" (symbol-name suffix))))))

(defun multistate--set-keymap-parent (keymap parent list)
  "Set KEYMAP PARENT recursively.

LIST is an alist of KEYMAP PARENT pairs from `multistate--state-list'."
  (when (and keymap parent (not (keymap-parent (eval keymap))))
    ;; (message "Multistate setting %s parent keymap for %s keymap" parent keymap)
    (set-keymap-parent (eval keymap) (eval parent))
    (let* ((keymap parent)
           (parent (alist-get keymap list)))
      ;; setup parent if it is deferred
      (when parent
	(multistate--set-keymap-parent keymap parent list)))))

(defmacro multistate--maybe-create-state-keymap (name)
  "Create NAME state keymap and set its parent."
  `(unless (boundp (quote ,name))
     (defvar ,name (make-sparse-keymap) ,(format "Multistate %s state keymap." name))))

(defmacro multistate--maybe-create-state-hooks (name enter-name exit-name)
  "Create ENTER-NAME and EXIT-NAME hooks for state NAME."
  `(progn
     (unless (boundp (quote ,enter-name))
       (defvar ,enter-name (list)
         ,(format "Multistate %s enter hook." (symbol-name name))))
     (unless (boundp (quote ,exit-name))
       (defvar ,exit-name (list)
         ,(format "Multistate %s exit hook." (symbol-name name))))))

(defmacro multistate--maybe-create-state-function (name enable-name test-name)
  "Create ENABLE-NAME and TEST-NAME functions for state NAME."
  `(progn
     (unless (boundp (quote ,enable-name))
       (defun ,enable-name ()
         (format "switch to state %s." ,(symbol-name name))
         (interactive)
         (unless (,test-name)
           ;; run exit hook
           (when multistate-mode
             (let* ((state (ht-get multistate--state-list multistate--state))
                    (hook (when state (ht-get state 'exit-hook))))
               (run-hooks hook)))
           ;; set current name
           (setq multistate--state (quote ,name))
           (let* ((state (ht-get multistate--state-list (quote ,name)))
                  (keymap (when state (ht-get state 'keymap)))
                  (parent (when state (ht-get state 'parent)))
                  (hook (when state (ht-get state 'enter-hook)))
                  (cursor (when state (ht-get state 'cursor)))
                  (lighter (when state (ht-get state 'lighter))))
             ;; run deferred setup of keymap parent
             (multistate--set-keymap-parent
              keymap parent
              (ht-map (lambda (state table) `(,(ht-get table 'keymap) . ,(ht-get table 'parent)))
                      multistate--state-list))
             ;; swap keymap
             (setf (alist-get 'multistate-mode minor-mode-map-alist) (eval keymap))
             (when multistate-mode
               ;; change lighter
               (when multistate-lighter-format
                 (setcar (cdr (assq 'multistate-mode minor-mode-alist))
                         (concat multistate-lighter-indicator
                                 (when lighter (format multistate-lighter-format lighter))))
                 (force-mode-line-update))
               ;; change cursor
               (when multistate-manage-cursor
                 (setq-local cursor-type cursor))
               ;; run enter hooks
               (run-hooks hook))))))
     (unless (boundp (quote ,test-name))
       (defun ,test-name ()
         (string= (symbol-name multistate--state) ,(symbol-name name))))))

;;;###autoload
(cl-defun multistate-define-state (name &key lighter (cursor t) parent)
  "Define new NAME state.

LIGHTER will be passed to `multistate-lighter-format' to indicate state.
CURSOR will be applied when switched to this state.
PARENT keymap will be setup for state keymap.
Use `multistate-suppress-map' to suppress global keymap bindings."
  (when (ht-contains? multistate--state-list name)
    (error (format "state %s already exists." name)))
  (let ((map-name (multistate--new-name name 'map))
        (enter-name (multistate--new-name name 'enter-hook))
        (exit-name (multistate--new-name name 'exit-hook))
        (enable-name (multistate--new-name name))
	(test-name (multistate--new-name name 'p)))
    ;; create keymap, hooks and functions
    (eval `(multistate--maybe-create-state-keymap ,map-name))
    (eval `(multistate--maybe-create-state-hooks ,name ,enter-name ,exit-name))
    (eval `(multistate--maybe-create-state-function ,name ,enable-name ,test-name))
    (ht-set! multistate--state-list name (ht<-alist `((lighter . ,lighter)
                                                      (cursor . ,cursor)
                                                      (keymap . ,map-name)
                                                      (parent . ,parent)
                                                      (enter-hook . ,enter-name)
                                                      (exit-hook . ,exit-name))))
    map-name))

;;;###autoload
(define-minor-mode multistate-mode
  "Toggle `multistate-mode' minor mode.

With a prefix argument ARG, enable `multistate-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is
`toggle'.

This minor mode provides modal editing features by creating
multiple keymaps and swapping them on demand."
  nil
  multistate-lighter-indicator
  (make-sparse-keymap)
  (when multistate--state
    (let* ((state (ht-get multistate--state-list multistate--state))
           (cursor (ht-get state 'cursor))
           (lighter (ht-get state 'lighter))
           (enter-hook (ht-get state 'enter-hook))
           (exit-hook (ht-get state 'exit-hook)))
      (if multistate-mode
          (progn
            (when multistate-manage-cursor
              (setq-local cursor-type cursor))
            (when multistate-lighter-format
              (setcar (cdr (assq 'multistate-mode minor-mode-alist))
                      (concat multistate-lighter-indicator
                              (when lighter (format multistate-lighter-format lighter))))
              (force-mode-line-update))
            (when multistate-run-deferred-hooks (run-hooks enter-hook)))
        (progn
          (when multistate-manage-cursor (kill-local-variable 'cursor-type))
          (when multistate-run-deferred-hooks (run-hooks exit-hook)))))))

(defun multistate--maybe-activate ()
  "Activate multistate mode if current buffer is not minibuffer.

This is used by function `multistate-global-mode'."
  (unless (minibufferp)
    (multistate-mode 1)))

;;;###autoload
(define-globalized-minor-mode multistate-global-mode
  multistate-mode
  multistate--maybe-activate)

(provide 'multistate)

;;; multistate.el ends here
