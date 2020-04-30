;;; tests.el --- Tests for display-buffer-control -*- lexical-binding: t -*-

;; Author: Matsievskiy S.V.
;; Maintainer: Matsievskiy S.V.
;; Version: 20.03.28
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://gitlab.com/matsievskiysv/multistate
;; Keywords: convenience


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

;; Tests for display-buffer-control

;;; Code:

(require 'multistate)
(require 'ht)


(ert-deftest multistate-test-new-name ()
  "Test `multistate--new-name'."
  (should (string= (symbol-name (multistate--new-name 'name 'suffix))
                   "multistate-name-state-suffix"))
  (should (string= (symbol-name (multistate--new-name 'name))
                   "multistate-name-state")))

(ert-deftest multistate-test-new-keymap ()
  "Test `multistate--maybe-create-state-keymap'."
  (multistate--maybe-create-state-keymap test-keymap-1)
  (should (boundp 'test-keymap-1))
  (should (keymapp test-keymap-1)))

(ert-deftest multistate-test-create-state-hooks ()
  "Test `multistate--maybe-create-state-hooks'."
  (multistate--maybe-create-state-hooks name test-enter-hook-1 test-exit-hook-1)
  (should (boundp 'test-enter-hook-1))
  (should (boundp 'test-exit-hook-1)))

(ert-deftest multistate-test-create-state-function ()
  "Test `multistate--maybe-create-state-function'."
  (multistate--maybe-create-state-function name test-enter-func-1 test-test-func-1)
  (test-test-func-1)
  (should (fboundp 'test-enter-func-1))
  (should (fboundp 'test-test-func-1)))

(ert-deftest multistate-test-define-state ()
  "Test `multistate-define-state'."
  ;; initial state
  (should (boundp 'multistate--state))
  (should-not multistate--state)
  (should (keymapp multistate-suppress-map))
  ;; add states
  (multistate-define-state
   'one
   :parent 'multistate-two-state-map)
  (multistate-define-state
   'two
   :parent 'multistate-suppress-map)
  ;; check created variables and functions
  (should (fboundp 'multistate-one-state))
  (should (fboundp 'multistate-two-state))
  (should (fboundp 'multistate-one-state-p))
  (should (fboundp 'multistate-two-state-p))
  (should-not (multistate-one-state-p))
  (should-not (multistate-two-state-p))
  (should (boundp 'multistate-one-state-enter-hook))
  (should (boundp 'multistate-one-state-exit-hook))
  (should (boundp 'multistate-two-state-enter-hook))
  (should (boundp 'multistate-two-state-exit-hook))
  (should (keymapp multistate-one-state-map))
  (should (keymapp multistate-two-state-map))
  ;; check initial parents
  (should-not (keymap-parent multistate-one-state-map))
  (should-not (keymap-parent multistate-two-state-map))
  (add-hook 'multistate-one-state-enter-hook
            (lambda () (setq multistate-one-state-enter-hook-executed t)))
  (add-hook 'multistate-one-state-exit-hook
            (lambda () (setq multistate-one-state-exit-hook-executed t)))
  (add-hook 'multistate-two-state-enter-hook
            (lambda () (setq multistate-two-state-enter-hook-executed t)))
  (add-hook 'multistate-two-state-exit-hook
            (lambda () (setq multistate-two-state-exit-hook-executed t)))
  ;; check run deferred hooks
  (setq multistate-run-deffered-hooks t)
  (should-not (boundp 'multistate-one-state-enter-hook-executed))
  (should-not (boundp 'multistate-one-state-exit-hook-executed))
  (should-not (boundp 'multistate-two-state-enter-hook-executed))
  (should-not (boundp 'multistate-two-state-exit-hook-executed))
  (multistate-one-state)
  (multistate-mode 1)
  (should (boundp 'multistate-one-state-enter-hook-executed))
  (should-not (boundp 'multistate-one-state-exit-hook-executed))
  (multistate-mode 0)
  (should (boundp 'multistate-one-state-exit-hook-executed))
  (should-not (boundp 'multistate-two-state-enter-hook-executed))
  (should-not (boundp 'multistate-two-state-exit-hook-executed))
  ;; check deferred parent setup
  (should (keymap-parent multistate-one-state-map))
  (should (keymap-parent multistate-two-state-map))
  ;;
  (define-key multistate-one-state-map (kbd "a") 'forward-char)
  (define-key multistate-two-state-map (kbd "a") 'backward-char)
  (define-key multistate-two-state-map (kbd "b") 'beginning-of-line)
  ;; key a from state one should shadow key b from state two
  (should (string= (symbol-name (key-binding "a"))
                   "self-insert-command"))
  (should (string= (symbol-name (key-binding "b"))
                   "self-insert-command"))
  (multistate-mode 1)
  (should (multistate-one-state-p))
  (should-not (multistate-two-state-p))
  (should (string= (symbol-name (key-binding "a"))
                   "forward-char"))
  (should (string= (symbol-name (key-binding "b"))
                   "beginning-of-line"))
  (multistate-two-state)
  (should-not (multistate-one-state-p))
  (should (multistate-two-state-p))
  (should (string= (symbol-name (key-binding "a"))
                   "backward-char"))
  (should (string= (symbol-name (key-binding "b"))
                   "beginning-of-line"))
  (multistate-mode 0)
  (should (string= (symbol-name (key-binding "a"))
                   "self-insert-command"))
  (should (string= (symbol-name (key-binding "b"))
                   "self-insert-command")))

;;; tests.el ends here
