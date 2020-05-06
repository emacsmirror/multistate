[![MELPA](https://melpa.org/packages/multistate-badge.svg)](https://melpa.org/#/multistate)

`multistate-mode` is basically an [`evil-mode`](https://github.com/emacs-evil/evil) without
[vi](https://www.vim.org/).
It allows to define states for modal editing.
Unlike `evil-mode` it doesn't come with predefined states and key bindings, doesn't mess with system
hooks so that you could configure your system the way you want.

## Installation ##

Package is available [MELPA](https://melpa.org/#/) archive.
To install it, type <kbd>M-x package-install RET multistate RET</kbd>.

## Initial state ##

There's no predefined *default* state.
With no defined states `multistate-mode` just sits quietly in the background.

## State definition ##

In order to define state use function

```emacs-lisp
(multistate-define-state NAME [LIGHTER] [CURSOR] [PARENT] [DEFAULT])
```

The only required function argument is `NAME`.
This is a symbol that will be used as a base name for you state.

For example,

```emacs-lisp
(multistate-define-state 'visual)
```

will create `visual` state.

Optional arguments:

* `LIGHTER` string will be used as a state indicator
* `CURSOR` argument will control cursor style when state is enabled. See documentation for
`cursor-type` variable to learn about possible `CURSOR` values
* `PARENT` argument allows specifying [parent
keymap](https://www.gnu.org/software/emacs/manual/html_node/elisp/Inheritance-and-Keymaps.html#Inheritance-and-Keymaps)
for the current state. This will be discussed in more detail later
* `DEFAULT` mark state as default. This is an initial state for the new buffers.

For example,

```emacs-lisp
(multistate-define-state
    'motion
    :lighter "M"
    :cursor 'hollow
    :parent 'multistate-suppress-map
	:default t)
```

will create `motion` state, that is indicated my `M` lighter and hollow cursor.
All global `self-insert-command` definitions will be overwritten as `undefined`.
This will be a default state.

## Switching states ##

Call to `multistate-define-state` will create two functions:

* `multistate-<name>-state`
* `multistate-<name>-state-p`

where `<name>` is a name of the state.
In example above these names would be `multistate-motion-state` and `multistate-motion-state-p`.

The first function `multistate-<name>-state` will switch state to `<name>`, the second function
`multistate-<name>-state-p` will test if current state is `<name>`.

`multistate-<name>-state` has two optional arguments: `NO-EXIT-HOOK` and `NO-ENTER-HOOK`.
These are intended for non-interactive use and will inhibit `multistate-<name>-state-enter-hook`,
`multistate-<name>-state-exit-hook` and `multistate-change-state-hook`.

## Hooks ##

Call to `multistate-define-state` will also create two hooks:

* `multistate-<name>-state-enter-hook`
* `multistate-<name>-state-exit-hook`

Use `add-hook` and `remove-hook` to add or remove functions from these hooks respectively.
These hooks will be executed upon entering and exiting state.
`multistate-run-deffered-hooks` customization option controls if hooks will be run when
`multistate-mode` is toggled.

Additionally you may use `multistate-mode-enter-hook` and `multistate-mode-exit-hook` to set
functions to run when `multistate-mode` is enabled or disabled.

Functions from `multistate-change-state-hook` are executed upon changing states after state
specific `multistate-<name>-state-exit-hook`.
They are not run when `NO-EXIT-HOOK` argument is `t`.

Example: show state key bindings when changing states using
[which-key](https://github.com/justbur/emacs-which-key)

```emacs-lisp
(add-hook 'multistate-change-state-hook (lambda () (which-key-show-keymap (multistate-manage-variables 'keymap) t)))
```

## Binding keys ##

Created state keymap will be named:

* `multistate-<name>-state-map`

You may add keybindings to it in ordinary fashion:

```emacs-lisp
(define-key multistate-motion-state-map (kbd "f") #'forward-char)
;; or
(bind-key "f" #'forward-char multistate-motion-state-map)
```

## Customization options ##

* `multistate-lighter-indicator` -- `multistate-mode` lighter. Defaults to ` ꟽ`
* `multistate-lighter-format` -- current state indicator format. Defaults to "❲%s❳"
* `multistate-run-deferred-hooks` -- control if current state hooks will be run when toggling
  `multistate-mode`. Defaults to `t`
* `multistate-suppress-no-digits`  -- do not use digits and minus sign as prefix arguments in
  `multistate-suppress-map` If set to `nil`, digits will be aliases for `C-<digit>`
  `universal-argument` functions. Defaults to `nil`

## `use-package` example ##

It's convenient to use [`use-package`](https://github.com/jwiegley/use-package) for state
definition.

The following code recreates some of `evil-mode` states keybindings (just enough to showcase
`multistate-mode`).

*Note:* in this example <kbd>`</kbd> key is used instead of <kbd>ESC</kbd> to return to normal state.

```emacs-lisp
(use-package multistate
  :custom
  (multistate-global-mode t)
  :hook
  ;; enable selection is Visual state
  (multistate-visual-state-enter . (lambda () (set-mark (point))))
  (multistate-visual-state-exit .  deactivate-mark)
  ;; enable overwrite-mode in Replace state
  (multistate-replace-state-enter . overwrite-mode)
  (multistate-replace-state-exit .  (lambda () (overwrite-mode 0)))
  :config
  ;; Emacs state
  (multistate-define-state 'emacs :lighter "E")
  ;; Insert state
  (multistate-define-state
   'insert
   :lighter "I"
   :cursor 'bar
   :parent 'multistate-emacs-state-map)
  ;; Normal state
  (multistate-define-state
   'normal
   :default t
   :lighter "N"
   :cursor 'hollow
   :parent 'multistate-suppress-map)
  ;; Replace state
  (multistate-define-state
   'replace
   :lighter "R"
   :cursor 'hbar)
  ;; Motion state
  (multistate-define-state
   'motion
   :lighter "M"
   :cursor 'hollow
   :parent 'multistate-suppress-map)
  ;; Visual state
  (multistate-define-state
   'visual
   :lighter "V"
   :cursor 'hollow
   :parent 'multistate-motion-state-map)
  ;; Enable multistate-mode globally
  (multistate-global-mode 1)
  :bind
  (:map multistate-emacs-state-map
        ("C-z" . multistate-normal-state))
  (:map multistate-insert-state-map
        ("`" . multistate-normal-state))
  (:map multistate-normal-state-map
        ("C-z" . multistate-emacs-state)
        ("i" . multistate-insert-state)
        ("R" . multistate-replace-state)
        ("v" . multistate-visual-state)
        ("m" . multistate-motion-state)
        ("/" . search-forward)
        ("?" . search-backward)
        ("x" . delete-char)
        ("X" . backward-delete-char))
  (:map multistate-motion-state-map
        ("`" . multistate-normal-state)
        ("h" . backward-char)
        ("j" . next-line)
        ("k" . previous-line)
        ("l" . forward-char)
        ("^" . move-beginning-of-line)
        ("$" . move-end-of-line)
        ("gg" . beginning-of-buffer)
        ("G" . end-of-buffer))
  (:map multistate-replace-state-map
        ("`" . multistate-normal-state)))
```

