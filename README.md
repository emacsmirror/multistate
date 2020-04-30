`multistate-mode` is basically an [`evil-mode`](https://github.com/emacs-evil/evil) without
[vi](https://www.vim.org/).
It allows to define states for modal editing.
Unlike `evil-mode` it doesn't come with predefined states and key bindings, doesn't set hooks but
allows you to configure your system from the ground up.

**Disclaimer** `multistate-mode` states are not buffer local and probably wont be until Emacs 37 arrival.

## How does it work ##

Emacs minor-mode key bindings
[overwrite](https://www.gnu.org/software/emacs/manual/html_node/elisp/Searching-Keymaps.html#Searching-Keymaps)
global key bindings.
By setting minor mode keymap user may overwrite global `self-insert-command` keybindings with other
functions.
This doesn't overwrite actual global key bindings but only shadows them.
Thus, user has Emacs operating in *Normal mode* when this minor mode is enabled and in *Insert mode*
when it is disabled (look at [`modalka-mode`](https://github.com/mrkkrp/modalka) readme for more verbose
description of this concept).
`multistate-mode` takes it one step further by allowing user to define a set of minor mode keymaps.
These keymaps are swapped in place of minor mode keymap when corresponding state is enabled thus
allowing modal editing with multiple states.

## Installation ##

Package is available [MELPA](https://melpa.org/#/) archive.
To install it, type <kbd>M-x package-install RET multistate RET</kbd>.

## Initial state ##

There's no predefined *default* state.
With no states defined `multistate-mode` useless.

## State definition ##

In order to define state use function

```emacs-lisp
(multistate-define-state NAME [LIGHTER] [CURSOR] [PARENT])
```

The only required function argument is `NAME`.
This is a symbol that will be used as a base name for you state.

For example,

```emacs-lisp
(multistate-define-state 'visual)
```

will create `visual` state.

Optional `LIGHTER` string will be used as a state indicator.
Optional `CURSOR` argument will control cursor style when state is enabled. See documentation for
`cursor-type` variable to learn about possible `CURSOR` values.
Optional `PARENT` argument allows to specify [parent
keymap](https://www.gnu.org/software/emacs/manual/html_node/elisp/Inheritance-and-Keymaps.html#Inheritance-and-Keymaps)
for the current state. This will be discussed in more detail later.

For example,

```emacs-lisp
(multistate-define-state
    'motion
    :lighter "M"
    :cursor 'hollow
    :parent 'multistate-suppress-map)
```

will create `motion` state, that is indicated my `M` lighter and hollow cursor.
All global `self-insert-command` definitions will be overwritten as `undefined`.

## Switching states ##

Call to `multistate-define-state` will create two functions:

* `multistate-<name>-state`
* `multistate-<name>-state-p`

where `<name>` is a name of the state.
In example above these names would be `multistate-visual-state` and `multistate-visual-state-p`.

The first function `multistate-<name>-state` will switch state to `<name>`, the second function
`multistate-<name>-state-p` will test if current state is `<name>`.

## Hooks ##

Call to `multistate-define-state` will also create two hooks:

* `multistate-<name>-state-enter-hook`
* `multistate-<name>-state-exit-hook`

Use `add-hook` and `remove-hook` to add or remove functions from these hooks.
These hooks will be executed upon entering and exiting state respectively.
`multistate-run-deffered-hooks` customization option controls if hooks will be run when
`multistate-mode` is toggled.

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
* `multistate-manage-cursor` -- if set to `t` `multistate-mode` will change cursor appearance
  according to `:cursor` argument. If set to `nil`, cursor appearance will not be altered. Defaults
  to `t`
* `multistate-run-deferred-hooks` -- control if current state hooks will be run when toggling
  `multistate-mode`. Defaults to `t`
* `multistate-suppress-no-digits`  -- do not use digits and minus sign as prefix arguments in
  `multistate-suppress-map` If set to `nil`, digits will be aliases for `C-<digit>`
  `universal-argument` functions. Defaults to `t`

## `use-package` example ##

It's convenient to use [`use-package`](https://github.com/jwiegley/use-package) for state
definition.

The following code recreates some of `evil-mode` states keybindings (just enough to showcase
`multistate-mode`).

*Note:* in this example <kbd>`</kbd> key is used instead of <kbd>ESC</kbd> to return to normal state.

```emacs-lisp
(use-package multistate
  :demand
  :hook
  ;; enable selection is Visual state
  (multistate-visual-state-enter . (lambda () (set-mark (point))))
  (multistate-visual-state-exit .  deactivate-mark)
  ;; enable overwrite-mode in Replace state
  (multistate-replace-state-enter . overwrite-mode)
  (multistate-replace-state-exit .  (lambda () (overwrite-mode 0)))
  :config
  ;; Emacs/Insert state
  (multistate-define-state 'emacs :lighter "E")
  ;; Normal state
  (multistate-define-state
   'normal
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
  ;; Enable Normal state at startup
  (multistate-normal-state)
  ;; Enable multistate-mode globally
  (multistate-global-mode 1)
  :bind
  (:map multistate-emacs-state-map
        ("C-z" . multistate-normal-state))
  (:map multistate-normal-state-map
        ("C-z" . multistate-emacs-state)
        ("i" . multistate-emacs-state)
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

