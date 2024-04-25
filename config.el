;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.config/doom/config")

;; Org Config

(load "org-config")

;;;;;;;;;;;;;;;; Doom Config ;;;;;;;;;;;;;;;;;;;

;; Doom fonts
(setq doom-font (font-spec :family "Fira Code" :size 16))

;; Annoyingly Inter is called "Inter Variable" on Ubuntu and "Inter" on Nix
(if (string= (system-name) "neon")
    (setq doom-variable-pitch-font (font-spec :family "Inter Variable" :size 18))
  (setq doom-variable-pitch-font (font-spec :family "Inter" :size 18)))

;; Use Fish shell
(setq shell-file-name (executable-find "bash"))
(setq-default vterm-shell (executable-find "fish"))
(setq-default explicit-shell-file-name (executable-find "fish"))

;; Spelling dictionary
(setq ispell-dictionary "british")
(setq ispell-personal-dictionary "/home/struan/Sync/personal_dictionary.pws")

;; User information
(setq user-full-name "Struan Robertson"
      user-mail-address "contact@struanrobertson.co.uk")

;; Doom theme
;; (setq doom-theme 'doom-nord)
(use-package! doom-nano-modeline
  :config
  (doom-nano-modeline-mode 1)
  (global-hide-mode-line-mode 1))


(after! doom-themes
  (load-theme 'doom-nano-dark t))

;; Lets try use text based searching rather than line numbers
(setq display-line-numbers-type nil)

;; Dont display line numbers in text mode
(remove-hook! 'text-mode-hook
  #'display-line-numbers-mode)

;; Add redo command
(after! undo-fu
  (map! :map undu-fu-mode-map "C-?" #'undu-fu-only-redo))

;; Re-enable auto save and backup files
(setq auto-save-default t
      make-backup-files t)

;; Auto add file to recent files list when using systemd service
(add-hook 'find-file-hook 'recentf-save-list)

;; Auto save bookmarks file
(defun bookmark-and-save ()
  "Bookmark location and then save to bookmarks file"
  (interactive)
  (bookmark-set)
  (bookmark-save))
(map! :leader "b m" #'bookmark-and-save)

;; Disable exit confirmation
(setq confirm-kill-emacs nil)

;; Load ssh-agent for laptop
;; (if (string= (system-name) "nixlaptop")
;;     (setenv "SSH_AUTH_SOCK" "/run/user/1000/ssh-agent.socket"))

;; Extend time before autocomplete
(after! company
  (setq company-idle-delay nil))

;; Disable invasive lsp-mode features
(after! lsp-ui
  (setq lsp-ui-sideline-enable nil  ; no more useful than flycheck
        lsp-ui-doc-enable nil))     ; redundant with K

;; Allow evil-escape in vterm
(after! evil
  (setq evil-escape-excluded-major-modes '(neotree-mode treemacs-mode)))

;; Window resizing hydra
(defhydra doom-window-resize-hydra (:hint nil)
  "
             _k_ increase height
_h_ decrease width    _l_ increase width
             _j_ decrease height
"
  ("h" evil-window-decrease-width)
  ("j" evil-window-increase-height)
  ("k" evil-window-decrease-height)
  ("l" evil-window-increase-width)

  ("q" nil))

(map!
 (
  :leader
  :prefix "w"
  :desc "Hydra resize" :n "SPC" #'doom-window-resize-hydra/body))

;;Use evil bindings to control vterm

(defun +press/down-key ()
  (interactive)
  (vterm-send-key "<down>"))

(defun +press/up-key ()
  (interactive)
  (vterm-send-key "<up>"))

(defun +press/left-key ()
  (interactive)
  (vterm-send-key "<left>"))

(defun +press/right-key ()
  (interactive)
  (vterm-send-key "<right>"))

(map! :after vterm
      :map vterm-mode-map
      :ni "C-k" #'+press/up-key
      :ni "C-j" #'+press/down-key
      :ni "C-h" #'+press/left-key
      :ni "C-l" #'+press/right-key)

;; Debugging
(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(map! :map dap-mode-map
      :leader
      :prefix ("d" . "dap")
      ;; basics
      :desc "dap next"          "n" #'dap-next
      :desc "dap step in"       "i" #'dap-step-in
      :desc "dap step out"      "o" #'dap-step-out
      :desc "dap continue"      "c" #'dap-continue
      :desc "dap hydra"         "h" #'dap-hydra
      :desc "dap debug restart" "r" #'dap-debug-restart
      :desc "dap debug"         "s" #'dap-debug

      ;; debug
      :prefix ("dd" . "Debug")
      :desc "dap debug recent"  "r" #'dap-debug-recent
      :desc "dap debug last"    "l" #'dap-debug-last

      ;; eval
      :prefix ("de" . "Eval")
      :desc "eval"                "e" #'dap-eval
      :desc "eval region"         "r" #'dap-eval-region
      :desc "eval thing at point" "s" #'dap-eval-thing-at-point
      :desc "add expression"      "a" #'dap-ui-expressions-add
      :desc "remove expression"   "d" #'dap-ui-expressions-remove

      :prefix ("db" . "Breakpoint")
      :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
      :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
      :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
      :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message)

;;;;;;;;;;;;;;;; Avy Config ;;;;;;;;;;;;;;;;;;;

;; Use avy to navigate through all open windows
(setq avy-all-windows t)

;; Replace goto-line with avy-goto-line as it is more flexible and can use numbers anyway
(map! "M-g g" #'avy-goto-line)

;;Unmap evil keys
(map! :after evil
      :map evil-scroll-page-down
      "C-f" nil)
;; Avy goto char
(map!
 "C-f" #'avy-goto-char-2
 :nv "C-f" #'avy-goto-char-2)

;;;;;;;;;;;;;;;; Julia Config ;;;;;;;;;;;;;;;;;;;

;; (setq lsp-julia-package-dir nil)
;; (setq lsp-julia-flags `("-J/home/struan/Development/Julia/languageserver.so"))
(setenv "JULIA_NUM_THREADS" "auto")
(after! lsp-julia
  (setq lsp-julia-default-environment "~/.julia/environments/v1.10"))

;; (after! julia-repl
;; (julia-repl-set-terminal-backend 'vterm))

;;;;;;;;;;;;;;;; Python Config ;;;;;;;;;;;;;;;;;;;
(setenv "PIPENV_VERBOSITY" "-1")
