;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Use jupyter-kernel in Org babel source blocks
(package! jupyter)

;; Unpin org-roam for use with org-roam-ui
(unpin! org-roam)

;; UI for visualising org-roam
(package! org-roam-ui)

;; Automatic toggling of LaTeX fragments
(package! org-fragtog)

;; Automaticaly toggle hiding emphesis markers
(package! org-appear)

;; Integrate citar with org-roam
(package! citar-org-roam)

;; Yuck mode for editing eww files
(package! yuck-mode)

;; Grammarly LSP
(package! lsp-grammarly)

;; Just mode syntax highlighting
(package! just-mode)
