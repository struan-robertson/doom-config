;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;; Doom Config ;;;;;;;;;;;;;;;;;;;

;; Doom fonts
(setq doom-font (font-spec :family "Fira Code" :size 16)
      doom-variable-pitch-font (font-spec :family "Inter" :size 18))

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
(setq doom-theme 'doom-nord)

;; Display relative line numbers
(setq display-line-numbers-type 'relative)

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

;; Disable massive lsp docs
(setq lsp-ui-doc-enable nil)

;; Load ssh-agent
;; TODO: hacky, should really make systemd import the variable
(setenv "SSH_AUTH_SOCK" "/run/user/1000/ssh-agent")

;;;;;;;;;;;;;;;; Org Config ;;;;;;;;;;;;;;;;;;;

;; Default notes directory
(setq org-directory "~/Sync/Notes")

(defun my/display-ansi-colors ()
  "Render Jupyter ansi output correctly"
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun my/journal-file-title-setup ()
  "Return the appropriate string for a new journal file."
  (let ((filename (format-time-string "%Y-%m-%d.org")))
    (if (file-exists-p (concat "~/Sync/Notes/daily/" filename))
        "\n* %?" ; If file exists
        "#+TITLE: %<%Y-%m-%d>\n\n* %?" ; If file does not exist
      )))

(defun my/open-journal-today ()
 "Open today's journal file or create it if it doesn't exist."
  (interactive)
  (let ((journal-dir "/home/struan/Sync/Notes/daily/")
        (today-filename (format-time-string "%Y-%m-%d.org")))
    (let ((full-path (concat journal-dir today-filename)))
      (if (not (file-exists-p full-path))
          (write-region (concat "#+TITLE: " (format-time-string "%Y-%m-%d") "\n\n* ") nil full-path))
        (find-file full-path))))

(defun org-babel-execute:chess (body)
  "Execute a block of Chess code with org-babel.
This function is called by `org-babel-execute-src-block'."

  (unless (file-exists-p ".chess")
    (make-directory ".chess" t))

  (let* ((output-file (expand-file-name (format "%s.svg" (secure-hash 'sha1 body)) "./.chess"))
         (cmd (format "python ~/.doom.d/bin/elchess.py \"%s\" \"%s\" " body output-file)))
    (message cmd)
    (shell-command cmd)
    (org-babel-result-to-file output-file)))

(after! org

  ;;Appearence
  (custom-set-faces!
    '(org-document-title :height 1.3)
    '(org-level-1 :inherit outline-1 :weight extra-bold :height 1.4)
    '(org-level-2 :inherit outline-2 :weight bold :height 1.15)
    '(org-level-3 :inherit outline-3 :weight bold :height 1.12)
    '(org-level-4 :inherit outline-4 :weight bold :height 1.09)
    '(org-level-5 :inherit outline-5 :weight semi-bold :height 1.06)
    '(org-level-6 :inherit outline-6 :weight semi-bold :height 1.03)
    '(org-level-7 :inherit outline-7 :weight semi-bold)
    '(org-level-8 :inherit outline-8 :weight semi-bold))
  (setq! org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-hide-leading-stars nil
        org-startup-indented nil
        org-startup-folded t
        org-startup-with-latex-preview t
        org-edit-src-content-indentation 0
        org-src-window-setup 'current-window)

  ;; Automatically use mixed pitch mode
  (add-hook 'org-mode-hook 'mixed-pitch-mode)

  ;; Open journal
  (map! :leader
        "nj" #'my/open-journal-today)

  ;; Capture templates
  (setq org-capture-templates
        '(("j" "Journal Entry"
           plain (file (lambda () (concat "/home/struan/Sync/Notes/daily/"
                                          (format-time-string "%Y-%m-%d.org"))))
         (function my/journal-file-title-setup))
          ("m" "Minutes"
           plain (file (lambda () (format-time-string "/home/struan/Sync/Notes/Projects/Doctorate/Minutes/%Y-%m-%d/minutes.org")))
           "#+TITLE: Minutes\n#+DATE: %U\n#+SUBTITLE: %<%d-%m-%Y>\n#+OPTIONS: toc:nil\n\n *Struan, %? in attendance* \n\n\n\n* TODO Actions for next meeting [/]\n  - [ ] ")))

  ;; Global bibliography
  (setq org-cite-global-bibliography '("/home/struan/Sync/Roam/biblio.bib"))

  ;; Jupyter-python settings
  (setq! org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                        (:kernel . "python3")))
  ;; Fix ansi colors returned from Jupyter kernel
  (add-hook 'org-babel-after-execute-hook #'my/display-ansi-colors)

  ;; Scale latex
  ;;(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  ;; Load "chess" language to allow for chess boards to be rendered
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((chess . t)))

  ;; Recursively search for agenda items within the notes directory
  ;; Shouldnt pose much of a performance issue as the Notes directory wont be as large as roam.
  (defun refresh-org-agenda-files ()
    (setq org-agenda-files
          (directory-files-recursively "~/Sync/Notes" org-agenda-file-regexp)))
  (add-hook 'org-agenda-mode-hook #'refresh-org-agenda-files)

)

;; Org-roam-ui
(use-package! websocket
    :after org-roam)
(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


;; Org export to pdf
(map! :map org-mode-map
         "M-p"  'org-latex-export-to-pdf)

;; Org-Roam config
(use-package! org-roam
  :after org
  :config
        ;;Directory containing roam files
        (setq org-roam-directory "/home/struan/Sync/Roam")

        ;; https://jethrokuan.github.io/org-roam-guide/
        (setq org-roam-capture-templates
              '(("m" "main" plain "%?"
                :if-new (file+head "main/${slug}.org"
                                   "#+title: ${title}\n")
                :immediate-finish t
                :unnarrowed t)

                ("r" "literature note" plain "%?"
                :if-new (file+head "reference/${title}.org" "#+title: ${title}\n")
                :immediate-finish t
                :unnarrowed t)
                ("n" "literature note (citation)" plain "%?"
                    :if-new (file+head
                    "reference/${citar-citekey}.org"
                    "#+title: ${note-title}\n\n[cite:@${citar-citekey}]")
                    :immediate-finish t
                    :unnarrowed t)))

                (cl-defmethod org-roam-node-type ((node org-roam-node))
                "Return the TYPE of NODE."
                        (condition-case nil
                                (file-name-nondirectory
                                        (directory-file-name
                                                (file-name-directory
                                                        (file-relative-name (org-roam-node-file node) org-roam-directory))))
                                (error "")))

                (setq org-roam-node-display-template
                        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag))))

;; Citar config
(use-package! citar
  :after org
  :config (setq
           citar-bibliography '("/home/struan/Sync/Roam/biblio.bib")
           citar-notes-path '("/home/struan/Sync/Roam/reference/")))
(use-package! citar-org-roam
  :after citar org-roam
  :config (citar-org-roam-mode
           (setq citar-org-roam-note-title-template "${title}"
                 citar-org-roam-capture-template-key "n")))

;; Automatically enter fragtog mode
(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode)
  )

;; Automatically enter appear mode
(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config (setq
           org-appear-autolinks nil
           org-appear-autoentities t
           org-appear-autosubmarkers t ))


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

(setq lsp-julia-package-dir nil)
(setq lsp-julia-flags `("-J/home/struan/Sync/bin/languageserver.so"))
