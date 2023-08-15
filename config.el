;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Fira Code" :size 16)
      doom-variable-pitch-font (font-spec :family "Inter" :size 18))

(setq user-full-name "Struan Robertson"
      user-mail-address "contact@struanrobertson.co.uk")

(setq doom-theme 'doom-nord)

(setq display-line-numbers-type 'relative)
(remove-hook! 'text-mode-hook
  #'display-line-numbers-mode)

;; Add redo command
(after! undo-fu
  (map! :map undu-fu-mode-map "C-?" #'undu-fu-only-redo))

;; Re-enable auto save and backup files
(setq auto-save-default t
      make-backup-files t)

;; Disable exit confirmation
(setq confirm-kill-emacs nil)

;; Disable massive lsp docs
(setq lsp-ui-doc-enable nil)

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
  "Open today's journal file or create it if it doesn't exits."
 "Open today's journal file or create it if it doesn't exist."
  (interactive)
  (let ((journal-dir "/home/struan/Sync/Notes/daily/")
        (today-filename (format-time-string "%Y-%m-%d.org")))
    (let ((full-path (concat journal-dir today-filename)))
      (if (not (file-exists-p full-path))
          (write-region (concat "#+TITLE: " (format-time-string "%Y-%m-%d") "\n\n* ") nil full-path))
        (find-file full-path))))

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
         (function my/journal-file-title-setup))))

  ;; Global bibliography
  (setq org-cite-global-bibliography '("/home/struan/Sync/Roam/biblio.bib"))

  ;; Jupyter-python settings
  (setq! org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                        (:kernel . "python3")))
  ;; Fix ansi colors returned from Jupyter kernel
  (add-hook 'org-babel-after-execute-hook #'my/display-ansi-colors)

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  ;; Emacs Chess
  ;; https://web.archive.org/web/20230321080457/https://gist.github.com/salehmu/4188cac7d0d034eda6ea99160e94dc57
  ;; Called by org-babel-execute-src-block
  ;; (defun org-babel-execute:chess (body params)
  ;;   "Display a FEN code with org-babel"
  ;;   (let* ((output-file (concat (file-name-sans-extension (buffer-file-name)) (format-time-string "%Y-%m-%d_%H-%M-%S")) ))
  ;;         (cmd (format "python ~/.doom.d/bin/elchess.py %s %s" body output-file)))
  ;;   (shell-command cmd)
  ;;   (org-babel-result-to-file (file-name-nondirectory output-file)))

  ;; (setq org-babel-default-header-args:chess
  ;;       '((:results . "raw")))
  ;;

(defun org-babel-execute:chess (body params)
  "Execute a block of Chess code with org-babel.
This function is called by `org-babel-execute-src-block'."

  (unless (file-exists-p ".chess")
    (make-directory ".chess" t))

  (let* ((output-file (expand-file-name (format "%s.svg" (secure-hash 'sha1 body)) "./.chess"))
         (cmd (format "python ~/.doom.d/bin/elchess.py \"%s\" \"%s\" " body output-file)))
    (message cmd)
    (shell-command cmd)
    (org-babel-result-to-file output-file)))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((chess . t)))

  )

;; Org export to pdf
(map! :map org-mode-map
         "M-p"  'org-latex-export-to-pdf)

;; Didnt seem to work when coming after citar
(use-package! org-roam
  :after org
  :config (setq org-roam-directory "/home/struan/Sync/Roam")

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

;; Org-roam config

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

(setq lsp-julia-package-dir nil)
(setq lsp-julia-flags `("-J/home/struan/languageserver.so"))
