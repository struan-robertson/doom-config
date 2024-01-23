;;; $DOOMDIR/org-config.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;; Org Config ;;;;;;;;;;;;;;;;;;;

;; Default notes directory
(setq org-directory "~/Sync/Notes")

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

;; Auto tag latex equation with correct number
;; Credit: https://kitchingroup.cheme.cmu.edu/blog/2016/11/07/Better-equation-numbering-in-LaTeX-fragments-in-org-mode/
(defun org-renumber-environment (orig-func &rest args)
  (let ((results '())
        (counter -1)
        (numberp))

    (setq results (cl-loop for (begin .  env) in
                           (org-element-map (org-element-parse-buffer) 'latex-environment
                             (lambda (env)
                               (cons
                                (org-element-property :begin env)
                                (org-element-property :value env))))
                           collect
                           (cond
                            ((and (string-match "\\\\begin{equation}" env)
                                  (not (string-match "\\\\tag{" env)))
                             (cl-incf counter)
                             (cons begin counter))
                            ((string-match "\\\\begin{align}" env)
                             (prog2
                                 (cl-incf counter)
                                 (cons begin counter)
                               (with-temp-buffer
                                 (insert env)
                                 (goto-char (point-min))
                                 ;; \\ is used for a new line. Each one leads to a number
                                 (cl-incf counter (count-matches "\\\\$"))
                                 ;; unless there are nonumbers.
                                 (goto-char (point-min))
                                 (cl-decf counter (count-matches "\\nonumber")))))
                            (t
                             (cons begin nil)))))

    (when (setq numberp (cdr (assoc (point) results)))
      (setf (car args)
            (concat
             (format "\\setcounter{equation}{%s}\n" numberp)
             (car args)))))

  (apply orig-func args))

(after! org

  ;;Appearence
  (custom-theme-set-faces!
    'doom-nord
    '(org-document-title :inherit outline-1 :height 1.5 :family "Inter Display" :weight extra-bold)
    '(org-level-1 :inherit outline-1 :height 1.75 :family "Inter Display" :weight extra-bold)
    '(org-level-2 :inherit outline-2 :height 1.15 :family "Inter Display" :weight bold)
    '(org-level-3 :inherit outline-3 :height 1.12 :family "Inter Display" :weight bold)
    '(org-level-4 :inherit outline-4 :height 1.09 :family "Inter Display" :weight bold)
    '(org-level-5 :inherit outline-5 :height 1.06 :family "Inter Display" :weight semi-bold)
    '(org-level-6 :inherit outline-6 :height 1.03 :family "Inter Display" :weight semi-bold)
    '(org-level-7 :inherit outline-7 :family "Inter Display" :weight semi-bold)
    '(org-level-8 :inherit outline-7 :family "Inter Display" :weight semi-bold))

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

  ;; Scale latex fragments for laptop, hacky but couldnt think of a way to calculate DPI
  ;; A potential fix would be to calculate DPI from Hyprland scale factor
  ;; Higher Hyprland scale = lower latex scale
  ;; (if (string= (system-name) "nixlaptop")
  (if (string= (system-name) "neon")
      (setq org-format-latex-options (plist-put org-format-latex-options :scale 0.5)))

  ;; Convert latex fragments to SVG for crispiness
  ;; Slows things down a lot so maybe dont use for desktop
  ;; Also could use normal process at a higher res and then scale down
  (setq org-preview-latex-default-process 'dvisvgm)

  ;; Auto renumber equations
  (advice-add 'org-create-formula-image :around #'org-renumber-environment)

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
;; (use-package! org-fragtog
;;   :after org
;;   :hook (org-mode . org-fragtog-mode)
;;   )

;; Automatically enter appear mode
(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config (setq
           org-appear-autolinks nil
           org-appear-autoentities t
           org-appear-autosubmarkers t ))
