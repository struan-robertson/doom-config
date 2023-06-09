(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" "016f665c0dd5f76f8404124482a0b13a573d17e92ff4eb36a66b409f4d1da410" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" default))
 '(ein:output-area-inlined-images t)
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
      (lambda nil
        (if
            (y-or-n-p "Tangle?")
            (org-babel-tangle)))
      nil t)))
 '(warning-suppress-types '((org-element-cache) (org-element-cache) (defvaralias))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block (nil :foreground nil :inherit 'fixed-pitch))
 '(org-checkbox (nil :inherit 'fixed-pitch))
 '(org-code (nil :inherit '(shadow fixed-pitch)))
 '(org-document-title ((t (:height 1.3))))
 '(org-level-1 ((t (:inherit outline-1 :weight extra-bold :height 1.4))))
 '(org-level-2 ((t (:inherit outline-2 :weight bold :height 1.15))))
 '(org-level-3 ((t (:inherit outline-3 :weight bold :height 1.12))))
 '(org-level-4 ((t (:inherit outline-4 :weight bold :height 1.09))))
 '(org-level-5 ((t (:inherit outline-5 :weight semi-bold :height 1.06))))
 '(org-level-6 ((t (:inherit outline-6 :weight semi-bold :height 1.03))))
 '(org-level-7 ((t (:inherit outline-7 :weight semi-bold))))
 '(org-level-8 ((t (:inherit outline-8 :weight semi-bold))))
 '(org-meta-line (nil :inherit '(font-lock-comment-face fixed-pitch)))
 '(org-special-keyword (nil :inherit '(font-lock-comment-face fixed-pitch)))
 '(org-table (nil :inherit '(shadow fixed-pitch)))
 '(org-verbatim (nil :inherit '(shadow fixed-pitch)))
 '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
(put 'customize-variable 'disabled nil)
(put 'customize-group 'disabled nil)
