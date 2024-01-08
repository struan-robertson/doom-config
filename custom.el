(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("75b2a02e1e0313742f548d43003fcdc45106553af7283fb5fad74359e07fe0e2" "e87fd8e24e82eb94d63b1a9c79abc8161d25de9f2f13b64014d3bf4b8db05e9a" "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "3cdd0a96236a9db4e903c01cb45c0c111eb1492313a65790adb894f9f1a33b2d" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
      (lambda nil
        (if
            (y-or-n-p "Tangle?")
            (org-babel-tangle)))
      nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:height 1.3))))
 '(org-level-1 ((t (:inherit outline-1 :weight extra-bold :height 1.4))))
 '(org-level-2 ((t (:inherit outline-2 :weight bold :height 1.15))))
 '(org-level-3 ((t (:inherit outline-3 :weight bold :height 1.12))))
 '(org-level-4 ((t (:inherit outline-4 :weight bold :height 1.09))))
 '(org-level-5 ((t (:inherit outline-5 :weight semi-bold :height 1.06))))
 '(org-level-6 ((t (:inherit outline-6 :weight semi-bold :height 1.03))))
 '(org-level-7 ((t (:inherit outline-7 :weight semi-bold))))
 '(org-level-8 ((t (:inherit outline-8 :weight semi-bold)))))
(put 'customize-variable 'disabled nil)
(put 'customize-group 'disabled nil)
