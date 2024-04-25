(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-safe-themes
   '("34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "75b2a02e1e0313742f548d43003fcdc45106553af7283fb5fad74359e07fe0e2" "e87fd8e24e82eb94d63b1a9c79abc8161d25de9f2f13b64014d3bf4b8db05e9a" "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "3cdd0a96236a9db4e903c01cb45c0c111eb1492313a65790adb894f9f1a33b2d" "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098" default))
 '(ignored-local-variable-values
   '((lsp-julia-default-environment . "/nix/store/bwph9lp2qpakklf001y8wgs74ylyq240-julia-depot/project/")))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(safe-local-variable-values
   '((julia-snail-executable . "/nix/store/g7cy2rkkns8i07izk4rd8qgp3glmfy3b-julia-1.10.0-env/bin/julia")
     (lsp-julia-default-environment . "/nix/store/3z1yrhi41n3ifx14sv2vq2wycj7lip9r-julia-depot/project/")
     (lsp-julia-default-environment . "/nix/store/007kg7vgm7rh4hjkl8g4gm7yyaaa1sdp-julia-depot/project/")
     (lsp-julia-default-environment . "/nix/store/bln5wb3cbhy7xad3k9w6yvav0fng2wif-julia-depot/project/")
     (lsp-julia-default-environment . "/nix/store/l1jbpacnjhskidylrpncasivhjpcyr71-julia-depot/project/")
     (lsp-julia-default-environment . "/nix/store/asv54j1hs6n2pgzg3nrnbklk8qfs3i7s-julia-depot/project/")
     (julia-snail-executable . "/nix/store/pqwcp88n6zzxbn6d6h0vqfrq6nlmiq87-julia-1.10.0-env/bin/julia")
     (lsp-julia-default-environment . "/nix/store/cgah3w678d7i9gnbiz4f2zras3gd0ai3-julia-depot/project/")
     (eval add-hook 'after-save-hook
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
 '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
(put 'customize-variable 'disabled nil)
(put 'customize-group 'disabled nil)
