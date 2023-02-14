(require 'org)
; (org-babel-load-file (concat user-emacs-directory "README.org"))
(org-babel-load-file "~/pure-emacs/README.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-quick-access-keys '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
 '(company-quick-access-modifier 'super)
 '(custom-safe-themes
   '("443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "2853dd90f0d49439ebd582a8cbb82b9b3c2a02593483341b257f88add195ad76" "bbdefca19798136ead944525aa6bca77f6877baacf9ac62b801aceebfeb7539c" "121abce177018221286ce0336c7df5ad0dd67787f82b19c0b7f7b1e37bdd97dc" "545ab1a535c913c9214fe5b883046f02982c508815612234140240c129682a68" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "1cae4424345f7fe5225724301ef1a793e610ae5a4e23c023076dc334a9eb940a" "4d8fd45c4d6d2df0b93e11820b5e45208c8874cea1cbecab4f8791fab70e909a" "3fdf9702a9c361c411bd9b2b0f04023821682a128249367cdad55e7c302f58df" "946e5ebd393de2d019b796e74523e98304a63ff40f8e531952400b51a32fa288" "ddcbca57afcb429dc36de557df1b2bc7fc7855f24365294b5604ba2536dcb0e2" "1254717caa5d89e9b1e190547f6cd2ca24c8d11b9d1685dafdce5d996cc61485" "56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "8d8207a39e18e2cc95ebddf62f841442d36fcba01a2a9451773d4ed30b632443" "1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5" "7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184" "a9abd706a4183711ffcca0d6da3808ec0f59be0e8336868669dc3b10381afb6f" "8b6506330d63e7bc5fb940e7c177a010842ecdda6e1d1941ac5a81b13191020e" "0c08a5c3c2a72e3ca806a29302ef942335292a80c2934c1123e8c732bb2ddd77" "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "70b596389eac21ab7f6f7eb1cf60f8e60ad7c34ead1f0244a577b1810e87e58c" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" "a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" "d0dfb465b2bb36c368f2a54747421707275fde385aee85b6b0ad4347f9ca137e" "17ae702ef0825f1582e43bff838a9c688877809b6c40597de262bfa8fead9d5c" "d6eb6ea68990ea35ebcf411bc1d77b0d32ea9313c21ce0678e85cb442f07cd4a" "1f29818e1b961ad3b7720ac4dacf0ce9fb7af37d9f8ae17a812edc874589151a" "d4a02820fada527bad6414f1ba432d87155fa627f3111a226018885d3127f093" "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478" "b54376ec363568656d54578d28b95382854f62b74c32077821fdfd604268616a" "680f62b751481cc5b5b44aeab824e5683cf13792c006aeba1c25ce2d89826426" "631c52620e2953e744f2b56d102eae503017047fb43d65ce028e88ef5846ea3b" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "51c71bb27bdab69b505d9bf71c99864051b37ac3de531d91fdad1598ad247138" "a589c43f8dd8761075a2d6b8d069fc985660e731ae26f6eddef7068fece8a414" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "a44e2d1636a0114c5e407a748841f6723ed442dc3a0ed086542dc71b92a87aee" "e7820b899036ae7e966dcaaec29fd6b87aef253748b7de09e74fdc54407a7a02" "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "180641b59315dcbf610ac82acbd80eb878ed969ba57a371345b4c58cff35d309" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "bdf2b5b617ea284bf2635f28509b3a5ce3fb1ed82cac00196a71cd8403fa3e60" "e8808c6a799296c4a49355880ebc14eb319bae39c3d1637c0c05f315e0f8f4df" default))
 '(mini-frame-show-parameters '((top . 10) (width . 0.7) (left . 0.5)))
 '(openai-key "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")
 '(package-selected-packages
   '(k8s-mode package-lint bm vc-use-package google-translate hotfuzz epc ox-gfm doom-themes org-modern prettier org-make-toc jump-tree org-superstar vterm-toggle undo-fu dockerfile-mode consult-projectile nano-theme dashboard flycheck go-playground folding no-littering npm ob-dart treesit-auto direnv lsp-ltex persistent-scratch git ob-restclient string-inflection material-theme python-mode nodejs-repl nano-modeline js-doc json-mode auto-dark doom-lib general emms-player-simple-mpv vimish-fold workgroups evil-commentary esup code-review evil-cleverparens all-the-icons-completion which-key lispy gist yasnippet-snippets auto-rename-tag evil-matchit floobits rustic rainbow-mode copilot evil-surround lsp-grammarly undo-tree magit-todos emmet-mode all-the-icons-dired tree-edit pug-mode spell-fu org-fancy-priorities jist ts-fold elisp-format wakatime-mode ob-async mini-frame undo-fu-session nginx-mode docstr reverse-im embark-consult marginalia ng2-mode jinja2-mode doom-modeline pocket-reader format-all persistent-soft lsp-ui git-timemachine grip-mode jenkinsfile-mode codegpt plantuml-mode request-deferred org-noter-pdftools lsp-pyright web-mode fnhh flutter origami orderless lsp-dart lua-mode helpful multi-vterm rainbow-delimiters cider reveal-in-osx-finder turbo-log ts-comint vertico-posframe wgrep org-preview-html paren-face yaml-pro jumplist quelpa-use-package kubernetes-evil ns-auto-titlebar package-build elisp-autofmt git-gutter-fringe org-roam-ui ob-typescript autothemer deadgrep file-info persp-mode-project-bridge git-link pipenv kind-icon blamer evil-collection fill-column-indicator company-box docker-compose-mode better-jumper dirvish))
 '(safe-local-variable-values
   '((eval when
           (and
            (buffer-file-name)
            (not
             (file-directory-p
              (buffer-file-name)))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (require 'package-recipe-mode nil t)
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require 'package-recipe-mode)))
           (unless
               (derived-mode-p 'emacs-lisp-mode)
             (emacs-lisp-mode))
           (package-build-minor-mode)
           (setq-local flycheck-checkers nil)
           (set
            (make-local-variable 'package-build-working-dir)
            (expand-file-name "../working/"))
           (set
            (make-local-variable 'package-build-archive-dir)
            (expand-file-name "../packages/"))
           (set
            (make-local-variable 'package-build-recipes-dir)
            default-directory)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dap-ui-pending-breakpoint-face ((t (:underline "dim gray"))))
 '(dap-ui-verified-breakpoint-face ((t (:underline "green"))))
 '(org-document-title ((t (:inherit outline-1 :height 2.5))))
 '(org-level-1 ((t (:inherit outline-1 :height 2.0))))
 '(org-level-2 ((t (:inherit outline-2 :height 2.0))))
 '(org-level-3 ((t (:inherit outline-3 :height 2.0))))
 '(org-level-4 ((t (:inherit outline-4 :height 2.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 2.0))))
 '(show-paren-mismatch ((t (:foreground "#FF3399")))))
(put 'narrow-to-region 'disabled nil)
