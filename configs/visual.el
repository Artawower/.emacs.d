
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                  VISUAL
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(scroll-bar-mode -1)
(menu-bar-mode -1)
(if window-system
    (tool-bar-mode -1)
  )
(setq inhibit-splash-screen t)
(set-frame-font "JetBrainsMono Nerd Font 14" nil t)

;; Ligatures
(defconst jetbrains-ligature-mode--ligatures
  '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
    "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
    "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
    "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
    "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
    "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
    "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
    "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
    ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
    "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
    "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
    "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
    "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
    "&="))

(sort jetbrains-ligature-mode--ligatures (lambda (x y) (> (length x) (length y))))

(dolist (pat jetbrains-ligature-mode--ligatures)
  (set-char-table-range composition-function-table
                        (aref pat 0)
                        (nconc (char-table-range composition-function-table (aref pat 0))
                               (list (vector (regexp-quote pat)
                                             0
                                             'compose-gstring-for-graphic)))))

;; (quelpa
;;  '(queue :version original :fetcher url
;;          :url "https://github.com/rougier/emacs-splash" ))
;; (quelpa '(splash-screen :repo "rougier/emacs-splash" :fetcher github))
;; (quelpa '(emacs-splash :fetcher git :url "https://github.com/rougier/emacs-splash"))
;; (use-package splash-screen
;;   :ensure t
;;   :defer 0.1)
;; (use-package splash-screen
  ;; :defer t
  ;; :ensure t
  ;; :straight (emacs-splash :type git :host github :repo "rougier/emacs-splash")
 ;; )

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-moonlight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package all-the-icons
  :ensure t
  :defer 0.5s)

(use-package doom-modeline
  :ensure t
  :defer t
  :config
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-file-name-style 'file-name)
  )

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))

(use-package ivy-posframe
  :ensure t
  :after ivy
  :diminish
  :custom-face
  (ivy-posframe-border ((t (:background "#4FAAEA"))))
  :init
  (ivy-posframe-mode 1)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display))
        ivy-posframe-height-alist '((t . 20))
        ivy-posframe-height 20
        ivy-posframe-parameters '((internal-border-width . 2) (left-fringe . 18) (right-fringe . 18) )
        )
  (defun ivy-posframe-get-size ()
    "The default functon used by `ivy-posframe-size-function'."
    (list
     :height ivy-posframe-height
     :width ivy-posframe-width
     :min-height (or ivy-posframe-min-height
                     (let ((height (+ ivy-height 1)))
                       (min height (or ivy-posframe-height height))))
     :min-width (or ivy-posframe-min-width
                    (let ((width (round (* (frame-width) 0.85))))
                      (min width (or ivy-posframe-width width))))))

  )

(use-package all-the-icons-ivy
  :ensure t
  :after ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :ensure t
  :defer 0.2)

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))
