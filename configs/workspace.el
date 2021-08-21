;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                     WORKSPACE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package projectile
  :ensure t
  :defer 0.1)

(use-package treemacs
  :ensure t
  :defer t
  :config
  :bind
  (:map global-map
   ("M-0"       . treemacs-select-window)
   ("C-x t 1"   . treemacs-delete-other-windows)
   ("C-x t t"   . treemacs)
   ("C-x t B"   . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package counsel
  :ensure t
  :defer 0.1
  :config
  (counsel-mode 1)
  )

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(use-package ivy
  :ensure t
  :defer 0.1
  :init
  ;; :bind (("ESC" . ivy-exit))
  :bind
  (("C-w" . backward-kill-word)
   ("C-h" . delete-backward-char)
   ("C-u" . backward-kill-line)
   :map ivy-minibuffer-map
   ;; ("C-j" . ivy-next-line)
   ;; ("C-k" . ivy-previous-line))
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (define-key ivy-minibuffer-map (kbd "<ESC>") 'minibuffer-keyboard-quit)

  (ivy-mode 1))

(use-package swiper
  :ensure t
  :defer 0.1
  :config
  (define-key swiper-map (kbd "<ESC>") 'minibuffer-keyboard-quit)

  )

(use-package selectrum
  :ensure t
  :defer 0.1
  :after ivy
  :config
  (selectrum-mode +1))


;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                                  NAVIGATION
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :defer 0.1
  :config
  (setq-default evil-kill-on-visual-paste nil)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-previous-visual-line)
  (evil-mode 1)
  )
