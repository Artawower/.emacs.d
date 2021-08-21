(require 'package)
(customize-set-variable 'package-archives
                        `(,@package-archives
                          ("melpa" . "https://melpa.org/packages/")
                          ("org" . "https://orgmode.org/elpa/")
                          ("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/")
                          ))
(customize-set-variable 'package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(put 'use-package 'lisp-indent-function 1)

(use-package use-package-core
  :custom
  ;; (use-package-verbose t)
  ;; (use-package-minimum-reported-time 0.005)
  (use-package-enable-imenu-support t))

;; Quelpa
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

;; (defun my-exec-path-from-shell-initialize ()

;;   (setenv "PERL5LIB" (concat "~/perl5/lib/perl5" ":"
;;                              (getenv "PERL5LIB")))

;;   (setenv "LC_ALL" "en_US.UTF-8")
;;   (exec-path-from-shell-initialize)
;;   (exec-path-from-shell-copy-env "SSH_AGENT_PID")
;;   (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(use-package exec-path-from-shell
  :ensure t
  :defer 0.1
  :config
  (setenv "PERL5LIB" (concat "~/perl5/lib/perl5" ":"
                             (getenv "PERL5LIB")))

  (setenv "LC_ALL" "en_US.UTF-8")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
  ;; (add-hook 'after-init-hook 'my-exec-path-from-shell-initialize)
  )

(use-package gcmh
  :ensure t
  :defer 0.1
  :init
  (gcmh-mode 1))

(setq tab-always-indent nil)

(setq warning-minimum-level :emergency)

(setq backup-directory-alist '(("." . "~/.emacs-saves")))

(global-set-key (kbd "C-x C-i") 'company-tabnine)
(global-set-key (kbd "C-k") nil)

(use-package which-key
  :ensure t
  :defer 0.1
  :config
  (which-key-mode)
  )

(global-set-key (kbd "C-S-k") 'shrink-window)
(global-set-key (kbd "C-S-j") 'enlarge-window)
(global-set-key (kbd "C-S-l") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-h") 'shrink-window-horizontally)
(global-set-key (kbd "C-c l") 'smerge-keep-lower)
(global-set-key (kbd "C-c u") 'smerge-keep-upper)
(global-set-key (kbd "C-c a") 'smerge-keep-all)
(global-set-key (kbd "C-c j") 'smerge-next)
(global-set-key (kbd "C-c k") 'smerge-prev)

(global-set-key (kbd "s-e") 'emmet-expand-line)
(global-set-key (kbd "C-s") 'save-buffer)

(use-package reverse-im
  :ensure t
  :defer 0.1
  :config
  (reverse-im-activate "russian-computer"))

(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

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
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line)
   ("<ESC>" . minibuffer-keyboard-quit)
   ("C-SPC" . ivy-call-and-recenter)
   :map ivy-switch-buffer-map ("C-k" . ivy-previous-line))
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil) ;; Delete ^ at start
  (setq ivy-on-del-error-function #'ignore) ;; After backspace empty line prevent exit buffer
  (setq enable-recursive-minibuffers t)
  (setq projectile-completion-system 'ivy)
  ;; (define-key ivy-minibuffer-map (kbd "<ESC>") 'minibuffer-keyboard-quit)

  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  :init
  (ivy-rich-mode 1)
  )

(use-package smex
  :ensure t
  :after ivy
  )

(use-package swiper
  :ensure t
  :defer 0.1
  :config
  (define-key swiper-map (kbd "<ESC>") 'minibuffer-keyboard-quit)
  (global-set-key (kbd "s-f") 'swiper)
  )

;; (defun +m/find-file ()
;;   "Find file in project root"
;;   (interactive)
;;   (setq project-root-path 'projectile-project-root)
;;   (counsel-find-file "" (projectile-project-root)))

(use-package counsel
  :ensure t
  :defer 0.1
  :bind (
         :map evil-normal-state-map
         ("SPC f r" . counsel-recentf)
         ("SPC SPC" . counsel-projectile-find-file)
         ("SPC /" . counsel-projectile-rg)
         :map counsel-mode-map
         ("C-k" . nil)
         )
  :config
  (define-key counsel-mode-map (kbd "C-k") 'ivy-previous-line-or-history)
  ;; (setq counsel-ag-base-command "ag --vimgrep -a %s")
  (counsel-mode 1)
  )


(defun my-ivy-with-thing-at-point (cmd &optional dir)
  "Wrap a call to CMD with setting "
  (let ((ivy-initial-inputs-alist
         (list
          (cons cmd (thing-at-point 'symbol)))))
    (funcall cmd nil dir)))

(defun counsel-projectile-rg-at-point ()
  "Ivy version of `projectile-rg', using."
  (interactive)
  (my-ivy-with-thing-at-point
   'counsel-projectile-rg
   ))

(defun my-counsel-ag-from-here (&optional dir)
  "Start ag but from the directory the file is in (otherwise I would
        be using git-grep)."
  (interactive "D")
  (my-ivy-with-thing-at-point
   'counsel-rg
   (or dir (file-name-directory (buffer-file-name)))))

(defun my-counsel-git-grep ()
  (interactive)
  (my-ivy-with-thing-at-point
   'counsel-git-grep))

(use-package counsel-projectile
  :ensure t
  :after counsel
  :bind (
         :map evil-normal-state-map
         ;; ("SPC *" . my-counsel-ag-from-here)
         ("SPC f p" . counsel-projectile-recentf)
         ("SPC f P" . counsel-projectile-switch-project)
         ("SPC *" . my-counsel-git-grep))
  )

(use-package perspective
  :ensure t
  :defer 0.1
  :bind (:map evil-normal-state-map
              ("SPC TAB r" . persp-rename)
              ("SPC TAB n" . persp-next)
              ("SPC TAB p" . persp-prev)
              ("SPC TAB s" . persp-switch)
              ("SPC b b" . persp-ivy-switch-buffer)
              ("SPC TAB d" . persp-kill))   ; or use a nicer switcher, see below
  :config
  (persp-mode))

(use-package treemacs
  :ensure t
  :defer 0.1
  ;; :bind (:map treemacs-mode-map
  ;;             ("w" . treemacs-set-width)
  ;;             ("r" . treemacs-refresh)
  ;;             ("R" . treemacs-rename)
  ;;             ("yp" . treemacs-copy-absolute-path-at-point)
  ;;             ("SPC o p"   . treemacs)
  ;;             )
  :config
  (evil-define-key 'treemacs treemacs-mode-map (kbd "SPC o p") #'treemacs)
  (setq treemacs-width 50))


  (use-package treemacs-evil
    ;; :after (treemacs evil)
    :defer 0.1
    :ensure t
    :bind (:map evil-normal-state-map
                ("SPC o p"   . treemacs)
                ("SPC o P" . treemacs-find-file)))

  (use-package treemacs-projectile
    :after (treemacs projectile)
    :ensure t)


  (use-package treemacs-magit
    :after (treemacs magit)
    :ensure t)

(use-package projectile
  :ensure t
  :defer 0.1
  :custom
  ;; (projectile-enable-caching t)
  (projectile-track-known-projects-automatically nil)
  :init
  (projectile-global-mode +1)
  :config
  ;; (setq-default
  ;;  projectile-cache-file (expand-file-name ".projectile-cache" user-emacs-directory)
  ;;  projectile-known-projects-file (expand-file-name ".projectile-bookmarks" user-emacs-directory))
  (setq projectile-globally-ignored-files
        (append '(".pyc"
                  ".class"
                  "~")
                projectile-globally-ignored-files))
  )

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer 
        (delq (current-buffer) 
              (remove-if-not 'buffer-file-name (buffer-list)))))

(use-package evil
  :ensure t
  :defer 0.1
  :bind
  (:map evil-normal-state-map
        ("SPC ." . find-file)
        ("SPC b b" . switch-to-buffer)
        ("SPC h v" . describe-variable)
        ("SPC h f" . describe-function)
        ("SPC b O" . kill-other-buff)
        ("SPC o t" . vterm-toggle-cd)
        ("SPC t l" . global-display-line-numbers-mode)
        ("SPC RET" . counsel-bookmark)
        ("f" . avy-goto-char))
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (setq-default evil-kill-on-visual-paste nil)
  (evil-mode 1)
  )

(use-package evil-matchit
  :ensure t
  :after evil-mode
  :init
  ;; :config
  ;; (evilmi-load-plugin-rules '(mhtml-mode) '(ng2-html-mode ng2-html))
  ;; (evilmi-load-plugin-rules '(html-mode) '(ng2-html-mode ng2-html))
  )
(evilmi-load-plugin-rules '(ng2-html-mode) '(html))
(global-evil-matchit-mode 1)

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init)
  )

(use-package evil-commentary
  :after evil
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :ensure t
  :defer 0.1
  :config (global-evil-surround-mode 1))

(use-package evil-leader
  :ensure t
  :after evil
  :config
  ;; (setq evil-leader/no-prefix-mode-rx t)
  (evil-leader/set-key
    "f" 'evil-find-char
    "b" 'evilem-motion-previous-line
    "p" 'prettier-prettify
    "k" 'save-buffer-without-dtw

    "d" 'dup-debug

    "o" 'org-mode
    "q" 'kill-current-buffer
    "v" 'vterm
    "`" 'vterm-toggle-cd
    "i" 'git-messenger:popup-message
    "t" 'google-translate-at-point
    "T" 'google-translate-query-translate

    "a" 'counsel-org-agenda-headlines
    "c" 'dired-create-empty-file
    "p" 'my-format-all
    "s" 'publish-org-blog
    "g" 'dogears-go

    "h" 'lsp-ui-doc-show
    "e" 'lsp-treemacs-errors-list
    "r" 'treemacs-select-window
    )
  (global-evil-leader-mode 1)
  )

(use-package avy
  :ensure t
  :defer 0.1
  )

(use-package google-translate
  :ensure t
  :defer 0.2
  :custom
  (google-translate-backend-method 'curl)
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

(use-package bm
  :ensure t
  :defer 0.2
  :init
  (global-set-key (kbd "s-b") 'bm-toggle)
  (global-set-key (kbd "C-M-n") 'bm-next)
  (global-set-key (kbd "C-M-p") 'bm-previous)
  :config
  (setq bm-face
        '((((class grayscale)
            (background light)) (:background nil))
          (((class grayscale)
            (background dark))  (:background nil))
          (((class color)
            (background light)) (:foreground "red" :background nil))
          (((class color)
            (background dark))  (:foreground "red" :background nil)))
        )
  (define-key global-map [f8] 'bookmark-jump)
  (define-key global-map [f9] 'bookmark-set)


  (setq bookmark-default-file "~/.emacs.d/bookmarks")  ;;define file to use.
  (setq bookmark-save-flag 1)  ;save bookmarks to .emacs.bmk after each entry
  )

(setq ispell-program-name "aspell")
;; You could add extra option "--camel-case" for since Aspell 0.60.8
;; @see https://github.com/redguardtoo/emacs.d/issues/796
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))

(use-package restclient
  :ensure t
  :defer 0.1
  )

(use-package git-gutter
  :ensure t
  :defer t
  :init
  (global-git-gutter-mode)
  (global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "C-x n") 'git-gutter:next-hunk)
  )
(use-package git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :after git-gutter
  :demand fringe-helper
  :config
  ;; subtle diff indicators in the fringe
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
    [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
    nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
    [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
    nil nil 'center))

(use-package magit
  :ensure t
  :defer t
  :bind
  (:map evil-normal-state-map
        ("SPC g g" . magit-status)
        :map magit-mode-map
        ("1" . nil)
        ("2" . nil)
        ("3" . nil)
        ("4" . nil))
  :config
  ;; (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; (setq magit-git-debug t)
  ;; Open remote repo
  (defun parse-url (url)
    "convert a git remote location as a HTTP URL"
    (if (string-match "^http" url)
        url
      (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                                "https://\\2/\\3"
                                url)))
  (defun magit-open-repo ()
    "open remote repo URL"
    (interactive)
    (let ((url (magit-get "remote" "origin" "url")))
      (progn
        (browse-url (parse-url url))
        (message "opening repo %s" url))))


  (add-hook 'magit-mode-hook
            (lambda ()
              (local-set-key (kbd "o") 'magit-open-repo)))

  (defun forge-browse-buffer-file ()
    (interactive
     (browse-url
      (let
          ((rev (magit-get-current-branch))
           (repo (forge-get-repository 'stub))
           (file (file-relative-name buffer-file-name (projectile-project-root))))
        (forge--format repo "https://%h/%o/%n/blob/%r/%f"
                       `((?r . ,rev) (?f . ,file)))))))
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

  )

(use-package forge
  :ensure t
  :after magit
  :config
  ;; (setq auth-sources '((:source "~/.authinfo")))
  (setq auth-sources '("~/.authinfo"))
  (push '("git.palex-soft.com" "git.palex-soft.com/api/v4"
          "gpalex" forge-gitlab-repository)
        forge-alist)
  (add-to-list 'ghub-insecure-hosts "git.palex-soft.com/api/v4")
  )


(use-package pretty-hydra
  :after git-messenger
  :ensure t
  :bind ("<f6>" . toggles-hydra/body)
  :config
  (setq centaur-icon t)                          ;
  (defun icons-displayable-p ()
    "Return non-nil if `all-the-icons' is displayable."
    (and centaur-icon
         (display-graphic-p)
         (require 'all-the-icons nil t)))
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:foreground ,(face-background 'highlight))))
          (height (or height 1.0))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "all-the-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face)))))

(use-package git-messenger
  :ensure t
  :defer 0.1
  :bind (:map vc-prefix-map
              ("p" . git-messenger:popup-message)
              :map git-messenger-map
              ("m" . git-messenger:copy-message))
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t)
  ;; :config
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra git-messenger-hydra (:color blue)
        ("s" git-messenger:popup-show "show")
        ("c" git-messenger:copy-commit-id "copy hash")
        ("m" git-messenger:copy-message "copy message")
        ("," (catch 'git-messenger-loop (git-messenger:show-parent)) "go parent")
        ("q" git-messenger:popup-close "quit")))

    (defun my-git-messenger:format-detail (vcs commit-id author message)
      (if (eq vcs 'git)
          (let ((date (git-messenger:commit-date commit-id))
                (colon (propertize ":" 'face 'font-lock-comment-face)))
            (concat
             (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                     (propertize "Commit" 'face 'font-lock-keyword-face) colon
                     (propertize (substring commit-id 0 8) 'face 'font-lock-comment-face)
                     (propertize "Author" 'face 'font-lock-keyword-face) colon
                     (propertize author 'face 'font-lock-string-face)
                     (propertize "Date" 'face 'font-lock-keyword-face) colon
                     (propertize date 'face 'font-lock-string-face))
             (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
             message
             (propertize "\nPress q to quit" 'face '(:inherit (font-lock-comment-face italic)))))
        (git-messenger:format-detail vcs commit-id author message)))

    (defun my-git-messenger:popup-message ()
      "Popup message with `posframe', `pos-tip', `lv' or `message', and dispatch actions with `hydra'."
      (interactive)
      (let* ((vcs (git-messenger:find-vcs))
             (file (buffer-file-name (buffer-base-buffer)))
             (line (line-number-at-pos))
             (commit-info (git-messenger:commit-info-at-line vcs file line))
             (commit-id (car commit-info))
             (author (cdr commit-info))
             (msg (git-messenger:commit-message vcs commit-id))
             (popuped-message (if (git-messenger:show-detail-p commit-id)
                                  (my-git-messenger:format-detail vcs commit-id author msg)
                                (cl-case vcs
                                  (git msg)
                                  (svn (if (string= commit-id "-")
                                           msg
                                         (git-messenger:svn-message msg)))
                                  (hg msg)))))
        (setq git-messenger:vcs vcs
              git-messenger:last-message msg
              git-messenger:last-commit-id commit-id)
        (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
        (git-messenger-hydra/body)
        (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
               (let ((buffer-name "*git-messenger*"))
                 (posframe-show buffer-name
                                :string popuped-message
                                :left-fringe 8
                                :right-fringe 8
                                ;; :poshandler #'posframe-poshandler-window-top-right-corner
                                :poshandler #'posframe-poshandler-window-top-right-corner
                                ;; Position broken with xwidgets and emacs 28
                                ;; :position '(-1 . 0)
                                :y-pixel-offset 20
                                :x-pixel-offset -20
                                :internal-border-width 2
                                :lines-truncate t
                                :internal-border-color (face-foreground 'font-lock-comment-face)
                                :accept-focus nil)
                 (unwind-protect
                     (push (read-event) unread-command-events)
                   (posframe-delete buffer-name))))
              ((and (fboundp 'pos-tip-show) (display-graphic-p))
               (pos-tip-show popuped-message))
              ((fboundp 'lv-message)
               (lv-message popuped-message)
               (unwind-protect
                   (push (read-event) unread-command-events)
                 (lv-delete-window)))
              (t (message "%s" popuped-message)))
        (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))
    (advice-add #'git-messenger:popup-close :override #'ignore)
    ;; (advice-add #'git-messenger:popup-close :override #'(setq modal-opened 0))
    (advice-add #'git-messenger:popup-message :override #'my-git-messenger:popup-message)))

(use-package git-timemachine
  :ensure t
  :bind (:map evil-normal-state-map ("SPC g t" . git-timemachine))
  :defer 0.1)

(defun smerge-try-smerge ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^<<<<<<< " nil t)
      (require 'smerge-mode)
      (smerge-mode 1))))
(add-hook 'find-file-hook 'smerge-try-smerge t)
(add-hook 'after-revert-hook 'smerge-try-smerge t)

(use-package undo-tree
  :ensure t
  :after evil
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode)
  (evil-set-undo-system 'undo-tree)
  )

(use-package vterm
  :ensure t
  :defer 0.1)

(use-package vterm-toggle
  :ensure t
  :defer t
  :after vterm
  :config
  (setq vterm-toggle-scope 'project)
  )

(use-package recentf
  :ensure t
  :defer 0.1
  :config (progn (setq recentf-auto-cleanup 'never
                       recentf-max-menu-items 50
                       recentf-max-saved-items 400
                       recentf-save-file
                       (expand-file-name ".recentf" user-emacs-directory))
                 (recentf-mode t)
                 (add-hook 'find-file-hook 'recentf-save-list)
                 ))

;; (define-globalized-minor-mode global-hs-minor-mode
;;   hs-minor-mode hs-minor-mode)

;; (global-hs-minor-mode 1)

(use-package origami
  :ensure t
  :defer 0.t
  :init
  (global-origami-mode 1)
  )

(defun mhtml-forward (arg)
  (interactive "P")
  (pcase (get-text-property (point) 'mhtml-submode)
    ('nil (sgml-skip-tag-forward 1))
    (submode (forward-sexp))))

;; Adds the tag and curly-brace detection to hs-minor-mode for mhtml.
(add-to-list 'hs-special-modes-alist
             '(mhtml-mode
               "{\\|<[^/>]*?"
               "}\\|</[^/>]*[^/]>"
               "<!--"
               mhtml-forward
               nil))

;; (use-package session
;;   :ensure t
;;   :defer 0.1
;;   :config
;;   (add-hook 'after-init-hook 'session-initialize)
;;   )

(use-package doom-themes
  :ensure t
  :defer 0.1
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
  ;; (doom-themes-org-config)

  :bind (:map evil-normal-state-map ("SPC h t" . load-theme))
  )

(use-package heaven-and-hell
  :ensure t
  :after doom-themes
  :config
  (setq heaven-and-hell-theme-type 'dark) ;; Omit to use light by default
  (setq heaven-and-hell-themes
        ;; '((light . zaiste)
        '((light . doom-one-light)
          (dark . doom-moonlight))) ;; Themes can be the list: (dark . (tsdh-dark wombat))
  ;; Optionall, load themes without asking for confirmation.
  (setq heaven-and-hell-load-theme-no-confirm t)
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
         ("<f5>" . heaven-and-hell-toggle-theme)))

(scroll-bar-mode -1)
(menu-bar-mode -1)
(if window-system
    (tool-bar-mode -1)
  )
(setq inhibit-splash-screen t)
(set-default 'truncate-lines t)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))


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

(use-package all-the-icons
  :ensure t
  :defer 0.5s)

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :ensure t
  :defer 0.2)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package all-the-icons-ivy-rich
  :ensure t
  :after counsel-projectile
  :init
  (all-the-icons-ivy-rich-mode 1))

(use-package doom-modeline
  :ensure t
  :defer t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-icon (display-graphic-p))
  (setq auto-revert-check-vc-info t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-buffer-file-name-style 'file-name)
  )

(use-package nyan-mode
  :init
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

(use-package wakatime-mode
  :ensure t
  :config
  (global-wakatime-mode)
  )

(use-package indent-guide
  :ensure t
  :defer 0.1
  :init
  (indent-guide-global-mode 1)
  :custom-face
  (indent-guide-face ((t (:foreground "#7592e8" :slant normal))))

  :config
  (setq indent-guide-threshold 0)
  (setq indent-guide-char "¦")
  ;; (set-face-attribute 'indent-guide-face nil
  ;;                     :foreground "#d2ecff")

  ;; (set-face-foreground 'indent-guide-face "red")
  (add-hook 'ng2-html-mode 'indent-guide-mode)
  (add-hook 'ng2-ts-mode 'indent-guide-mode)
  (add-hook 'yaml-mode 'indent-guide-mode)
  (add-hook 'html-mode 'indent-guide-mode)
  (add-hook 'python-mode 'indent-guide-mode)
  (add-hook 'web-mode 'indent-guide-mode)
  (add-hook 'scss-mode 'indent-guide-mode)
  (add-hook 'css-mode 'indent-guide-mode)
  (add-hook 'go-mode 'indent-guide-mode)
  )

(use-package presentation
  :ensure t
  :bind (:map presentation-mode-map ("SPC t b" . presentation-mode))
  :defer 0.1)

(setq-default tab-width 2)

(use-package tree-sitter-langs
  :ensure t
  :defer 0.1
  )
(use-package tree-sitter
  :ensure t
  :after tree-sitter-langs
  :hook ((typescript-mode . tree-sitter-hl-mode)
         (js-mode . tree-sitter-hl-mode)
         (go-mode . tree-sitter-hl-mode)
         (python-mode . tree-sitter-hl-mode)
         (ng2-mode . tree-sitter-hl-mode))
  :config
  (setq js-indent-level 2)
  (push '(ng2-html-mode . html) tree-sitter-major-mode-language-alist)
  (push '(ng2-ts-mode . typescript) tree-sitter-major-mode-language-alist)
  (global-tree-sitter-mode)
  )

(use-package hl-todo
  :ensure t
  :defer 0.1
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF")))
  (global-hl-todo-mode 1))

(use-package yasnippet
  :ensure t
  :after company
  :init
  (yas-global-mode 1)
  :config

  (setq yas-snippet-dirs
        '("~/doom.d/snippets"                 ;; personal snippets
          ))
  ;; (defvar company-mode/enable-yas t
  ;;   "Enable yasnippet for all backends.")

  ;; (defun company-mode/backend-with-yas (backend)
  ;;   (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
  ;;       backend
  ;;     (append (if (consp backend) backend (list backend))
  ;;             '(:with company-yasnippet))))

  ;; (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  (global-set-key (kbd "C-c C-s") 'yas-new-snippet)
  (yas-reload-all)
  )

(use-package company
  :ensure t
  :defer 0.1
  :bind (:map company-active-map
              ("C-j" . company-select-next)
              ("C-k" . company-select-previous)
              :map company-mode-map
              ("TAB" . nil)
              ("C-x C-i" . 'company-complete-common)
              ("C-x C-o" . 'company-capf))
  :init
  (global-company-mode t)
  :config
  (setq company-idle-delay 0.2))

(use-package company-tabnine
  :ensure t
  :after company
  :bind(:map evil-insert-state-map
             ("C-x C-i" . company-tabnine))
  :config
  (add-to-list 'company-backends #'company-capf #'company-tabnine))

(use-package lsp-mode
  :ensure t
  :defer 0.1
  :hook ((js-mode . lsp)
         (go-mode . lsp)
         (javascript-mode . lsp)
         (web-mode . lsp)
         (vue-mode . lsp))
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.3)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-enable-on-type-formatting nil)

  (lsp-signature-auto-activate nil)

  ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
  ;; :init
  ;; (setq lsp-signature-auto-activate nil)
  :config
  (setq lsp-eldoc-hook nil)           ;; doesn't seem to work
  (fmakunbound 'lsp-signature-activate)
  (defun lsp-signature-activate ()
    (message nil)
    )
  (setenv "GOPATH" (concat (getenv "HOME") "/go"))
  (setenv "PATH" (concat (getenv "HOME") "/go/bin"))
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook '(lambda () (setq lsp-diagnostic-package :none)))
  (setq lsp-modeline-diagnostics-scope :workspace)
  (setq lsp-file-watch-threshold 4000)
  (setq lsp-ui-sideline-show-code-actions nil)
  ;; (setq lsp-print-performance t)
  (setq lsp-idle-delay 0.500)
  (setq lsp-enable-file-watchers nil) ;; boost performance ?
  )

(use-package lsp-ui
  :ensure t
  :after lsp
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; (setq lsp-ui-doc-position 'top)
  ;; (setq lsp-ui-doc-max-width 180)
  ;; (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-diagnostic-max-line-length 200)
  (setq lsp-ui-sideline-diagnostic-max-lines 5)
  ;; (setq lsp-ui-sideline-show-symbol t)
  ;; (setq lsp-ui-doc-alignment 'window)
  (setq lsp-diagnostic-clean-after-change t)
  ;; (setq lsp-ui-doc-delay 0.8)
  ;; (setq lsp-ui-doc-use-webkit t)
  ;; (setq lsp-ui-doc-use-childframe t)
  ;; (setq lsp-ui-sideline-show-code-actions nil)
  (add-hook 'before-save-hook #'+format/buffer nil t)
  :init
  (setq lsp-ui-sideline-diagnostic-max-lines 5)
  )

(use-package prettier-js
  :ensure t
  :defer 0.1
  :hook ((ng2-html-mode . prettier-js-mode)
         (ng2-ts-mode . prettier-js-mode)
         (js-mode . prettier-js-mode))
  )

(defun my-format-all ()
  "Format code and org mode blocks"
  (interactive)
  (if (and (eq major-mode 'org-mode)
           (org-in-src-block-p t))
      (format-org-mode-block)
    (format-all-buffer)
    ))
(use-package format-all
  :ensure t
  :defer 0.1
  :init
  (setq formatters '((typescript-mode . "prettier") (js-mode . "prettier") (go-mode "gofmt")))
  (format-all-mode 1))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package dap-mode
  :ensure t
  :defer 0.1)

(use-package js
  :ensure t
  :defer 0.1
  :hook (js-mode . lsp-mode)
  :init
  (setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")
  )

(use-package ts-mode
  :ensure t
  :defer 0.1
  :config
  (setq typescript-indent-level 2)
  (add-to-list 'auto-mode-alist '("\.ts\'" . typescript-mode))

  )

(use-package ng2-mode
  :ensure t
  :after ts-mode
  :config
  (setq read-process-output-max (* 1024 1024))

  (setq lsp-clients-angular-language-server-command
        '("node"
          "/usr/local/lib/node_modules/@angular/language-server"
          "--ngProbeLocations"
          "/usr/local/lib/node_modules"
          "--tsProbeLocations"
          "/usr/local/lib/node_modules"
          "--stdio"))

  )

(defun init-angular-env ()
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'prettier-js-mode)
  (add-hook 'ng2-html-mode-hook #'lsp)
  (add-hook 'ng2-mode #'lsp)
  )

(with-eval-after-load 'typescript-mode (init-angular-env))
(with-eval-after-load 'ng2-html (init-angular-env))

(use-package scss-mode
  :ensure t
  :defer 0.1
  :hook ((scss-mode . lsp-mode)))
;; (with-eval-after-load 'css-mode
;;   (defun revert-buffer-no-confirm ()
;;     "Revert buffer without confirmation."
;;     (interactive)
;;     (revert-buffer :ignore-auto :noconfirm))

;;   (defun run-sass-auto-fix ()
;;     "Run sass auto fix if cli tool exist"
;;     (interactive)
;;     (let ((default-directory (file-name-directory buffer-file-name)))
;;       (shell-command "sass-lint-auto-fix")
;;       (revert-buffer-no-confirm)
;;       (message "SASS FORMATTED")
;;       ))
;;   (add-hook 'scss-mode-hook '(lambda () (add-hook 'after-save-hook #'run-sass-auto-fix t t)))
;;   )

(use-package go-mode
  :ensure t
  :defer t
  :hook (go-mode . lsp-mode))

;; (use-package dap-go
;;   :ensure t
;;   :after go-mode
;;   :config
;;   (require 'dap-ui)
;;   (setq dap-auto-configure-features '(sessions locals controls tooltip))
;;   (set-fringe-style (quote (14 . 10))) ;; Left breakpoint sqr size ;
;;   )

(use-package pipenv
  :ensure t
  :defer 0.1
  :hook (python-mode . pipenv-mode)
  :config
  (setenv "WORKON_HOME" (concat (getenv "HOME") "/.local/share/virtualenvs"))
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package pyvenv
  :ensure t
  :defer 0.1
  :demand t
  :config
  (setq pyvenv-workon "social-network-promotion-qKnIBgNK")  ; Default venv
  (pyvenv-tracking-mode 1))

(use-package python-mode
  :ensure t
  :defer 0.1
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq tab-width 4)
              (setq python-indent-offset 4)
              (setq global-flycheck-mode 1)
              )
            )
  )

(use-package lsp-python-ms
  :ensure t
  :defer 0.1
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

(setq lsp-ui-sideline-diagnostic-max-lines 4)
(use-package rustic
  :ensure t
  :defer 0.1
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t
        rustic-format-display-method 'ignore)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(add-hook 'before-save-hook #'+format/buffer nil t)
(use-package web-mode
  :ensure t
  :defer 0.1
  :config
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (add-hook 'web-mode-hook '(lambda () (setq lsp-diagnostic-package :none)))
  (add-hook 'web-mode-hook #'company-mode)
  (add-hook 'web-mode-hook #'flycheck-mode)
  (add-hook 'web-mode-hook #'prettier-js-mode)
  (add-hook 'web-mode-hook #'lsp)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  ;; (flycheck-add-next-checker 'typescript-tide '(warning . typescript-tslint) 'append)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  ;; (flycheck-add-next-checker 'typescript-tide)
  ;; (flycheck-add-next-checker 'typescript-tide '(warning . typescript-tslint) 'append)
  ;; (flycheck-add-mode 'lsp-ui 'web-mode)


  ;; (add-hook 'web-mode-hook 'my-flycheck-setup)

  (setq-default indent-tabs-mode nil)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq typescript-indent-level 2)

  (setq mmm-vue-html-mode-exit-hook (lambda ()
                                      (message "Run when leaving vue-html mode")
                                      (emmet-mode -1)))
  (setq mmm-vue-html-mode-enter-hook (lambda ()
                                       (message "Run when entering vue-html mode")
                                       (emmet-mode 1)))
  )

(use-package emmet-mode
  :hook ((scss-mode . emmet-mode) (css-mode . emmet-mode) (ng2-html-mode . emmet-mode) (html-mode . emmet-mode))
  :ensure t
  :defer 0.1
  ;; :config
  ;; (setq emmet-move-cursor-between-quotes t)
  )

(use-package docker-compose-mode
  :ensure t
  :defer 0.1)

(use-package dockerfile-mode
  :ensure t
  :defer 0.1)

(use-package jenkinsfile-mode
    :ensure t
    :defer 0.1
    :config
    )

(use-package ox-json
  :ensure t
  :defer 0.1)

(add-hook 'org-mode-hook (lambda ()
                           "Beautify Org Checkbox Symbol"
                           (push '("[ ]" .  "☐") prettify-symbols-alist)
                           (push '("[X]" . "☑" ) prettify-symbols-alist)
                           (push '("[-]" . "❍" ) prettify-symbols-alist)
                           (push '("#+BEGIN_SRC" . "↦" ) prettify-symbols-alist)
                           (push '("#+END_SRC" . "⇤" ) prettify-symbols-alist)
                           (push '("#+BEGIN_EXAMPLE" . "↦" ) prettify-symbols-alist)
                           (push '("#+END_EXAMPLE" . "⇤" ) prettify-symbols-alist)
                           (push '("#+BEGIN_QUOTE" . "↦" ) prettify-symbols-alist)
                           (push '("#+END_QUOTE" . "⇤" ) prettify-symbols-alist)
                           (push '("#+begin_quote" . "↦" ) prettify-symbols-alist)
                           (push '("#+end_quote" . "⇤" ) prettify-symbols-alist)
                           (push '("#+begin_example" . "↦" ) prettify-symbols-alist)
                           (push '("#+end_example" . "⇤" ) prettify-symbols-alist)
                           (push '("#+begin_src" . "↦" ) prettify-symbols-alist)
                           (push '("#+end_src" . "⇤" ) prettify-symbols-alist)
                           (prettify-symbols-mode)))

(use-package org-indent
  :ensure nil
  :defer t
  :init
  (add-hook 'org-mode-hook 'org-indent-mode))

(use-package ob-restclient
  :ensure t
  :defer 0.1)
(defun format-org-mode-block ()
  "Format org mode code block"
  (interactive "p")
  ;; (execute-kbd-macro (kbd "C-c ' C-x h C-M-\\ C-c '"))
  ;; (execute-kbd-macro (read-kbd-macro "C-c ' C-x h C-M-\\ C-c '"))
   (org-edit-special)
   (format-all-ensure-formatter)
   (format-all-buffer)
   (org-edit-src-exit)
  )

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure t
  ;; :bind
  ;; (:map org-mode-map ("C-o f" . format-org-mode-block))
  :config
  (progn
    (define-key org-mode-map "\C-x a f" "\C-x h \C-M-\\ \C-c")
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((restclient . t)))
    (custom-set-faces
     '(org-level-1 ((t (:inherit outline-1 :height 2.0))))
     '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
     '(org-level-3 ((t (:inherit outline-3 :height 1.25))))
     '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
     '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
     )
    (add-to-list 'org-tag-faces '("@.*" . (:foreground "red")))

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((restclient . t)))


    (defun publish-org-blog()
      "Publish this note to du-blog!"
      (interactive)

      (message (concat
                "node /Users/darkawower/projects/pet/it-blog/emacs-blog/index.js"
                (buffer-file-name)))
      (shell-command
       (concat
        "node /Users/darkawower/projects/pet/it-blog/emacs-blog/index.js "
        (buffer-file-name))
       ))

    (setenv "NODE_PATH"
            (concat
             (getenv "HOME") "/org-node/node_modules"  ":"
             (getenv "NODE_PATH")
             )
            )

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((js . t)))

    (defun org-babel-execute:typescript (body params)
      (let ((org-babel-js-cmd "npx ts-node < "))
        (org-babel-execute:js body params)))

    (defvar org-babel-js-function-wrapper
      ""
      "Javascript code to print value of body.")
    )
  )

;; (with-eval-after-load 'org
;;   (define-key org-mode-map "\C-x \Cp" ))

(use-package org-superstar
  :ensure t
  :defer 0.1
  ;; :after org
  :hook ((org-mode . org-superstar-mode)
         )
  :config
  (setq org-directory "~/Yandex.Disk.localized/org")
  (setq org-agenda-files '("~/Yandex.Disk.localized/org/articles"))
  (setq org-agenda-files '("~/Yandex.Disk.localized/org/strudy"))
  (setq org-agenda-files (directory-files-recursively "~/Yandex.Disk.localized/org/" "\\.org$"))

  )

(use-package org-roam
  :ensure t
  :defer 0.1
  :bind (:map evil-normal-state-map ("SPC n r f" . org-roam-node-find))
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-completion-system 'ivy)
  (setq org-roam-directory "~/Yandex.Disk.localized/org-roam")
  )

;; (use-package org-roam-server
;;   :ensure t
;;   :after org-roam
;;   :config
;;   (setq org-roam-server-host "127.0.0.1"
;;         org-roam-server-port 8080
;;         org-roam-server-authenticate nil
;;         org-roam-server-export-inline-images t
;;         org-roam-server-serve-files nil
;;         org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
;;         org-roam-server-network-poll t
;;         org-roam-server-network-arrows nil
;;         org-roam-server-network-label-truncate t
;;         org-roam-server-network-label-truncate-length 60
;;         org-roam-server-network-label-wrap-length 20)
;;   (defun org-roam-server-open ()
;;     "Ensure the server is active, then open the roam graph."
;;     (interactive)
;;     (smartparens-global-mode -1)
;;     (org-roam-server-mode 1)
;;     (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))
;;     (smartparens-global-mode 1))
;;   )

;; (after! org-roam
;;   (smartparens-global-mode -1)
;;   (org-roam-server-mode)
;;   (smartparens-global-mode 1))
