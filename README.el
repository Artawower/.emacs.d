(let* ((normal-gc-cons-threshold (* 20 1024 1024))
     (init-gc-cons-threshold (* 128 1024 1024)))
(setq gc-cons-threshold init-gc-cons-threshold)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 20 1024 1024)))))

(setq read-process-output-max (* 1024 1024))

(require 'package)

(customize-set-variable 'package-archives
                        `(,@package-archives
                          ("melpa" . "https://melpa.org/packages/")
                          ("melpa-stable" . "http://stable.melpa.org/packages/")
                          ("org" . "https://orgmode.org/elpa/")
                          ;; ("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/")
                          ))
(customize-set-variable 'package-enable-at-startup nil)
(package-initialize)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; (setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(setq use-package-always-ensure nil)

(setq warning-minimum-level :emergency)

;; Change backup folders
(setq backup-directory-alist '(("." . "/Users/darkawower/tmp/emacs-backups")))

(setq confirm-kill-emacs 'y-or-n-p)
(defalias 'yes-or-no-p 'y-or-n-p)

(defun my-add-additional-space-when-not-exist (_)
  "Add additional sapce if previous char is not space!"
  (unless (eq (char-before) ? )
    (insert " ")))

(advice-add 'org-insert-link :before 'my-add-additional-space-when-not-exist)

(defun format-org-mode-block ()
  "Format org mode code block"
  (interactive "p")
  (org-edit-special)
  (format-all-ensure-formatter)
  (format-all-buffer)
  (org-edit-src-exit))

(defun @setup-org-mode-faces ()
  "Setup faces for org mode"
  (custom-set-faces
   '(org-document-title ((t (:inherit outline-1 :height 2.5))))
   '(org-level-1 ((t (:inherit outline-1 :height 2.0))))
   '(org-level-2 ((t (:inherit outline-2 :height 2.0))))
   '(org-level-3 ((t (:inherit outline-3 :height 2.0))))
   '(org-level-4 ((t (:inherit outline-4 :height 2.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 2.0))))))

(defun my-switch-to-xwidget-buffer (&optional a b)
  "Switch to xwidget buffer."
  (interactive)
  (switch-to-first-matching-buffer "xwidget webkit"))

(defun my-toggle-default-browser ()
  "Toggle default browser for preview"
  (interactive)
  (if (eq browse-url-browser-function #'browse-url-default-browser)
      (progn (setq browse-url-browser-function #'xwidget-webkit-browse-url)
             (advice-add 'browse-url :after #'my-switch-to-xwidget-buffer))
    (progn
      (setq browse-url-browser-function #'browse-url-default-browser)
      (advice-remove 'browse-url #'my-switch-to-xwidget-buffer))))

(defun switch-to-first-matching-buffer (regex)
  (switch-to-buffer (car (remove-if-not (apply-partially #'string-match-p regex)
                                        (mapcar #'buffer-name (buffer-list))))))

(defun +select-window-by-name (regexp)
  "Selects the window with buffer NAME"
  (select-window
   (car (seq-filter
     (lambda (window)
       (string-match-p regexp (buffer-name (window-buffer window))))
     (window-list-1 nil 0 t)))))

(defun my-remove-cr (&optional begin end)
  "Remove line prefixes ending with carriage-return.

BEGIN END specifies region, otherwise works on entire buffer."
  (save-excursion
    (goto-char (or begin (point-min)))
    (while (re-search-forward "^.*\033\\[2K\033\\[1G" end t)
      (replace-match ""))))

(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))

(defun xah-copy-to-register-1 ()
  "Copy current line or text selection to register 1.
See also: `xah-paste-from-register-1', `copy-to-register'.

;;;; Register copy past
URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version 2017-01-23"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (progn (setq $p1 (region-beginning))
               (setq $p2 (region-end)))
      (progn (setq $p1 (line-beginning-position))
             (setq $p2 (l(defun xah-paste-from-register-1 ()
                           "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'.
URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
                           (interactive)
                           (when (use-region-p)
                             (delete-region (region-beginning) (region-end)))
                           (insert-register ?1 t))ine-end-position))))
    (copy-to-register ?1 $p1 $p2)
    (message "Copied to register 1: 「%s」." (buffer-substring-no-properties $p1 $p2))))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'.
URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(defun my-open-kitty-right-here ()
  "Open or switch kitty to root directory of current project."
  (interactive)
  (let* ((cmd (concat "open -a kitty.app --args \"cd\" " default-directory)))
    (shell-command cmd)))

(defun my-copy-pwd ()
  "Copy PWD command to clipboard"
  (interactive)
  (when (buffer-file-name)
    (kill-new (replace-regexp-in-string " " "\\\\\  " (file-name-directory (buffer-file-name))))))

(defun my-copy-file-name ()
  "Copy file name command to clipboard"
  (interactive)
  (when (buffer-file-name)
    (kill-new (file-name-nondirectory (buffer-file-name)))))

(defun my-copy-full-path ()
  "Copy full path till file to clipboard"
  (interactive)
  (when (buffer-file-name)
    (kill-new (replace-regexp-in-string " " "\\\\\  " (buffer-file-name)))))

(defun my-vterm-change-current-directory-to-active-buffer-pwd ()
  "Just exec CD to pwd of active buffer."
  (interactive)
  (when-let* ((file-name (buffer-file-name))
              (file-dir (file-name-directory file-name))
              (file-dir (replace-regexp-in-string " " "\\\\\  " file-dir)))
    (message "FILE: %s" file-dir)
    (save-window-excursion
      (switch-to-first-matching-buffer "vterm")
      (vterm-send-C-c)
      (vterm-send-string (concat "cd " file-dir))
      (vterm-send-return)
      )
    (evil-window-down 1)))

(defun my-forge-browse-buffer-file ()
  (interactive)
  (browse-url
   (let
       ((rev (cond ((and (boundp git-timemachine-mode) git-timemachine-mode) (git-timemachine-kill-revision))
                   ((and (boundp magit-gitflow-mode) magit-gitflow-mode) (magit-copy-buffer-revision))
                   (t "master")))
        (repo (forge-get-repository 'stub))
        (file (magit-file-relative-name buffer-file-name))
        (highlight
         (if
             (use-region-p)
             (let ((l1 (line-number-at-pos (region-beginning)))
                   (l2 (line-number-at-pos (- (region-end) 1))))
               (format "#L%d-L%d" l1 l2))
           ""
           )))
     (message "rev: %s" rev)
     (if (not file)
         (if-let ((path (forge--split-remote-url (forge--get-remote))))
                  (message "https://%s/%s/%s/commit/%s" (nth 0 path) (nth 1 path) (nth 2 path) rev)
           (user-error "Cannot browse non-forge remote %s" (forge--get-remote)))

       (forge--format repo "https://%h/%o/%n/blob/%r/%f%L"
                      `((?r . ,rev) (?f . ,file) (?L . ,highlight)))))))

(setq my-transparency-disabled-p t)
(defun my-toggle-transparency ()
  "Toggle transparency"
  (interactive)
  (let* ((not-transparent-p (and (boundp 'my-transparency-disabled-p) my-transparency-disabled-p))
         (alpha (if not-transparent-p 100 95)))
    (setq my-transparency-disabled-p (not not-transparent-p))
    (message "%s" alpha)
    (progn
      (set-frame-parameter (selected-frame) 'alpha `(,alpha . ,alpha))
      (add-to-list 'default-frame-alist `(alpha . (,alpha . ,alpha))))))

(defun my-insert-todo-by-current-git-branch ()
  "Insert todo for current git branch."
  (interactive)
  (let* ((branch-name (magit-get-current-branch))
         (vw (string-match "\\(?1:[A-Za-z0-9]+\/\\)\\(?2:VW-[0-9]+\\)" branch-name))
         (task-number (match-string 2 branch-name))
         (todo-msg (or task-number branch-name)))
    (insert (format "TODO: %s " todo-msg))
    (comment-line 1)
    ;; (forward-line 1)
    (previous-line)
    (end-of-line)
    (indent-according-to-mode)
    (evil-insert 1)))

(defun my-run-sass-auto-fix ()
  "Run sass auto fix if cli tool exist"
  (interactive)
  (save-window-excursion
    (let ((default-directory (file-name-directory buffer-file-name)))
      (async-shell-command "sass-lint-auto-fix")
      ;; (revert-buffer-no-confirm)
      (message "SASS FORMATTED"))))

(defun my-insert-tab ()
  "Insert simple tab"
  (interactive)
  (insert "\t"))

(setq +m-color-main "#61AFEF"
      +m-color-secondary "#FF3399"
      +m-color-yellow "#FFAA00"
      +m-color-blue "#00AEE8"
      +m-color-cyan "#00CED1"
      +m-color-green "#00D364")

(use-package all-the-icons-dired
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package hl-todo
  :defer 2
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#E5C07B")
          ("FIXME"  . "#E06C75")
          ("DEBUG"  . "#C678DD")
          ("REFACTOR"  . "#C678DD")
          ("GOTCHA" . "#FF4500")
          ("NOTE"   . "#98C379")
          ("QUESTION"   . "#98C379")
          ("STUB"   . "#61AFEF")))
  (global-hl-todo-mode 1))

(fringe-mode '16)

(progn
  (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
  (add-to-list 'default-frame-alist '(alpha . (95 . 95))))

(defface bookmark-menu-heading
  '((t :foreground "#7a88cf"
       :background unspecified))
  "Face for patching nano")

(straight-use-package
 '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(use-package nano-theme-light)

(use-package nano-theme-dark
  :config
  (scroll-bar-mode -1))

(use-package nano-faces
  :config
  (nano-faces))

(use-package nano-colors
  :after nano-faces)

(use-package nano-theme
  :after nano-theme-dark
  :config
  (nano-theme)
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font 15" :italic nil :height 146))

(use-package nano-modeline
  :after nano-theme
  :config
  (setq nano-font-size 15)
  (setq nano-font-family-monospaced "JetBrainsMono Nerd Font 15")
  (nano-modeline-default-mode)
  (scroll-bar-mode -1))


(use-package nano-splash
  :after nano-theme)

(use-package nano-help
  :after nano-theme)

(use-package nano-layout :config (scroll-bar-mode -1))

;; (use-package nano-command
;;    :config
;;    (nano-command-mode))

(set-frame-font "JetBrainsMono Nerd Font 15" nil t)

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

(defvar nano-theme-light-var t)
(defun nano-change-theme-dark ()
  (interactive)
  (nano-theme-set-dark)
  (nano-faces)
  (nano-theme)
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font 15" :italic nil :height 146)
  (@setup-org-mode-faces))

(defun nano-change-theme-light ()
  (interactive)
  (nano-theme-set-light)
  (nano-faces)
  (nano-theme)
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font 15" :italic nil :height 146)
  (@setup-org-mode-faces))

(defun nano-change-theme ()
  (interactive)
  (if nano-theme-light-var (nano-change-theme-dark) (nano-change-theme-light))
  (setq nano-theme-light-var (not nano-theme-light-var)))

(use-package auto-dark
  :defer 5
  :config
  (defun auto-dark--ns-set-theme (appearance)
    "Set light/dark theme using emacs-plus ns-system-appearance.
Argument APPEARANCE should be light or dark."
    ;; (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('dark
       (nano-change-theme-dark)
       (run-hooks 'auto-dark-dark-mode-hook))
      ('light
       (nano-change-theme-light)
       (run-hooks 'auto-dark-light-mode-hook))))
  (add-hook 'auto-dark-mode-hook #'nano-change-theme-dark)
  (add-hook 'auto-light-mode-hook #'nano-change-theme-light)
  (auto-dark-mode))

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq ring-bell-function 'ignore)

(use-package hl-todo
  :defer 1
  :hook (org-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#E5C07B")
          ("HOLD"   . "#E5C07B")
          ("FIXME"  . "#E06C75")
          ("DEBUG"  . "#C678DD")
          ("REFACTOR"  . "#C678DD")
          ("GOTCHA" . "#FF4500")
          ("NOTE"   . "#98C379")
          ("QUESTION"   . "#98C379")
          ("STUB"   . "#61AFEF")))
  (global-hl-todo-mode 1)
  (hl-todo-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (evil-commentary-mode))

(use-package evil
  :after dired
  :init
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :config
  (evil-set-undo-system 'undo-redo)
  (setq-default evil-kill-on-visual-paste nil)
  (evil-mode 1)
  :bind
  (:map evil-insert-state-map
   ("C-u" . evil-delete-back-to-indentation)
   ("s-Y" . xah-copy-to-register-1)
   ("s-P" . xah-paste-from-register-1)
   ("s-p" . yank-from-kill-ring)
   ("s-." . ace-window)

   :map evil-normal-state-map
   ;; Core
   ("SPC h r e" . (lambda () (interactive) (load-file "~/pure-emacs/init.el")))
   ;; Consult
   ("SPC f r" . consult-recent-file)
   ("SPC /" . counsel-projectile-rg)
   ;; Presentation
   ("SPC t b" . presentation-mode)
   ;; TODO move to treemacs
   ;; Treemacs
   ("SPC o p"   . treemacs)
   ("SPC t a" . treemacs-add-project-to-workspace)
   ("SPC o P" . treemacs-find-file)
   ;; Projectile
   ("SPC p p" . consult-projectile-switch-project)
   ("SPC p a" . projectile-add-known-project)
   ;; Window
   ("SPC w r" . evil-window-rotate-downwards)
   ("SPC w v" . evil-window-vsplit)
   ("SPC w s" . evil-window-split)
   ;; Buffers
   ("SPC b ]" . next-buffer)
   ("SPC b [" . previous-buffer)
   ("SPC b b" . consult-buffer)
   ;; Org
   ("SPC m t" . org-todo)
   ("SPC m n" . org-store-link)
   ("SPC m l l" . org-insert-link)
   ("SPC ." . find-file)
   ("SPC h v" . describe-variable)
   ("SPC h f" . describe-function)
   ("SPC h F" . describe-face)
   ("SPC b O" . kill-other-buff)
   ("SPC o t" . vterm-toggle-cd)
   ("SPC t l" . global-display-line-numbers-mode)
   ("SPC s i" . consult-imenu)
   ("SPC RET" . consult-bookmark)
   ("SPC b n" . evil-buffer-new)
   ("SPC q" . kill-current-buffer)
   ("SPC b q" . kill-current-buffer)
   ("SPC v l" . visual-line-mode)
   ("C-u" . evil-scroll-up)
   ("SPC g t" . git-timemachine)
   ("SPC h t" . load-theme)
   ;; ("SPC b b" . persp-ivy-switch-buffer)
   ;; ("SPC b b" . persp-switch-to-buffer)
   ("SPC b B" . consult-buffer)
   ("SPC TAB d" . persp-kill)
   ("f" . avy-goto-char)
   ;; Perspective keybindings
   ("SPC TAB r" . persp-rename)
   ("SPC TAB n" . persp-next)
   ("SPC TAB p" . persp-prev)
   ;; ("SPC TAB s" . persp-switch)
   ("SPC TAB s" . persp-window-switch)
   ("SPC f p" . counsel-projectile-recentf)
   ("SPC f P" . counsel-projectile-switch-project)
   ("SPC *" . (lambda () (interactive) (consult-git-grep nil (thing-at-point 'symbol))))
   ;; Frames
   ("SPC f b" . (lambda () (interactive) (switch-to-buffer-other-frame "*scratch*")))
   ("SPC n r f" . org-roam-node-find)
   ;; git
   ("SPC g g" . magit-status)
   :map global-map
   ;; Org mode
   ("C-c t" . org-time-stamp-inactive)
   :map org-read-date-minibuffer-local-map
   ("C-j" . (lambda () (interactive)
              (org-eval-in-calendar '(calendar-forward-week 1))))
   ("C-l" . (lambda () (interactive)
              (org-eval-in-calendar '(calendar-forward-day 1))))
   ("C-k" . (lambda () (interactive)
              (org-eval-in-calendar '(calendar-backward-week 1))))
   ("C-h" . (lambda () (interactive)
              (org-eval-in-calendar '(calendar-backward-day 1))))))

(use-package bm
  :defer t
  :custom-face
  (bm-face ((t (:foreground ,+m-color-secondary))))
  :bind (("C-M-n" . bm-next)
         ("C-M-p" . bm-previous)
         ("s-b" . bm-toggle)))

(use-package avy
  :defer t

  :bind (:map evil-normal-state-map
         ("f" . avy-goto-word-1)
         ("SPC k l" . avy-kill-whole-line)
         ("SPC k r" . avy-kill-region))
  :custom
  (avy-single-candidate-jump t)
  (avy-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?a ?s ?d ?f ?g ?h ?j ?k ?l ?z ?x ?c ?v ?b ?n ?m)))

(use-package ace-window
    :bind (:map evil-normal-state-map
                ("s-." . ace-window))
    :defer t)

(use-package evil-matchit
  :defer t)

(evilmi-load-plugin-rules '(ng2-html-mode) '(html))
(global-evil-matchit-mode 1)

(use-package which-key
  :defer 2
  :config
  (which-key-setup-side-window-right)
  (which-key-mode))

(define-key global-map (kbd "C-h") (make-sparse-keymap))
(global-set-key (kbd "C-S-l") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-h") 'shrink-window-horizontally)
(global-set-key (kbd "<C-S-up>") 'shrink-window)
(global-set-key (kbd "C-S-j") 'enlarge-window)
(global-set-key (kbd "<C-S-down>") 'enlarge-window)
(global-set-key (kbd "C-S-k") 'shrink-window)
(global-set-key (kbd "s-y") 'yas-expand)

(use-package general
  :config
  (general-define-key
   :keymaps 'override
   "C-w" 'backward-kill-word
   "s-w" 'evil-window-delete
   "C-h C-k" 'describe-key-briefly
   "\t" 'google-translate-smooth-translate
   "s-<backspace>" 'evil-delete-back-to-indentation
   "C-<tab>" 'my-insert-tab
   "s-k" (lambda () (interactive) (end-of-line) (kill-whole-line)))

  (general-define-key
   :keymaps 'minibuffer-mode-map
   "C-w" 'backward-kill-word
   "C-x" (lambda () (interactive) (end-of-line) (kill-whole-line)))

  (general-override-mode)
  (general-define-key
   :states '(normal)
   :keymaps 'override
   :prefix "SPC"
   "SPC"  'projectile-find-file)

  (general-define-key
   :keymaps 'read-expression-map
   "C-w" 'backward-kill-word
   "C-h" 'previous-history-element
   "C-l" 'next-history-element
   "ESC" 'keyboard-escape-quit)

  (general-define-key
   :keymaps 'org-src-mode-map
   "C-c C-c" 'org-edit-src-exit))

(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer"))

(define-key dired-mode-map (kbd "SPC") nil)
(setq insert-directory-program "gls" dired-use-ls-dired t)
(use-package dired+)

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  ;; Go back home? Just press `bh'
  (dirvish-bookmark-entries
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")))
  ;; (dirvish-header-line-format '(:left (path) :right (free-space)))
  (dirvish-mode-line-format ; it's ok to place string inside
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  ;; Don't worry, Dirvish is still performant even you enable all these attributes
  (dirvish-attributes '(all-the-icons file-size collapse subtree-state vc-state git-msg))
  ;; Maybe the icons are too big to your eyes
  (dirvish-all-the-icons-height 0.8)
  ;; In case you want the details at startup like `dired'
  ;; (dirvish-hide-details nil)
  :config
  ;; (dirvish-peek-mode)
  (setq dired-kill-when-opening-new-dired-buffer t)
  (setq dirvish-reuse-session t)
  ;; Dired options are respected except a few exceptions, see *In relation to Dired* section above
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq dirvish-default-layout '(1 0.3 0.7))
  ;; Enable mouse drag-and-drop files to other applications
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  (setq dirvish-attributes '(vc-state subtree-state collapse git-msg file-size))
  (advice-add #'+dired/quit-all :after (lambda () (interactive) (dirvish-kill (dirvish-prop :dv))))
  :bind
  ;; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dired-mode-map ; Dirvish respects all the keybindings in this map
   ("h" . dired-up-directory)
   ("j" . dired-next-line)
   ("k" . dired-previous-line)
   ("l" . dired-find-file)
   ("i" . wdired-change-to-wdired-mode)
   ("." . dired-omit-mode)
   ("b"   . dirvish-bookmark-jump)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("s"   . dirvish-quicksort) ; remapped `dired-sort-toggle-or-edit'
   ("?"   . dirvish-dispatch)  ; remapped `dired-summary'
   ("TAB" . dirvish-subtree-toggle)
   ("M-n" . dirvish-history-go-forward)
   ("M-p" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-f" . dirvish-toggle-fullscreen)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(use-package vterm
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC o v" . vterm)))

(use-package vterm-toggle
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC t ]" . vterm-toggle-forward)
              ("SPC t [" . vterm-toggle-backward)
              ("SPC t n" . (lambda () (interactive)
                             (let ((current-buffer-name (buffer-name)))
                               (vterm-toggle--new)
                               (delete-window)
                               (display-buffer current-buffer-name)
                               (vterm-toggle-forward))))
              ("SPC t x" . (lambda (args) (interactive "P")
                             (when (string-match "vterm" (buffer-name))
                               (let ((kill-buffer-query-functions nil))
                                 (kill-this-buffer)
                                 (+vterm/toggle args)))))
              ("SPC o h" . (lambda () (interactive)
                             (+vterm/toggle t)))
              ("SPC t h" . vterm-toggle-hide)
              ("SPC t k" . my-open-kitty-right-here))
  :config
  (setq vterm-kill-buffer-on-exit nil)
  (setq vterm-toggle-scope 'project))

(use-package secret-mode
  :defer t)

(use-package yasnippet
  :defer 2
  :config
  (setq yas-snippet-dirs '("~/.doom.d/snippets"))
  (yas-global-mode 1))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(use-package format-all
  :defer t
  ;; :hook ((js2-mode typescript-mode ng2-html-mode ng2-ts-mode go-mode) . format-all-mode)
  :hook ((json-mode go-mode dart-mode) . format-all-mode)
  :config
  (add-to-list '+format-on-save-enabled-modes 'typescript-mode t)
  (add-to-list '+format-on-save-enabled-modes 'ng2-mode t)
  (add-to-list '+format-on-save-enabled-modes 'js2-mode t))

(use-package prettier
  :defer t
  :bind (:map evil-normal-state-map
         ("\+p" . prettier-prettify))
  :hook ((typescript-tsx-mode typescript-mode js2-mode json-mode ng2-mode ng2-html-mode html-mode web-mode) . prettier-mode))

(use-package flymake
  :after evil
  :bind (:map evil-normal-state-map
         ("C-f ]" . flymake-goto-next-error)
         ("C-f [" . flymake-goto-prev-error)))

(use-package flymake-diagnostic-at-point
  :after flymake
  :hook (flmake-mode . flymake-diagnostic-at-point-mode))

(use-package autopair
  :defer t

  :config
  (autopair-global-mode))

(use-package tree-sitter-langs
  :defer t)

(use-package tree-sitter
  :after tree-sitter-langs
  :hook ((go-mode
          typescript-mode
          css-mode
          typescript-tsx-mode
          html-mode
          scss-mode
          ng2-mode
          js-mode
          python-mode
          rust-mode
          ng2-ts-mode
          ng2-html-mode) . tree-sitter-hl-mode)
  :config
  (push '(ng2-html-mode . html) tree-sitter-major-mode-language-alist)
  (push '(ng2-ts-mode . typescript) tree-sitter-major-mode-language-alist)
  (push '(scss-mode . css) tree-sitter-major-mode-language-alist)
  (push '(scss-mode . typescript) tree-sitter-major-mode-language-alist)
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(use-package tree-edit

  :defer t)

(use-package corfu
  ;; Optional customizations
  :defer 2
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match t)        ;; Automatically quit if there is no match
  (corfu-auto-delay 0.1)
  (corfu-echo-documentation nil) ;; Do not show documentation in the echo area

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
         ("TAB" . corfu-next)
         ([tab] . corfu-next)
         ("C-j" . corfu-next)
         ("C-k" . corfu-previous)
         ("S-TAB" . corfu-previous)
         ([backtab] . corfu-previous)
         :map evil-insert-state-map
         ("C-x C-o" . completion-at-point)
         ("C-SPC" . completion-at-point))
  :init
	      (global-corfu-mode)
  :config
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  (evil-make-overriding-map corfu-map))

(use-package corfu-doc
  :after corfu
  :straight (corfu-doc :type git :host github :repo "galeo/corfu-doc")
  :hook (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
              ("M-j" . corfu-doc-scroll-down)
              ("M-k" . corfu-doc-scroll-up)))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package lsp
  :hook ((clojure-mode
          scss-mode
          go-mode
          css-mode
          js-mode
          typescript-mode
          vue-mode
          web-mode
          ng2-html-mode
          ng2-ts-mode
          python-mode
          dart-mode
          typescript-tsx-mode) . lsp-deferred)
  :bind (:map evil-normal-state-map
              ("SPC f n" . flycheck-next-error)
              ("g i" . lsp-goto-implementation)
              ("SPC l a" . lsp-execute-code-action)
              ("SPC l r" . lsp-find-references)
              ("SPC l w" . lsp-restart-workspace)
              ("SPC r l" . lsp))
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay 0.3)
  (lsp-enable-on-type-formatting nil)
  (lsp-eldoc-render-all nil)
  (lsp-prefer-flymake nil)
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
  (lsp-yaml-schemas '((kubernetes . ["/auth-reader.yaml", "/deployment.yaml"])))
  ;; Disable bottom help info
  (lsp-signature-render-documentation nil)
  (lsp-signature-auto-activate nil)
  ;; (lsp-use-plists t)
  (lsp-enable-file-watchers nil)
  (lsp-file-watch-threshold 5000)
  :config
  (setq lsp-javascript-display-return-type-hints t)
  (setq lsp-json-schemas
        `[
          (:fileMatch ["ng-openapi-gen.json"] :url "https://raw.githubusercontent.com/cyclosproject/ng-openapi-gen/master/ng-openapi-gen-schema.json")
          (:fileMatch ["package.json"] :url "http://json-schema.org/draft-07/schema")
          ])
  (set-face-attribute 'lsp-face-highlight-read nil :background "#61AFEF")
  ;; Flycheck patch checkers
  (require 'flycheck)
  (require 'lsp-diagnostics)
  (lsp-diagnostics-flycheck-enable)
  ;; Golang
  (defun lsp-go-install-save-hooks ()
    (flycheck-add-next-checker 'lsp '(warning . go-gofmt) 'append)
    (flycheck-add-next-checker 'lsp '(warning . go-golint))
    (flycheck-add-next-checker 'lsp '(warning . go-errcheck))
    (flycheck-add-next-checker 'lsp '(warning . go-staticcheck))

    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
        lsp-pyls-plugins-flake8-enabled nil)

  (setq lsp-disabled-clients '(html html-ls))
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\venv\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\pyenv\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.cache\\'")
  (set-face-attribute 'lsp-face-highlight-textual nil :background "#c0caf5")
  (setq lsp-eldoc-hook nil))

(use-package lsp-yaml
  :defer t
  :hook (yaml-mode . lsp-mode))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-diagnostic-max-line-length 100
        lsp-ui-sideline-diagnostic-max-lines 8
        lsp-ui-doc-delay 2
        lsp-ui-doc-position 'top
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-border +m-color-main))

(use-package lsp-dart
  :defer t
  :hook (dart-mode . (lambda () (interactive)
                       (add-hook 'after-save-hook
                                 (lambda ()
                                   (flutter-run-or-hot-reload)
                                   ;; (flutter-hot-restart)
                                   )
                                 t t)))
  :custom
  (lsp-dart-dap-flutter-hot-reload-on-save t)
  :config
  (defun lsp-dart-flutter-widget-guide--add-overlay-to (buffer line col string)
    "Add to BUFFER at LINE and COL a STRING overlay."
    (save-excursion
      (goto-char (point-min))
      (forward-line line)
      (move-to-column col)
      (when (string= lsp-dart-flutter-widget-guide-space (string (following-char)))
        (let ((ov (make-overlay (point) (1+ (point)) buffer)))
          (overlay-put ov 'category 'lsp-dart-flutter-widget-guide)
          (overlay-put ov 'display (propertize string
                                               'face 'custom-comment-tag)))))))

(defun compile-eslint--find-filename ()
  "Find the filename for current error."
  (save-match-data
    (save-excursion
      (when (re-search-backward (rx bol (group "/" (+ any)) eol))
        (list (match-string 1))))))

(defun @setup-compilation-errors ()
      (setq compilation-scroll-output t)
  (setq compilation-error-regexp-alist '())
  (setq compilation-error-regexp-alist-alist '())


  ;; eslint https://github.com/Fuco1/compile-eslint/blob/master/compile-eslint.el
  (when (not compilation-error-regexp-alist-alist)
    (setq compilation-error-regexp-alist-alist '()))

  (let ((form `(eslint
                ,(rx-to-string
                  '(and (group (group (+ digit)) ":" (group (+ digit)))
                        (+ " ") (or "error" "warning")))
                compile-eslint--find-filename
                2 3 2 1)))

    (if (assq 'eslint compilation-error-regexp-alist-alist)
        (setf (cdr (assq 'eslint compilation-error-regexp-alist-alist)) (cdr form))
      (push form compilation-error-regexp-alist-alist)))
  (push 'eslint compilation-error-regexp-alist)



  (add-to-list 'compilation-error-regexp-alist '("^[[:blank:]]*\\([/_-\\.[:alnum:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\) - error.*$" 1 2 3))
  ;; React
  (add-to-list 'compilation-error-regexp-alist '("[[:blank:]]*\\([/_\\.[:alnum:]-]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\) - error.*$" 1 2 3))
  ;; Angular
  (add-to-list 'compilation-error-regexp-alist '("^Error: \\([_[:alnum:]-/.]*\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))

  ;; Flutter
  ;; (add-to-list 'compilation-error-regexp-alist '("[[:blank:]]*\\([/_\\.[:alnum:]-]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\): Error.*$"))
  (add-to-list 'compilation-error-regexp-alist 'dart-analyze)
  (add-to-list 'compilation-error-regexp-alist-alist '(dart-analyze "\\([^ ]*\\.dart\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)))

(use-package compile
  :defer t
  :config
  (@setup-compilation-errors))

(use-package floobits
  :defer t)

(use-package dap-mode
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC d n" . dap-next)
              ("SPC d i" . dap-step-in)
              ("SPC d o" . dap-step-out)
              ("SPC d c" . dap-continue)
              ("SPC d Q" . dap-disconnect)
              ("SPC d q" . dap-disconnect)
              ("SPC d d" . (lambda () (interactive)
                             (call-interactively #'dap-debug)
                             (set-window-buffer nil (current-buffer))))
              ("SPC d r" . dap-debug-recent)
              ("SPC d l" . dap-ui-locals)
              ("SPC d b" . dap-ui-breakpoints)
              ("SPC d s" . dap-ui-sessions)
              ("SPC d e" . dap-debug-last)
              ("SPC d p" . (lambda () (interactive)
                             (set-window-buffer nil (current-buffer))
                             (dap-breakpoint-toggle)))
              ("SPC d e" . dap-debug-edit-template))
  :init
  (dap-mode 1)
  (setq dap-auto-configure-features '(sessions locals))
  (require 'dap-go)
  (require 'dap-node))

(use-package vundo
  :defer 1
  :config
  ;; Take less on-screen space.
  (setq vundo-compact-display t)

  ;; Better contrasting highlight.
  (custom-set-faces
   '(vundo-node ((t (:foreground "#808080"))))
   '(vundo-stem ((t (:foreground "#808080"))))
   '(vundo-highlight ((t (:foreground "#FFFF00")))))

  ;; Use `HJKL` VIM-like motion, also Home/End to jump around.
  (define-key vundo-mode-map (kbd "l") #'vundo-forward)
  (define-key vundo-mode-map (kbd "<right>") #'vundo-forward)
  (define-key vundo-mode-map (kbd "h") #'vundo-backward)
  (define-key vundo-mode-map (kbd "<left>") #'vundo-backward)
  (define-key vundo-mode-map (kbd "j") #'vundo-next)
  (define-key vundo-mode-map (kbd "<down>") #'vundo-next)
  (define-key vundo-mode-map (kbd "k") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<up>") #'vundo-previous)
  (define-key vundo-mode-map (kbd "<home>") #'vundo-stem-root)
  (define-key vundo-mode-map (kbd "<end>") #'vundo-stem-end)
  (define-key vundo-mode-map (kbd "q") #'vundo-quit)
  (define-key vundo-mode-map (kbd "C-g") #'vundo-quit)
  (define-key vundo-mode-map (kbd "RET") #'vundo-confirm))

(use-package copilot
  :defer 5
  :bind
  ("s-]" . copilot-next-completion)
  ("s-[" . copilot-previous-completion)
  ("s-l" . copilot-accept-completion)
  ("s-j" . copilot-complete)
  ("s-;" . copilot-accept-completion-by-word)
  ;; :custom
  ;; (copilot-idle-delay 0.5)
  :config
  (setq copilot--previous-point nil)
  (setq copilot--previous-window-width nil)
  (copilot-diagnose)

  (defun copilot--preserve-positions ()
    (setq copilot--previous-point (point))
    (setq copilot--previous-window-width (blamer--real-window-width)))

  (defun copilot--positions-changed-p ()
    (or (not (equal (point)  copilot--previous-point))
        (not (equal (window-width) copilot--previous-window-width))))


  (defun copilot--rerender ()
    (when-let ((copilot--changed (copilot--positions-changed-p)))
      (copilot-clear-overlay)
      (copilot--preserve-positions)
      (blamer--clear-overlay)
      (when (evil-insert-state-p) (copilot-complete))))

  (add-hook 'post-command-hook #'copilot--rerender)
  ;; (add-hook 'evil-insert-state-exit-hook 'copilot-clear-overlay)
  (add-hook 'evil-insert-state-entry-hook (lambda ()
                                            (setq blamer--block-render-p t)
                                            (blamer--clear-overlay)))
  (add-hook 'evil-normal-state-entry-hook (lambda ()
                                            (message "Okay, now blamer should works correctly!")
                                            (setq blamer--block-render-p nil)
                                            (copilot-clear-overlay)))
  ;; (copilot-clear-overlay)) nil t)
  )

(use-package turbo-log
  :defer t
  :bind (("C-s-l" . turbo-log-print)
         ("C-s-i" . turbo-log-print-immediately)
         ("C-s-h" . turbo-log-comment-all-logs)
         ("C-s-s" . turbo-log-uncomment-all-logs)
         ("C-s-x" . turbo-log-delete-all-logs)
         ("C-s-[" . turbo-log-paste-as-logger )
         ("C-s-]" . turbo-log-paste-as-logger-immediately))
  :custom
  (turbo-log-allow-insert-without-tree-sitter-p t)
  ;; (turbo-log-payload-format-template "")
  ;; (turbo-log-payload-format-template "\x1b[35m%s: ")
  (turbo-log-payload-format-template "%s: ")
  :config
  (turbo-log-configure
   :modes (typescript-mode js2-mode web-mode ng2-ts-mode js-mode)
   :strategy merge
   :post-insert-hooks (prettier-prettify lsp)
   :msg-format-template "'🦄: %s'"))

(use-package auto-rename-tag
  :defer t
  :hook ((html-mode ng2-html-mode-hook vue-mode web-mode) . auto-rename-tag-mode)
  :config
  (auto-rename-tag-mode 1))

(use-package string-inflection
  :defer t
  :bind ("C-s-c" . string-inflection-all-cycle))

(use-package magit
  :defer t
  :bind (:map magit-mode-map
         ("s-<return>" . magit-diff-visit-worktree-file)
         :map evil-normal-state-map
         ("SPC g i" . (lambda () (interactive) (wakatime-ui--clear-modeline) (magit-status))))
  :hook
  (magit-process-mode . compilation-minor-mode)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullcolumn-most-v1)
  (define-key transient-map        "q" 'transient-quit-one)
  (define-key transient-edit-map   "q" 'transient-quit-one)
  (define-key transient-sticky-map "q" 'transient-quit-seq)
  (add-hook 'magit-process-mode #'disable-magit-hooks)
  ;; (add-hook 'magit-process-mode-hook #'compilation-mode)
  (setcdr magit-process-mode-map (cdr (make-keymap)))
  (set-keymap-parent magit-process-mode-map special-mode-map)
  (advice-add
   'ansi-color-apply-on-region
   :before
   #'my-remove-cr)
  (setq magit-process-finish-apply-ansi-colors t))

(use-package gist                       ;
  :defer t
  :bind (:map gist-list-menu-mode-map
         ("j" . next-line)
         ("k" . previous-line)
         ("c" . gist-fork)
         ("x" . gist-kill-current)
         ("f" . avy-goto-word-1)
         ("v" . evil-visual-char)
         :map evil-normal-state-map
         ("SPC g l g" . gist-list)))

(use-package git-gutter
  :after git-gutter-fringe
  :bind (:map evil-normal-state-map
              ("SPC g [" . git-gutter:previous-hunk)
              ("SPC g ]" . git-gutter:next-hunk)
              ("SPC g r" . git-gutter:revert-hunk))
  :config
  (set-face-foreground 'git-gutter:modified +m-color-main) ;; background color
  (set-face-foreground 'git-gutter:added +m-color-green)
  (set-face-foreground 'git-gutter:deleted +m-color-red)
  :init
  (global-git-gutter-mode))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package blamer
  :defer 5
  :bind (
         ("C-c i" . blamer-show-commit-info)
         ("C-c h" . (lambda () (interactive) (blamer-show-commit-info 'visual)))
         ("s-i" . blamer-show-posframe-commit-info))
  :custom
  (blamer-idle-time 0.8)
  (blamer-min-offset 20)
  (blamer-max-commit-message-length 65)
  (blamer-commit-formatter "◉ %s")
  (blamer-view 'overlay)
  (blamer-uncommitted-changes-message "uncommitted yet")
  :custom-face
  (blamer-face ((t :inherit font-lock-comment-face
                   :italic t
                   :font "Fira Code 14"
                   :height 0.9
                   :background unspecified)))
  :config
  (tooltip-mode)
  (setq blamer-tooltip-function 'blamer-tooltip-commit-message)


  (defun blamer-callback-show-commit-diff (commit-info)
    (interactive)
    (let ((commit-hash (plist-get commit-info :commit-hash)))
      (when commit-hash
        (magit-show-commit commit-hash))))

  (defun blamer-callback-open-remote (commit-info)
    (interactive)
    (let ((commit-hash (plist-get commit-info :commit-hash)))
      (when commit-hash
        (message commit-hash)
        (forge-browse-commit commit-hash))))

  (setq blamer-bindings '(("<mouse-3>" . blamer-callback-open-remote)
                          ("<mouse-1>" . blamer-callback-show-commit-diff)))

  (global-blamer-mode 1))

(use-package paren-face :defer t)

(use-package elisp-mode
  :defer t

  :hook ((emacs-lisp-mode . paren-face-mode))

  :bind (("C-c o" . outline-cycle)
         ("C-c r" . outline-show-all)
         ("C-c m" . outline-hide-body)
         ("C-c ]" . outline-next-heading)
         ("C-c [" . outline-previous-heading)
         ("C-c c" . counsel-outline)
         ("C-c e" . outline-hide-entry)
         ("C-c t" . outline-toggle-children)
         ("C-c b" . outline-cycle-buffer))
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq rainbow-delimiters-mode -1))))

(use-package package-build
  :defer t)

(use-package package-lint

  :defer t)

(use-package clojure-mode
  :hook ((clojure-mode . format-all-mode)
         (clojure-mode . paren-face-mode))
  :defer t)

(use-package cider
  :defer t)

(setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")
(use-package typescript-mode
  :defer 10
  :custom
  (lsp-clients-typescript-server-args '("--stdio"))
  :config
  (setq typescript-indent-level 2)
  (add-to-list 'auto-mode-alist '("\.ts\'" . typescript-mode)))

(use-package ng2-mode
  :after typescript-mode
  :hook (ng2-html-mode . web-mode)
  :config
  (setq lsp-clients-angular-language-server-command
        '("node"
          "/usr/local/lib/node_modules/@angular/language-server"
          "--ngProbeLocations"
          "/usr/local/lib/node_modules"
          "--tsProbeLocations"
          "/usr/local/lib/node_modules"
          "--stdio")))

(use-package js2-mode
  :defer t
  :hook (js2-mode . js2-highlight-unused-variables-mode))

(use-package npm
  :defer t)

(use-package nodejs-repl
  :defer t)

(use-package go-playground
  :defer t)

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package rustic
  :defer t
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

(use-package python-mode
  :defer t
  :hook (python-mode . format-all-mode)
  :config
  (setq python-indent-level 4)
  (add-hook 'python-mode-hook
            (lambda ()
              (require 'lsp-pyright)
              (lsp-deferred)
              (setq indent-tabs-mode nil)
              (setq tab-width 4))))

(setq lsp-pyright-multi-root nil)
(use-package lsp-pyright
  :defer t
  :config
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-auto-search-paths t)
  (setq lsp-pyright-log-level "trace")
  (setq lsp-pyright-multi-root nil)
  (setq lsp-pyright-use-library-code-for-types t)
  (setq lsp-pyright-venv-directory "/Users/darkawower/.local/share/virtualenvs/spice-farm-YhO8T07I")
  (setq lsp-pyright-diagnostic-mode "workspace"))

(use-package pipenv
  :defer t
  :hook (python-mode . pipenv-mode)
  :config
  (setenv "WORKON_HOME" (concat (getenv "HOME") "/.local/share/virtualenvs"))
  (add-hook 'pyvenv-post-activate-hooks #'lsp-restart-workspace)
  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

(use-package lsp-volar
  :after lsp-mode)

(use-package web-mode
  :defer t
  :mode (("\\.vue\\'" . web-mode)
         ("\\.tsx\\'" . typescript-tsx-mode)
         ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-comment-formats
        '(("java"       . "/*")
          ("javascript" . "//")
          ("typescript" . "//")
          ("vue"        . "//")
          ("php"        . "/*")
          ("pug"        . "//")
          ("css"        . "/*")))
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(use-package pug-mode
  :defer t)

(use-package emmet-mode
  :hook ((scss-mode . emmet-mode) (css-mode . emmet-mode) (ng2-html-mode . emmet-mode) (html-mode . emmet-mode))
  :defer t)

(use-package css-mode
  :defer 10
  :hook ((css-mode . my-setup-tabnine) (scss-mode . my-setup-tabnine))
  :config
  (defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm)))

(use-package json-mode
  :defer 5
  :hook (json-mode . format-all-mode))

(use-package dart-mode
  :defer t
  ;; Optional
  :hook (dart-mode . flutter-test-mode))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-c C-r" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/Applications/flutter/"))

(use-package lua-mode

  :defer t)

(use-package docker-compose-mode
  :defer t)

(use-package dockerfile-mode
  :defer t
  :config
  (add-hook 'compilation-filter-hook #'my-remove-cr -90))

(use-package jenkinsfile-mode
  :defer t
  :config)

(use-package kubernetes
  :defer 6
  :commands (kubernetes-overview)
  :bind (:map evil-normal-state-map
              ("SPC o K" . kubernetes-overview))
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package k8s-mode
  :defer t)

(use-package kubernetes-evil
  :after kubernetes)

(use-package nginx-mode
  :defer t)

(use-package jinja2-mode
  :defer t)

(use-package grip-mode
  :after markdown-mode
  :custom
  (browse-url-browser-function 'browse-url-generic)
  ;; (grip-url-browser #'browse-url-firefox-program)
  :config
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential))))

(use-package org
  :mode (("\\.org$" . org-mode))
  :bind (:map evil-normal-state-map
              ("SPC h ]" . org-next-visible-heading)
              ("SPC h [" . org-previous-visible-heading))
  :config
  (setq org-src-preserve-indentation t)
  (add-hook 'org-mode-hook
            (lambda () (imenu-add-to-menubar "Imenu")))
  (setq org-imenu-depth 8)
  (@setup-org-mode-faces)
  

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"     ; A task that needs doing & is ready to do
           "PROJ(p)"     ; A project, which usually contains other tasks
           "IDEA(i)"     ; Idea
           "PROGRESS(s)" ; A task that is in progress
           "WAIT(w)"     ; Something external is holding up this task
           "TEST(c)"     ; In TEST statement
           "BLOCK(b)"    ; task blocked
           "REJECTED(x)" ; somebody rejected idea :(
           "FEEDBACK(f)" ; Feedback required
           "REVIEW(r)"   ; Somebody reviewed your feature
           "HOLD(h)"     ; This task is paused/on hold because of me
           "|"
           "DONE(d)"     ; Task successfully completed
           "KILL(k)")    ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"      ; A task that needs doing
           "[-](S)"      ; Task is in progress
           "[?](W)"      ; Task is being held up or paused
           "|"
           "[X](D)"))    ; Task was completed
        org-todo-keyword-faces
        '(("[-]"        . +org-todo-active)
          ("PROGRESS"   . org-todo)
          ("DONE"       . org-todo)
          ("IDEA"       . org-todo)
          ("[?]"        . +org-todo-onhold)
          ("WAIT"       . +org-todo-onhold)
          ("TEST"       . +org-todo-active)
          ("FEEDBACK"   . +org-todo-onhold)
          ("REVIEW"     . +org-todo-onhold)
          ("HOLD"       . +org-todo-onhold)
          ("PROJ"       . +org-todo-project)
          ("BLOCK"       . +org-todo-cancel)
          ("REJECTED"       . +org-todo-cancel)
          ("KILL"       . +org-todo-cancel)))

  (setq org-hide-emphasis-markers t)
  (setq org-use-property-inheritance t)

  (add-to-list 'org-tag-faces '("@.*" . (:foreground "red")))

  ;; Increase priorities count
  (setq org-highest-priority ?A
        org-default-priority ?C
        org-lowest-priority ?E)


  (defun publish-org-blog()
    "Publish this note to du-blog!"
    (interactive)
    (require 'ox-gfm)
    (setq org-export-with-sub-superscripts '{})
    (defun org-gfm-format-toc (headline) "")
    (org-gfm-export-to-markdown)
    (let ((file-path (replace-regexp-in-string " " "\\\\\  " (buffer-file-name))))

      (message (concat
                "node /Users/darkawower/projects/pet/it-blog/emacs-blog/index.js"
                file-path))
      (shell-command
       (concat
        "node /Users/darkawower/projects/pet/it-blog/emacs-blog/index.js "
        file-path))))

  (setenv "NODE_PATH"
          (concat
           (getenv "HOME") "/org-node/node_modules"  ":"
           (getenv "NODE_PATH")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((typescript . t)
     (js . t)
     (restclient . t)))

  (defun org-babel-execute:typescript (body params)
    (let ((org-babel-js-cmd "npx ts-node < "))
      (org-babel-execute:js body params)))

  (defvar org-babel-js-function-wrapper
    ""
    "Javascript code to print value of body.")
  ;; Applications for opening from org files
  (if (assoc "\\.pdf\\'" org-file-apps)
      (setcdr (assoc "\\.pdf\\'" org-file-apps) 'emacs)
    (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs) t))
  (add-hook 'org-mode-hook
            (lambda () (imenu-add-to-menubar "Imenu"))))

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
                           (push '("#+TITLE:" . "") prettify-symbols-alist)
                           (push '("#+DESCRIPTION:" . "") prettify-symbols-alist)
                           (push '("#+ID:" . "") prettify-symbols-alist)
                           (push '("#+FILETAGS:" . "") prettify-symbols-alist)
                           (push '("#+STARTUP:" . "") prettify-symbols-alist)
                           (push '("#+ACTIVE:" . "") prettify-symbols-alist)
                           (push '("#+START_SPOILER" . "") prettify-symbols-alist)
                           (push '("#+CLOSE_SPOILER" . "") prettify-symbols-alist)
                           (push '("#+BEGIN_HIDDEN" . "") prettify-symbols-alist)
                           (push '("#+END_HIDDEN" . "") prettify-symbols-alist)
                           (prettify-symbols-mode)))

(use-package org-fancy-priorities
  :after org
  :ensure t
  :hook (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '((?A . "🔥")
                                    (?B . "⬆")
                                    (?C . "❗")
                                    (?D . "⬇")
                                    (?E . "❓")
                                    (?1 . "🔥")
                                    (?2 . "⚡")
                                    (?3 . "⮮")
                                    (?4 . "☕")
                                    (?I . "Important"))))

(use-package org-indent
  :defer 8
  :init
  (add-hook 'org-mode-hook 'org-indent-mode))

(use-package org-superstar
  :defer 5
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-directory "~/Yandex.Disk.localized/Dropbox/org")
  (setq org-agenda-files (append (directory-files-recursively "~/Yandex.Disk.localized/Dropbox/org/" "\\.org$")
                                 (directory-files-recursively "~/projects/pet" "\\.org$"))))

(use-package org-roam
  :after org
  :bind (:map evil-normal-state-map
               ("SPC n r i" . org-roam-node-insert))
  :init
  (setq org-roam-v2-ack t)
  :config
  ;; (org-roam-db-autosync-enable)
  (cl-defmethod org-roam-node-mtitle ((node org-roam-node))
    "Return customized title of roam node"
    (let* ((tags (org-roam-node-tags node))
           (title (org-roam-node-title node)))
      (if (not tags)
          title
        (setq joined-text (string-join tags ", "))
        (concat (propertize (format "(%s) " joined-text) 'face `(:foreground ,+m-color-main :weight bold :slant italic)) title))))
  ;; (setq org-roam-completion-system 'ivy)
  (setq org-roam-completion-system 'vertico)
  (setq org-roam-node-display-template "${mtitle:100}")
  (setq org-roam-directory (file-truename "~/org-roam")))

(use-package websocket
  :after org-roam)

(use-package org-yt
  :defer 20
  :config
  (defun org-image-link (protocol link _description)
    "Interpret LINK as base64-encoded image data."
    (cl-assert (string-match "\\`img" protocol) nil
               "Expected protocol type starting with img")
    (let ((buf (url-retrieve-synchronously (concat (substring protocol 3) ":" link))))
      (cl-assert buf nil
                 "Download of image \"%s\" failed." link)
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "\r?\n\r?\n")
        (buffer-substring-no-properties (point) (point-max)))))

  (org-link-set-parameters
   "imghttp"
   :image-data-fun #'org-image-link)

  (org-link-set-parameters
   "imghttps"
   :image-data-fun #'org-image-link))

(use-package web-roam
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC n p" . web-roam-publish-file)))

(use-package ob-async
  :defer t
  :config
  (setq ob-async-no-async-languages-alist '("ipython")))

(use-package restclient
  :defer t)

(use-package ob-restclient
  :defer 8)

(use-package ob-dart
  :after org
  :defer t
  :config
  (add-to-list 'org-babel-load-languages  '(dart . t)))

(use-package ob-typescript
  :defer t
  :config
  (setq ob-async-no-async-languages-alist '("ipython")))

(use-package org-insert
  :bind (:map org-mode-map
              ("<C-return>" . +org/insert-item-below)
              ("<C-S-return>" . +org/insert-item-above))
  :straight (org-insert :type git :host github :repo "hlissner/doom-emacs" :files ("modules/lang/org/autoload/org.el")))

(defun my-set-spellfu-faces ()
  "Set faces for correct spell-fu working"
  (interactive)
  (setq spell-fu-faces-include '(tree-sitter-hl-face:comment
                                 tree-sitter-hl-face:doc
                                 tree-sitter-hl-face:string
                                 tree-sitter-hl-face:function
                                 tree-sitter-hl-face:variable
                                 tree-sitter-hl-face:type
                                 tree-sitter-hl-face:method
                                 tree-sitter-hl-face:function.method
                                 tree-sitter-hl-face:function.special
                                 tree-sitter-hl-face:attribute
                                 font-lock-comment-face
                                 font-lock-doc-face
                                 font-lock-string-face
                                 lsp-face-highlight-textual
                                 default))
  (setq spell-fu-faces-exclude (append spell-fu-faces-exclude
                                       '(diredfl-file-name))))
(use-package spell-fu
  :bind (:map evil-normal-state-map
              ("z g" . spell-fu-word-add))
  :defer 5
  :config
  (setq ispell-program-name "aspell")
  (setq spell-fu-directory "~/.doom.d/dictionary")
  (setq ispell-program-name "aspell"
        ;;           ;; Notice the lack of "--run-together"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=56"))
  (setq spell-fu-ignore-modes '(dired-mode vterm-mode elfeed-search-mode))

  (add-hook 'spell-fu-mode-hook
            (lambda ()
              (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
              (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "ru"))
              (spell-fu-dictionary-add
               (spell-fu-get-personal-dictionary "en-personal" "/Users/darkawower/.doom.d/dictionary/.pws"))
              (spell-fu-dictionary-add
               (spell-fu-get-personal-dictionary "ru-personal" "/Users/darkawower/.doom.d/dictionary/ru.pws"))))

  ;; Camel case support
  (setq-default spell-fu-word-regexp
                (rx
                 (or
                  ;; lowercase
                  (seq
                   (one-or-more lower)
                   (opt
                    (any "'’")
                    (one-or-more lower)
                    word-end))

                  ;; capitalized
                  (seq
                   upper
                   (zero-or-more lower)
                   (opt
                    (any "'’")
                    (one-or-more lower)
                    word-end))

                  ;; uppercase
                  (seq
                   (one-or-more upper)
                   (opt
                    (any "'’")
                    (one-or-more upper)
                    word-end)))))

  (defun cs/spell-fu-check-range (pos-beg pos-end)
    (let (case-fold-search)
      (spell-fu-check-range-default pos-beg pos-end)))

  (setq-default spell-fu-check-range #'cs/spell-fu-check-range)
  (global-spell-fu-mode)
  (my-set-spellfu-faces))

(use-package lsp-grammarly
  :defer t)

(use-package google-translate
  :defer 10
  :bind (:map google-translate-minibuffer-keymap
        ("C-k" . google-translate-next-translation-direction)
        ("C-n" . google-translate-next-translation-direction)
        ("C-l" . google-translate-next-translation-direction)
        :map evil-normal-state-map
        ("\\ t" . google-translate-smooth-translate))
  :config
  (require 'google-translate-smooth-ui)
  (setq google-translate-backend-method 'curl)
  (setq google-translate-pop-up-buffer-set-focus t)
  (setq google-translate-translation-directions-alist
        '(("en" . "ru") ("ru" . "en") ))
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130)))

(use-package wakatime-mode
  :defer 2
  :config
  (global-wakatime-mode))

(recentf-mode)

(use-package projectile
  :config
  (projectile-mode +1))

(use-package helpful
  :defer t
  :bind (("C-h k" . helpful-key)
         :map evil-normal-state-map
         ("SPC h v" . helpful-variable)
         ("SPC h f" . helpful-function)
         ("SPC h ." . helpful-at-point)))

(use-package vertico
  :after evil-collection
  :bind (:map evil-normal-state-map
              ("SPC '" . vertico-repeat)
              ("SPC f P" . (lambda ()
                             (interactive)
                             (call-interactively 'find-file)
                             (kill-whole-line)
                             (insert "~/pure-emacs")))
              :map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-SPC" . vertico-quick-jump)
              ("C-d" . vertico-scroll-up)
              ("C-u" . vertico-scroll-down)
              ("C-o" . embark-act)
              ("C-q" . vertico-exit-input)
              ("C-n" . vertico-next-group)
              ("C-p" . vertico-previous-group)
              ("<escape>" . abort-minibuffers)
              ("C-d" . (lambda ()
                         (interactive)
                         (kill-whole-line)
                         (insert "~/")))
              ("C-o" . (lambda ()
                         (interactive)
                         (embark-act)))
              ("C-r" . (lambda ()
                         (interactive)
                         (kill-whole-line)
                         (insert "/"))))
  :init
  (vertico-mode)
  (vertico-buffer-mode -1)
  (setq vertico-cycle t)
  :config
  (add-hook 'minibuffer-setup-hook 'vertico-repeat-save))

(use-package vertico-repeat
  :after vertico)

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init
  (marginalia-mode))

(use-package consult
  :defer t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("s-i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ;; ("s-." . consult-find)
         ("M-s D" . consult-locate)
         ;; ("C-c C-m" . consult-grep)
         ;;         ("C-c C-g" . consult-git-grep)
         ;;        ("C-c C-r" . consult-ripgrep)
         ("s-f" . consult-line)
         ;;         ("C-g C-m" . consult-line-multi)
         ("M-s m" . consult-multqqi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)
         :map evil-normal-state-map
         ("SPC /" . consult-ripgrep)
         ("SPC SPC" . projectile-find-file))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  ;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
      ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
      ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
      ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
      ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

(use-package consult-projectile :defer t)

(use-package embark
  :custom
  (embark-prompter 'embark-completing-read-prompter)
  (embark-verbose-indicator-excluded-actions t)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; :config
	      )
  ;; ;; Hide the mode line of the Embark live/completions buffers
  ;; (add-to-list 'display-buffer-alist
  ;;              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
  ;;                nil
  ;;                (window-parameters (mode-line-format . none))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package all-the-icons-completion
  :after vertico
  :config
  (all-the-icons-completion-mode))

(use-package pdf-view
  :defer t
  :hook (pdf-view-mode . pdf-view-themed-minor-mode))
