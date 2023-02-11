(setq warning-minimum-level :emergency)
(setq warning-suppress-log-types '((comp) (undo discard-info)))

(let* ((normal-gc-cons-threshold (* 20 1024 1024))
     (init-gc-cons-threshold (* 128 1024 1024)))
(setq gc-cons-threshold init-gc-cons-threshold)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 20 1024 1024)))))

(setq read-process-output-max (* 1024 1024))

(require 'package)

(customize-set-variable 'package-archives
                        `(,@package-archives
                          ("melpa" . "http://melpa.org/packages/")
                          ;; ("melpa" . "http://melpa.milkbox.net/packages/")
                          ( "jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
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

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

(use-package fnhh
  :straight (:type git :host github :repo "a13/fnhh")
  :config
  (fnhh-mode 1))

;; Change backup folders
(setq backup-directory-alist '(("." . "/Users/darkawower/tmp/emacs-backups")))

(setq confirm-kill-emacs 'y-or-n-p)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq initial-major-mode (quote fundamental-mode))

(use-package direnv
  :config
  (direnv-mode))

(defalias 'use-package! 'use-package
"Alias for call use-package from doom modules")

(ignore-errors
  (load! "~/pure-emacs/private.el"))

(when (eq system-type 'darwin)
  ;; (setq browse-url-firefox-program "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser")
  (setq browse-url-firefox-program nil)
  (setq browse-url-generic-program "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
        browse-url-browser-function 'browse-url-generic))

(set-default 'truncate-lines t)

(use-package doom-lib
  :ensure t
  :straight (doom-lib
             :host github
             :repo "hlissner/doom-emacs"
             :files ("lisp/doom-lib.el" "lisp/lib/buffers.el")))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defun doom-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local, :append, and/or :depth [N], which will make the
     hook buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be a quoted function, a quoted list
     thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
     implicitly be wrapped in a lambda).

\(fn HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (doom--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil))
                     next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook (nreverse ',hook-forms))
         (dolist (func (list ,@func-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))))))

(defun doom--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (ensure-list (doom-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defvar @before-buffer-changed-hook nil
  "Hook run before a buffer is changed.")

(add-to-list 'window-buffer-change-functions
             (lambda (&rest _)
               (run-hooks '@before-buffer-changed-hook)))

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

(defun @change-buffer (change-buffer)
  "Call CHANGE-BUFFER until current buffer is not in `my-skippable-buffers'."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (string-prefix-p "*" (buffer-name))
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))

(defun @next-buffer ()
  "Variant of `next-buffer' that skips `my-skippable-buffers'."
  (interactive)
  (@change-buffer 'next-buffer))

(defun @previous-buffer ()
  "Variant of `previous-buffer' that skips `my-skippable-buffers'."
  (interactive)
  (@change-buffer 'previous-buffer))

(defun my-remove-cr (&optional begin end)
  "Remove line prefixes ending with carriage-return.

BEGIN END specifies region, otherwise works on entire buffer."
  (save-excursion
    (goto-char (or begin (point-min)))
    (while (re-search-forward "^.*\033\\[2K\033\\[1G" end t)
      (replace-match ""))))

(defun toggle-maximize-buffer ()
	"Maximize buffer"
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

(defun @delete-this-file (&optional path force-p)
  "Delete PATH, kill its buffers and expunge it from vc/magit cache.
If PATH is not specified, default to the current buffer's file.
If FORCE-P, delete without confirmation."
  (interactive
   (list (buffer-file-name (buffer-base-buffer))
         current-prefix-arg))
  (let* ((path (or path (buffer-file-name (buffer-base-buffer))))
         (short-path (and path (abbreviate-file-name path))))
    (unless path
      (user-error "Buffer is not visiting any file"))
    (unless (file-exists-p path)
      (error "File doesn't exist: %s" path))
    (unless (or force-p (y-or-n-p (format "Really delete %S?" short-path)))
      (user-error "Aborted"))
    (let ((buf (current-buffer)))
      (unwind-protect
          (progn (delete-file path t) t)
        (if (file-exists-p path)
            (error "Failed to delete %S" short-path)
          ;; Ensures that windows displaying this buffer will be switched to
          ;; real buffers (`doom-real-buffer-p')
          (doom/kill-this-buffer-in-all-windows buf t)
          (doom-files--update-refs path)
          (message "Deleted %S" short-path))))))

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

(defun my-insert-tab ()
  "Insert simple tab"
  (interactive)
  (insert "\t"))

(defun @open-emacs-config ()
  "Open folder with emacs config"
  (interactive)
  (let ((default-directory "~/pure-emacs/README.org"))
    (call-interactively 'find-file)))

(defun @switch-to-scratch ()
  "Switch to scratch buffer"
  (interactive)
  (let* ((buffer-name "*scratch*")
         (buffer-exist (get-buffer buffer-name))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (pop-to-buffer buffer)
      (when (equal (buffer-substring-no-properties (point-min) (point-max)) "")
        (persistent-scratch-restore))
      (evil-insert-state)
      )))

(defun @run-sass-auto-fix ()
  "Run sass auto fix if cli tool exist"
  (interactive)
  (save-window-excursion
    (let ((default-directory (file-name-directory buffer-file-name)))
      (async-shell-command "sass-lint-auto-fix")
      ;; (revert-buffer-no-confirm)
      (message "SASS FORMATTED"))))

(defun @find-definition ()
  "Find lsp definition when lsp exist and enabled, or find evil definition."
  (interactive)
  (cond ((bound-and-true-p lsp-bridge-mode) (lsp-bridge-find-def))
        ((and (bound-and-true-p lsp-mode) (bound-and-true-p lsp-ui-mode) lsp-ui-mode) (lsp-ui-peek-find-definitions))
        ((and (bound-and-true-p lsp-mode) lsp-mode) (lsp-find-definition))
        (t (evil-goto-definition))))

(setq use-package-verbose t)

(use-package all-the-icons
  :ensure t
  :config
  (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'prepend)
  (setq inhibit-compacting-font-caches t))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(use-package ns-auto-titlebar
  :config
  (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

(setq +m-color-main "#61AFEF"
      +m-color-secondary "#FF3399"
      +m-color-yellow "#FFAA00"
      +m-color-blue "#00AEE8"
      +m-color-cyan "#00CED1"
      +m-color-green "#00D364"
      +org-todo-onhold "#FFAA00")

(use-package all-the-icons-dired
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package rainbow-mode
  :hook ((css-mode scss-mode org-mode typescript-mode js-mode emacs-lisp-mode dart-mode). rainbow-mode)
  :defer t)

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

(use-package rainbow-mode
  :defer t
  :hook ((css-mode emacs-lisp-mode org-mode) . rainbow-mode))

(pixel-scroll-mode)

(defun @correct-my-fringe (&optional ignore)
  "Set fringes for current active window."
  (interactive)
  (unless (eq fringe-mode '16)
    (fringe-mode '16))
;; (set-window-margins nil 2)
)


(add-hook 'after-init-hook #'@correct-my-fringe)
(add-hook 'buffer-list-update-hook #'@correct-my-fringe)
;; (set-window-margins (selected-window) 1 1)
;; (fringe-mode '(32 . 0))

(use-package display-fill-column-indicator
  :defer t
  :config
  (setq display-fill-column-indicator-column 80))

(defvar after-load-theme-hook nil
   "Hook run after a color theme is loaded using `load-theme'.")
 (defadvice load-theme (after run-after-load-theme-hook activate)
   "Run `after-load-theme-hook'."
   (run-hooks 'after-load-theme-hook))

(defadvice load-theme (before theme-dont-propagate activate)
 (mapcar #'disable-theme custom-enabled-themes))

(fringe-mode '16)

(progn
  (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
  (add-to-list 'default-frame-alist '(alpha . (95 . 95))))

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

(use-package doom-modeline
  :defer t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'file-name))

;; (setq straight-current-profile 'pinned)
;; (add-to-list 'straight-x-pinned-packages
;;                '("catppuccin-theme" . "86b16248ef2ce450def4a00a0ca5170c102da623"))

;; (package-vc-install "https://github.com/catppuccin/emacs" "300e7ba1d5b4225db4f9f2d451eac1238073d3ba")
;; (package-vc-install "https://github.com/catppuccin/emacs" "785c9f57a43c4c90041290556d999db3d9da5be9")

(use-package catppuccin-theme
  :ensure t
  ;; :vc (:fetcher "github" :repo catppuccin/emacs :rev 86b16248ef2ce450def4a00a0ca5170c102da623)
  ;; :vc (:fetcher "github" :repo "catppuccin/emacs")
  :straight (catppuccin :type git :host github :repo "artawower/emacs" :branch "old")
;; :straight t
  :config
  (setq catppuccin-flavor 'latte))

(use-package dashboard
  ;; :ensure t
  :config
  (dashboard-setup-startup-hook))

(set-frame-font "JetBrainsMono Nerd Font 15" nil t)
;; (set-frame-font "Monaco Font 15" nil t)

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
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font 14" :italic nil :height 146)
  (@setup-org-mode-faces))

(defun nano-change-theme-light ()
  (interactive)
  (nano-theme-set-light)
  (nano-faces)
  (nano-theme)
  (set-face-attribute 'default nil :font "JetBrainsMono Nerd Font 14" :italic nil :height 146)
  (@setup-org-mode-faces))

(defun nano-change-theme ()
  (interactive)
  (if nano-theme-light-var (nano-change-theme-dark) (nano-change-theme-light))
  (setq nano-theme-light-var (not nano-theme-light-var)))

(defun @set-vterm-autocomplete-color ()
  "Change color of autocomplete inside vterm"
  (interactive)
  (when (facep 'vterm-color-black)
    (set-face-attribute 'vterm-color-black nil :foreground +m-color-secondary :background +m-color-secondary)))

(use-package auto-dark
  ;; :hook ((after-load-theme-hook auto-dark-mode) . (lambda () (set-face-attribute 'vterm-color-black nil :foreground +m-color-secondary :background +m-color-secondary)))
  :config
  ;; (defun auto-dark--ns-set-theme (appearance)
  ;;     "Set light/dark theme using emacs-plus ns-system-appearance.
  ;; Argument APPEARANCE should be light or dark."
  ;;     ;; (mapc #'disable-theme custom-enabled-themes)
  ;;     (pcase appearance
  ;;       ('dark
  ;;        (nano-change-theme-dark)
  ;;        (run-hooks 'auto-dark-dark-mode-hook))
  ;;       ('light
  ;;        (nano-change-theme-light)
  ;;        (run-hooks 'auto-dark-light-mode-hook))))
  ;;   (add-hook 'auto-dark-mode-hook #'nano-change-theme-dark)
  ;;   (add-hook 'auto-light-mode-hook #'nano-change-theme-light)
  (add-hook 'auto-dark-dark-mode-hook #'@set-vterm-autocomplete-color)
  (add-hook 'auto-dark-light-mode-hook #'@set-vterm-autocomplete-color)
  (add-hook 'auto-dark-dark-mode-hook #'posframe-delete-all)
  (add-hook 'auto-dark-light-mode-hook #'posframe-delete-all)
  ;; (setq auto-dark-dark-theme 'doom-moonlight)
  (setq auto-dark-dark-theme 'catppuccin-frappe)
  (setq auto-dark-light-theme 'doom-one-light)
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

(use-package general
  :config
  (general-define-key
   :keymaps 'override
   "C-w" 'backward-kill-word
   "s-w" '+workspace/close-window-or-workspace
   "C-h C-k" 'describe-key-briefly
   "C-h C-b" 'describe-keymap
   "\t" 'google-translate-smooth-translate
   "s-<backspace>" 'evil-delete-back-to-indentation
   "C-<tab>" 'my-insert-tab
   "C-h C-m" 'describe-mode
   "C-h C-f" 'describe-face
   "s-p" 'yank-from-kill-ring
   "s-n" 'evil-buffer-new
   "s-P" 'xah-paste-from-register-1
   "C-x C-o" 'company-complete
   "s-k" (lambda () (interactive) (end-of-line) (kill-whole-line)))
  (general-define-key
   :states '(insert)
   "C-u" 'evil-delete-back-to-indentation
   "s-y" 'yas-expand
   "s-P" 'xah-paste-from-register-1
   "s-." 'ace-window)
  (general-define-key
   :keymaps '(minibuffer-local-map read--expression-map minibuffer-local-shell-command-map)
   "C-w" 'backward-kill-word
   "C-k" 'previous-history-element
   "C-p" 'previous-history-element
   "C-u" 'evil-delete-back-to-indentation
   ;; "<tab>" 'completion-at-point
   ;; "<tab>" 'completion-at-point
   "C-j" 'next-history-element
   "C-n" 'next-history-element
   "<escape>" 'keyboard-escape-quit
   "C-x" (lambda () (interactive) (end-of-line) (kill-whole-line)))
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   "C-u" 'evil-scroll-up
   "s-Y" 'xah-copy-to-register-1
   "s-r" (lambda () (interactive) (set-mark-command nil) (evil-avy-goto-char))
   "s-P" 'xah-paste-from-register-1)
  (general-define-key
   :states '(visual normal)
   :keymaps 'override
   :prefix "\\"
    "f" 'avy-goto-char
    "b" 'my-switch-to-xwidget-buffer
    "w" 'avy-goto-word-0
    "]" 'flycheck-next-error
    "[" 'flycheck-previous-error
    "d" 'dap-debug
  
    "o" 'org-mode
    "q" 'kill-current-buffer
    "v" 'vterm
    "`" 'vterm-toggle-cd
    "i" 'git-messenger:popup-message
    "t" 'google-translate-smooth-translate
    "T" 'google-translate-query-translate
    "a" 'counsel-org-agenda-headlines
    "c" 'dired-create-empty-file
    "p" 'format-all-buffer--no-bufferjump
    "s" 'publish-org-blog
    "g" 'codegpt
    ;; Evil
    "=" 'evil-record-macro
    "-" 'evil-execute-macro
    "0" 'my-toggle-default-browser
    "h" 'lsp-ui-doc-toggle
    "e" 'lsp-treemacs-errors-list
    "l" 'lsp-execute-code-action
    "r" 'treemacs-select-window
    "m" 'toggle-maximize-buffer)
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   :prefix "SPC"
   "SPC"  'projectile-find-file
   "tr" 'read-only-mode
   "hre" (lambda () (interactive) (load-file "~/pure-emacs/init.el"))
   "hm" 'describe-mode
   "om" (lambda () (interactive) (pop-to-buffer "*Messages*"))
   "mox" 'execute-extended-command
   "bI" 'ibuffer
   "vtt" 'my-toggle-transparency
   "fD" '@delete-this-file
   "+" 'narrow-to-region
   "-" 'widen
   "er" 'eval-region
   "ti" 'my-insert-todo-by-current-git-branch
  
   ;; ;; Presentation
   ;; ("SPC t b" . presentation-mode)
   ;; ;; TODO move to treemacs
   ;; ;; Treemacs
   ;; ("SPC o p"   . treemacs)
   ;; ("SPC t a" . treemacs-add-project-to-workspace)
   ;; ("SPC o P" . treemacs-find-file)
   ;; ;; Window
   "wr" 'evil-window-rotate-downwards
   "wv" 'evil-window-vsplit
   "ws" 'evil-window-split
   ;; ;; Buffers
   "b ]" '@next-buffer
   "b [" '@previous-buffer
   "]" '@next-buffer
   "[" '@previous-buffer
   "." 'find-file
   "hv" 'describe-variable
   "hf" 'describe-function
   "hF" 'describe-face
   "tl" 'global-display-line-numbers-mode
   "bn" 'evil-buffer-new
   "vl" 'visual-line-mode
   "ht" 'load-theme)

    (general-define-key
   :keymaps 'read-expression-map
   "C-w" 'backward-kill-word
   "C-h" 'previous-history-element
   "ESC" 'keyboard-escape-quit)

  (general-define-key
   :keymaps 'org-src-mode-map
   "C-c C-c" 'org-edit-src-exit)

(general-override-mode))

(use-package reverse-im
  :config
  (reverse-im-activate "russian-computer"))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  (evil-commentary-mode))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :config
  (define-key evil-visual-state-map (kbd ">") '@evil-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") '@evil-shift-left-visual)
  (define-key evil-visual-state-map [tab] '@evil-shift-right-visual)
  (define-key evil-visual-state-map [S-tab] '@evil-shift-left-visual)
  
  (defun @evil-shift-left-visual ()
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))
  
  (defun @evil-shift-right-visual ()
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))
  (defun evil-next-line--check-visual-line-mode (orig-fun &rest args)
    (if visual-line-mode
        (apply 'evil-next-visual-line args)
      (apply orig-fun args)))
  
  (advice-add 'evil-next-line :around 'evil-next-line--check-visual-line-mode)
  
  (defun evil-previous-line--check-visual-line-mode (orig-fun &rest args)
    (if visual-line-mode
        (apply 'evil-previous-visual-line args)
      (apply orig-fun args)))
  
  (advice-add 'evil-previous-line :around 'evil-previous-line--check-visual-line-mode)
  (setq evil-respect-visual-line-mode t)
  (evil-set-undo-system 'undo-redo)
  (setq-default evil-kill-on-visual-paste nil)
  (evil-mode 1))

(use-package bm
:defer t
:custom-face
(bm-face ((t (:foreground ,+m-color-secondary :background unspecified))))
:custom
(bm-in-lifo-order t)
:bind (("C-M-n" . bm-next)
        ("C-M-p" . bm-previous)
        ("s-b" . bm-toggle)))

(use-package avy
  :defer t
  :general
  (:states '(normal visual)
           :keymaps 'override
           "f" 'avy-goto-word-1
           "SPC k l" 'avy-kill-whole-line
           "SPC k r" 'avy-kill-region)
  (:keymaps '(minibuffer-local-mode-map read--expression-map)
            "C-l" 'avy-goto-char
            "C-f" 'avy-goto-char)
  :custom
  (avy-single-candidate-jump t)
  (avy-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?o ?p ?a ?s ?d ?f ?g ?h ?j ?k ?l ?z ?x ?c ?v ?b ?n ?m)))

(defun @avy-ignore-current-buffer ()
  "Ignore current buffer when using avy."
  (interactive)
  (add-to-list 'aw-ignored-buffers (buffer-name)))

(use-package ace-window
  :bind (:map evil-normal-state-map
              ("s-." . ace-window)
              ("SPC a i" . @avy-ignore-current-buffer))
  :defer t
  :config
  (setq aw-ignored-buffers (delq 'treemacs-mode aw-ignored-buffers))
  (add-to-list 'aw-ignored-buffers 'dap-ui--locals-buffer)
  (defun @avy-ignore-current-buffer ()
    "Ignore current buffer when using avy."
    (interactive)
    (add-to-list 'aw-ignored-buffers (buffer-name))))

(use-package evil-matchit
:defer t)

(evilmi-load-plugin-rules '(ng2-html-mode) '(html))
(global-evil-matchit-mode 1)

(defun @better-jump-preserve-pos-advice (oldfun &rest args)
  "Preserve position when jumping."
  (let ((old-pos (point)))
    (apply oldfun args)
    (when (> (abs (- (line-number-at-pos old-pos) (line-number-at-pos (point))))
             1)
      (better-jumper-set-jump old-pos))))

(use-package better-jumper
  ;; :hook
  ;; (consult-after-jump . @better-jump-preserve-pos)
  ;; (buffer-list-update . (lambda () (better-jumper-set-jump)))
  ;; (@before-buffer-changed . (lambda () (better-jumper-set-jump)))
  :general
  (:states '(normal, visual)
           "C-o" 'better-jumper-jump-backward
           "C-i" 'better-jumper-jump-forward
           "SPC sp" 'better-jumper-set-jump)
  :custom
  ;; (better-jumper-add-jump-behavior #'replace)
  (better-jumper-use-evil-jump-advice nil)
  :config
  ;; (advice-add 'evil-next-line :around #'@better-jump-preserve-pos-advice)
  ;; (advice-add 'evil-previous-line :around #'@better-jump-preserve-pos-advice)
  (advice-add '@find-definition :around #'@better-jump-preserve-pos-advice)
  (advice-add '@find-definition :around 'evil-scroll-line-to-center)
  (better-jumper-mode 1))

(use-package ts-movement
  ;; :straight (ts-movement :type git :host github :repo "haritkapadia/ts-movement")
  :load-path "vendor/ts-movement"
  :bind (:map evil-normal-state-map
              ("C-c ." . tsm/hydra/body)
              ("\"" . tsm/clear-overlays)
              ("M-[" . tsm/node-parent)
              ("M-]" . tsm/node-child)
              ("M-}" . tsm/node-next)
              ("M-{" . tsm/node-prev))
  :hook (prog-mode . ts-movement-mode))

(use-package ts-hopper
  :straight (ts-hopper :type git :host github :repo "artawower/ts-hopper.el")
  :init
  (ts-hopper-init)
  :general (:states '(normal visual)
                 :keymaps 'override
                 "\'" 'ts-hopper-define-context
                 "\\k" 'ts-hopper-mode
                 "[" 'ts-hopper-hop-prev-context
                 "]" 'ts-hopper-hop-next-context)
  :config
  (add-hook 'ts-hopper--after-hop-hook (lambda () (call-interactively 'evil-scroll-line-to-center))))

(use-package dired
  :defer t
  :ensure nil
  :general
  (:keymaps 'dired-mode-map
	    "C-c C-e" 'wdired-change-to-wdired-mode)
  :config
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (add-hook 'dired-mode-hook 'auto-revert-mode))

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

(use-package treemacs
  :defer t
  :bind (:map treemacs-mode-map
              ("@" . evil-execute-macro))
  :custom-face
  (font-lock-doc-face ((t (:inherit nil))))
  (doom-themes-treemacs-file-face ((t (:inherit font-lock-doc-face :slant italic))))
  (doom-themes-treemacs-root-face ((t (:inherit nil :slant italic))))
  (treemacs-root-face ((t (:inherit variable-pitch :slant italic))))
  :custom
  (treemacs-width 45)
  :config
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; (doom-themes-org-config)
)

(use-package file-info
  :bind (("C-c d" . 'file-info-show))
  :defer t
  :config
  (setq hydra-hint-display-type 'posframe)
  (setq hydra-posframe-show-params '(:poshandler posframe-poshandler-frame-center
                                               :internal-border-width 3
                                               :internal-border-color "#61AFEF"
                                               :left-fringe 16
                                               :right-fringe 16)))

(use-package reveal-in-osx-finder
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC o f" . reveal-in-osx-finder))
  :ensure t)

(defun @clear-term-history ()
  "Clear terminal history inside vterm."
  (interactive)
  (when (eq major-mode 'vterm-mode)
    (vterm--self-insert)
    (vterm-send-string "clear")
    (vterm-send-return)))
  

(advice-add 'vterm-clear-scrollback :before #'@clear-term-history)

(defun @vterm-open-here ()
  "Open nodejs repl inside current buffer!"
  (interactive)
  (let ((nodejs-repl-buffer-name "*vterm*"))
    (if (get-buffer nodejs-repl-buffer-name)
        (switch-to-buffer nodejs-repl-buffer-name)
      (progn
        (switch-to-buffer nodejs-repl-buffer-name)
        (vterm)))))

(use-package vterm
  :defer 5
  :hook (vterm-mode . (lambda () (set-face-attribute 'vterm-color-black nil :foreground +m-color-secondary :background +m-color-secondary)))
  :custom
  (vterm-max-scrollback 5000)
  :general
  (:states '(normal visual)
           "SPC ov" 'vterm
           "SPC oV" '@vterm-open-here)
  (:keymaps '(vterm-mode-map vterm-copy-mode-map)
            "C-u" 'vterm--self-insert)

  (:keymaps '(vterm-mode-map vterm-copy-mode-map)
            :states '(normal visual)
            "SPC mc" 'vterm-copy-mode)
  :config
  (set-face-attribute 'vterm-color-black nil :foreground +m-color-secondary :background +m-color-secondary)
  (defun @clear-term-history ()
    "Clear terminal history inside vterm."
    (interactive)
    (when (eq major-mode 'vterm-mode)
      (vterm--self-insert)
      (vterm-send-string "clear")
      (vterm-send-return)))
    
  
  (advice-add 'vterm-clear-scrollback :before #'@clear-term-history))

(defun @vterm-change-current-directory-to-active-buffer-pwd ()
  "Just exec CD to pwd of active buffer."
  (interactive)
  (when-let* ((file-name (buffer-file-name))
              (file-dir (file-name-directory file-name))
              (file-dir (replace-regexp-in-string " " "\\\\\  " file-dir)))
    ;; (save-window-excursion
      (vterm-toggle-show)
      (switch-to-first-matching-buffer "vterm")
      (evil-insert 0)
      (vterm-send-C-c)
      (vterm-send-string (concat "cd " file-dir))
      (vterm-send-return)
      ;; )
    (evil-window-down 1)))

(use-package vterm-toggle
  :defer 5
  :general (:states '(normal visual)
                    :keymaps 'override
                    "SPC oh" (lambda () (interactive)
                               (+vterm/toggle t))
                    "SPC th" 'vterm-toggle-hide
                    "SPC ot" 'vterm-toggle-cd
                    "SPC oT" '@vterm-change-current-directory-to-active-buffer-pwd
                    "SPC tk" 'my-open-kitty-right-here)
  (:states '(normal visual)
           :keymaps 'vterm-mode-map
           "SPC t]" 'vterm-toggle-forward
           "SPC t[" 'vterm-toggle-backward
           "SPC tn" (lambda () (interactive)
                      (let ((current-buffer-name (buffer-name)))
                        (vterm-toggle--new)
                        (delete-window)
                        (display-buffer current-buffer-name)
                        (vterm-toggle-forward)))
           "SPC tx" (lambda (args) (interactive "P")
                      (when (string-match "vterm" (buffer-name))
                        (let ((kill-buffer-query-functions nil))
                          (kill-this-buffer)
                          (+vterm/toggle args)))))

  :config
  (setq vterm-kill-buffer-on-exit nil)
  (setq vterm-toggle-scope 'project)
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                     (let ((buffer (get-buffer buffer-or-name)))
                       (with-current-buffer buffer
                         (or (equal major-mode 'vterm-mode)
                             (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                  (display-buffer-reuse-window display-buffer-at-bottom)
                  ;;(display-buffer-reuse-window display-buffer-in-direction)
                  ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                  ;;(direction . bottom)
                  ;;(dedicated . t) ;dedicated is supported in emacs27
                  (reusable-frames . visible)
                  (window-height . 0.3))))

(use-package secret-mode
  :straight (:type git :host github :repo "bkaestner/redacted.el")
  :defer t)

(defun @persp-kill-other-buffers ()
  "Kill all buffers except current buffer."
  (interactive)
  (let ((current-buffer (current-buffer))
        (killed-buffer-count 0))
    (mapc (lambda (buffer)
            (unless (eq buffer current-buffer)
              (setq killed-buffer-count (1+ killed-buffer-count))
              (kill-buffer buffer)))
          (persp-buffer-list))
    (message "Killed %s buffers" killed-buffer-count)))

(load "~/pure-emacs/vendor/doom-workspaces.el")

(defface +workspace-tab-selected-face
  '((t :inherit nano-face-header-popout))
  "Face for selected persp tab bar"
  :group 'persp-tabbar)

(defface +workspace-tab-face
  '((t :inherit nano-face-popout ))
  "Face for persp tab bar"
  :group 'persp-tabbar)

(defvar +workspaces-main "main"
  "The name of the primary and initial workspace, which cannot be deleted.")

(defvar +workspaces-switch-project-function #'doom-project-find-file
  "The function to run after `projectile-switch-project' or
`counsel-projectile-switch-project'. This function must take one argument: the
new project directory.")

(defvar +workspaces-on-switch-project-behavior 'non-empty
  "Controls the behavior of workspaces when switching to a new project.")

(use-package workgroups)

(use-package persp-mode
  :after workgroups
  :general
  (:keymaps 'override
  :states '(normal visual insert)
            "s-1" '+workspace/switch-to-0
            "s-2" '+workspace/switch-to-1
            "s-3" '+workspace/switch-to-2
            "s-4" '+workspace/switch-to-3
            "s-5" '+workspace/switch-to-4
            "s-6" '+workspace/switch-to-5
            "s-7" '+workspace/switch-to-6
            "s-8" '+workspace/switch-to-7
            "s-9" '+workspace/switch-to-8)
  
  (:keymaps 'override
            :states '(normal visual)
            "SPC bO" '@persp-kill-other-buffers
            "SPC bb" 'consult-projectile-switch-to-buffer
            "SPC bk" 'persp-kill-buffer
            "SPC pc" 'persp-copy
            "SPC pk" 'persp-kill
            "SPC pS" '+workspace/save
            "SPC ps" 'persp-save-state-to-file
            "SPC pL" '+workspace/load
            "SPC pl" 'persp-load-state-from-file
            "SPC pd" '+workspace/delete
            "SPC <tab>d" '+workspace/delete
            "SPC pr" '+workspace/rename
            "SPC pj" 'persp-switch
            "SPC pn" '+workspace/quick-new
            "SPC <tab>n" '+workspace/quick-new
            "SPC po" '+workspace/other
            "SPC b O" '@persp-kill-other-buffers
            "SPC <tab> <tab>" '+workspace/display
            "SPC b i" (lambda (arg)
                        (interactive "P")
                        (with-persp-buffer-list () (ibuffer arg))))
  :init
  (add-hook 'window-setup-hook (lambda () (persp-mode 1)))
	:custom
  (persp-autokill-buffer-on-remove 'kill-weak)
  (persp-reset-windows-on-nil-window-conf nil)
  (persp-add-buffer-on-after-change-major-mode t)
  (persp-nil-hidden t)
  (persp-nil-name "default")
  (persp-auto-save-fname "autosave")
  (persp-save-dir (concat default-directory "workspaces/"))
  (persp-set-last-persp-for-new-frames t)
  (persp-switch-to-added-buffer nil)
  (persp-kill-foreign-buffer-behaviour 'kill)
  (persp-remove-buffers-from-nil-persp-behaviour nil)
  (persp-auto-resume-time -1) ; Don't auto-load on startup
  (persp-auto-save-opt (if noninteractive 0 1))
  (persp-save-dir "~/tmp/workspaces/")
  :config
  (set-persp-parameter 'dont-save-to-file t nil)
  (defvar after-switch-to-buffer-functions nil)
  (defvar after-display-buffer-functions nil)
  
  (if (fboundp 'advice-add)
      ;;Modern way
      (progn
        (defun after-switch-to-buffer-adv (&rest r)
          (apply #'run-hook-with-args 'after-switch-to-buffer-functions r))
        (defun after-display-buffer-adv (&rest r)
          (apply #'run-hook-with-args 'after-display-buffer-functions r))
        (advice-add #'switch-to-buffer :after #'after-switch-to-buffer-adv)
        (advice-add #'display-buffer   :after #'after-display-buffer-adv)))
  (persp-mode 1)
  ;; (add-hook 'after-switch-to-buffer-functions
  ;;             #'(lambda (bn) (when (and persp-mode
  ;;                                       (not persp-temporarily-display-buffer))
  ;;                              (persp-add-buffer bn))))
  (add-hook! '(persp-mode-hook persp-after-load-state-functions)
    (defun +workspaces-ensure-no-nil-workspaces-h (&rest _)
      (when persp-mode
        (dolist (frame (frame-list))
          (when (string= (safe-persp-name (get-current-persp frame)) persp-nil-name)
            ;; Take extra steps to ensure no frame ends up in the nil perspective
            (persp-frame-switch (or (cadr (hash-table-keys *persp-hash*))
                                    +workspaces-main)
                                frame))))))
  
  (add-hook! 'persp-mode-hook
    (defun +workspaces-init-first-workspace-h (&rest _)
      "Ensure a main workspace exists."
      (when persp-mode
        (let (persp-before-switch-functions)
          ;; Try our best to hide the nil perspective.
          (when (equal (car persp-names-cache) persp-nil-name)
            (pop persp-names-cache))
          ;; ...and create a *real* main workspace to fill this role.
          (unless (or (persp-get-by-name +workspaces-main)
                      ;; Start from 2 b/c persp-mode counts the nil workspace
                      (> (hash-table-count *persp-hash*) 2))
            (persp-add-new +workspaces-main))
          ;; HACK Fix #319: the warnings buffer gets swallowed when creating
          ;;      `+workspaces-main', so display it ourselves, if it exists.
          (when-let (warnings (get-buffer "*Warnings*"))
            (save-excursion
              (display-buffer-in-side-window
               warnings '((window-height . shrink-window-if-larger-than-buffer))))))))
    (defun +workspaces-init-persp-mode-h ()
      (cond (persp-mode
             ;; `uniquify' breaks persp-mode. It renames old buffers, which causes
             ;; errors when switching between perspective (their buffers are
             ;; serialized by name and persp-mode expects them to have the same
             ;; name when restored).
             (when uniquify-buffer-name-style
               (setq +workspace--old-uniquify-style uniquify-buffer-name-style))
             (setq uniquify-buffer-name-style nil)
             ;; Ensure `persp-kill-buffer-query-function' is last
             (remove-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function)
             (add-hook 'kill-buffer-query-functions #'persp-kill-buffer-query-function t)
             ;; Restrict buffer list to workspace
             (advice-add #'doom-buffer-list :override #'+workspace-buffer-list))
            (t
             (when +workspace--old-uniquify-style
               (setq uniquify-buffer-name-style +workspace--old-uniquify-style))
             (advice-remove #'doom-buffer-list #'+workspace-buffer-list)))))
  (defun +workspace/quick-new ()
    "Create new workspace quickly"
    (interactive)
    (let ((name (format "#%s" (+workspace--generate-id))))
      (persp-switch name)
      (switch-to-buffer (doom-fallback-buffer))
      (with-current-buffer (doom-fallback-buffer)
        (delete-other-windows)
        (+workspace/display)))))

(defun +ibuffer/visit-workspace-buffer (&optional select-first)
  "Visit buffer, but switch to its workspace if it exists."
  (interactive "P")
  (let ((buf (ibuffer-current-buffer t)))
    (unless (buffer-live-p buf)
      (user-error "Not a valid or live buffer: %s" buf))
    (if-let (workspaces
             (cl-loop for wk in (+workspace-list)
                      if (+workspace-contains-buffer-p buf wk)
                      collect wk))
        (+workspace-switch
         (if (and (not select-first) (cdr workspaces))
             (or (completing-read "Select workspace: " (mapcar #'persp-name workspaces))
                 (user-error "Aborted"))
           (persp-name (car workspaces))))
      ;; Or add the buffer to the current workspace
      (persp-add-buffer buf))
    (switch-to-buffer buf)))

(use-package ibuffer
  :defer t
  :general
  (:states '(normal visual)
           :keymaps 'override
           "SPC bI" 'projectile-ibuffer
           "SPC bi" 'ibuffer)
  (:states '(normal visual)
           :keymaps 'ibuffer-mode-map
           "<return>" '+ibuffer/visit-workspace-buffer))

(with-eval-after-load "ibuffer"
  (require 'ibuf-ext)

  (define-ibuffer-filter persp
      "Toggle current view to buffers of current perspective."
    (:description "persp-mode"
     :reader (persp-prompt nil nil (safe-persp-name (get-frame-persp)) t))
    (find buf (safe-persp-buffers (persp-get-by-name qualifier))))

  (defun persp-add-ibuffer-group ()
    (let ((perspslist (mapcar #'(lambda (pn)
                                  (list pn (cons 'persp pn)))
                              (nconc
                               (delete* persp-nil-name
                                        (persp-names-current-frame-fast-ordered)
                                        :test 'string=)
                               (list persp-nil-name)))))
      (setq ibuffer-saved-filter-groups
            (delete* "persp-mode" ibuffer-saved-filter-groups
                     :test 'string= :key 'car))
      (push
       (cons "persp-mode" perspslist)
       ibuffer-saved-filter-groups)))

  (defun persp-ibuffer-visit-buffer ()
    (let ((buf (ibuffer-current-buffer t))
          (persp-name (get-text-property
                       (line-beginning-position) 'ibuffer-filter-group)))
      (persp-switch persp-name)
      (switch-to-buffer buf)))

  (define-key ibuffer-mode-map (kbd "RET") 'persp-ibuffer-visit-buffer)

  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (persp-add-ibuffer-group)
                (ibuffer-switch-to-saved-filter-groups "persp-mode"))))

(use-package persp-mode-project-bridge
  :after persp-mode
  :config
  (with-eval-after-load "persp-mode-projectile-bridge-autoloads"
    (add-hook 'persp-mode-projectile-bridge-mode-hook
              #'(lambda ()
                  (if persp-mode-projectile-bridge-mode
                      (persp-mode-projectile-bridge-find-perspectives-for-all-buffers)
                    (persp-mode-projectile-bridge-kill-perspectives))))
    (add-hook 'after-init-hook
              #'(lambda ()
                  (persp-mode-projectile-bridge-mode 1))
              t)))

(use-package no-littering)
(setq backup-directory-alist `(("." . "~/.emacs-saves")))
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq backup-by-copying t)

(use-package origami
  :hook ((org-mode dart-mode yaml-mode) . origami-mode)
  :after evil)

(defun @fold-close-all ()
  "Close all folds."

  (interactive)
  (cond ((bound-and-true-p origami-mode) (call-interactively 'origami-close-all-nodes))
        ((bound-and-true-p ts-fold-mode) (ts-fold-close-all))
        (t (evil-close-folds))))

(defun @fold-close ()
  "Close the fold under the cursor."

  (interactive)
  (cond ((bound-and-true-p origami-mode) (call-interactively 'origami-close-node))
        ((bound-and-true-p ts-fold-mode) (ts-fold-close))
        (t (evil-close-fold))))


(defun @fold-open-all ()
  "Open all folds."

  (interactive)
  (cond ((bound-and-true-p origami-mode) (call-interactively 'origami-open-all-nodes))
        ((bound-and-true-p ts-fold-mode) (ts-fold-open-all))
        (t (evil-open-folds))))

(defun @fold-open ()
  "Open the fold under the cursor."

  (interactive)
  (cond ((bound-and-true-p origami-mode) (call-interactively 'origami-open-node))
        ((bound-and-true-p ts-fold-mode) (ts-fold-open))
        (t (evil-open-fold))))

(defun @fold-toggle-all ()
  "Toggle all folds."

  (interactive)
  (cond ((bound-and-true-p origami-mode) (call-interactively 'origami-toggle-all-nodes))
        ((bound-and-true-p ts-fold-mode) (ts-fold-toggle-all))
        (t (evil-toggle-fold))))

(defun @fold-toggle ()
  "Toggle fold at point."

  (interactive)
  (save-excursion 
    (end-of-line)
  (cond ((bound-and-true-p origami-mode) (call-interactively 'origami-toggle-node))
        ((bound-and-true-p ts-fold-mode) (ts-fold-toggle))
        (t (evil-toggle-fold)))))


(defun @fold-next ()
  "Go to the next fold."

  (interactive)
  (cond ((bound-and-true-p origami-mode) (call-interactively 'origami-next-fold))
        ((bound-and-true-p ts-fold-mode) (ts-fold-next))
        (t (evil-next-fold))))

(defun @fold-previous ()
  "Go to the previous fold."

  (interactive)
  (cond ((bound-and-true-p origami-mode) (call-interactively 'origami-previous-fold))
        ((bound-and-true-p ts-fold-mode) (ts-fold-previous))
        (t (evil-previous-fold))))

(defun @ts-fold-init ()
  "Init ts-fold."
  (interactive)
  (when (and (bound-and-true-p tree-sitter-mode)
             (member major-mode '(ng2-ts-mode
                                  typescript-mode
                                  js-mode
                                  python-mode
                                  html-mode
                                  json-mode
                                  go-mode
                                  scss-mode
                                  css-mode
                                  bash-mode)))
    (ts-fold-mode 1)))

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :hook ((tree-sitter-mode
          web-mode
          ng2-html-mode
          ng2-ts-mode
          typescript-mode
          js-mode
          python-mode
          html-mode
          json-mode
          go-mode
          bash-mode
          css-mode
          scss-mode) . @ts-fold-init)
  :general
  (:keymaps 'override :states '(normal)
            "zM" '@fold-close-all
            "zm" '@fold-close
            "zR" '@fold-open-all
            "zr" '@fold-open
            "zA" '@fold-toggle-all
            "za" '@fold-toggle
            "zj" '@fold-next
            "zk" '@fold-previous)
  :config
  (add-to-list 'ts-fold-range-alist '(ng2-ts-mode . ((export_clause . ts-fold-range-seq)
                                                     (statement_block . ts-fold-range-seq)
                                                     (comment . ts-fold-range-c-like-comment))) t)

  (add-to-list 'ts-fold-range-alist '(web-mode . (html-mode
                                                  (element . ts-fold-range-html)
                                                  (comment ts-fold-range-seq 1 -1))))

  (add-to-list 'ts-fold-range-alist '(ng2-html-mode . (html-mode
                                                       (element . ts-fold-range-html)
                                                       (comment ts-fold-range-seq 1 -1))))
  (add-to-list 'ts-fold-range-alist '(scss-mode . ((keyframe_block_list . ts-fold-range-seq)
                                                   (block . ts-fold-range-seq)
                                                   (comment . ts-fold-range-c-like-comment))) t)

  ;; TODO: DOESN'T WORK for scss, needs another rules (check it later for custom pareser)
  (add-to-list 'ts-fold-range-alist '(scss-mode . (css-mode
                                                   (keyframe_block_list . ts-fold-range-seq)
                                                   (block . ts-fold-range-seq)
                                                   (comment . ts-fold-range-c-like-comment))))


  (add-to-list 'ts-fold-foldable-node-alist '(ng2-ts-mode comment statement_block export_clause))
  (add-to-list 'ts-fold-foldable-node-alist '(web-mode comment element))
  (add-to-list 'ts-fold-foldable-node-alist '(scss-mode comment block keyframe_block_list))
  (add-to-list 'ts-fold-foldable-node-alist '(ng2-html-mode comment element)))

(add-to-list 'display-buffer-alist '("^\\*scratch\\*$" . (display-buffer-at-bottom)))
(add-to-list 'display-buffer-alist '("^\\*quick:scratch\\*$" . (display-buffer-at-bottom)))

(defun @save-scratch-buffer-p ()
  "Return non-nil if the current buffer is the scratch buffer."
  (and (get-buffer "*scratch*") (string= (buffer-name) "*scratch*")))

(setq persistent-scratch-scratch-buffer-p-function #'@save-scratch-buffer-p)

(defun @persistent-scratch-save (&optional file)
  "Save the current state of scratch buffers.
When FILE is non-nil, the state is saved to FILE; when nil or when called
interactively, the state is saved to `persistent-scratch-save-file'.
What state exactly is saved is determined by `persistent-scratch-what-to-save'.

When FILE is nil and `persistent-scratch-backup-directory' is non-nil, a copy of
`persistent-scratch-save-file' is stored in that directory, with a name
representing the time of the last `persistent-scratch-new-backup' call."
  (interactive)
  (let* ((actual-file (or file persistent-scratch-save-file))
         (tmp-file (concat actual-file ".new"))
         (old-umask (default-file-modes))
         (str (persistent-scratch--save-state-to-string)))

    (unless (equal str "nil")
            (set-default-file-modes #o600)
            (unwind-protect
                (let ((coding-system-for-write 'utf-8-unix))
                  (write-region str nil tmp-file nil 0))
              (set-default-file-modes old-umask))
            (run-hook-with-args 'persistent-scratch-before-save-commit-functions tmp-file)
            (rename-file tmp-file actual-file t)
            (when (called-interactively-p 'interactive)
              (message "Wrote persistent-scratch file %s" actual-file))))
  (unless file
    (persistent-scratch--update-backup)
    (persistent-scratch--cleanup-backups)))

(advice-add 'persistent-scratch-save :override #'@persistent-scratch-save)

(use-package persistent-scratch
 :general
 (:states '(normal visual)
          :keymaps 'override
          "SPC x" '@switch-to-scratch)
  :config
  (setq persistent-scratch-autosave-interval 5)
  (persistent-scratch-setup-default)
)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs '("~/.doom.d/snippets"))
  (yas-global-mode 1))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(defun format-all-buffer--no-bufferjump ()
    "format-all-buffer without jumps of cursor"
    (interactive)
    (let ((point (point)) (wstart (window-start)))
      (call-interactively 'format-all-buffer)
      (goto-char point)
      (set-window-start (selected-window) wstart)))

(use-package format-all
  :defer t
  ;; :hook ((js2-mode typescript-mode ng2-html-mode ng2-ts-mode go-mode) . format-all-mode)
  :hook ((json-mode go-mode dart-mode) . format-all-mode)
  ;; :config
  ;; (add-to-list '+format-on-save-enabled-modes 'typescript-mode t)
  ;; (add-to-list '+format-on-save-enabled-modes 'ng2-mode t)
  ;; (add-to-list '+format-on-save-enabled-modes 'js2-mode t)
)

(use-package prettier
  :bind (:map evil-normal-state-map
              ("\+p" . prettier-prettify))
  :config
  (add-to-list 'prettier-major-mode-parsers '(typescript-ts-mode . (typescript babel-ts)))
  :init
  (global-prettier-mode))

(use-package flycheck
  :bind (:map evil-normal-state-map
              ("SPC f ]" . flycheck-next-error)
              ("SPC f [" . flycheck-previous-error)
              ("SPC e l" . flycheck-list-errors))
  :init
  (global-flycheck-mode)
  :config
  (set-face-attribute 'flycheck-fringe-error nil :background nil :foreground +m-color-secondary)
  (set-face-attribute 'flycheck-error-list-error nil :background nil :foreground +m-color-secondary)
  (set-face-attribute 'error nil :background nil :foreground +m-color-secondary)

  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode))

(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package tree-sitter-langs
  :after spell-fu)

(defun init-tree-sitter-hl-mode ()
  "Function for init tree-sitter-hl-mode in correct order.

This need for correct highlighting of incorrect spell-fu faces."
  (tree-sitter-hl-mode -1)
  (my-set-spellfu-faces)
  (tree-sitter-hl-mode))

(use-package tree-sitter
  :after (evil tree-sitter-langs)
  :hook ((go-mode
          web-mode
          typescript-mode
          css-mode
          typescript-tsx-mode
          html-mode
          scss-mode
          ng2-mode
          ng2-html-mode
          js-mode
          python-mode
          rust-mode
          ng2-ts-mode
          ng2-html-mode) . init-tree-sitter-hl-mode)
  :config
  (push '(ng2-html-mode . html) tree-sitter-major-mode-language-alist)
  (push '(web-mode . html) tree-sitter-major-mode-language-alist)
  (push '(ng2-ts-mode . typescript) tree-sitter-major-mode-language-alist)
  (push '(scss-mode . css) tree-sitter-major-mode-language-alist)
  ;; (push '(scss-mode . typescript) tree-sitter-major-mode-language-alist)

  (push '(html-ts-mode . ng2-html-mode) treesit-auto-fallback-alist)

  ;; TODO: remove, 
  (push '(typescript-ts-mode . typescript) tree-sitter-major-mode-language-alist)
  (push '(go-ts-mode . go) tree-sitter-major-mode-language-alist)
  (push '(python-ts-mode . python) tree-sitter-major-mode-language-alist)
  (push '(css-ts-mode . css) tree-sitter-major-mode-language-alist)
  (push '(html-ts-mode . ng2-html) tree-sitter-major-mode-language-alist)

  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(use-package tree-edit
  :defer t)

;; (use-package treesit-langs
;;   :straight (:type git :host github :repo "ymarco/tree-sitter-langs")
;;   :hook ((typescript-ts-base-mode
;;           go-ts-mode
;;           python-ts-mode
;;           css-ts-mode) . init-tree-sitter-hl-mode)
;;   :config
;;   (push '(typescript-ts-mode . typescript) tree-sitter-major-mode-language-alist)
;;   (push '(go-ts-mode . go) tree-sitter-major-mode-language-alist)
;;   (push '(python-ts-mode . python) tree-sitter-major-mode-language-alist)
;;   (push '(css-ts-mode . css) tree-sitter-major-mode-language-alist)
;;   (add-hook #'python-ts-mode-hook #'init-tree-sitter-hl-mode)

;;   (defalias 'treesit-can-enable-p 'treesit-available-p))

(use-package treesit
  :commands (treesit-install-language-grammar nf/treesit-install-all-languages)
  :hook ((typescript-ts-base-mode
          go-ts-mode
          python-ts-mode
          css-ts-mode) . init-tree-sitter-hl-mode)
  :init
  (setq treesit-language-source-alist
   '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
     (c . ("https://github.com/tree-sitter/tree-sitter-c"))
     (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
     (css . ("https://github.com/tree-sitter/tree-sitter-css"))
     (go . ("https://github.com/tree-sitter/tree-sitter-go"))
     (html . ("https://github.com/tree-sitter/tree-sitter-html"))
     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
     (json . ("https://github.com/tree-sitter/tree-sitter-json"))
     (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
     (make . ("https://github.com/alemuller/tree-sitter-make"))
     (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" "ocaml/src" "ocaml"))
     (python . ("https://github.com/tree-sitter/tree-sitter-python"))
     (php . ("https://github.com/tree-sitter/tree-sitter-php"))
     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "typescripts" "tsx"))
     (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
     (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
     (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
     (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
     (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))
  :custom 
  ;; (treesit-extra-load-path '("/Users/darkawower/pure-emacs/tree-sit-parsers"))
  (treesit-extra-load-path '("/Users/darkawower/pure-emacs/tree-sitter"))
  :config
  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75)))))

(use-package company
  :defer t
  :hook (emacs-lisp-mode . company-mode)
  :bind (:map evil-insert-state-map
              ("C-'" . company-yasnippet)
              ("C-x C-o" . company-complete)
              :map company-active-map
              ("C-w" . backward-kill-word)
              ("<escape>" . (lambda () (interactive)
                              (company-cancel)
                              (evil-normal-state))))
  :init
  (setq lsp-completion-provider :capf)
  :config
  (setq company-idle-delay 0.2)
  (setq company-quick-access-modifier 'super)

  (setq company-show-quick-access t)
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-char-regexp "[A-z:-]")
  (custom-set-variables
   '(company-quick-access-keys '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
   '(company-quick-access-modifier 'super)))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(show-paren-mode 1)
(custom-set-faces
 `(show-paren-mismatch ((t (:foreground ,+m-color-secondary)))))

(defun @lsp/uninstall-server (dir)
  "Delete a LSP server from `lsp-server-install-dir'."
  (interactive
   (list (read-directory-name "Uninstall LSP server: " lsp-server-install-dir nil t)))
  (unless (file-directory-p dir)
    (user-error "Couldn't find %S directory" dir))
  (delete-directory dir 'recursive)
  (message "Uninstalled %S" (file-name-nondirectory dir)))

(use-package lsp-mode
  :after flycheck
  :hook ((clojure-mode
          scss-mode
          go-mode
          css-mode
          js-mode
          typescript-mode
          vue-mode
          web-mode
          html-mode
          ng2-ts-mode
          python-mode
          dart-mode
          typescript-tsx-mode

          ;; Treesit
          html-ts-mode
          typescript-ts-mode
          go-ts-mode
          js-ts-mode
          bash-ts-mode
          tsx-ts-mode) . lsp-deferred)
  :general (:states '(normal visual)
                    :keymaps 'override
                    "SPC fn" 'flycheck-next-error
                    "gi" 'p-goto-implementation
                    "SPC la" 'lsp-execute-code-action
                    "SPC im" 'lsp-ui-imenu
                    "SPC lr" 'lsp-find-references
                    "SPC lw" 'lsp-restart-workspace
                    "SPC rl" 'lsp
                    "gd" '@find-definition
                    "SPC la" 'lsp-execute-code-action
                    "SPC cr" 'lsp-rename)
  :init
  (setq lsp-headerline-breadcrumb-enable nil)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-idle-delay 0.3)
  ;; (lsp-completion-provider :none)
  (lsp-enable-on-type-formatting nil)
  (lsp-eldoc-render-all nil)
  (lsp-prefer-flymake nil)
  (lsp-modeline-diagnostics-scope :workspace)
  ;; (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
  (lsp-clients-typescript-server-args '("--stdio"))
  (lsp-completion-default-behaviour :insert)
  (lsp-yaml-schemas '((kubernetes . ["/auth-reader.yaml", "/deployment.yaml"])))
  (lsp-disabled-clients '(html-ls))
  ;; (lsp-completion-provider :none)
  ;; (lsp-completion-provider :capf)
  ;; Disable bottom help info
  (lsp-signature-render-documentation nil)
  (lsp-signature-auto-activate nil)
  (lsp-enable-snippet nil)
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
  (mapc #'lsp-flycheck-add-mode '(typescript-mode js-mode css-mode vue-html-mode))
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
  (set-face-foreground 'lsp-face-highlight-read +m-color-secondary)
  (set-face-foreground 'lsp-face-highlight-textual +m-color-secondary)

  
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\venv\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\pyenv\\'")
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.cache\\'")
  (set-face-attribute 'lsp-face-highlight-textual nil :background "#c0caf5")
  ;; Install corfu completion for lsp
  ;; (defun corfu-lsp-setup ()
  ;; (setq-local completion-styles '(orderless basic)
  ;;             completion-category-defaults nil))
  ;; (add-hook 'lsp-mode-hook #'corfu-lsp-setup)
  (@setup-compilation-errors)
  (setq lsp-disabled-clients '(html-ls))
  (setq lsp-eldoc-hook nil))

(use-package lsp-yaml
  :defer t
  :hook (yaml-mode . lsp-mode))

(use-package yaml-pro
  :defer t
  :hook (yaml-mode . yaml-pro-mode))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-ui-peek-mode-map
              ("C-j" . lsp-ui-peek--select-next)
              ("C-k" . lsp-ui-peek--select-prev))
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
                                   ;; (flutter-run-or-hot-reload)
                                   (flutter-hot-restart)
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

(add-to-list 'display-buffer-alist '("^\\*compilation\\*$" . (display-buffer-at-bottom)))

(defun @setup-compilation-errors ()
  (interactive)
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
  :general
  (:states '(normal visual)
           :keymaps 'override
           "SPC cc" 'compile
           "SPC cC" 'recompile
           "SPC cv" (lambda ()
                      (interactive)
                      (compilation-display-error)
                      (+select-window-by-name "*compilation.*")))
  :config
  (defun display-buffer-from-compilation-p (_buffer-name _action)
    (unless current-prefix-arg
      (with-current-buffer (window-buffer)
        (derived-mode-p 'compilation-mode))))
  
  (push '(display-buffer-from-compilation-p
          display-buffer-use-some-window
          (inhibit-same-window . nil))
        display-buffer-alist)
  (@setup-compilation-errors))

(use-package floobits
  :defer t)

(defun @dap-delete-local-terminal (&optional _)
  "Delete local terminal."
  (interactive (list (dap--cur-active-session-or-die)))
  (save-window-excursion
    (let* ((buffer-list (mapcar #'buffer-name (buffer-list)))
           (filtered-buffers (seq-filter (lambda (buffer)
                                           (string-match-p "\\*DEBUG" buffer))
                                         buffer-list))
           (buffer-modified-p nil))
      (mapc #'kill-buffer filtered-buffers))))

(use-package dap-mode
  :demand
  :bind (:map evil-normal-state-map
              ("SPC d n" . dap-next)
              ("SPC d i" . dap-step-in)
              ("SPC d o" . dap-step-out)
              ("SPC d c" . dap-continue)
              ("SPC d Q" . dap-disconnect)
              ("SPC d q" . dap-disconnect)
              ("SPC d d" . dap-debug)
              ("SPC d r" . dap-debug-recent)
              ("SPC d l" . dap-ui-locals)
              ("SPC d b" . dap-ui-breakpoints)
              ("SPC d p" . dap-breakpoint-toggle)
              ("SPC d g" . dap-ui-breakpoints-browse)
              ("SPC d s" . dap-ui-sessions)
              ("SPC d e" . dap-debug-last)
              ("SPC d w" . dap-ui-show-many-windows)
              ("SPC d x" . dap-breakpoint-delete-all)
              ("SPC d W" . dap-ui-hide-many-windows)
              ("SPC d e" . dap-debug-edit-template))
  :config
  (defun dap-internal-terminal-vterm (command title debug-session)
    (with-current-buffer (dap--make-terminal-buffer title debug-session)
      (require 'vterm)
      (let ((vterm-shell command)
            (vterm-kill-buffer-on-exit nil))
        (vterm-mode))))
  (advice-add 'dap-disconnect :before #'@dap-delete-local-terminal)
  ;; (advice-remove 'dap-disconnect #'@dap-delete-local-terminal)

  (with-eval-after-load 'dap-ui
    (setq-default dap-ui-buffer-configurations
                  `((,dap-ui--breakpoints-buffer     . ((side . left)   (slot . 2) (window-width  . ,treemacs-width)))
                    (,dap-ui--expressions-buffer     . ((side . left)   (slot . 3) (window-width  . 0.30)))
                    (,dap-ui--locals-buffer          . ((side . right)  (slot . 1) (window-width  . 0.4)))
                    (,dap-ui--sessions-buffer        . ((side . right)  (slot . 2) (window-width  . 0.30)))
                    (,dap-ui--repl-buffer            . ((side . bottom) (slot . 1) (window-height . 0.30)))
                    ;; (,dap-ui--debug-window-buffer    . ((side . bottom) (slot . 2) (window-width  . 0.30)))
                    )))
    ;; (setq dap-ui-locals-expand-depth 3))
  ;; (setq dap-auto-configure-features '(locals controls tooltip))
  ;; (setq dap-auto-configure-features '(locals tooltip))
  (advice-add 'dap-disconnect :after '@dap-delete-local-terminal)
  (custom-set-faces
   '(dap-ui-pending-breakpoint-face ((t (:underline "dim gray"))))
   '(dap-ui-verified-breakpoint-face ((t (:underline "green")))))
  (require 'dap-go)
  (require 'dap-node))

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package undo-fu
  :defer t
  :bind (:map evil-normal-state-map
              ("u" . undo-fu-only-undo)
              ("C-r" . undo-fu-only-redo)))

(use-package undo-fu-session
  :config
  (global-undo-fu-session-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package copilot
  :defer 5
  :straight (copilot :host github :repo "zerolfx/copilot.el" :files ("dist" "copilot.el"))
  :general
  (:keymaps 'override
            "s-]" 'copilot-next-completion
            "s-[" 'copilot-previous-completion
            "s-l" 'copilot-accept-completion
            "s-j" 'copilot-complete
            "s-;" 'copilot-accept-completion-by-word)
  :custom
  (copilot-idle-delay 0.1)
  :config
  (setq copilot--previous-point nil)
  (setq copilot--previous-window-width nil)
  (copilot-diagnose)
  (global-copilot-mode)
  (add-hook 'evil-insert-state-entry-hook (lambda ()
                                            (setq blamer--block-render-p t)
                                            (blamer--clear-overlay)))
  (add-hook 'evil-normal-state-entry-hook (lambda ()
                                            (setq blamer--block-render-p nil)
                                            (copilot-clear-overlay)))
)

(use-package electric
  :defer t
  :hook ((prog-mode . electric-indent-local-mode)
         (prog-mode . electric-pair-mode))
  :config

  (setq electric-pair-preserve-balance nil
        electric-pair-delete-adjacent-pairs nil
        electric-pair-skip-whitespace nil
        electric-pair-open-newline-between-pairs t))

(defun @electric-pair--verify-cursor-position (&optional char)
  "Verify cusor position after inserting CHAR bracket."

  (when (member (char-before) '(?\) ?}))
    ;; (when (member (char-before) '(?\) ))
    (copilot-clear-overlay)
    (backward-char))

  (when (and (member (char-after) '(?\( ?\{))
             (member (chart-after (1+ (point))) '(?\) ?\})))
    (forward-char)))

(advice-add 'electric-pair--insert :after #'@electric-pair--verify-cursor-position)

(use-package turbo-log
  :defer t
  :straight (turbo-log :type git :host github :repo "Artawower/turbo-log.el")
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
   :modes (typescript-ts-mode tsx-ts-mode typescript-mode js2-mode web-mode ng2-ts-mode js-mode)
   :strategy merge
   :post-insert-hooks (prettier-prettify lsp)
;; :msg-format-template "'🦄: %s'"
   :msg-format-template "'✎: %s'"))

(use-package auto-rename-tag
  :defer t
  :hook ((html-mode ng2-html-mode-hook vue-mode web-mode) . auto-rename-tag-mode)
  :config
  (auto-rename-tag-mode 1))

(use-package string-inflection
  :defer t
  :bind ("C-s-c" . string-inflection-all-cycle))

(use-package persistent-soft
  :defer t)

(use-package persistent-kmacro
  :straight (:host github :repo "artawower/persistent-kmacro.el")
  :defer t
  :general (:keymaps 'override
                     :states 'normal
                     "SPC me" 'persistent-kmacro-execute-macro
                     "SPC ma" 'persistent-kmacro-name-last-kbd-macro
                     "SPC mr" 'persistent-kmacro-remove-macro))

(use-package magit
  :defer t
  :general
  (:keymaps 'magit-mode-map
            :states '(normal visual)
            "C-1" 'magit-section-show-level-1
            "C-2" 'magit-section-show-level-2
            "C-3" 'magit-section-show-level-3
            "C-4" 'magit-section-show-level-4
            "q" 'kill-current-buffer
            "Q" 'bury-buffer
            "Z" 'magit-stash
            "zz" 'evil-scroll-line-to-center
            "1" 'digit-argument
            "2" 'digit-argument
            "3" 'digit-argument
            "4" 'digit-argument
            "P" 'magit-fetch
            "f" 'evil-avy-goto-word-1)
  :bind (:map magit-mode-map
              ("s-<return>" . magit-diff-visit-worktree-file)
              :map evil-normal-state-map
              ("SPC g g" . magit-status)
              ("SPC g n" . magit-todo-list)
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

(use-package magit-todos
  :defer t
  :general
  (:states '(normal visual)
   :keymaps 'override
   "SPC g n" 'magit-todos-list))

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

(use-package jist
  :ensure t
  :custom
  (jist-github-token (getenv "GITHUB_TOKEN"))
  :general
  (:keymaps 'override
            :states '(normal visual)
            "SPC gcl" 'jist-commit
            "SPC glg" 'jist-list
            "SPC gpg" 'jist-refetch-gists)
  (:keymaps 'jist-gist-list-mode-map
            :states '(normal visual)
            "<return>" 'jist-browse-gist)
  )

(use-package git-gutter
  :after git-gutter-fringe
  :bind (:map evil-normal-state-map
              ("SPC g [" . git-gutter:previous-hunk)
              ("SPC g ]" . git-gutter:next-hunk)
              ("SPC g r" . git-gutter:revert-hunk))
  :config
  (set-face-foreground 'git-gutter:modified +m-color-main) ;; background color
  (set-face-foreground 'git-gutter:added +m-color-green)
  (set-face-foreground 'git-gutter:deleted +m-color-secondary)
  :init
  (global-git-gutter-mode))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package blamer
  :defer 5
  :straight (:type git :host github :repo "artawower/blamer.el" :branch "feature/user-avatar-preview")
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
        (forge-browse-commit commit-hash))))

  (setq blamer-bindings '(("<mouse-3>" . blamer-callback-open-remote)
                          ("<mouse-1>" . blamer-callback-show-commit-diff)))

  (global-blamer-mode 1))

(use-package git-timemachine
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC g t" . git-timemachine)))

(global-set-key (kbd "C-c l") 'smerge-keep-lower)
(global-set-key (kbd "C-c u") 'smerge-keep-upper)
(global-set-key (kbd "C-c a") 'smerge-keep-all)
(global-set-key (kbd "C-c j") 'smerge-next)
(global-set-key (kbd "C-c k") 'smerge-prev)

(use-package browse-at-remote
  :defer t
  :general
  (:states '(normal visual)
   :keymaps 'override
   "SPC o r" 'browse-at-remote))

(use-package forge
  :after magit
  :config
  (setq auth-sources '("~/.authinfo")))

(use-package code-review
  :defer t
  :general
  (:states '(normal insert) :keymaps 'magit-mode-map
            "C-c C-r" 'code-review-forge-pr-at-point)
  (:states '(normal insert) :keymaps 'code-review-mode-map
            "r" 'code-review-transient-api)
  :config
  (setq code-review-fill-column 80)
  (setq code-review-download-dir "/tmp/code-review/")
  (setq code-review-new-buffer-window-strategy #'switch-to-buffer)
  (setq code-review-auth-login-marker 'forge))

(use-package paren-face :defer t)

(use-package elisp-mode
  :defer t

  :hook ((emacs-lisp-mode . paren-face-mode)
         (emacs-lisp-mode . (lambda () (setq fill-column 80))))

  :bind (("C-c o" . outline-cycle)
         ("C-c r" . outline-show-all)
         ("C-c m" . outline-hide-body)
         ("C-c ]" . outline-next-heading)
         ("C-c [" . outline-previous-heading)
         ("C-c c" . counsel-outline)
         ("C-c e" . outline-hide-entry)
         ("C-c t" . outline-toggle-children)
         ("C-c b" . outline-cycle-buffer)
         ;; TODO: eval buffer or region 
         ("C-c C-c" . eval-buffer))

  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq rainbow-delimiters-mode -1))))

(use-package package-build
  :defer t)

(use-package package-lint

  :defer t)

(use-package elisp-autofmt
  :defer t
  :hook (emacs-lisp-mode . elisp-autofmt-mode)
  :custom
  (elisp-autofmt-python-bin "/opt/homebrew/bin/python3"))

(use-package clojure-mode
  :hook ((clojure-mode . format-all-mode)
         (clojure-mode . paren-face-mode))
  :defer t)

(use-package cider
  :defer t)

(use-package jest
  :defer t)

(setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")
(use-package typescript-mode
  :defer t  
  :hook (typescript-mode . (lambda () (setq-local fill-column 120)
                             (typescript-ts-mode)))
  :custom
  (lsp-clients-typescript-server-args '("--stdio"))
  :config
  (setq typescript-indent-level 2)
  ;; (add-to-list 'auto-mode-alist '("\.ts\'" . typescript-mode))
  (@setup-compilation-errors))

(use-package ts-comint
  :defer t
  :ensure t)

(use-package ng2-mode
  :after typescript-mode
  :config
  (setq lsp-clients-angular-language-server-command
        '("node"
          "/Users/darkawower/.npm-global/lib/node_modules/@angular/language-server"
          "--ngProbeLocations"
          "/Users/darkawower/.npm-global/lib/node_modules"
          "--tsProbeLocations"
          "/Users/darkawower/.npm-global/lib/node_modules"
          "--stdio")))

(use-package js2-mode
  :defer t
  :hook (js2-mode . js2-highlight-unused-variables-mode))

(use-package npm
  :defer t)

(use-package nodejs-repl
  :ensure t
  :general
  (:states '(normal visual)
           :keymaps 'override
           "SPC rn" 'nodejs-repl)

  (:states '(insert)
           :keymaps 'nodejs-repl-mode-map
           "C-j" 'comint-next-input
           "C-k" 'comint-previous-input)

  :defer t
  :config
  (defun @open-nodejs-repl-here ()
    "Open nodejs repl inside current buffer!"
    (interactive)
    (let ((nodejs-repl-buffer-name "*nodejs*"))
      (if (get-buffer nodejs-repl-buffer-name)
          (switch-to-buffer nodejs-repl-buffer-name)
        (progn
          (switch-to-buffer nodejs-repl-buffer-name)
          (nodejs-repl)))))
  (add-to-list 'display-buffer-alist '("^\\*nodejs\\*$" . (display-buffer-same-window))))

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

(defun @open-ipython-repl-here ()
  "Open python repl inside current buffer!"
  (interactive)
  (let ((python-repl-buffer-name "*Python*"))
    (if (get-buffer python-repl-buffer-name)
        (switch-to-buffer python-repl-buffer-name)
      (progn
        (switch-to-buffer python-repl-buffer-name)
        (run-python)))))

(use-package python
  :defer t
  :general
  (:states '(normal visual)
    :keymaps 'override
    "SPC r p" '@open-ipython-repl-here)
  :config
  (setq python-shell-interpreter "ipython"))

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
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp-deferred)))
  :config
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-auto-search-paths t)
  (setq lsp-pyright-log-level "trace")
  (setq lsp-pyright-multi-root nil)
  (setq lsp-pyright-use-library-code-for-types t)
  ;; (setq lsp-pyright-venv-directory "/Users/darkawower/.local/share/virtualenvs/spice-farm-YhO8T07I")
  (setq lsp-pyright-diagnostic-mode "workspace"))

(use-package pipenv
  :defer t
  :hook (python-mode . pipenv-mode)
  :config
  (setenv "WORKON_HOME" (concat (getenv "HOME") "/.local/share/virtualenvs"))
  (add-hook 'pyvenv-post-activate-hooks #'lsp-restart-workspace)
  ;; This hook will copy venv from pyvenv to lsp pyright
  (add-hook 'pyvenv-post-activate-hooks (lambda ()
                                          (setq lsp-pyright-venv-directory pyvenv-virtual-env)
                                          (lsp-restart-workspace)))

  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))

(use-package lsp-volar
  :defer t)

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
  :general 
  (:keymaps 'override
   :states 'insert
   "s-e" 'emmet-expand-line)
  :defer t)

(use-package css-mode
  :defer 10
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
  :general
  (:keymaps 'dart-mode-map
            "C-c C-r" #'flutter-run-or-hot-reload)
  (:states '(normal visual)
           :keymaps '(dart-mode-map flutter-mode-map)
           "SPC m f s" #'flutter-run
           "SPC m f R" #'flutter-hot-restart
           "SPC m f r" #'flutter-run-or-hot-reload)
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
  :defer t
  :custom
  (browse-url-browser-function 'browse-url-generic)
  ;; (grip-url-browser #'browse-url-firefox-program)
  :config
  (let ((credential (auth-source-user-and-password "api.github.com")))
    (setq grip-github-user (car credential)
          grip-github-password (cadr credential))))

(use-package ox-gfm
  :defer t)

(use-package org
  :mode (("\\.org$" . org-mode))
  :general
  (:states '(normal visual)
           :keymaps 'override
           "SPC m t" 'org-todo
           "SPC m n" 'org-store-link
           "SPC m l l" 'org-insert-link
           "SPC nl" 'org-store-link
           "SPC dt" 'org-time-stamp-inactive
           "SPC mlt" 'org-toggle-link-display)
  (:states '(normal visual)
           :keymaps 'org-mode-map
           "SPC dt" 'org-time-stamp-inactive
           "SPC st" 'org-set-tags-command
           "SPC mx" 'org-toggle-checkbox
           "<return>" '+org/dwim-at-point)
  (:keymaps 'org-read-date-minibuffer-local-map
            "C-s" 'org-goto-calendar)
  (:keymaps 'calendar-mode-map
            "<return>" 'org-calendar-select)

  :bind (:map evil-normal-state-map
              ("SPC h ]" . org-next-visible-heading)
              ("SPC h [" . org-previous-visible-heading))
  :config
  (setq org-src-preserve-indentation t)
  (add-hook 'org-mode-hook
            (lambda () (imenu-add-to-menubar "Imenu")))
  (setq org-imenu-depth 8)
  (@setup-org-mode-faces)

  (setq org-src-window-setup 'current-window)
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
     (python . t)
     (restclient . t)))

  (defun org-babel-execute:typescript (body params)
    (let ((org-babel-js-cmd "npx ts-node < "))
      (org-babel-execute:js body params)))

  (defvar org-babel-js-function-wrapper
    ""
    "Javascript code to print value of body.")
  ;; Applications for opening from org files
  ;; (if (assoc "\\.pdf\\'" org-file-apps)
  ;;     (setcdr (assoc "\\.pdf\\'" org-file-apps) 'emacs)
  ;;   (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs) t))

  ;; (add-to-list 'org-file-apps
  ;;              '("\\.pdf\\'" . (lambda (file link)
  ;;                                (org-pdfview-open link))))
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

                           (push '("#+TITLE:" . "") prettify-symbols-alist)
                           (push '("#+DESCRIPTION:" . "") prettify-symbols-alist)
                           (push '("#+ID:" . "") prettify-symbols-alist)
                           (push '("#+FILETAGS:" . "") prettify-symbols-alist)
                           (push '("#+STARTUP:" . "") prettify-symbols-alist)
                           (push '("#+ACTIVE:" . "") prettify-symbols-alist)
                           (push '("#+START_SPOILER" . "") prettify-symbols-alist)
                           (push '("#+CLOSE_SPOILER" . "") prettify-symbols-alist)
                           (push '("#+BEGIN_HIDDEN" . "") prettify-symbols-alist)
                           (push '("#+END_HIDDEN" . "") prettify-symbols-alist)


                           ;; (push '("#+TITLE:" . "") prettify-symbols-alist)
                           ;; (push '("#+DESCRIPTION:" . "") prettify-symbols-alist)
                           ;; (push '("#+ID:" . "") prettify-symbols-alist)
                           ;; (push '("#+FILETAGS:" . "") prettify-symbols-alist)
                           ;; (push '("#+STARTUP:" . "") prettify-symbols-alist)
                           ;; (push '("#+ACTIVE:" . "") prettify-symbols-alist)
                           ;; (push '("#+START_SPOILER" . "") prettify-symbols-alist)
                           ;; (push '("#+CLOSE_SPOILER" . "") prettify-symbols-alist)
                           ;; (push '("#+BEGIN_HIDDEN" . "") prettify-symbols-alist)
                           ;; (push '("#+END_HIDDEN" . "") prettify-symbols-alist)

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
  :after org
  :init
  (add-hook 'org-mode-hook 'org-indent-mode))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-directory "~/Yandex.Disk.localized/Dropbox/org"))

(use-package org-roam
  :after org
  :bind (:map evil-normal-state-map
              ("SPC n r i" . org-roam-node-insert)
              ("SPC n r f" . org-roam-node-find))
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
  :after org
  :straight (:host github :repo "TobiasZawada/org-yt")
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
  :after org
  :bind (:map org-mode-map
              ("<C-return>" . +org/insert-item-below)
              ("<C-S-return>" . +org/insert-item-above))
  :straight (org-insert :type git :host github :repo "hlissner/doom-emacs" :files ("modules/lang/org/autoload/org.el")))

(defun @apply-agenda-directory-url ()
  "Upload agenda from nested dirs."
  (interactive)
  (setq org-agenda-files (append (directory-files-recursively "~/Yandex.Disk.localized/Dropbox/org/" "\\.org$")
                                 (directory-files-recursively "~/projects/pet" "\\.org$"))))

(use-package org-agenda
  :defer t
  :config
  (setq org-agenda-files (directory-files-recursively "~/Yandex.Disk.localized/Dropbox/org/calendar/" "\\.org$")))

(use-package org-make-toc
  :after org
  :bind (:map org-mode-map
              ("C-c g" . org-make-toc)))

(use-package ox-gfm
  :defer t
  :straight (ox-gfm :type git :host github :repo "larstvei/ox-gfm"))

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
  (if (boundp 'spell-fu-faces-exclude)
    (setq spell-fu-faces-exclude (append spell-fu-faces-exclude
                                         '(diredfl-file-name)))
    (defvar spell-fu-faces-exclude
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

(use-package google-translate
  :defer 10
  :bind (:map google-translate-minibuffer-keymap
        ("C-k" . google-translate-next-translation-direction)
        ("C-n" . google-translate-next-translation-direction)
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
  :defer 5
  :config
  (global-wakatime-mode))

(use-package wakatime-ui
  :after wakatime-mode
  :straight (wakatime-ui :host github :repo "Artawower/wakatime-ui.el")
  :custom
  (wakatim-ui-schedule-url "https://wakatime.com/share/@darkawower/af1bfb85-2c8b-44e4-9873-c4a91b512e8d.png")
  :config
  (wakatime-ui-mode))

(recentf-mode)

(use-package projectile
  :general
  (:states 'normal
           "SPC pa" 'projectile-add-known-project)
  :config
  (defun @set-perps-workspace-name-by-switched-project ()
    "Set perps workspace name by switched project"
    (interactive)
    (when (and (bound-and-true-p persp-mode)
               (bound-and-true-p projectile-mode))
      (persp-rename (projectile-project-name))))
  
  (add-hook 'projectile-after-switch-project-hook
            #'@set-perps-workspace-name-by-switched-project)
  
  (add-hook 'projectile-find-file-hook
            #'@set-perps-workspace-name-by-switched-project)
  (projectile-mode +1))

(use-package helpful
  :defer t
  :bind (("C-h k" . helpful-key)
         :map evil-normal-state-map
         ("SPC h v" . helpful-variable)
         ("SPC h f" . helpful-function)
         ("SPC h ." . helpful-at-point)))

(use-package vertico
  :defer t
  :bind (:map evil-normal-state-map
              ("SPC ;" . vertico-repeat-last)
              ("SPC '" . vertico-repeat)
              :map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-n" . vertico-next-group)
              ("C-p" . vertico-previous-group)
              ("C-l" . vertico-quick-jump)
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
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
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
  :config
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
  :after vertico
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init
  (marginalia-mode)
  :config
  (pushnew! marginalia-command-categories
          '(+default/find-file-under-here . file)
          '(flycheck-error-list-set-filter . builtin)
          '(persp-switch-to-buffer . buffer)
          '(projectile-find-file . project-file)
          '(projectile-recentf . project-file)
          '(projectile-switch-to-buffer . buffer)
          '(projectile-switch-project . project-file)))

(defun @consult-line-or-region ()
  "Search in buffer depends or visual or normal line"
  (interactive)
  (if (region-active-p)
      (let ((substring (buffer-substring (region-beginning) (region-end))))
        (deactivate-mark)
        (consult-line substring))
    (consult-line)))

(use-package consult
  :defer t
  :general (:states '(normal visual)
                    :keymaps 'override
                    "s-f" '@consult-line-or-region
                    "SPC bB" 'consult-buffer
                    "SPC fP" '@open-emacs-config
                    "SPC /" 'consult-ripgrep
                    "SPC *" (lambda () (interactive) (consult-ripgrep nil (thing-at-point 'symbol)))
                    "SPC si" 'consult-imenu
                    "SPC RET" 'consult-bookmark
                    "SPC cm" 'consult-mark
                    "SPC fr" 'consult-recent-file
                    "SPC SPC" 'consult-projectile-find-file
                    "M-n" (lambda () (interactive) (search-forward (car consult--line-history)))
                    "M-p" (lambda () (interactive) (search-backward (car consult--line-history))))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq recentf-max-saved-items 3000)
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  ;; (advice-add #'register-preview :override #'consult-register-window)

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
   consult-projectile-find-file consult-git-grep consult-grep consult-projectile-recentf consult-ripgrep
   consult-bookmark consult-recent-file consult-xref consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; my/command-wrapping-consult    ;; disable auto previews inside my command
   :preview-key (list :debounce 0.2 "C-SPC"))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-SPC")

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

(use-package consult-projectile
  :general (:states 'normal
                    "SPC pp" 'consult-projectile-switch-project
                    "SPC pi" 'projectile-invalidate-cache
                    "SPC pa" 'projectile-add-known-project)
  :config
  (setq consult-projectile-use-projectile-switch-project t)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  :defer t)

(use-package embark
  :custom
  (embark-indicators '(embark-minimal-indicator embark-highlight-indicator embark-isearch-highlight-indicator))
  :general
  ("C-." 'embark-act         ;; pick some comfortable binding
   "C-;" 'embark-dwim        ;; good alternative: M-.
   "C-h B" 'embark-bindings)
  (:keymaps '(minibuffer-local-mode-map read--expression-map vertico-map)
            "C-SPC" '+vertico/embark-preview
            ;; "C-SPC" 'consult-preview-at-point
            ;; "C-SPC" 'consult--buffer-preview
 						"C-u" 'evil-delete-back-to-indentation
            "C-<return>" '+vertico/embark-preview)
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (defvar-keymap embark-projectile-map
    :doc "Example keymap with a few file actions"
    :parent embark-general-map
    "d" #'projectile-remove-known-project)
  
  (add-to-list 'embark-keymap-alist '(consult-projectile-project . embark-projectile-map))
  (defun copy-grep-results-as-kill (strings)
    (embark-copy-as-kill
     (mapcar (lambda (string)
               (substring string
                          (1+ (next-single-property-change
                               (1+ (next-single-property-change 0 'face string))
                               'face string))))
             strings)))
  
  (add-to-list 'embark-multitarget-actions 'copy-grep-results-as-kill)
  
  (defvar-keymap embark-consult-grep-map
    :doc "Keymap for actions for consult-grep results."
    :parent embark-general-map
    "w" #'copy-grep-results-as-kill)
  
  (setf (alist-get 'consult-grep embark-keymap-alist) 'embark-consult-grep-map))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(defun +vertico/embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (save-selected-window
      (let ((embark-quit-after-action nil))
        (embark-dwim)))))

(use-package all-the-icons-completion
  :init
  (all-the-icons-completion-mode))

(use-package wgrep
  :general 
  (:keymaps 'override
            "C-c C-w" 'wgrep-change-to-wgrep-mode)
  :after vertico)

(use-package pdf-tools
  :ensure t
  :defer t
  :config
  (pdf-tools-install))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package browse-hist
  :ensure t
  :general
  (:keymaps '(normal visual)
            :modes 'override
            "SPC bh" 'browser-hist-search)
  :custom
  (browser-hist-default-browser 'brave)
  :straight (browse-hist :type git :host github :repo "agzam/browser-hist.el"))

(use-package openai
  :straight (:host github :repo "emacs-openai/openai"))

(use-package codegpt 
  :straight (:type git :host github :repo "emacs-openai/codegpt")
  :config
  (add-to-list 'codegpt-action-alist '("JSDoc" . "Write JSDoc for this method"))
  (defun codegpt--internal (instruction start end)
    "Do INSTRUCTION with partial code.

The partial code is defined in with the region, and the START nad END are
boundaries of that region in buffer."
    (let ((text (string-trim (buffer-substring start end)))
          (original-window (selected-window)))
      (codegpt--ask-in-buffer instruction
        (insert text "\n\n")
        (openai-completion
         (buffer-string)
         (lambda (data)
           (openai--with-buffer codegpt-buffer-name
             (openai--pop-to-buffer codegpt-buffer-name)
             (let* ((choices (openai-completion--data-choices data))
                    (result (openai-completion--get-choice choices))
                    (original-point (point)))
               (insert "\n" (string-trim result) "\n")
               (fill-paragraph original-point (point))
               (end-of-buffer)))
           (unless codegpt-focus-p
             (select-window original-window))))
        (unless codegpt-focus-p
          (select-window original-window))))))

(use-package pocket-reader
  :general
  (:keymaps 'override
            :states '(normal visual)
            "SPC ep" 'pocket-reader)
  (:keymaps 'pocket-reader-mode-map
            :states '(normal visual)
            "<return>" 'pocket-reader-open-in-external-browser
            "s" 'pocket-reader-search
            "d" 'pocket-reader-delete
            "r" 'pocket-reader-refresh
            "a" 'pocket-reader-add-link
            "ta" 'pocket-reader-add-tags
            "m" 'pocket-reader-more
            "w" 'pocket-reader-copy-url
            "tf" 'pocket-reader-tag-search)
  :defer t)

(use-package plantuml-mode
  :defer t
  :config
  (setq plantuml-indent-level 2)
  (setq plantuml-output-type "png")
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (setq plantuml-exec-mode 'jar)
  (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar"))

(use-package empv
  :ensure t
  :straight (:host github :repo "isamert/empv.el"))
