;;; -*- lexical-binding: t; -*-
;; (setq gc-cons-threshold 200000000)
;; (run-with-idle-timer 5 t #'garbage-collect)

;; ;; If we have the native compiler, use it
;; (if (and (fboundp 'native-comp-available-p)
;;          (native-comp-available-p))
;;     (progn
;;       (message "Native compilation is available")
;;       (setq comp-deferred-compilation t))
;;   (message "Native complation is *not* available"))

;; ;; My org file is posted using writefreely, which uses local variables
;; (add-to-list 'safe-local-variable-values '(writefreely-post-id . "wf83bq5jwz"))
;; (add-to-list 'safe-local-variable-values '(writefreely-post-token . nil))

;; (setq config-file (concat user-emacs-directory "/Users/darkawower/vanila/config.org"))
;; (org-babel-load-file config-file)

;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: October 16, 2014
;; Homepage: https://github.com/angrybacon/dotemacs

;; This program is free software. You can redistribute it and/or modify it under
;; the terms of the Do What The Fuck You Want To Public License, version 2 as
;; published by Sam Hocevar.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.
;;
;; You should have received a copy of the Do What The Fuck You Want To Public
;; License along with this program. If not, see http://www.wtfpl.net/.

;;; Commentary:

;; Following lines load an Org file and build the configuration code out of it.

;;; Code:

(setq enable-local-variables :safe)
(let ((default-directory user-emacs-directory)
      (file-name-handler-alist nil)
      (gc-cons-percentage .6)
      (gc-cons-threshold most-positive-fixnum)
      (read-process-output-max (* 1024 1024)))

  ;; Disable that pesky echo message
  (setq inhibit-startup-echo-area-message user-login-name)

  ;; Mark safe variables early so that tangling won't break
  (put 'after-save-hook 'safe-local-variable
       (lambda (value) (equal value '(org-babel-tangle t))))
  (put 'display-line-numbers-width 'safe-local-variable 'integerp)

  ;; Tangle and compile if necessary only, then load the configuration
  (let* ((.org "config.org")
         (.el (concat (file-name-sans-extension .org) ".el"))
         (modification-time
          (file-attribute-modification-time (file-attributes .org))))
    (require 'org-macs)
    (unless (org-file-newer-than-p .el modification-time)
      (require 'ob-tangle)
      (org-babel-tangle-file .org .el "emacs-lisp"))
    (load-file .el))

  ;; Set the working directory to home regardless of where Emacs was started from
  (cd "~/")

  ;; Collect garbage when all else is done
  (garbage-collect))

