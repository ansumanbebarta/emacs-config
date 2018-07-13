;;; init.el --- Ansuman Bebarta's Emacs init file for GNU Emacs.

;; Description: Emacs init file
;; Author: Ansuman Bebarta <ansuman.bebarta@gmail.com>
;; Maintainer: Ansuman Bebarta <ansuman.bebarta@gmail.com>
;; Keyword: local, convenience

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This is my personal startup file for GNU Emacs.  It has only recently
;; been tested on GNU Emacs 24.3.1, though it may run on other versions.

;;; Code:

;; Personal information
;; --------------------

(setq user-full-name "Ansuman Bebarta"
      user-mail-address "ansuman.bebarta@gmail.com")

;; Package Management
;; ------------------

(require 'cl)
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("elpy" . "https://jorgenschaefer.github.io/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Define default packages
;; -----------------------

(defvar required-packages
  '(
    ace-window
    key-chord
    exec-path-from-shell
    linum-off

    ;; Theme
    base16-theme
    spaceline
    diminish
    fancy-battery

    ;; Evil
    evil
    evil-escape
    evil-magit
    evil-leader

    ;; Project
    helm
    helm-projectile
    perspective
    projectile
    project-explorer

    ;; Syntax
    flycheck

    ;; Python
    elpy
    jinja2-mode
    virtualenvwrapper

    ;; Yaml
    yaml-mode
    )
  "A list of packages used.")

;; Install packages
;; ----------------

(defun packages-installed-p ()
  "Check whether all required packages all installed or not."
  (loop for package in required-packages
	when (not (package-installed-p package))
	do (return nil)
	finally (return t)))

(unless (packages-installed-p)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")

  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))


;; Start up options
;; --------------------

(setq current-language-environment "English")

(setq inhibit-splash-screen t
      initial-scratch-message nil)

(setq-default default-buffer-file-coding-system 'utf-8-unix)

; No scroll, tool or menu.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(delete-selection-mode t)
(transient-mark-mode t)
(setq select-enable-clipboard t)

; No need of back ups.
(setq make-backup-files nil)

; Display settings.
(blink-cursor-mode -1)
(set-fringe-mode 0)
(show-paren-mode t)
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines) (toggle-indicate-empty-lines))


; Indentation and spaces for tabs.
(setq indent-tabs-mode nil
      tab-width 4)

; Ignore case while searching.
(setq case-fold-search t)

; Misc settings.
(defalias 'yes-or-no-p 'y-or-n-p)
(define-coding-system-alias 'UTF-8 'utf-8)
(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

; recentf-mode allows you to access the list of recent files which can be
; used by ido and helm. Let’s save its file somewhere else and change the
; size of its history while we’re at it.
(setq recentf-save-file "~/.emacs.d/etc/recentf"
      recentf-max-saved-items 50)

; The history of prompts like M-: can be saved,
; but let’s change its save file and history length first.
(setq savehist-file "~/.emacs.d/etc/savehist"
      history-length 150)

; Start from the last place you were in a file the next time you visit
; Put save file somewhere else
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/etc/saveplace")

; Put bookmarks somewhere else
(setq bookmark-default-file "~/.emacs.d/etc/bookmarks")

(if (not (eq system-type "windows-nt"))
    (progn
      (exec-path-from-shell-copy-env "PATH")
      (exec-path-from-shell-copy-env "PYTHONPATH")))

;; Linum mode
;; ----------

; Show side line
(global-linum-mode t)

(require 'linum-off)
(add-to-list 'linum-disabled-modes-list 'project-explorer-mode)

(if (display-graphic-p)
    (setq linum-format "%5d \u2502 ")
  (setq linum-format "%5d | "))

(setq linum-format "%5d \u2502 ")
(linum-on)
(setq line-number-display-limit-width 10000)

(column-number-mode t)


;; Theme
;; -----

(defun apply-theme()
  "Apply theme."
  (require 'base16-atelier-savanna-theme)

  ;; Battery in percentage
  (fancy-battery-mode)
  (setq fancy-battery-show-percentage t)

  ;; Spaceline-config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (setq spaceline-minor-modes-p nil))

(if (eq system-type 'darwin)
    (setq ns-use-srgb-colorspace nil))

(if (display-graphic-p)
    (apply-theme))

;; Font
;; ----
(if (member "Monaco" (font-family-list))
    (set-face-attribute
     'default nil :font "Monaco 14"))

;; Custom functions
;; ----------------

(defun ans/find-user-init-file ()
  "Edit the EMACS init file in another window."
  (interactive)
  (find-file user-init-file))

(defun ans/activate-venv (name)
  "Activate virtualenv with given NAME."
  (venv-workon name))

(defun ans/overload-persp (name)
  "Work on persp NAME to determine extra operations."
  (let (
	(env-name (replace-regexp-in-string "^py-" "" name))
	(condition (eq (search "py-" name) 0))
	)
    (if condition (ans/activate-venv env-name))
    )
  )

(defun ans/current-persp-name ()
  "Return current perspective name."
  (persp-name persp-curr))

(defun ans/close-term-buffer-on-exit ()
  "Close 'ansi-term' buffer after exit."
  (let* ((buff (current-buffer))
	 (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
	(if (string= event "finished\n")
	    (kill-buffer, buff))))))

(defun ans/open-term ()
  "Open 'ansi-term'."
  (interactive)
  (ansi-term "/bin/bash"))

(add-hook 'term-exec-hook 'ans/close-term-buffer-on-exit)
(eval-after-load "term" '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

(defun ans/goto-definition ()
  "Include logic to make language independent."
  (interactive)
  (message "%s" major-mode)
  (cond ((equal major-mode 'python-mode) (elpy-goto-definition))
        (t (message "goto-definition for %s is not defined" major-mode)))
  )

(defun ans/initlines (lines mode)
  "Add initial LINES to specific MODE."
  (when (and (equal major-mode mode) (equal 0 (buffer-size)))
    (loop for line in lines
          do (progn
               (insert line)
               (newline))
          )
  ))

;; Evil
;; ----

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

(require 'evil)
(evil-mode 1)

(require 'evil-magit)

(evil-escape-mode t)
(setq-default evil-escape-key-sequence "jk")
(setq-default evil-escape-delay 0.2)

;; Project setup
;; -------------

(require 'helm-config)
(helm-mode t)
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)

(projectile-mode)
(setq projectile-enable-caching t)

(persp-mode)

(require 'helm-projectile)
(helm-projectile-on)

(add-hook 'persp-created-hook
      '(lambda ()
         (ans/overload-persp (ans/current-persp-name))))
(add-hook 'persp-switch-hook
      '(lambda ()
         (ans/overload-persp (ans/current-persp-name))))

; Flycheck
(global-flycheck-mode)

;; Company mode
;; ------------

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-select-next)
     (define-key company-active-map [tab] 'company-select-next)))

(setq-default company-selection-wrap-around t)

;; Python configurations
;; -----------------------------------

; Python setup
(elpy-enable)
(setq elpy-rpc-backend "jedi")

(remove-hook 'elpy-modules 'elpy-module-flymake)

(defvar py-init-lines
  '(
    "#!/usr/bin/env python"
    "# -*- coding: utf-8 -*-"
    )
  "Init lines to add to empty python files."
  )

(add-hook 'after-change-major-mode-hook
          '(lambda ()
             (ans/initlines py-init-lines 'python-mode)
             )
          )

;; Key configurations
;; ------------------

(evil-leader/set-key
  "I" 'ans/find-user-init-file
  "c" 'comment-or-uncomment-region
  "G" 'magit-status
  "W" 'ace-window
  "T" 'ans/open-term

  ;; b stands for buffer
  "bf" 'helm-find-files
  "bk" 'kill-buffer
  "br" 'rename-buffer
  "bR" 'revert-buffer
  "bs" 'persp-switch-to-buffer

  ;; l statnds for language
  "lg" 'ans/goto-definition
  "lb" 'ans/go-back

  ;; p stands for project
  "pp" 'helm-projectile-switch-project
  "pf" 'helm-projectile-find-file
  "pg" 'helm-projectile-grep
  "pi" 'projectile-invalidate-cache

  ;; t stands for tree
  "tt" 'project-explorer-toggle

  ;; w stands for workspace
  "ws" 'persp-switch
  "wk" 'persp-kill
  "wl" 'persp-next
  "wh" 'persp-prev
  )
