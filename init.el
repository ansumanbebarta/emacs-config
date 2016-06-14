;;--------------------------------------------------------------------
;; Package initializations
;;--------------------------------------------------------------------

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(
    base16-theme
    ;; Use to change minor mode look on modeline
    diminish
    ;; Emacs IPython Notebook
    ein
    ;; Emacs rpc (explore later)
    ;; epc
    elpy
    evil
    evil-leader
    ;; Gather PATH from shell
    exec-path-from-shell
    ;; Used to show battery info in modeline
    fancy-battery
    ;; Realtime syntax checking
    flycheck
    helm
    helm-projectile
    ;; Allows to bind commands to combination of keys
    key-chord
    ;; Explore later
    ;; paradox
    ;; Provide many workspaces (explore later)
    perspective
    projectile
    project-explorer
    ;; Checks pep8
    py-autopep8
    spaceline
    ))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)

;;--------------------------------------------------------------------
;; Basic customization
;;--------------------------------------------------------------------

;; Answer with y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Language
(setq current-language-environment "English")

;; Don't show startup screen
(setq inhibit-startup-screen t)

;; Set scratch message to empty string
(setq initial-scratch-message "")

;; Don't show the menu bar
(menu-bar-mode -1)

;; Don't show the tool bar
(tool-bar-mode -1)

;; Don't show scroll bar
(toggle-scroll-bar -1)

;; Don't blink the cursor
(blink-cursor-mode -1)

;; Show line number on left and column number on mode line
(global-linum-mode t)
(setq linum-format "%5d \u2502 ")
(line-number-mode t)
(setq line-number-display-limit-width 10000)
(column-number-mode t)

;; Use space instead of tabs
(setq indent-tabs-mode -1)

;; Ignore case while searching
(setq case-fold-search t)

;; Require final newlines in files when they are saved
(setq require-final-newline t)

;; Remove trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Abstract back files and autosave files
(setq backup-directory-alist '((".*" . "~/.emacs.d/backup")))
(setq version-control t)
(setq delete-old-versions t)
(setq auto-save-list-file-prefix "~/.emacs.d/autosave/")
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t)))

;; recentf-mode allows you to access the list of recent files which can be
;; used by ido and helm. Let’s save its file somewhere else and change the
;; size of its history while we’re at it.
(setq recentf-save-file "~/.emacs.d/etc/recentf"
      recentf-max-saved-items 50)

;; The history of prompts like M-: can be saved,
;; but let’s change its save file and history length first.
(setq savehist-file "~/.emacs.d/etc/savehist"
      history-length 150)

;; Start from the last place you were in a file the next time you visit
;; Put save file somewhere else
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/etc/saveplace")

;; Put bookmarks somewhere else
(setq bookmark-default-file "~/.emacs.d/etc/bookmarks")

;; Don't need fringe
(set-fringe-mode 0)

;; Setting font to monaco
(if (member "Monaco" (font-family-list))
    (set-face-attribute
     'default nil :font "Monaco 14"))

;;--------------------------------------------------------------------
;; Theme and modeline setup
;;--------------------------------------------------------------------

;; Theme
(require 'base16-ateliersulphurpool-dark-theme)

;; Fix modeline issue in mac
(if (eq system-type 'darwin)
    (setq ns-use-srgb-colorspace nil))

;; Battery in percentage
(fancy-battery-mode)
(setq fancy-battery-show-percentage t)

;; Spaceline-config
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(setq spaceline-minor-modes-p nil)

;;--------------------------------------------------------------------
;; Package configuations
;;--------------------------------------------------------------------

;; Gather PATH from shell as GUI emacs don't do by default
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Evil-leader-config
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

;; Evil-config
(require 'evil)
(evil-mode 1)

;; Key chord
(key-chord-mode 1)

;; Helm-config
(require 'helm-config)
(helm-mode t)
(setq helm-mode-fuzzy-match t)
(setq helm-completion-in-region-fuzzy-match t)

;; Projectile-config
(projectile-global-mode)
(setq projectile-enable-caching t)

;; Perspective-config
(persp-mode)

;; Helm-projectile-config
(require 'helm-projectile)
(helm-projectile-on)

;; Python setup
(elpy-enable)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Replace your virtualenv name at below
(let ((virtualenv-workon-starts-python -1))
  (pyvenv-workon "aic"))
(when (executable-find "ipython")
  (elpy-use-ipython))

;;--------------------------------------------------------------------
;; Custom functions
;;--------------------------------------------------------------------

(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

;; Fix for ansi term "4m" issue in mac
;; http://stackoverflow.com/questions/8918910/weird-character-zsh-in-emacs-terminal/8920373#8920373
(defun use-zsh-ansi-term ()
  "User zsh as default shell for ansi-term."
  (interactive)
  (ansi-term "/usr/local/bin/zsh"))

;;--------------------------------------------------------------------
;; Custom keys
;;--------------------------------------------------------------------

;; Use up and down arrows to go up and down in history in ipython
(define-key comint-mode-map (kbd "<up>")
      'comint-previous-input)
(define-key comint-mode-map (kbd "<next>")
      'comint-next-input)

;; Escape to normal mode with 'jk' in evil-mode
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; Key configuration with evil-leader
(evil-leader/set-key
  "I" 'find-user-init-file
  "T" 'use-zsh-ansi-term
  "f" 'helm-find-files
  "c" 'comment-or-uncomment-region

  ;; b stands for buffer
  "bk" 'kill-buffer
  "br" 'rename-buffer
  "bR" 'revert-buffer
  "bs" 'persp-switch-to-buffer

  ;; e stands for elpy
  "ed" 'elpy-goto-definition
  "es" 'elpy-shell-switch-to-shell
  "er" 'elpy-shell-send-region-or-buffer
  "ec" 'elpy-shell-send-current-statement

  ;; p stands for project
  "pp" 'helm-projectile-switch-project
  "pf" 'helm-projectile-find-file
  "pg" 'helm-projectile-grep
  "pi" 'projectile-invalidate-cache

  ;; t stands for tree
  "tt" 'project-explorer-toggle

  ;; w stands for workspace
  "ws" 'persp-switch
  "wr" 'persp-rename
  "wk" 'persp-kill
  "wl" 'persp-next
  "wh" 'persp-prev
  )
