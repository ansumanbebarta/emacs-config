(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; -----------------------------------------------------------------------------
;; Package initialisations
;; -----------------------------------------------------------------------------

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(ace-window
    ein
    elpy
    evil
    flx-ido
    flycheck
    key-chord
    magit
    markdown-mode
    material-theme
    multi-term
    projectile
    py-autopep8
    smart-mode-line
    smart-mode-line-powerline-theme
    ))

(mapc #'(lambda (package)
	  (unless (package-installed-p package)
	    (package-install package)))
      myPackages)

;; -----------------------------------------------------------------------------
;; Basic customisations
;; -----------------------------------------------------------------------------

;; Answer with y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use spaces, not tabs, when indenting
(setq indent-tabs-mode -1)

;; Ignore case when searching
(setq case-fold-search t)

;; Require final newlines in files when they are saved
(setq require-final-newline t)

;; Language
(setq current-language-environment "English")

;; Don't show startup screen
(setq inhibit-startup-screen t)

;; Don't show the menu bar
(menu-bar-mode -1)

;; Don't show the tool bar
(require 'tool-bar)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Set fringe width to 10
(require 'fringe)
(setq-default left-fringe-width 10)
(setq-default right-fringe-width 0)
(set-face-attribute 'fringe nil :background "black")

(global-linum-mode t)
(setq linum-format "%3d \u2502")
(line-number-mode t)
(column-number-mode t)

(require 'mwheel)
(mouse-wheel-mode t)

(put 'suspend-frame 'disabled t)

;; Customize the cursor
(blink-cursor-mode -1)

;; -----------------------------------------------------------------------------
;; Package configurations
;; -----------------------------------------------------------------------------

(load-theme 'material t)
(setq sml/theme 'powerline)
(sml/setup)

(require 'evil)
(evil-mode 1)

;; Map "jk" to go to normal mode from insert modef for evil
(require 'key-chord)
(key-chord-mode t)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; Enable ido and use flx-ido
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces -1)

;; Enable Projectile
(projectile-global-mode)
(setq projectile-enable-caching 1)
(setq dired-use-ls-dired nil)
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program -1))

;; Enable elpy
(elpy-enable)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(let ((virtualenv-workon-starts-python -1))
  (pyvenv-workon "aic"))
(elpy-use-ipython)

;; Multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")

;; Ace window
(require 'ace-window)
(global-set-key (kbd "C-x o") 'ace-window)

;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Magit customization
(global-set-key (kbd "C-x g") 'magit-status)
