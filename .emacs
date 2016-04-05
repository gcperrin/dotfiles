;;; Emacs is not a package manager, and here we load its package manager!
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                 ("elpa" . "http://tromey.com/elpa/")
                 ;; TODO: Maybe, use this after emacs24 is released
                 ;; (development versions of packages)
                 ("melpa" . "http://melpa.milkbox.net/packages/")
                 ))
 (add-to-list 'package-archives source t))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(80-column-rule t)
 '(package-selected-packages
   (quote
	(less-css-mode web-mode jade-mode gdb-mi crosshairs yasnippet ac-html-bootstrap ac-html column-enforce-mode ac-js2 js2-mode jedi tabbar-ruler tabbar nav color-theme)))
 '(show-trailing-whitespace t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'html-mode-hook
  (lambda ()
          ;; Default indentation is usually 2 spaces, changing to 4.
    (set (make-local-variable 'sgml-basic-offset) 2)))

;; JS indent
(setq js-indent-level 2)

;; JSX HTML highlighting
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
    ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "color-79")
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "White")
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "color-69"))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; More JSX shit
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
	  (let ((web-mode-enable-part-face nil))
		ad-do-it)
	ad-do-it))

;; ColorTheme
(require 'color-theme)
(color-theme-initialize)
(color-theme-robin-hood)

;; Disable backup
(setq backup-inhibited t)

;; Disable auto save
(setq auto-save-default nil)

;; Font size
(set-face-attribute 'default nil :height 105)

;; Tabs
(require 'tabbar)
(tabbar-mode t)

(global-set-key [M-left] 'tabbar-backward-tab)
(global-set-key [M-right] 'tabbar-forward-tab)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)

(add-to-list 'auto-mode-alist '("\\SConscript$" . python-mode))

;; C / C++ stuff
(require 'cc-mode)
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;;; activate ecb
(require 'ecb)
;; (require 'ecb-autoloads)

(setq ecb-layout-name "leftright3")
(setq ecb-show-sources-in-directories-buffer 'always)
(setq ecb-compile-window-height 12)

;;; yasnippet
;;; should be loaded before auto complete so that they can work together
;;;(require 'yasnippet)
;;;(yas-global-mode 1)

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
;;;(require 'auto-complete-config)
;;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;;(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
;;;(ac-set-trigger-key "TAB")
;;;(ac-set-trigger-key "<tab>")

;; Bind C-p to the ctl-x-map.
;;;(define-key map (kbd "C-a") M-x)
