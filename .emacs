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
 '(custom-safe-themes
	 (quote
		("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
	 (quote
		(exec-path-from-shell json-mode smart-mode-line neotree sr-speedbar auto-complete less-css-mode web-mode jade-mode gdb-mi crosshairs yasnippet ac-html-bootstrap ac-html column-enforce-mode ac-js2 js2-mode jedi tabbar-ruler tabbar nav color-theme flycheck)))
 '(show-trailing-whitespace t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(neo-dir-link-face ((t (:foreground "brightcyan" :bold t))))
 '(neo-expand-btn-face ((t (:foreground "brightwhite" :bold t))))
 '(neo-file-link-face ((t (:foreground "color-63" :bold t))))
 '(neo-header-face ((t (:foreground "color-130"))))
 '(neo-root-dir-face ((t (:foreground "color-171")))))

;; Neotree startup
(setq neo-window-width 30)
(add-hook 'after-init-hook #'neotree-toggle)

;; Smartline
(sml/setup)

;; Whitespace-cleanup macro
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Auto-complete pairing of syntax elements
(electric-pair-mode)

;; New line jumps cause shit
(defun smarter-move-beginning-of-line (arg)
	(interactive "^p")
	(setq arg (or arg 1))

	;; Move lines first
	(when (/= arg 1)
	  (let ((line-move-visual nil))
		(forward-line (1- arg))))

	(let ((orig-point (point)))
	  (back-to-indentation)
	  (when (= orig-point (point))
		(move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
				                'smarter-move-beginning-of-line)

(global-set-key (kbd "\C-q") 'move-beginning-of-line)

;; Copy whole line
(global-set-key (kbd "\C-c\C-d") "\C-a\C- \C-n\M-w\C-y")

(add-hook 'html-mode-hook
  (lambda ()
		;; Default indentation is usually 2 spaces, changing to 4.
    (set (make-local-variable 'sgml-basic-offset) 2)))

;; JS indent
(setq js-indent-level 2)

;; JSX HTML highlighting
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; Autocomplete setup
(ac-config-default)
;;(setq ac-auto-start 4)
(setq ac-auto-show-menu nil)
;; Show 0.8 second later
(setq ac-auto-show-menu 0.8)
(add-to-list 'ac-modes 'web-mode)

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
    ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-ac-sources-alist
	  '(("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "color-79" :bold t)
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "White")
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "color-69" :bold t))
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq web-mode-ac-sources-alist
	  '(("html" . (ac-source-words-in-buffer ac-source-abbrev))))

;; More JSX shit
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
			(let ((web-mode-enable-part-face nil))
				ad-do-it)
		ad-do-it))

;; ESlint flycheck setup
;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
							(append flycheck-disabled-checkers
											'(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
							(append flycheck-disabled-checkers
											'(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
	  (exec-path-from-shell-initialize))


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

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)

(add-to-list 'auto-mode-alist '("\\SConscript$" . python-mode))

;; C / C++ stuff
(require 'cc-mode)
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 2 indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;; Bind C-p to the ctl-x-map.
;;;(define-key map (kbd "C-a") M-x)
