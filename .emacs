;; User Info
(setq user-full-name "Greg Perrin")
(setq user-mail-address "gregoryperrin.ucb@gmail.com")

;; UI setup
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(global-linum-mode)
(set-face-foreground 'linum "color-242")
(setq linum-format "%4d \u2502 ")
(setq mac-command-modifier 'meta
      mac-option-modifier 'none)

;;; sys clipboard
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;; Misc native issues
(setq backup-inhibited t) ; disable backup
(setq auto-save-default nil) ; disable autosave
(setq gc-cons-threshold 100000000)

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

;; Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq standard-indent 2)

;; Matching delimiter mode
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Face recognition
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; Evil nav extension
(global-set-key (kbd "C-j")
                (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "C-k")
                (lambda () (interactive) (previous-line 5)))

;; Install use-package if necessary
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives (append package-archives
			 '(("melpa" . "http://melpa.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("elpy" . "http://jorgenschaefer.github.io/packages/"))))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package evil
  :ensure t ;; install the evil package if not installed
  :init ;; tweak evil's configuration before loading it
  (setq evil-want-keybinding nil)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "TAB") nil))
  :config ;; tweak evil after loading it
  (evil-mode))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

(use-package evil-leader
  :ensure t
  :after evil
  :init
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "w" 'save-buffer
    "q" 'kill-buffer
    "p" 'show-file-name
    "\\" 'split-window-horizontally 
    "<right>" 'shrink-window-horizontally
    "<left>" 'enlarge-window-horizontally
    "h" 'ace-window
    "b" 'counsel-switch-buffer
    "r" 'counsel-rg
    "t" 'neotree-toggle
    "i" 'lsp-ui-imenu
    "f" 'swiper
    "e" 'counsel-M-x)
  :config
  (global-evil-leader-mode))

(use-package evil-commentary
  :ensure t
  :after evil
  :init
  (evil-commentary-mode))


(use-package key-chord
  :ensure t
  :after evil
  :config
  (key-chord-mode 1)
  :init
  (key-chord-define evil-insert-state-map "fd" 'evil-normal-state)
  (key-chord-define evil-visual-state-map "fd" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-visual-state-map "jk" 'evil-normal-state))

(use-package color-theme-modern
  :ensure t
  :init
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory "~/.emacs.d/themes/"))
  :config
  (load-theme 'kingsajz t t)
  (enable-theme 'kingsajz))

(use-package spaceline
  :config
  :ensure t
  :after color-theme-modern
  :init
  (spaceline-emacs-theme))

(use-package neotree
  :ensure t
  :config
  (add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            ;; dumb hack to fix line jumps in tree
            (define-key evil-normal-state-local-map (kbd "C-j") (lambda () (interactive) (next-line 5))) 
            (define-key evil-normal-state-local-map (kbd "C-k") (lambda () (interactive) (previous-line 5))) 
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-change-root)))
  (setq neo-window-width 35)
  (setq-default neo-show-hidden-files t))

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?j ?k ?l)
        aw-dispatch-always t))

(use-package ivy
  :ensure t
  :after key-chord
  :init
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-display-style 'fancy)
  :bind (
         :map ivy-minibuffer-map
              ("C-j" . 'ivy-next-line)
              ("C-k" . 'ivy-previous-line))
  :config
  (ivy-mode t))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)))
  
(use-package swiper
  :ensure t
  )

(use-package dumb-jump
  :ensure t
  :after evil evil-collection
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  )

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (smartparens-global-mode))

(use-package wgrep
  :ensure t
  :after ivy counsel)

(use-package wgrep-ag
  :ensure t
  :after wgrep ivy counsel)

(use-package imenu-list
  :ensure t
  :config
  (setq imenu-list-focus-after-activation t))

;; Company
(use-package company
  :ensure t
  :after color-theme-modern evil
  :bind (:map company-active-map
              ("C-j" . company-select-next)
              ("C-k" . company-select-previous))
  :config
  (setq company-idle-delay 0)
  :init
  (set (make-local-variable 'company-backends)
       '((company-files company-keywords company-capf company-dabbrev-code company-etags company-dabbrev)))
  (global-company-mode)
  (with-eval-after-load 'company
    (define-key company-active-map (kbd "TAB") nil)))

(use-package lsp-mode
  :ensure t
  :after company
  :hook ((web-mode . lsp))
  :config
  )

(use-package lsp-ui
  :ensure t
  :config
  (lsp-ui-doc-mode -1)
  (setq lsp-ui-doc-enable nil)
  :config
  (add-hook 'lsp-ui-imenu-mode
              (lambda () (interactive) (linum-mode -1))))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  ;; disable jshint since we prefer eslint checking
  ;; (setq-default flycheck-disabled-checkers
	;; 						  (append flycheck-disabled-checkers
	;; 										  '(javascript-jshint)))
  
  ;; (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default flycheck-temp-prefix ".flycheck")
  (setq-default flycheck-disabled-checkers
							  (append flycheck-disabled-checkers
											  '(json-jsonlist)))
  (set-face-attribute 'flycheck-error nil :foreground "red"))

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-opening t
        web-mode-enable-auto-indentation t
        web-mode-enable-block-face t
        web-mode-enable-part-face t
        web-mode-enable-comment-keywords t
        web-mode-enable-css-colorization t
        web-mode-enable-current-element-highlight t
        ;; web-mode-enable-heredoc-fontification t
        web-mode-enable-engine-detection t
        web-mode-markup-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2
        web-mode-block-padding 0
        web-mode-comment-style 2)
  (lambda ()
    (web-mode-set-content-type "jsx")
    ;; (tern-mode)
    ;; (company-mode)
    (message "now set to: %s" web-mode-content-type))
  :config
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "color-79" :bold t)
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "White")
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "color-69" :bold t)
  (set-face-attribute 'web-mode-function-call-face nil :foreground "color-105" :bold t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "ts")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))
  )

(use-package dockerfile-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black"))))
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-scrollbar-bg ((t (:background "#121212"))))
 '(company-scrollbar-fg ((t (:background "#121212"))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
 '(neo-dir-link-face ((t (:foreground "brightcyan" :bold t))))
 '(neo-expand-btn-face ((t (:foreground "brightwhite" :bold t))))
 '(neo-file-link-face ((t (:foreground "color-63" :bold t))))
 '(neo-header-face ((t (:foreground "color-130"))))
 '(neo-root-dir-face ((t (:foreground "color-171")))))
