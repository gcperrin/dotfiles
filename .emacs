;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1c10e946f9a22b28613196e4c02b6508970e3b34660282ec92d9a1c293ee81bb" default)))
 '(package-selected-packages
   (quote
    (avy company-statistics company-tern tern tide js2-mode go-mode flycheck json-mode ace-window spaceline web-mode company neotree evil-commentary key-chord color-theme color-theme-modern evil-leader evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black"))))
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
 '(font-lock-comment-face ((t (:foreground "color-012"))))
 '(neo-dir-link-face ((t (:foreground "brightcyan" :bold t))))
 '(neo-expand-btn-face ((t (:foreground "brightwhite" :bold t))))
 '(neo-file-link-face ((t (:foreground "color-63" :bold t))))
 '(neo-header-face ((t (:foreground "color-130"))))
 '(neo-root-dir-face ((t (:foreground "color-171")))))


;;; ACE WINDOW ;;;
(require 'ace-window)
(setq aw-keys '(?j ?k ?l)
      aw-dispatch-always t)

;;; AVY ;;;
(require 'avy)


;;; EVIL MODE ;;;
(require 'evil)
(evil-mode 1)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "fd" 'evil-normal-state)
(key-chord-define evil-visual-state-map "fd" 'evil-normal-state)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-normal-state)


(global-set-key (kbd "C-j")
		(lambda () (interactive) (next-line 5)))
(global-set-key (kbd "C-k")
		(lambda () (interactive) (previous-line 5)))

(require 'evil-leader)
(global-evil-leader-mode)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "w" 'save-buffer
  "q" 'kill-buffer
  "j" 'evil-ex
  "k" 'execute-extended-command
  "t" 'neotree-toggle
  "\\" 'split-window-horizontally
  "n" 'next-buffer
  "p" 'previous-buffer
  "f" 'ace-window
  "l" 'goto-line
  "c" 'avy-goto-char
  "g" 'replace-regexp
  "r" 'query-replace-regexp
  )

(require 'evil-commentary)
(evil-commentary-mode)

;;; ELECTRIC PAIR MODE ;;;
(electric-pair-mode)
(setq-default electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)


;;; TABS CONFIG ;;;
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq standard-indent 2)
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "TAB") nil))


;;; COLOR THEMES ;;;
(require 'color-theme-modern)
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/themes/"))
(load-theme 'kingsajz t t)
(enable-theme 'kingsajz)


;;; NEOTREE ;;;
(require 'neotree)
(setq neo-window-width 35)
(setq-default neo-show-hidden-files t)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))


;;; COMPANY MODE ;;;
(require 'company)
(setq company-tooltip-limit 20) ; bigger popup window
(setq company-tooltip-align-annotations 't) ; align annotations to the right tooltip border
(setq company-idle-delay 0) ; decrease delay before autocompletion popup shows
(setq company-idle-delay-tooltip 0); popup display delay
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(setq company-dabbrev-downcase nil) ; stop downcase returns
(setq company-tooltip-maximum-width 50)

(add-to-list 'company-backends 'company-tern)

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-common)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common)
     (define-key company-active-map (kbd "C-j") 'company-select-next-or-abort)
     (define-key company-active-map (kbd "C-k") 'company-select-previous-or-abort)))

(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend-with-delay
        company-preview-frontend
        company-echo-metadata-frontend))

(add-hook 'after-init-hook 'company-statistics-mode)
(add-hook 'after-init-hook 'global-company-mode)


;;; WEB MODE ;;;
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-opening t
        web-mode-enable-auto-indentation t
        web-mode-enable-block-face t
        web-mode-enable-part-face t
        web-mode-enable-comment-keywords t
        web-mode-enable-css-colorization t
        ;; web-mode-enable-current-element-highlight t
        web-mode-enable-heredoc-fontification t
        web-mode-enable-engine-detection t
        web-mode-markup-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2
        web-mode-block-padding 0
        web-mode-comment-style 2)
)

(add-hook 'web-mode-hook  'my-web-mode-hook)

;; (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
;; (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
;; (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

(set-face-attribute 'web-mode-html-tag-face nil :foreground "color-79" :bold t)
(set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "White")
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "color-69" :bold t)
(set-face-attribute 'web-mode-function-call-face nil :foreground "color-105" :bold t)

(add-hook 'web-mode-hook (lambda () (tern-mode t)))

;;; FLYCHECK ;;;
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode) ; global flycheck

;; JS
(setq-default

 ;; js2-mode
 js2-basic-offset 2
 js-indent-level 2

 ;; Let flycheck handle parse errors
 js2-show-parse-errors nil
 js2-strict-missing-semi-warning nil
 js2-strict-trailing-comma-warning nil
 js2-include-node-externs t)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
							(append flycheck-disabled-checkers
											'(javascript-jshint)))

(flycheck-add-mode 'javascript-eslint 'web-mode)

(setq-default flycheck-temp-prefix ".flycheck")
(setq-default flycheck-disabled-checkers
							(append flycheck-disabled-checkers
											'(json-jsonlist)))

(set-face-attribute 'flycheck-error nil :foreground "red")

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)


;;; SPACELINE ;;;
(require 'spaceline-config)
(spaceline-emacs-theme)


;;; MISC ;;;
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; whitespace cleanup

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq backup-inhibited t) ; disable backup
(setq auto-save-default nil) ; disable autosave

;; Matching delimiter mode
(show-paren-mode 1)
(setq show-paren-delay 0)

;; Face recognition
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; Line numbers
(global-linum-mode)
(set-face-foreground 'linum "color-242")
(setq linum-format "%4d \u2502 ")


;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
