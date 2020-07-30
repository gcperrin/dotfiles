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
 '(package-selected-packages
   (quote
    (cmake-mode evil-collection counsel ivy major-mode-icons evil-magit magit docker-compose-mode markdown-mode add-node-modules-path company-tern company-shell tide js2-mode irony-eldoc flycheck-irony company-irony irony flycheck json-mode ace-window spaceline web-mode company neotree evil-commentary key-chord color-theme-modern evil-leader evil))))
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

;;; SYSTEM CLIPBOARD ;;;
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;;; IVY :::
(require 'ivy)
(ivy-mode 1)
;; (setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;;; COUNSEL ;;;
(require 'counsel)
(counsel-mode 1)

(setq evil-want-keybinding nil)
;; (setq evil-collection-setup-minibuffer t)

(require 'evil-collection)
;; (setq evil-want-keybinding nil)
(setq evil-collection-setup-minibuffer t)
(with-eval-after-load 'ivy (require 'evil-collection-ivy))

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
  "e" 'toggle-flycheck-error-buffer
  "l" 'goto-line
  "c" 'avy-goto-char
  "g" 'replace-regexp
  "s" 'query-replace-regexp
  "M" 'counsel-find-file
  "m" 'counsel-git-grep
  "<right>" 'shrink-window-horizontally
  "<left>" 'enlarge-window-horizontally
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

(require 'irony)
  (progn
    ;; If irony server was never installed, install it.
    (unless (irony--find-server-executable) (call-interactively #'irony-install-server))

    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)

    ;; Use compilation database first, clang_complete as fallback.
    (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
                                                      irony-cdb-clang-complete))

    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )


;;; COMPANY MODE ;;;
(require 'company)
(setq company-tooltip-limit 20) ; bigger popup window
(setq company-tooltip-align-annotations 't) ; align annotations to the right tooltip border
(setq company-idle-delay 0) ; decrease delay before autocompletion popup shows
(setq company-idle-delay-tooltip 0); popup display delay
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(setq company-dabbrev-downcase nil) ; stop downcase returns
(setq company-tooltip-maximum-width 50)

(add-hook 'after-init-hook 'global-company-mode)

;; I use irony with company to get code completion.
(require 'company-irony)
(eval-after-load 'company '(add-to-list 'company-backends 'company-irony))

;; ;; I use irony with flycheck to get real-time syntax checking.
;; (require 'flycheck-irony)
;; (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(require 'irony-eldoc)
(add-hook 'irony-mode-hook #'irony-eldoc)

(require 'company-irony-c-headers)
;; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
  '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))
  ;; (setq irony-additional-clang-options '("-std=c++11")))

(require 'company-tern)
(add-to-list 'company-backends 'company-tern)
;; (add-to-list 'company-backends 'company-files)

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

;;; WEB MODE ;;;
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (web-mode-set-content-type "jsx")
            (tern-mode)
            ;; (company-mode)
            (message "now set to: %s" web-mode-content-type)))

;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             ;; short circuit js mode and just do everything in jsx-mode
;;             (if (equal web-mode-content-type "javascript")
;;                 (web-mode-set-content-type "jsx")
;;               (message "now set to: %s" web-mode-content-type))))

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))
;; for better jsx syntax-highlighting in web-mode

;; ;; - courtesy of Patrick @halbtuerke
;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "javascript")
;;     (let ((web-mode-enable-part-face nil))
;;       ad-do-it)
;;     ad-do-it))


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
        web-mode-markup-indent-offset 4
        web-mode-attr-indent-offset 4
        web-mode-css-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-style-padding 4
        web-mode-script-padding 4
        web-mode-block-padding 0
        web-mode-comment-style 4)
)

(add-hook 'web-mode-hook 'my-web-mode-hook)


;; (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
;; (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
;; (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

(set-face-attribute 'web-mode-html-tag-face nil :foreground "color-79" :bold t)
(set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "White")
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "color-69" :bold t)
(set-face-attribute 'web-mode-function-call-face nil :foreground "color-105" :bold t)

;; (add-hook 'web-mode-hook (lambda ()
;;                           (set (make-local-variable 'company-backends) '(company-tern))
;;                           (company-mode t)))

;; (add-hook 'web-mode-hook (lambda () (company-mode t)))
;; (add-hook 'web-mode-hook (lambda () (tern-mode t)))

;;; FLYCHECK ;;;
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(set-face-attribute 'flycheck-error nil :foreground "red")

;; JS
;; (setq-default

;;  ;; js2-mode
;;  js2-basic-offset 2
;;  js-indent-level 2

;;  ;; Let flycheck handle parse errors
;;  js2-show-parse-errors nil
;;  js2-strict-missing-semi-warning nil
;;  js2-strict-trailing-comma-warning nil
;;  js2-include-node-externs t)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(flycheck-add-mode 'javascript-eslint 'web-mode)

(setq-default flycheck-temp-prefix ".flycheck")
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

(defun toggle-flycheck-error-buffer ()
  "toggle a flycheck error buffer."
  (interactive)
  (if (string-match-p "Flycheck errors" (format "%s" (window-list)))
      (dolist (w (window-list))
        (when (string-match-p "*Flycheck errors*" (buffer-name (window-buffer w)))
          (delete-window w)
          ))
    (flycheck-list-errors)

    (dolist (w (window-list))
      (when (string-match-p "*Flycheck errors*" (buffer-name (window-buffer w)))
        (shrink-window-if-larger-than-buffer w)
        ))
    )
  )

;; (require 'tide)
;; (defun setup-tide-mode ()
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-add-next-checker 'typescript-tide '(t . typescript-tslint) 'append)
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1))

;; ;; aligns annotation to the right hand side
;; (setq company-tooltip-align-annotations t)

;; ;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)
;; (add-hook 'typescript-mode-hook #'setup-tide-mode)

;; (add-hook 'typescript-mode-hook
;; 	        (lambda ()
;; 		        (when (string-equal "tsx" (file-name-extension buffer-file-name))
;; 		          (setup-tide-mode))))

;; (add-hook 'typescript-mode-hook
;; 	        (lambda ()
;; 		        (when (string-equal "ts" (file-name-extension buffer-file-name))
;; 		          (setup-tide-mode))))


;; ;; funky typescript linting in web-mode
;; (flycheck-add-mode 'typescript-tslint 'web-mode)

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-exists-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(eval-after-load 'web-mode
  '(add-hook 'web-mode-hook #'add-node-modules-path))

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

;; C / C++ mode
(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
