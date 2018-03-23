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
    (company neotree evil-commentary key-chord color-theme color-theme-modern evil-leader evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black")))))


;;; EVIL MODE ;;;
(require 'evil)
(evil-mode 1)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map  "fd" 'evil-normal-state)

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
  "n" 'neotree-toggle
)

(require 'evil-commentary)
(evil-commentary-mode)


;;; COLOR THEMES ;;;
(require 'color-theme-modern)
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/themes/"))
(load-theme 'robin-hood t t)
(enable-theme 'robin-hood)


;;; NEOTREE ;;;
(require 'neotree)
(setq neo-window-width 25)


;;; COMPANY MODE ;;;
(require 'company)
(setq company-tooltip-limit 30) ; bigger popup window
(setq company-tooltip-align-annotations 't) ; align annotations to the right tooltip border
(setq company-idle-delay 0) ; decrease delay before autocompletion popup shows
(setq company-idle-delay-tooltip 0);
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
(setq company-dabbrev-downcase nil) ; stop downcase returns

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

(add-hook 'after-init-hook 'global-company-mode)


;;; MISC ;;;
(add-hook 'before-save-hook 'delete-trailing-whitespace) ; whitespace cleanup

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Matching delimiter mode
(show-paren-mode 1)
(setq show-paren-delay 0)
