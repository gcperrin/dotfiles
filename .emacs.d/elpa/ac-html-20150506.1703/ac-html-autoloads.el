;;; ac-html-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "ac-haml" "ac-haml.el" (21864 59729 24428 575000))
;;; Generated autoloads from ac-haml.el

(autoload 'ac-haml-enable "ac-haml" "\
Add ac-haml sources into ac-sources and enable auto-comple-mode

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ac-html" "ac-html.el" (21864 59728 988428
;;;;;;  573000))
;;; Generated autoloads from ac-html.el

(defvar ac-source-html-tag '((candidates . ac-source-html-tag-candidates) (prefix . "<\\(.*\\)") (symbol . "t")))

(defvar ac-source-html-attribute '((candidates . ac-source-html-attribute-candidates) (prefix . "<\\w[^>]*[[:space:]]+\\(.*\\)") (symbol . "a")))

(defvar ac-source-html-attribute-value '((candidates . ac-source-html-attribute-value-candidates) (prefix . ac-html-value-prefix) (document . ac-source-html-attribute-value-document) (symbol . "v")))

(autoload 'ac-html-enable "ac-html" "\
Add ac-html sources into ac-sources and enable auto-comple-mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ac-jade" "ac-jade.el" (21864 59728 956428
;;;;;;  572000))
;;; Generated autoloads from ac-jade.el

(autoload 'ac-jade-enable "ac-jade" "\
Add ac-jade sources into ac-sources and enable auto-comple-mode

\(fn)" t nil)

;;;***

;;;### (autoloads nil "ac-slim" "ac-slim.el" (21864 59728 832428
;;;;;;  567000))
;;; Generated autoloads from ac-slim.el

(autoload 'ac-slim-enable "ac-slim" "\
Add ac-slim sources into ac-sources and enable auto-comple-mode

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("ac-html-pkg.el") (21864 59729 225067
;;;;;;  243000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; ac-html-autoloads.el ends here
