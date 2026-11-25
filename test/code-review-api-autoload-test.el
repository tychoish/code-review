;;; code-review-api-autoload-test.el --- Autoload cookies for the public API -*- lexical-binding: t; -*-

;;; Commentary:
;; Every public (non `--') function of the non-interactive API files
;; must carry an autoload cookie so callers can use `fboundp' with
;; `declare-function' and no `require'.

;;; Code:

(require 'ert)
(require 'seq)
(require 'subr-x)

(defconst code-review-api-autoload-test--files
  '("code-review-api.el"
    "code-review-api-viewed.el"
    "code-review-api-analysis.el"
    "code-review-api-send.el"))

(defconst code-review-api-autoload-test--root
  (expand-file-name
   ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root directory.")

(defun code-review-api-autoload-test--missing (file)
  "Return public definitions in FILE that lack an autoload cookie."
  (let (missing)
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name file code-review-api-autoload-test--root))
      (goto-char (point-min))
      (while (re-search-forward
              "^(\\(?:cl-\\)?def\\(?:un\\|macro\\) \\([^ \n()]+\\)" nil t)
        (let ((name (match-string 1))
              (bol (line-beginning-position)))
          (unless (string-match-p "--" name)
            (save-excursion
              (goto-char bol)
              (forward-line -1)
              (unless (looking-at-p ";;;###autoload")
                (push name missing)))))))
    (nreverse missing)))

(ert-deftest code-review-api-autoload-test-public-functions-have-cookies ()
  (seq-do
   (lambda (file)
     (should (equal nil (code-review-api-autoload-test--missing file))))
   code-review-api-autoload-test--files))

(provide 'code-review-api-autoload-test)
;;; code-review-api-autoload-test.el ends here
