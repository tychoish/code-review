;;; code-review-api-viewed-test.el --- ERT tests for the viewed API -*- lexical-binding: t; -*-

(require 'ert)
(require 'code-review-api-viewed)
(require 'code-review-test-helpers)

(defun code-review-api-viewed-test--diff ()
  "Return a two-file diff with index lines."
  (concat "diff --git a/app.el b/app.el\n"
          "index 1111111..2222222 100644\n--- a/app.el\n+++ b/app.el\n"
          "@@ -1,2 +1,2 @@\n context\n-old\n+new\n"
          "diff --git a/lib.el b/lib.el\n"
          "index 3333333..4444444 100644\n--- a/lib.el\n+++ b/lib.el\n"
          "@@ -1,2 +1,2 @@\n context\n-old\n+new\n"))

(defmacro code-review-api-viewed-test--with-pr (&rest body)
  "Run BODY with a stored owner/repo#7 review holding the fixture diff."
  (declare (indent 0))
  `(code-review-test--with-db
     (code-review-db--pullreq-create
      (code-review-github-repo :owner "owner" :repo "repo" :number "7"))
     (code-review-db--pullreq-raw-diff-update
      (code-review-api-viewed-test--diff))
     ,@body))

(ert-deftest code-review-api-viewed-test/round-trip ()
  (code-review-api-viewed-test--with-pr
    (should (equal (code-review-api-viewed "owner/repo" 7)
                   '(("app.el" :blob "2222222" :viewed-p nil)
                     ("lib.el" :blob "4444444" :viewed-p nil))))
    (should (code-review-api-set-viewed "owner/repo" 7 "app.el" "2222222" t))
    (should (equal (code-review-api-viewed "owner/repo" "7")
                   '(("app.el" :blob "2222222" :viewed-p t)
                     ("lib.el" :blob "4444444" :viewed-p nil))))
    ;; the UI index reads the same state
    (let ((code-review-db--pullreq-id
           (code-review-api-viewed--pr-id "owner/repo" 7)))
      (should (code-review-viewed--viewed-p
               (map-elt (code-review-viewed--index) "app.el"))))
    (should (eq t (code-review-api-set-viewed "owner/repo" 7 "app.el" nil nil)))
    (should-not (plist-get (cdr (car (code-review-api-viewed "owner/repo" 7)))
                           :viewed-p))))

(ert-deftest code-review-api-viewed-test/stale-blob-and-unknown-path ()
  (code-review-api-viewed-test--with-pr
    (should-not (code-review-api-set-viewed "owner/repo" 7 "app.el" "9999999" t))
    (should-not (code-review-api-set-viewed "owner/repo" 7 "nope.el" nil t))
    (should (seq-every-p (lambda (e) (null (plist-get (cdr e) :viewed-p)))
                         (code-review-api-viewed "owner/repo" 7)))
    ;; too-short abbreviations are refused
    (should-not (code-review-api-set-viewed "owner/repo" 7 "lib.el" "444" t))
    (should (code-review-api-set-viewed "owner/repo" 7 "lib.el" "4444444" t))))

(ert-deftest code-review-api-viewed-test/blob-change-invalidates ()
  (code-review-api-viewed-test--with-pr
    (code-review-api-set-viewed "owner/repo" 7 "app.el" "2222222" t)
    (let ((code-review-db--pullreq-id
           (code-review-api-viewed--pr-id "owner/repo" 7)))
      (code-review-viewed--expire '("app.el")))
    (should-not (plist-get (cdr (car (code-review-api-viewed "owner/repo" 7)))
                           :viewed-p))))

(ert-deftest code-review-api-viewed-test/unknown-review ()
  (code-review-test--with-db
    (should-error (code-review-api-viewed "owner/repo" 99) :type 'user-error)
    (should-error (code-review-api-viewed "bad" 1) :type 'user-error)))

(ert-deftest code-review-api-viewed-test/blob-match ()
  (should (code-review-api-viewed--blob-match-p "2222222" "2222222"))
  (should (code-review-api-viewed--blob-match-p "2222222" "22222223333"))
  (should-not (code-review-api-viewed--blob-match-p "" ""))
  (should-not (code-review-api-viewed--blob-match-p "222" "2222222"))
  (should-not (code-review-api-viewed--blob-match-p nil "2222222"))
  (should-not (code-review-api-viewed--blob-match-p "2222222" nil)))

(ert-deftest code-review-api-viewed-test/unchanged-and-hunk-clearing ()
  (code-review-api-viewed-test--with-pr
    (should (eq :unchanged
                (code-review-api-set-viewed "owner/repo" 7 "app.el" nil nil)))
    (should (eq t (code-review-api-set-viewed "owner/repo" 7 "app.el" nil t)))
    (should (eq :unchanged
                (code-review-api-set-viewed "owner/repo" 7 "app.el" nil t)))
    ;; a hunk-only mark is cleared by unmarking the file
    (let ((code-review-db--pullreq-id
           (code-review-api-viewed--pr-id "owner/repo" 7)))
      (code-review-db--reviewed-delete-path "app.el")
      (code-review-db--reviewed-insert "app.el" "-1,2 +1,2")
      (should (equal (code-review-db--reviewed-rows)
                     '((:path "app.el" :hunk-key "-1,2 +1,2")))))
    (should (eq t (code-review-api-set-viewed "owner/repo" 7 "app.el" nil nil)))
    (let ((code-review-db--pullreq-id
           (code-review-api-viewed--pr-id "owner/repo" 7)))
      (should-not (code-review-db--reviewed-rows)))))

(ert-deftest code-review-api-viewed-test/mark-follows-block-not-blob ()
  "A blob change with identical hunk headers keeps the mark (documented)."
  (code-review-api-viewed-test--with-pr
    (code-review-api-set-viewed "owner/repo" 7 "app.el" "2222222" t)
    (code-review-db--pullreq-raw-diff-update
     (replace-regexp-in-string "2222222" "5555555"
                               (code-review-api-viewed-test--diff)))
    (let ((state (car (code-review-api-viewed "owner/repo" 7))))
      (should (equal (plist-get (cdr state) :blob) "5555555"))
      (should (plist-get (cdr state) :viewed-p)))
    ;; the stale blob can no longer be used to write
    (should-not (code-review-api-set-viewed "owner/repo" 7 "app.el" "2222222" nil))))

(ert-deftest code-review-api-viewed-test/pending-sync-refuses ()
  (code-review-api-viewed-test--with-pr
    (let ((key (list (code-review-api-viewed--pr-id "owner/repo" 7) "app.el")))
      (setf (map-elt code-review-viewed--pending key) 'x)
      (unwind-protect
          (should-error (code-review-api-set-viewed "owner/repo" 7 "app.el" nil t)
                        :type 'user-error)
        (remhash key code-review-viewed--pending)))))

(provide 'code-review-api-viewed-test)
;;; code-review-api-viewed-test.el ends here
