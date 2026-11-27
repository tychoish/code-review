;;; code-review-api-test.el --- ERT tests for the public comment API -*- lexical-binding: t; -*-

(require 'ert)
(require 'code-review)
(require 'code-review-api)
(require 'code-review-test-helpers)

(defun code-review-api-test--open-pr ()
  "Create a saved pullreq record for owner/repo#7 in the temp db."
  (code-review-db--pullreq-create
   (code-review-github-repo :owner "owner" :repo "repo" :number "7"))
  (let ((pr (code-review-db-get-pullreq)))
    (oset pr saved t)
    (code-review-db-update pr)))

(defconst code-review-api-test--sent
  '((author (login . "bob")) (state . "COMMENTED")
    (comments (nodes ((databaseId . 123) (id . "PRRC_x") (bodyText . "sent") (bodyHTML . "sent") (body . "sent")
                      (path . "a.el") (line . 3) (side . "RIGHT")
                      (createdAt . "2026-01-01T00:00:00Z"))))))

(ert-deftest code-review-api-test/missing-pr ()
  (code-review-test--with-db
    (should-not (code-review-api-comments "owner/repo" "7"))
    (should-error (code-review-api-upsert-comment
                   "owner/repo" "7" '(:path "a" :body "b"))
                  :type 'user-error)
    (should-error (code-review-api-delete-comment "owner/repo" "7" 1)
                  :type 'user-error)))

(ert-deftest code-review-api-test/round-trip ()
  (code-review-test--with-db
    (code-review-api-test--open-pr)
    (let* ((c (code-review-api-upsert-comment
               "owner/repo" "7"
               '(:path "a.el" :line 5 :side "RIGHT" :body "héllo ✓ 日本" :author "me")))
           (id (plist-get c :id)))
      (should (>= id code-review-api--draft-id-base))
      (should (eq (plist-get c :status) 'draft))
      (should (equal (plist-get c :body) "héllo ✓ 日本"))
      (should (equal (plist-get c :author) "me"))
      (should (equal (code-review-api-comments "owner/repo" 7) (list c)))
      ;; update keeps id, position and untouched fields
      (let ((u (code-review-api-upsert-comment
                "owner/repo" "7" (list :id id :body "changed" :lines '(2 5)))))
        (should (equal (plist-get u :id) id))
        (should (equal (plist-get u :body) "changed"))
        (should (equal (plist-get u :lines) '(2 5)))
        (should (equal (plist-get u :path) "a.el"))
        (should (equal (plist-get u :author) "me"))
        (should (= 1 (length (code-review-api-comments "owner/repo" "7")))))
      ;; caller-chosen id
      (should (= (+ code-review-api--draft-id-base 42) (plist-get (code-review-api-upsert-comment
                                "owner/repo" "7" (list :id (+ code-review-api--draft-id-base 42) :path "b.el" :body "x"))
                               :id)))
      (should (code-review-api-delete-comment "owner/repo" "7" id))
      (should-not (code-review-api-delete-comment "owner/repo" "7" id))
      (should (equal (seq-map (lambda (c) (plist-get c :id))
                              (code-review-api-comments "owner/repo" "7"))
                     (list (+ code-review-api--draft-id-base 42)))))))

(ert-deftest code-review-api-test/sent-is-read-only ()
  (code-review-test--with-db
    (code-review-api-test--open-pr)
    (code-review-db--pullreq-raw-comments-update code-review-api-test--sent)
    (let ((c (car (code-review-api-comments "owner/repo" "7"))))
      (should (= 123 (plist-get c :id)))
      (should (eq 'sent (plist-get c :status)))
      (should (equal "PRRC_x" (plist-get c :remote-id)))
      (should-error (code-review-api-upsert-comment
                     "owner/repo" "7" (list :id 123 :body "no"))
                    :type 'user-error)
      (should-error (code-review-api-delete-comment "owner/repo" "7" 123)
                    :type 'user-error)
      (should (equal c (car (code-review-api-comments "owner/repo" "7")))))))

(ert-deftest code-review-api-test/validation ()
  (code-review-test--with-db
    (code-review-api-test--open-pr)
    (should-error (code-review-api-upsert-comment "owner/repo" "7" '(:body "x"))
                  :type 'user-error)
    (should-error (code-review-api-upsert-comment "bad" "7" '(:path "a" :body "x"))
                  :type 'user-error)
    (should-not (code-review-api-comments "owner/repo" "7"))))

(ert-deftest code-review-api-test/ui-draft-has-stable-id ()
  "A UI-made draft (no api-id) gets a stable derived id and is deletable."
  (code-review-test--with-db
    (code-review-api-test--open-pr)
    (code-review-db--pullreq-raw-comments-update
     '((author (login . "me")) (state . "LOCAL COMMENT")
       (comments (nodes ((internal-id . "uuid-1") (bodyText . "ui") (path . "u.el")
                         (line . 1) (side . "RIGHT") (local? . t))))))
    (let ((a (car (code-review-api-comments "owner/repo" "7")))
          (b (car (code-review-api-comments "owner/repo" "7"))))
      (should (eq 'draft (plist-get a :status)))
      (should (equal (plist-get a :id) (plist-get b :id)))
      (should (code-review-api-delete-comment "owner/repo" "7" (plist-get a :id)))
      (should-not (code-review-api-comments "owner/repo" "7")))))

(ert-deftest code-review-api-test/dry-run-rolls-back ()
  (code-review-test--with-db
    (code-review-api-test--open-pr)
    (let ((c (code-review-api-upsert-comment
              "owner/repo" "7" '(:path "a.el" :line 1 :body "dry" :dry-run nil))))
      (let ((d (code-review-api-upsert-comment
                "owner/repo" "7" '(:path "z.el" :line 1 :body "ghost") :dry-run t))
            (u (code-review-api-upsert-comment
                "owner/repo" "7" (list :id (plist-get c :id) :body "edited")
                :dry-run t)))
        (should (equal (plist-get d :body) "ghost"))
        (should (equal (plist-get u :body) "edited")))
      (should (equal (code-review-api-comments "owner/repo" "7") (list c)))
      (should (code-review-api-delete-comment
               "owner/repo" "7" (plist-get c :id) :dry-run t))
      (should (equal (code-review-api-comments "owner/repo" "7") (list c))))))

(ert-deftest code-review-api-test/renders-in-section-code ()
  "API-created drafts group in the existing section renderer."
  (code-review-test--with-db
    (code-review-api-test--open-pr)
    (code-review-api-upsert-comment
     "owner/repo" "7" '(:path "a.el" :line 5 :side "RIGHT" :body "render me" :author "me"))
    (let* ((group (code-review-utils-make-group (code-review-db--pullreq-raw-comments)))
           (obj (car (a-get group (car (a-keys group))))))
      (should (= 1 (length (a-keys group))))
      (should (code-review-local-comment-section-p obj))
      (should (equal "render me" (oref obj msg))))))

(ert-deftest code-review-api-test/id-validation-and-idempotence ()
  (code-review-test--with-db
    (code-review-api-test--open-pr)
    (dolist (bad (list 5 (1- code-review-api--draft-id-base)
                       code-review-api--id-limit "x"))
      (should-error (code-review-api-upsert-comment
                     "owner/repo" "7" (list :id bad :path "a" :body "b"))
                    :type 'user-error))
    (let ((id (+ code-review-api--draft-id-base 9)))
      (dotimes (i 3)
        (code-review-api-upsert-comment
         "owner/repo" "7" (list :id id :path "a.el" :line 1 :body (format "v%d" i))))
      (let ((all (code-review-api-comments "owner/repo" "7")))
        (should (= 1 (length all)))
        (should (equal "v2" (plist-get (car all) :body)))))))

(ert-deftest code-review-api-test/lines-collapse ()
  (code-review-test--with-db
    (code-review-api-test--open-pr)
    (let* ((id (+ code-review-api--draft-id-base 1))
           (c (code-review-api-upsert-comment
               "owner/repo" "7" (list :id id :path "a.el" :lines '(2 6) :body "r"))))
      (should (equal '(2 6) (plist-get c :lines)))
      (let ((u (code-review-api-upsert-comment
                "owner/repo" "7" (list :id id :lines nil))))
        (should-not (plist-get u :lines))
        (should (= 6 (plist-get u :line))))
      (let ((u (code-review-api-upsert-comment
                "owner/repo" "7" (list :id id :lines '(1 3)))))
        (should (equal '(1 3) (plist-get u :lines))))
      ;; moving :line below the stored start never leaves start > line
      (let ((u (code-review-api-upsert-comment
                "owner/repo" "7" (list :id id :line 0))))
        (should-not (plist-get u :lines)))
      (should-error (code-review-api-upsert-comment
                     "owner/repo" "7" (list :id id :lines '(5 2)))
                    :type 'user-error))))

(ert-deftest code-review-api-test/unsaved-pr-lookup ()
  (code-review-test--with-db
    (code-review-db--pullreq-create
     (code-review-github-repo :owner "owner" :repo "repo" :number "7"))
    (should-not (oref (code-review-db-get-pullreq) saved))
    (code-review-api-upsert-comment "owner/repo" "7" '(:path "a.el" :body "x"))
    (should (= 1 (length (code-review-api-comments "owner/repo" 7))))))

(ert-deftest code-review-api-test/survives-refetch ()
  (code-review-test--with-db
    (code-review-api-test--open-pr)
    (let ((c (code-review-api-upsert-comment
              "owner/repo" "7" '(:path "a.el" :line 1 :body "keep"))))
      (code-review-db--pullreq-raw-infos-update
       '((state . "OPEN") (reviews (nodes))))
      (should (equal (list c) (code-review-api-comments "owner/repo" "7"))))))

(ert-deftest code-review-api-test/reply-drafts ()
  (code-review-test--with-db
    (code-review-api-test--open-pr)
    (code-review-db--pullreq-raw-comments-update code-review-api-test--sent)
    (should-error (code-review-api-upsert-comment
                   "owner/repo" "7" '(:reply-to 999 :body "x"))
                  :type 'user-error)
    (let* ((id (+ code-review-api--draft-id-base 5))
           (r (code-review-api-upsert-comment
               "owner/repo" "7" (list :id id :reply-to 123 :body "re"))))
      (should (eq 'draft (plist-get r :status)))
      (should (= 123 (plist-get r :reply-to)))
      (should (equal "a.el" (plist-get r :path)))
      (should-error (code-review-api-upsert-comment
                     "owner/repo" "7" (list :id id :reply-to 1 :body "re"))
                    :type 'user-error)
      (should (equal "re2" (plist-get (code-review-api-upsert-comment
                                       "owner/repo" "7" (list :id id :body "re2"))
                                      :body)))
      ;; the section renderer sees it as a reply
      (let* ((group (code-review-utils-make-group (code-review-db--pullreq-raw-comments)))
             (objs (apply #'append (a-vals group))))
        (should (seq-some #'code-review-reply-comment-section-p objs)))
      (should (code-review-api-delete-comment "owner/repo" "7" id))
      (should (= 1 (length (code-review-api-comments "owner/repo" "7")))))))

(ert-deftest code-review-api-test/pending-review-comment ()
  (code-review-test--with-db
    (code-review-api-test--open-pr)
    (code-review-db--pullreq-raw-comments-update
     (a-assoc code-review-api-test--sent 'state "PENDING"))
    (let ((c (car (code-review-api-comments "owner/repo" "7"))))
      (should (eq 'pending (plist-get c :status)))
      (should-error (code-review-api-upsert-comment
                     "owner/repo" "7" (list :id 123 :body "n"))
                    :type 'user-error))))

(ert-deftest code-review-api-test/resolved-read ()
  (code-review-test--with-db
    (code-review-api-test--open-pr)
    (code-review-db--pullreq-raw-comments-update
     (a-assoc-in code-review-api-test--sent '(comments nodes)
                 (list (a-assoc (car (a-get-in code-review-api-test--sent '(comments nodes)))
                                'isResolved t))))
    (should (plist-get (car (code-review-api-comments "owner/repo" "7")) :resolved))))

;;; Verdict

(ert-deftest code-review-api-test/verdict-round-trip ()
  (code-review-test--with-db
    (code-review-api-test--open-pr)
    (should-not (code-review-api-verdict "owner/repo" "7"))
    (should-not (code-review-api-verdict-stale-p "owner/repo" "7" "aaa"))
    (let ((v (code-review-api-set-verdict "owner/repo" "7" 'changes "aaa")))
      (should (eq (plist-get v :state) 'changes))
      (should (equal (plist-get v :head-oid) "aaa"))
      (should (plist-get v :at))
      (should (equal v (code-review-api-verdict "owner/repo" 7))))
    (should-not (code-review-api-verdict-stale-p "owner/repo" "7" "aaa"))
    (should (code-review-api-verdict-stale-p "owner/repo" "7" "bbb"))
    ;; strings accepted; head oid optional (never stale)
    (should (eq 'approved (plist-get (code-review-api-set-verdict "owner/repo" "7" "approved") :state)))
    (should-not (code-review-api-verdict-stale-p "owner/repo" "7" "bbb"))
    ;; clear
    (should-not (code-review-api-set-verdict "owner/repo" "7" nil))
    (should-not (code-review-api-verdict "owner/repo" "7"))))

(ert-deftest code-review-api-test/verdict-errors ()
  (code-review-test--with-db
    (should-not (code-review-api-verdict "owner/repo" "7"))
    (should-error (code-review-api-set-verdict "owner/repo" "7" 'approved)
                  :type 'user-error)
    (code-review-api-test--open-pr)
    (should-error (code-review-api-set-verdict "owner/repo" "7" 'bogus)
                  :type 'user-error)
    (should-error (code-review-api-set-verdict "owner/repo" "7" 'approved 5)
                  :type 'user-error)
    (should-not (code-review-api-verdict "owner/repo" "7"))))

(ert-deftest code-review-api-test/verdict-dry-run ()
  (code-review-test--with-db
    (code-review-api-test--open-pr)
    (let ((v (code-review-api-set-verdict "owner/repo" "7" 'comment "h" :dry-run t)))
      (should (eq (plist-get v :state) 'comment)))
    (should-not (code-review-api-verdict "owner/repo" "7"))))

(ert-deftest code-review-api-test/verdict-unsaved-pr-and-reopen ()
  "The unsaved-PR fallback finds the row; reopening the same PR keeps it."
  (code-review-test--with-db
    (code-review-db--pullreq-create
     (code-review-github-repo :owner "owner" :repo "repo" :number "7"))
    (code-review-api-set-verdict "owner/repo" "7" 'approved "h1")
    (code-review-db--pullreq-create
     (code-review-github-repo :owner "owner" :repo "repo" :number "7"))
    (should (eq 'approved (plist-get (code-review-api-verdict "owner/repo" "7") :state)))))

(ert-deftest code-review-api-test/verdict-persists-in-existing-db-file ()
  "No schema change: a verdict survives reconnecting to the db file,
and a pre-existing v11 file (review slot nil) reads as no verdict."
  (let ((file (code-review-test--fresh-db-file)))
    (unwind-protect
        (progn
          (code-review-test--reset-db file)
          (code-review-api-test--open-pr)
          (should-not (code-review-api-verdict "owner/repo" "7"))
          (code-review-api-set-verdict "owner/repo" "7" 'approved "h")
          (code-review-test--reset-db file)
          (should (= 11 (closql--db-get-version (code-review-db))))
          (should (equal "h" (plist-get (code-review-api-verdict "owner/repo" "7") :head-oid))))
      (code-review-test--reset-db nil)
      (ignore-errors
        (delete-file file)
        (delete-file (concat file "-shm"))
        (delete-file (concat file "-wal"))))))

(ert-deftest code-review-api-test/verdict-shown-in-state-header ()
  (code-review-test--with-db
    (code-review-api-test--open-pr)
    (let ((pr (code-review-db-get-pullreq)))
      (oset pr state "OPEN")
      (code-review-db-update pr))
    (code-review-api-set-verdict "owner/repo" "7" 'changes "h")
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (root)
          (code-review-section-insert-state)))
      (should (string-match-p "pending verdict: changes" (buffer-string))))
    (code-review-api-set-verdict "owner/repo" "7" nil)
    (with-temp-buffer
      (magit-section-mode)
      (let ((inhibit-read-only t))
        (magit-insert-section (root)
          (code-review-section-insert-state)))
      (should-not (string-match-p "verdict" (buffer-string))))))

;;; Pullreq metadata

(ert-deftest code-review-api-test/pullreq-missing ()
  (code-review-test--with-db
    (should-not (code-review-api-pullreq "owner/repo" 7))
    (should-error (code-review-api-pullreq "bad" 7) :type 'user-error)))

(ert-deftest code-review-api-test/pullreq-metadata ()
  (code-review-test--with-db
    (let ((code-review-gh--pr-accounts (make-hash-table :test #'equal))
          (code-review-gh-default-account nil))
      (code-review-db--pullreq-create
       (code-review-github-repo :owner "owner" :repo "repo" :number "7"))
      (let ((pr (code-review-db-get-pullreq))
            (meta (code-review-api-pullreq "owner/repo" 7)))
        (should (equal (plist-get meta :id) (oref pr id)))
        (should-not (plist-get meta :account))
        (should-not (plist-get meta :head-oid))
        (should-not (plist-get meta :url))
        (should-not (plist-get meta :state)))
      (code-review-db--pullreq-create
       (code-review-github-repo
        :owner "owner" :repo "repo" :number "7"
        :url "https://github.com/owner/repo/pull/7"))
      (let ((pr (code-review-db-get-pullreq)))
        (oset pr saved t)
        (code-review-db-update pr)
        (code-review-db--pullreq-raw-infos-update
         '((headRefOid . "deadbeef") (state . "OPEN") (reviews (nodes))))
        (code-review-gh-register-pr-account pr "alice")
        (let ((meta (code-review-api-pullreq "owner/repo" 7)))
          (should (equal (plist-get meta :id) (oref pr id)))
          (should (equal (plist-get meta :account) "alice"))
          (should (equal (plist-get meta :head-oid) "deadbeef"))
          (should (equal (plist-get meta :url) "https://github.com/owner/repo/pull/7"))
          (should (equal (plist-get meta :state) "OPEN"))
          (should (equal meta (code-review-api-pullreq "owner/repo" "7"))))
        ;; falls back to default account when PR has no registered account
        (let ((code-review-gh--pr-accounts (make-hash-table :test #'equal))
              (code-review-gh-default-account "dflt"))
          (should (equal (plist-get (code-review-api-pullreq "owner/repo" 7) :account)
                         "dflt")))
        ;; falls back to raw-infos headRefOid when sha slot is nil
        (let ((fresh (code-review-db-get-pullreq)))
          (oset fresh sha nil)
          (code-review-db-update fresh))
        (should (equal (plist-get (code-review-api-pullreq "owner/repo" 7) :head-oid)
                       "deadbeef"))))))

(provide 'code-review-api-test)
;;; code-review-api-test.el ends here
