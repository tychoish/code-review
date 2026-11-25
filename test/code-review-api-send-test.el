;;; code-review-api-send-test.el --- ERT tests for the send API -*- lexical-binding: t; -*-

;; ghub is stubbed: no network, no `gh', no db.

(require 'ert)
(require 'cl-lib)
(require 'code-review-api-send)

(defvar code-review-api-send-test--calls nil
  "Requests seen by the stubs, newest first: (KIND URL-OR-QUERY PAYLOAD).")

(defvar code-review-api-send-test--head "aaa111"
  "Head sha the stubbed PR endpoint reports.")

(defvar code-review-api-send-test--responder nil
  "Function (KIND TARGET PAYLOAD) returning a value, :hang or (:fail STATUS DATA HEADERS).")

(defvar code-review-api-send-test--get-responder nil
  "Like the responder, for GET requests other than the default head lookup.")

(defvar code-review-api-send-test--stored-errorback nil
  "Errorback of the last request that was left hanging.")

(defun code-review-api-send-test--respond (kind target payload callback errorback)
  "Answer the request through the responder, calling CALLBACK or ERRORBACK."
  (push (list kind target payload) code-review-api-send-test--calls)
  (let ((res (cond
              ((and (eq kind 'ghub-get) code-review-api-send-test--get-responder)
               (funcall code-review-api-send-test--get-responder kind target payload))
              ((eq kind 'ghub-get)
               (if (string-match-p "/pulls/[0-9]+\\'" target)
                   `((head . ((sha . ,code-review-api-send-test--head))))
                 nil))
              (code-review-api-send-test--responder
               (funcall code-review-api-send-test--responder kind target payload))
              (t '((id . 99))))))
    (cond
     ((eq res :hang)
      (setq code-review-api-send-test--stored-errorback errorback))
     ((eq (car-safe res) :fail)
      (funcall errorback (list 'error 'http (nth 1 res) (nth 2 res)) (nth 3 res) nil nil))
     (t (funcall callback res)))))

(defmacro code-review-api-send-test--with-stubs (&rest body)
  "Run BODY with the ghub entry points, auth and sleeping stubbed."
  (declare (indent 0))
  `(let ((code-review-api-send-test--calls nil)
         (code-review-api-send-test--responder nil)
         (code-review-api-send-test--get-responder nil)
         (code-review-api-send-test--stored-errorback nil)
         (code-review-api-send-test--head "aaa111")
         (code-review-auth-backend (lambda (_host _account) "tok"))
         (code-review-api-send-sleep-function #'ignore)
         (code-review-api-send-timeout 5)
         (orig (seq-map (lambda (s) (cons s (symbol-function s)))
                        '(ghub-get ghub-post ghub-patch ghub-put ghub-query))))
     (unwind-protect
         (progn
           (seq-do (lambda (s)
                     (fset s (lambda (url _params &rest args)
                               (code-review-api-send-test--respond
                                s url (plist-get args :payload)
                                (plist-get args :callback) (plist-get args :errorback)))))
                   '(ghub-get ghub-post ghub-patch ghub-put))
           (fset 'ghub-query
                 (lambda (query vars &rest args)
                   (code-review-api-send-test--respond
                    'ghub-query query vars
                    (plist-get args :callback) (plist-get args :errorback))))
           ,@body)
       (seq-do (lambda (o)
                 (fset (car o) (cdr o)))
               orig))))

(defun code-review-api-send-test--kinds ()
  "Return the request kinds seen, oldest first."
  (seq-map #'car (reverse code-review-api-send-test--calls)))

(defun code-review-api-send-test--requests-to (fragment kinds)
  "Return the requests of KINDS whose target matches FRAGMENT, oldest first."
  (seq-filter (lambda (c)
                (and (memq (car c) kinds) (string-match-p fragment (cadr c))))
              (reverse code-review-api-send-test--calls)))

(defun code-review-api-send-test--posts-to (fragment)
  "Return the POST requests whose target matches FRAGMENT."
  (code-review-api-send-test--requests-to fragment '(ghub-post)))

(ert-deftest code-review-api-send-test/approve ()
  (code-review-api-send-test--with-stubs
    (let ((res (code-review-api-submit
                "o/r" 7 :event "APPROVE" :body "lgtm" :head-oid "aaa111")))
      (should (equal (plist-get res :sent) '(review)))
      (should-not (plist-get res :failed))
      (should (equal (plist-get res :event) "APPROVE"))
      (let ((payload (nth 2 (car (code-review-api-send-test--posts-to "/pulls/7/reviews")))))
        (should (equal (alist-get 'event payload) "APPROVE"))
        (should (equal (alist-get 'commit_id payload) "aaa111"))
        (should (equal (alist-get 'body payload) "lgtm"))))))

(ert-deftest code-review-api-send-test/request-changes-with-comments ()
  (code-review-api-send-test--with-stubs
    (let ((res (code-review-api-submit
                "o/r" 7 :event "REQUEST_CHANGES" :head-oid "aaa111" :body "see comments"
                :comments '((:id 1 :path "a.el" :body "fix" :line 3 :side "RIGHT")
                            (:id 2 :path "b.el" :body "and this" :line 9 :side "RIGHT")))))
      (should (equal (plist-get res :sent) '(review 1 2)))
      (should (= 1 (length (code-review-api-send-test--posts-to "/reviews"))))
      (let ((payload (nth 2 (car (code-review-api-send-test--posts-to "/reviews")))))
        (should (equal (alist-get 'event payload) "REQUEST_CHANGES"))
        (should (= 2 (length (alist-get 'comments payload))))))))

(ert-deftest code-review-api-send-test/comment-only-event ()
  "COMMENT needs no head check and works with a stale head."
  (code-review-api-send-test--with-stubs
    (let ((res (code-review-api-submit
                "o/r" 7 :event "COMMENT" :head-oid "old000"
                :comments '((:id 1 :path "a.el" :body "hm" :line 3 :side "RIGHT")))))
      (should (equal (plist-get res :sent) '(review 1)))
      (should (equal (plist-get res :head) "old000"))
      (should-not (code-review-api-send-test--requests-to "/pulls/7\\'" '(ghub-get))))))

(ert-deftest code-review-api-send-test/comments-without-event-post-individually ()
  (code-review-api-send-test--with-stubs
    (setq code-review-api-send-test--responder
          (lambda (_k target _p)
            (if (string-match-p "/pulls/7/comments" target)
                '((id . 501))
              '((id . 1)))))
    (let ((res (code-review-api-submit
                "o/r" 7 :head-oid "aaa111"
                :comments '((:id "c1" :path "a.el" :body "one" :line 3 :side "RIGHT")
                            (:id "c2" :path "b.el" :body "two" :line 4 :side "RIGHT")))))
      (should (equal (plist-get res :sent) '("c1" "c2")))
      (should (equal (plist-get res :remote-ids) '(("c1" . 501) ("c2" . 501))))
      (should (= 2 (length (code-review-api-send-test--posts-to "/pulls/7/comments"))))
      (should-not (code-review-api-send-test--posts-to "/reviews")))))

(ert-deftest code-review-api-send-test/stale-head-refused-before-anything-is-sent ()
  (code-review-api-send-test--with-stubs
    (seq-do (lambda (event)
              (setq code-review-api-send-test--calls nil)
              (should-error (code-review-api-submit
                             "o/r" 7 :event event :head-oid "stale00" :body "b"
                             :replies '((:id 1 :reply-to-id 5 :body "x")))
                            :type 'user-error)
              (should (equal (code-review-api-send-test--kinds) '(ghub-get))))
            '("APPROVE" "REQUEST_CHANGES"))))

(ert-deftest code-review-api-send-test/approve-needs-head-oid ()
  (code-review-api-send-test--with-stubs
    (should-error (code-review-api-submit "o/r" 7 :event "APPROVE") :type 'user-error)
    (should-not code-review-api-send-test--calls)))

(ert-deftest code-review-api-send-test/partial-failure-does-not-abort ()
  (code-review-api-send-test--with-stubs
    (setq code-review-api-send-test--responder
          (lambda (_k target _p)
            (cond
             ((string-match-p "/comments/11/replies" target)
              (list :fail 422 '((message . "Validation Failed") (errors . ("bad")))))
             (t '((id . 7))))))
    (let ((res (code-review-api-submit
                "o/r" 7 :head-oid "aaa111"
                :replies '((:id "r1" :reply-to-id 11 :body "nope")
                           (:id "r2" :reply-to-id 12 :body "fine"))
                :resolve '((:id "t1" :thread-id "PRT_1" :resolved t))
                :edits '((:id "e1" :kind "issue-comment" :comment-id 4 :body "edited")))))
      (should (equal (plist-get res :sent) '("r2" "t1" "e1")))
      (should (= 1 (length (plist-get res :failed))))
      (let ((failed (car (plist-get res :failed))))
        (should (equal (plist-get failed :id) "r1"))
        (should (eq (plist-get failed :kind) 'reply))
        (should (= (plist-get failed :status) 422))
        (should (string-match-p "Validation Failed" (plist-get failed :error)))
        (should-not (plist-get failed :retryable))))))

(ert-deftest code-review-api-send-test/failed-review-fails-its-comments-not-the-rest ()
  (code-review-api-send-test--with-stubs
    (setq code-review-api-send-test--responder
          (lambda (_k target _p)
            (if (string-match-p "/reviews" target)
                (list :fail 422 '((message . "Unprocessable")))
              '((id . 1)))))
    (let ((res (code-review-api-submit
                "o/r" 7 :event "COMMENT" :body "s" :head-oid "aaa111"
                :comments '((:id 1 :path "a.el" :body "x" :line 3 :side "RIGHT"))
                :resolve '((:id "t1" :thread-id "PRT_1" :resolved nil)))))
      (should (equal (plist-get res :sent) '("t1")))
      (should (equal (seq-map (lambda (f) (plist-get f :id)) (plist-get res :failed))
                     '(1 review))))))

(ert-deftest code-review-api-send-test/already-sent-items-are-skipped ()
  (code-review-api-send-test--with-stubs
    (let ((res (code-review-api-submit
                "o/r" 7 :head-oid "aaa111"
                :comments '((:id 1 :path "a.el" :body "x" :line 3 :remote-id "RC_1"))
                :replies '((:id "r1" :reply-to-id 11 :body "x" :remote-id "RC_2"))
                :resolve '((:id "t1" :thread-id "PRT_1" :resolved t :remote-resolved t)))))
      (should-not (plist-get res :sent))
      (should-not (plist-get res :failed))
      (should (equal (seq-map (lambda (s) (plist-get s :id)) (plist-get res :skipped))
                     '(1 "r1" "t1")))
      (should-not code-review-api-send-test--calls))))

(ert-deftest code-review-api-send-test/already-sent-comment-in-review-is-not-resent ()
  (code-review-api-send-test--with-stubs
    (let ((res (code-review-api-submit
                "o/r" 7 :event "COMMENT" :head-oid "aaa111"
                :comments '((:id 1 :path "a.el" :body "old" :line 3 :remote-id "RC_1")
                            (:id 2 :path "a.el" :body "new" :line 4)))))
      (should (equal (plist-get res :sent) '(review 2)))
      (should (equal (seq-map (lambda (s) (plist-get s :id)) (plist-get res :skipped)) '(1)))
      (let ((payload (nth 2 (car (code-review-api-send-test--posts-to "/reviews")))))
        (should (= 1 (length (alist-get 'comments payload))))))))

(ert-deftest code-review-api-send-test/invalid-item-fails-alone ()
  (code-review-api-send-test--with-stubs
    (let ((res (code-review-api-submit
                "o/r" 7 :head-oid "aaa111"
                :comments '((:id 1 :path "" :body "x" :line 3)
                            (:id 2 :path "a.el" :body "ok" :line 4 :side "RIGHT")))))
      (should (equal (plist-get res :sent) '(2)))
      (should (equal (seq-map (lambda (f) (plist-get f :id)) (plist-get res :failed)) '(1))))))

(ert-deftest code-review-api-send-test/rate-limit-is-waited-out-and-retried ()
  (code-review-api-send-test--with-stubs
    (let ((slept nil)
          (hits 0))
      (let ((code-review-api-send-sleep-function (lambda (s) (push s slept))))
        (setq code-review-api-send-test--responder
              (lambda (_k _t _p)
                (setq hits (1+ hits))
                (if (= hits 1)
                    (list :fail 403 '((message . "API rate limit exceeded"))
                          '(("Retry-After" . "3")))
                  '((id . 1)))))
        (let ((res (code-review-api-submit
                    "o/r" 7 :head-oid "aaa111"
                    :replies '((:id "r1" :reply-to-id 11 :body "x")))))
          (should (equal (plist-get res :sent) '("r1")))
          (should (equal slept '(3)))
          (should (= hits 2)))))))

(ert-deftest code-review-api-send-test/long-rate-limit-fails-without-waiting ()
  (code-review-api-send-test--with-stubs
    (setq code-review-api-send-test--responder
          (lambda (_k _t _p)
            (list :fail 429 nil '(("retry-after" . "9999")))))
    (let ((res (code-review-api-submit
                "o/r" 7 :head-oid "aaa111"
                :replies '((:id "r1" :reply-to-id 11 :body "x")))))
      (should (= 1 (length code-review-api-send-test--calls)))
      (should (plist-get (car (plist-get res :failed)) :retryable)))))

(ert-deftest code-review-api-send-test/5xx-create-not-retried-5xx-idempotent-is ()
  (code-review-api-send-test--with-stubs
    (let ((hits 0))
      (setq code-review-api-send-test--responder
            (lambda (_k target _p)
              (setq hits (1+ hits))
              (cond
               ((string-match-p "/replies" target) (list :fail 502 nil))
               ((= hits 2) (list :fail 503 nil))
               (t '((id . 1))))))
      (let* ((res (code-review-api-submit
                   "o/r" 7 :head-oid "aaa111"
                   :replies '((:id "r1" :reply-to-id 11 :body "x"))
                   :resolve '((:id "t1" :thread-id "PRT_1" :resolved t))))
             (failed (car (plist-get res :failed))))
        (should (equal (plist-get res :sent) '("t1")))
        (should (equal (plist-get failed :id) "r1"))
        (should (plist-get failed :maybe-accepted))
        (should-not (plist-get failed :retryable))
        (should (= 3 hits))))))

(ert-deftest code-review-api-send-test/text-is-sent-verbatim-never-read-from-files ()
  (code-review-api-send-test--with-stubs
    (code-review-api-submit
     "o/r" 7 :event "COMMENT" :head-oid "aaa111" :body "@/etc/passwd"
     :comments '((:id 1 :path "a.el" :body "@/etc/hostname" :line 1 :side "RIGHT")))
    (let ((payload (nth 2 (car (code-review-api-send-test--posts-to "/reviews")))))
      (should (equal (alist-get 'body payload) "@/etc/passwd"))
      (should (equal (alist-get 'body (aref (alist-get 'comments payload) 0))
                     "@/etc/hostname")))))

(ert-deftest code-review-api-send-test/bad-arguments-are-user-errors ()
  (code-review-api-send-test--with-stubs
    (should-error (code-review-api-submit "norepo" 7) :type 'user-error)
    (should-error (code-review-api-submit "o/r" "7") :type 'user-error)
    (should-error (code-review-api-submit "o/r" 7 :event "MERGE") :type 'user-error)))

(ert-deftest code-review-api-send-test/parse-error-shapes ()
  (let ((info (code-review-api-send--parse-error
               (list '(error http 403 ((message . "Secondary rate limit"))) nil))))
    (should (eq (plist-get info :kind) 'rate-limit)))
  (should (eq (plist-get (code-review-api-send--parse-error
                          (list '(error http 500 nil) nil))
                         :kind)
              'server))
  (should (eq (plist-get (code-review-api-send--parse-error
                          (list '(errors ((message . "boom")))))
                         :kind)
              'other))
  (should (eq (plist-get (code-review-api-send--parse-error
                          (list '(errors ((message . "API rate limit hit")))))
                         :kind)
              'rate-limit)))

(ert-deftest code-review-api-send-test/review-comment-remote-ids-are-fetched ()
  (code-review-api-send-test--with-stubs
    (setq code-review-api-send-test--responder
          (lambda (_k _t _p) '((id . 900))))
    (setq code-review-api-send-test--get-responder
          (lambda (_k target _p)
            (if (string-match-p "/reviews/900/comments" target)
                '(((id . 71) (path . "b.el") (line . 4) (body . "two"))
                  ((id . 70) (path . "a.el") (line . 3) (body . "one")))
              `((head . ((sha . ,code-review-api-send-test--head)))))))
    (let ((res (code-review-api-submit
                "o/r" 7 :event "COMMENT" :head-oid "aaa111"
                :comments '((:id "c1" :path "a.el" :body "one" :line 3)
                            (:id "c2" :path "b.el" :body "two" :line 4)))))
      (should (equal (plist-get res :remote-ids)
                     '((review . 900) ("c1" . 70) ("c2" . 71))))
      (should-not (plist-get res :warnings)))))

(ert-deftest code-review-api-send-test/review-comment-fetch-failure-warns ()
  (code-review-api-send-test--with-stubs
    (setq code-review-api-send-test--get-responder
          (lambda (_k _t _p) (list :fail 404 nil)))
    (let ((res (code-review-api-submit
                "o/r" 7 :event "COMMENT" :head-oid "aaa111"
                :comments '((:id "c1" :path "a.el" :body "one" :line 3)))))
      (should (equal (plist-get res :sent) '(review "c1")))
      (should (= 1 (length (plist-get res :warnings)))))))

(ert-deftest code-review-api-send-test/reply-remote-ids ()
  (code-review-api-send-test--with-stubs
    (setq code-review-api-send-test--responder
          (lambda (_k target _p)
            (if (string-match-p "/comments/11/replies" target) '((id . 801)) '((id . 802)))))
    (let ((res (code-review-api-submit
                "o/r" 7 :head-oid "aaa111"
                :replies '((:id "r1" :reply-to-id 11 :body "x")
                           (:id "r2" :reply-to-id 12 :body "y")))))
      (should (equal (plist-get res :remote-ids) '(("r1" . 801) ("r2" . 802)))))))

(ert-deftest code-review-api-send-test/on-item-fires-per-item-as-known ()
  (code-review-api-send-test--with-stubs
    (let ((seen nil))
      (setq code-review-api-send-test--responder
            (lambda (_k target _p)
              (if (string-match-p "/comments/11/replies" target)
                  (list :fail 422 nil)
                '((id . 5)))))
      (let ((res (code-review-api-submit
                  "o/r" 7 :head-oid "aaa111"
                  :on-item (lambda (id outcome remote detail)
                             (push (list id outcome remote (plist-get detail :reason)) seen))
                  :replies '((:id "r1" :reply-to-id 11 :body "x")
                             (:id "r2" :reply-to-id 12 :body "y" :remote-id 9)
                             (:id "r3" :reply-to-id 13 :body "z")))))
        (should (equal (nreverse seen)
                       '(("r1" failed nil nil)
                         ("r2" skipped nil already-sent)
                         ("r3" sent 5 nil))))
        (should (equal (plist-get res :sent) '("r3")))))))

(ert-deftest code-review-api-send-test/on-item-errors-do-not-abort ()
  (code-review-api-send-test--with-stubs
    (let ((res (code-review-api-submit
                "o/r" 7 :head-oid "aaa111"
                :on-item (lambda (&rest _) (error "checkpoint disk full"))
                :replies '((:id "r1" :reply-to-id 11 :body "x")
                           (:id "r2" :reply-to-id 12 :body "y")))))
      (should (equal (plist-get res :sent) '("r1" "r2"))))))

(ert-deftest code-review-api-send-test/timeout-on-create-is-maybe-accepted-not-retryable ()
  (code-review-api-send-test--with-stubs
    (let ((code-review-api-send-timeout 0.2))
      (setq code-review-api-send-test--responder (lambda (_k _t _p) :hang))
      (let* ((res (code-review-api-submit
                   "o/r" 7 :head-oid "aaa111"
                   :replies '((:id "r1" :reply-to-id 11 :body "x"))))
             (failed (car (plist-get res :failed))))
        (should (plist-get failed :maybe-accepted))
        (should-not (plist-get failed :retryable))
        (should (equal (plist-get failed :error) "timed out"))))))

(ert-deftest code-review-api-send-test/late-errorback-of-timed-out-request-is-ignored ()
  (code-review-api-send-test--with-stubs
    (let ((code-review-api-send-timeout 0.2))
      (setq code-review-api-send-test--responder
            (lambda (_k target _p)
              (cond
               ((string-match-p "/comments/11/replies" target) :hang)
               (t
                ;; the timed-out request answers late, during the next one
                (funcall code-review-api-send-test--stored-errorback
                         (list 'error 'http 500 nil) nil nil
                         (ghub--make-req
                          :url (url-generic-parse-url
                                "https://api.github.com/repos/o/r/pulls/7/comments/11/replies")))
                '((id . 3))))))
      (let ((res (code-review-api-submit
                  "o/r" 7 :head-oid "aaa111"
                  :replies '((:id "r1" :reply-to-id 11 :body "x")
                             (:id "r2" :reply-to-id 12 :body "y")))))
        (should (equal (plist-get res :sent) '("r2")))
        (should (equal (seq-map (lambda (f) (plist-get f :id)) (plist-get res :failed))
                       '("r1")))))))

(ert-deftest code-review-api-send-test/errors-outside-a-request-are-ignored ()
  (let ((code-review-api-send--error-box nil))
    (code-review-api-send--collect-error (list '(error http 500 nil) nil nil nil))
    (should-not code-review-api-send--error-box)))

(ert-deftest code-review-api-send-test/head-lookup-failure-nonstrict-warns ()
  (code-review-api-send-test--with-stubs
    (setq code-review-api-send-test--get-responder
          (lambda (_k _t _p) (list :fail 500 nil)))
    (let ((res (code-review-api-submit
                "o/r" 7 :event "COMMENT"
                :comments '((:id 1 :path "a.el" :body "x" :line 3)))))
      (should-not (plist-get res :head))
      (should (string-match-p "head lookup failed" (car (plist-get res :warnings))))
      (should (equal (plist-get res :sent) '(review 1))))
    ;; the lookup was retried as an idempotent read: 1 + 2 retries
    (should (= 3 (length (code-review-api-send-test--requests-to "/pulls/7\\'" '(ghub-get)))))))

(ert-deftest code-review-api-send-test/head-lookup-failure-strict-refuses ()
  (code-review-api-send-test--with-stubs
    (setq code-review-api-send-test--get-responder
          (lambda (_k _t _p) (list :fail 502 nil)))
    (should-error (code-review-api-submit "o/r" 7 :event "APPROVE" :head-oid "aaa111")
                  :type 'user-error)
    (should-not (code-review-api-send-test--posts-to "/reviews"))))

(ert-deftest code-review-api-send-test/validation ()
  (code-review-api-send-test--with-stubs
    (should-error (code-review-api-submit "o/r" 7 :event 'approve) :type 'user-error)
    (should-error (code-review-api-submit "o/r" 7 :event "REQUEST_CHANGES" :head-oid "aaa111")
                  :type 'user-error)
    (let ((res (code-review-api-submit
                "o/r" 7 :head-oid "aaa111"
                :comments '((:id 1 :path "a.el" :body "x" :line 3 :side "MIDDLE")
                            (:id 2 :path "a.el" :body "x" :line 3 :start-line 1)
                            (:id 3 :path "a.el" :body "x" :line 3)))))
      (should (equal (plist-get res :sent) '(3)))
      (should (equal (seq-map (lambda (f) (plist-get f :id)) (plist-get res :failed)) '(1 2)))
      (should (equal (alist-get 'side (nth 2 (car (code-review-api-send-test--posts-to "/pulls/7/comments"))))
                     "RIGHT")))))

(ert-deftest code-review-api-send-test/reconcile-lists-reviews-and-comments ()
  (code-review-api-send-test--with-stubs
    (setq code-review-api-send-test--get-responder
          (lambda (_k target _p)
            (if (string-match-p "/reviews\\'" target)
                '(((id . 1) (commit_id . "aaa") (body . "lgtm") (state . "APPROVED")
                   (user . ((login . "me")))))
              '(((id . 2) (commit_id . "aaa") (body . "c") (path . "a.el") (line . 3)
                 (user . ((login . "me"))))))))
    (let ((res (code-review-api-reconcile "o/r" 7 :account "me")))
      (should (equal (plist-get (car (plist-get res :reviews)) :state) "APPROVED"))
      (should (equal (plist-get (car (plist-get res :comments)) :path) "a.el"))
      (should-not (plist-get res :errors)))))

;; Node ids (GraphQL ids) alongside the REST ids.

(ert-deftest code-review-api-send-test/node-ids-review-and-comments ()
  (code-review-api-send-test--with-stubs
    (setq code-review-api-send-test--responder
          (lambda (_k _t _p) '((id . 900) (node_id . "PRR_900"))))
    (setq code-review-api-send-test--get-responder
          (lambda (_k target _p)
            (if (string-match-p "/reviews/900/comments" target)
                '(((id . 71) (node_id . "PRRC_71") (path . "b.el") (line . 4) (body . "two"))
                  ((id . 70) (node_id . "PRRC_70") (path . "a.el") (line . 3) (body . "one")))
              `((head . ((sha . ,code-review-api-send-test--head)))))))
    (let* ((seen nil)
           (res (code-review-api-submit
                 "o/r" 7 :event "COMMENT" :head-oid "aaa111"
                 :on-item (lambda (id _outcome _remote detail)
                            (push (cons id (plist-get detail :node-id)) seen))
                 :comments '((:id "c1" :path "a.el" :body "one" :line 3)
                             (:id "c2" :path "b.el" :body "two" :line 4)))))
      (should (equal (plist-get res :node-ids)
                     '((review . "PRR_900") ("c1" . "PRRC_70") ("c2" . "PRRC_71"))))
      (should (equal (plist-get res :remote-ids)
                     '((review . 900) ("c1" . 70) ("c2" . 71))))
      (should (equal (nreverse seen)
                     '((review . "PRR_900") ("c1" . "PRRC_70") ("c2" . "PRRC_71")))))))

(ert-deftest code-review-api-send-test/node-ids-individual-comments-and-replies ()
  (code-review-api-send-test--with-stubs
    (setq code-review-api-send-test--responder
          (lambda (_k target _p)
            (cond
             ((string-match-p "/comments/11/replies" target) '((id . 801) (node_id . "PRRC_801")))
             ((string-match-p "/comments/12/replies" target) '((id . 802) (node_id . "PRRC_802")))
             ((string-match-p "/pulls/7/comments\\'" target) '((id . 501) (node_id . "PRRC_501")))
             (t '((id . 802) (node_id . "PRRC_802"))))))
    (let ((res (code-review-api-submit
                "o/r" 7 :head-oid "aaa111"
                :comments '((:id "c1" :path "a.el" :body "one" :line 3))
                :replies '((:id "r1" :reply-to-id 11 :body "x")
                           (:id "r2" :reply-to-id 12 :body "y")))))
      (should (equal (plist-get res :node-ids)
                     '(("c1" . "PRRC_501") ("r1" . "PRRC_801") ("r2" . "PRRC_802")))))))

(ert-deftest code-review-api-send-test/node-ids-resolve-and-edit ()
  (code-review-api-send-test--with-stubs
    (let ((res (code-review-api-submit
                "o/r" 7 :head-oid "aaa111"
                :resolve '((:id "t1" :thread-id "PRRT_1" :resolved t))
                :edits '((:id "e1" :kind "review-comment" :comment-id 5 :body "b")))))
      (should (equal (plist-get res :sent) '("t1" "e1")))
      (should (equal (plist-get res :node-ids) '(("t1" . "PRRT_1")))))))

(ert-deftest code-review-api-send-test/node-ids-absent-when-response-has-none ()
  (code-review-api-send-test--with-stubs
    (let* ((details nil)
           (res (code-review-api-submit
                 "o/r" 7 :head-oid "aaa111"
                 :on-item (lambda (_id outcome _remote detail)
                            (push (cons outcome detail) details))
                 :replies '((:id "r1" :reply-to-id 11 :body "x")
                            (:id "r2" :reply-to-id 12 :body "y" :remote-id 9)))))
      (should-not (plist-get res :node-ids))
      (should (equal (nreverse details)
                     '((sent :node-id nil)
                       (skipped :node-id nil :id "r2" :reason already-sent)))))))

(ert-deftest code-review-api-send-test/on-item-failed-detail-keeps-fields ()
  (code-review-api-send-test--with-stubs
    (let ((detail nil))
      (setq code-review-api-send-test--responder (lambda (_k _t _p) (list :fail 422 nil)))
      (code-review-api-submit
       "o/r" 7 :head-oid "aaa111"
       :on-item (lambda (_id _outcome _remote d) (setq detail d))
       :replies '((:id "r1" :reply-to-id 11 :body "x")))
      (should (null (plist-get detail :node-id)))
      (should (eq (car detail) :node-id))
      (should (equal (plist-get detail :id) "r1"))
      (should (equal (plist-get detail :status) 422)))))

(ert-deftest code-review-api-send-test/reconcile-node-ids-and-user ()
  (code-review-api-send-test--with-stubs
    (setq code-review-api-send-test--get-responder
          (lambda (_k target _p)
            (if (string-match-p "/reviews\\'" target)
                '(((id . 1) (node_id . "PRR_1") (state . "APPROVED") (user . ((login . "me")))))
              '(((id . 2) (node_id . "PRRC_2") (path . "a.el") (user . ((login . "you")))
                 (body . "c"))))))
    (let* ((res (code-review-api-reconcile "o/r" 7 :account "me"))
           (r (car (plist-get res :reviews)))
           (c (car (plist-get res :comments))))
      (should (equal (plist-get r :node-id) "PRR_1"))
      (should (equal (plist-get r :user) "me"))
      (should (equal (plist-get c :node-id) "PRRC_2"))
      (should (equal (plist-get c :user) "you")))))

;;; code-review-api-send-test.el ends here
