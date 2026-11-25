;;; code-review-repo-test.el --- ERT tests for code-review-repo -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'code-review-db)
(require 'code-review-github)
(require 'code-review-repo)
(require 'code-review-test-helpers)

(defun code-review-repo-test--git (dir &rest args)
  "Run git ARGS in DIR, signaling with stderr on failure."
  (with-temp-buffer
    (let ((exit (apply #'call-process magit-git-executable nil t nil
                       (append (list "--no-pager" "-C" dir) args))))
      (unless (zerop exit)
        (error "git %s failed (%d): %s" args exit
               (buffer-substring-no-properties (point-min)
                                               (point-max)))))))

(defun code-review-repo-test--make-repo ()
  "A throwaway git repository with one commit.  Return its directory."
  (let ((dir (make-temp-file "cr-repo-cleanup-" t)))
    (code-review-repo-test--git dir "init" "-q" "-b" "main")
    (code-review-repo-test--git dir "config" "user.name" "t")
    (code-review-repo-test--git dir "config" "user.email" "t@t")
    (with-temp-file (expand-file-name "f.txt" dir)
      (insert "one\n"))
    (code-review-repo-test--git dir "add" "f.txt")
    (code-review-repo-test--git dir "-c" "user.name=t"
                                "-c" "user.email=t@t"
                                "commit" "-m" "one" "--no-verify")
    dir))

(defun code-review-repo-test--make-row (num finished?)
  "A saved row for owner/repo NUM, finished or not."
  (let ((pr (code-review-github-repo :owner "o" :repo "r"
                                     :number num)))
    (code-review-db--pullreq-create pr)
    (oset pr saved t)
    (when finished?
      (oset pr finished t)
      (oset pr finished-at (current-time-string)))
    (code-review-db-update pr)
    pr))

;;; req-2026-10-31-001: worktree-cleanup-prunes-finished-stale
;; covers: code-review-repo.el
;; covers-fn: code-review-repo-cleanup, code-review-repo--cleanup-plan
;; enforcement: test
(ert-deftest code-review-repo/req-2026-10-31-001-cleanup-prunes-stale-finished-worktrees ()
  "WHEN a review is finished and its worktree's head has not moved
for more than `code-review-repo-cleanup-days' days THE SYSTEM
SHALL prune that worktree — and never prune an UNFINISHED
review's worktree (however old) or a finished one whose head
moved recently: an active review's worktree never ages out."
  (let ((code-review-history-enabled nil)
        (code-review-review-budget-minutes nil)
        (code-review-budget-state-file (make-temp-file "cr-wt-"))
        (code-review-repo-cleanup-days 14))
    (code-review-test--with-db
      (let* ((dir (code-review-repo-test--make-repo))
             (head (code-review-repo--git dir "rev-parse" "HEAD")))
        ;; three worktrees: 1 stale+finished, 2 fresh+finished,
        ;; 3 stale+unfinished
        (dolist (num '("1" "2" "3"))
          (should (code-review-repo--ensure-worktree dir num head)))
        (let ((stale (days-to-seconds -20)))
          (set-file-times
           (code-review-repo--worktree-head-file dir "1")
           (seconds-to-time stale))
          (set-file-times
           (code-review-repo--worktree-head-file dir "3")
           (seconds-to-time stale)))
        ;; rows: #1 and #2 finished, #3 unfinished
        (code-review-repo-test--make-row "1" t)
        (code-review-repo-test--make-row "2" t)
        (code-review-repo-test--make-row "3" nil)
        ;; plan: only #1 (stale AND finished)
        (let ((plan (code-review-repo--cleanup-plan dir "o" "r")))
          (should (equal (mapcar #'car plan) '("1")))
          ;; execute: the worktree is gone, the others stay
          (let ((res (code-review-repo--cleanup-execute dir plan)))
            (should (equal res '(1 nil))))
          (should-not (file-directory-p
                       (code-review-repo--worktree-dir dir "1")))
          (should (file-directory-p
                   (code-review-repo--worktree-dir dir "2")))
          (should (file-directory-p
                   (code-review-repo--worktree-dir dir "3")))
          ;; the branch refs survive the removal
          (should (equal head (code-review-repo--git
                               dir "rev-parse" "HEAD"))))))))

(defun days-to-seconds (d) (* d 86400.0))

;;; req-2026-11-05-001: worktree-current-skips-refetch
;; covers: code-review-repo.el, code-review-section.el
;; covers-fn: code-review-repo--worktree-current?
;; enforcement: test
(ert-deftest code-review-repo/req-2026-11-05-001-worktree-current-skips-refetch ()
  "WHEN the bound worktree's HEAD already equals the PR row's
`sha' slot THE SYSTEM SHALL report the worktree current (the
refresh's network refetch is skipped) — and nil when the head
moved, the row has no sha, or no worktree is bound: a mismatched
guard must always fall back to the fetch."
  (let ((code-review-history-enabled nil))
    (code-review-test--with-db
      (let* ((dir (code-review-repo-test--make-repo))
             (head (code-review-repo--git dir "rev-parse" "HEAD"))
             (num "42")
             (pr (code-review-repo-test--make-row num nil)))
        ;; row carries the freshly fetched head (as raw-infos-update does)
        (oset pr sha head)
        (code-review-db-update pr)
        ;; no worktree bound: never current
        (let ((code-review-repo-worktree nil))
          (should-not (code-review-repo--worktree-current?)))
        ;; worktree at the row's head: current
        (should (code-review-repo--ensure-worktree dir num head))
        (let ((code-review-repo-worktree
               (code-review-repo--worktree-dir dir num)))
          (should (code-review-repo--worktree-current?))
          ;; head moved on: the fetch brings the NEW head into the
          ;; row while the worktree still sits at the old one
          (with-temp-file (expand-file-name "f.txt" dir)
            (insert "two\n"))
          (code-review-repo-test--git dir "add" "f.txt")
          (code-review-repo-test--git dir "-c" "user.name=t"
                                      "-c" "user.email=t@t"
                                      "commit" "-m" "two" "--no-verify")
          (oset pr sha (code-review-repo--git dir "rev-parse" "HEAD"))
          (code-review-db-update pr)
          (should-not (code-review-repo--worktree-current?)))
        ;; row without a sha: never current (fall back to the fetch)
        (oset pr sha nil)
        (code-review-db-update pr)
        (let ((code-review-repo-worktree
               (code-review-repo--worktree-dir dir num)))
          (should-not (code-review-repo--worktree-current?)))))))

(ert-deftest code-review-repo-test-main-worktree-from-linked ()
  "A linked worktree resolves to the main checkout and its name."
  (let* ((dir (code-review-repo-test--make-repo))
         (wt (expand-file-name ".wt/topic" dir)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory wt) t)
          (code-review-repo-test--git dir "worktree" "add" "-q" "-b" "topic" wt)
          (should (equal (file-truename dir)
                         (file-truename (code-review-repo-main-worktree wt))))
          (should (equal (file-name-nondirectory dir)
                         (code-review-repo-name wt)))
          (should (equal (file-name-nondirectory dir)
                         (code-review-repo-name dir))))
      (delete-directory dir t))))

;;; worktree resolver hook
(defun code-review-repo-test--commit (dir text)
  "Commit TEXT as f.txt in DIR; return the new HEAD."
  (with-temp-file (expand-file-name "f.txt" dir)
    (insert text))
  (code-review-repo-test--git dir "add" "f.txt")
  (code-review-repo-test--git dir "-c" "user.name=t" "-c" "user.email=t@t"
                              "commit" "-m" text "--no-verify")
  (code-review-repo--git dir "rev-parse" "HEAD"))

(defun code-review-repo-test--messages (thunk)
  "Call THUNK, return (VALUE . MESSAGES-STRING)."
  (let* ((msgs nil)
         (val (cl-letf* ((orig (symbol-function 'message))
                         ((symbol-function 'message)
                          (lambda (&rest args)
                            (push (apply #'format-message args) msgs))))
                (ignore orig)
                (funcall thunk))))
    (cons val (mapconcat #'identity (nreverse msgs) "\n"))))

(ert-deftest code-review-repo/worktree-resolver-used-when-matching ()
  "A matching resolved worktree wins, gets REPO/NUMBER, and is untouched."
  (let* ((dir (code-review-repo-test--make-repo))
         (head (code-review-repo--git dir "rev-parse" "HEAD"))
         (other (expand-file-name "resolved" (make-temp-file "cr-res-" t)))
         (seen nil)
         (code-review-worktree-resolver
          (lambda (repo number) (setq seen (list repo number)) other)))
    (code-review-repo-test--git dir "worktree" "add" "--detach" other head)
    (with-temp-file (expand-file-name "dirty.txt" other) (insert "x"))
    (unwind-protect
        (progn
          (should (equal (directory-file-name other)
                         (code-review-repo--ensure-worktree dir "7" head)))
          (should (equal (list dir 7) seen))
          (should (equal head (code-review-repo--git other "rev-parse" "HEAD")))
          (should (file-exists-p (expand-file-name "dirty.txt" other)))
          (should-not (file-exists-p (code-review-repo--worktree-dir dir "7"))))
      (delete-directory dir t)
      (delete-directory (file-name-directory other) t))))

(ert-deftest code-review-repo/worktree-resolver-falls-back ()
  "nil, a non-directory, or a signalling resolver fall back to the own worktree."
  (let* ((dir (code-review-repo-test--make-repo))
         (head (code-review-repo--git dir "rev-parse" "HEAD"))
         (own (code-review-repo--worktree-dir dir "8"))
         (msgs nil))
    (unwind-protect
        (seq-do
         (lambda (resolver)
           (let* ((code-review-worktree-resolver resolver)
                  (r (code-review-repo-test--messages
                      (lambda () (code-review-repo--ensure-worktree dir "8" head)))))
             (should (equal own (car r)))
             (push (cdr r) msgs)
             (should (file-exists-p (expand-file-name ".git" own)))
             (code-review-repo--git dir "worktree" "remove" "--force" own)))
         (list (lambda (_r _n) nil)
               (lambda (_r _n) "/nonexistent/cr-resolver-dir")
               (lambda (_r _n) (error "boom"))))
      (delete-directory dir t))
    (should (string-match-p "resolver failed: boom" (car msgs)))))

(ert-deftest code-review-repo/worktree-resolver-rejects-wrong-commit ()
  "A resolved worktree at another commit than HEAD-OID falls back."
  (let* ((dir (code-review-repo-test--make-repo))
         (head (code-review-repo--git dir "rev-parse" "HEAD"))
         (other (expand-file-name "resolved" (make-temp-file "cr-res-" t)))
         (code-review-worktree-resolver (lambda (_r _n) other)))
    (code-review-repo-test--git dir "worktree" "add" "--detach" other head)
    (let ((new (code-review-repo-test--commit dir "two")))
      (unwind-protect
          (let ((r (code-review-repo-test--messages
                    (lambda () (code-review-repo--ensure-worktree dir "9" new)))))
            (should (equal (code-review-repo--worktree-dir dir "9") (car r)))
            (should (string-match-p "is not at" (cdr r)))
            (should (equal head (code-review-repo--git other "rev-parse" "HEAD"))))
        (delete-directory dir t)
        (delete-directory (file-name-directory other) t)))))

(ert-deftest code-review-repo/worktree-resolver-rejects-other-repo ()
  "A resolved directory from a different repository falls back."
  (let* ((dir (code-review-repo-test--make-repo))
         (foreign (code-review-repo-test--make-repo))
         (head (code-review-repo--git dir "rev-parse" "HEAD"))
         (code-review-worktree-resolver (lambda (_r _n) foreign)))
    (unwind-protect
        (let ((r (code-review-repo-test--messages
                  (lambda () (code-review-repo--ensure-worktree dir "10" head)))))
          (should (equal (code-review-repo--worktree-dir dir "10") (car r)))
          (should (string-match-p "is not a worktree of" (cdr r))))
      (delete-directory dir t)
      (delete-directory foreign t))))

(provide 'code-review-repo-test)
;;; code-review-repo-test.el ends here
