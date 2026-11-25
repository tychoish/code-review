;;; code-review-api-analysis-test.el --- ERT tests for the analysis API -*- lexical-binding: t; -*-
;;
;; Fixture diffs against a temp git worktree.  Run in a fresh batch
;; emacs so a stale daemon cannot hide missing definitions.

(require 'ert)
(require 'code-review-api-analysis)
(require 'code-review-github)
(require 'code-review-test-helpers)

(defconst code-review-api-analysis-test--diff
  (concat "diff --git a/lib.py b/lib.py\n"
          "--- a/lib.py\n"
          "+++ b/lib.py\n"
          "@@ -1,2 +1,4 @@\n"
          " import os\n"
          "+def helper_new(x):\n"
          "+    return x + 1\n"
          " \n"
          "@@ -40,2 +42,3 @@\n"
          " pad\n"
          "+# comment only\n"
          " pad\n"
          "diff --git a/.github/workflows/ci.yml b/.github/workflows/ci.yml\n"
          "--- a/.github/workflows/ci.yml\n"
          "+++ b/.github/workflows/ci.yml\n"
          "@@ -3,2 +3,3 @@\n"
          "   steps:\n"
          "+    - run: pytest || true\n"
          "     - run: echo done\n")
  "Fixture PR diff: two lib.py hunks and one CI-gaming workflow hunk.")

(defun code-review-api-analysis-test--repo ()
  "A temp git repo holding the PR head of the fixture diff."
  (let* ((dir (file-name-as-directory (make-temp-file "cr-api-analysis-" t)))
         (default-directory dir))
    (call-process "git" nil nil nil "init" ".")
    (call-process "git" nil nil nil "config" "user.email" "t@t.test")
    (call-process "git" nil nil nil "config" "user.name" "t")
    (pcase-dolist (`(,path . ,text)
                   `(("lib.py" . ,(concat "import os\ndef helper_new(x):\n"
                                          "    return x + 1\n\n"
                                          (make-string 80 ?\n)))
                     ("main.py" . "from lib import helper_new\nhelper_new(1)\n")
                     (".github/workflows/ci.yml"
                      . "name: ci\n  steps:\n    - run: pytest || true\n")
                     ("specs/2026-11-01-001-lib.md"
                      . "---\npaths: [lib.py]\n---\n\nTHE SYSTEM SHALL keep lib.\n")))
      (let ((full (expand-file-name path dir)))
        (make-directory (file-name-directory full) t)
        (with-temp-file full (insert text))))
    (call-process "git" nil nil nil "add" "-A")
    (call-process "git" nil nil nil "commit" "-m" "init")
    dir))

(defmacro code-review-api-analysis-test--with-pr (worktree-p &rest body)
  "Run BODY with a stored PR o/r#1 carrying the fixture diff.
`repo' is the fixture checkout; the worktree lookup returns it when
WORKTREE-P, else nil."
  (declare (indent 1))
  `(code-review-test--with-db
     (let ((repo (code-review-api-analysis-test--repo))
           (code-review-history-enabled nil)
           (orig (symbol-function 'code-review-api-analysis--worktree)))
       (code-review-db--pullreq-create
        (code-review-github-repo :owner "o" :repo "r" :number "1"))
       (code-review-db--pullreq-raw-diff-update
        code-review-api-analysis-test--diff)
       (unwind-protect
           (progn
             (fset 'code-review-api-analysis--worktree
                   (lambda (_pr) (and ,worktree-p repo)))
             ,@body)
         (fset 'code-review-api-analysis--worktree orig)
         (delete-directory repo t)))))

(defun code-review-api-analysis-test--all-ranks (res)
  "Every rank in RES, sorted ascending."
  (sort (seq-mapcat
         (lambda (f)
           (seq-map (lambda (h) (plist-get (cdr h) :rank)) (cdr f)))
         res)
        #'<))

(ert-deftest code-review-api-analysis/shape-keys-and-tags ()
  (code-review-api-analysis-test--with-pr t
    (let* ((res (code-review-api-analysis "o/r" 1))
           (lib (cdr (assoc "lib.py" res)))
           (ci (cdr (assoc ".github/workflows/ci.yml" res))))
      (should (equal (seq-map #'car res)
                     '("lib.py" ".github/workflows/ci.yml")))
      ;; hunk key is the raw @@ ranges text, not an index
      (should (equal (seq-map #'car lib) '("-1,2 +1,4" "-40,2 +42,3")))
      (should (equal (seq-map #'car ci) '("-3,2 +3,3")))
      ;; helper_new is used (main.py) but no test references it
      (should (member "NO-TEST" (plist-get (cdr (assoc "-1,2 +1,4" lib)) :tags)))
      (should-not (member "NO-TEST"
                          (plist-get (cdr (assoc "-40,2 +42,3" lib)) :tags)))
      (should (member "CI-GAME" (plist-get (cdr (car ci)) :tags)))
      (should (equal (plist-get (cdr (assoc "-1,2 +1,4" lib)) :criteria)
                     '("2026-11-01-001")))
      (should-not (plist-get (cdr (car ci)) :criteria))
      (should (code-review-api-analysis-available-p "o/r" 1)))))

(ert-deftest code-review-api-analysis/scored-hunks-rank-first ()
  "Scored hunks rank first by score; RISK/INCIDENT tags come from the entry."
  (code-review-api-analysis-test--with-pr t
    (let ((orig (symbol-function 'code-review-analysis--hunks)))
      (unwind-protect
          (progn
            (fset 'code-review-analysis--hunks
                  (lambda (&rest _)
                    (list (list :path "lib.py" :ranges "-40,2 +42,3"
                                :score 0.9 :incidents 1)
                          (list :path "lib.py" :ranges "-1,2 +1,4"
                                :score 0.3 :incidents 0))))
            (let* ((res (code-review-api-analysis "o/r" 1))
                   (lib (cdr (assoc "lib.py" res)))
                   (hot (cdr (assoc "-40,2 +42,3" lib)))
                   (mild (cdr (assoc "-1,2 +1,4" lib)))
                   (ci (cdr (car (cdr (assoc ".github/workflows/ci.yml" res))))))
              (should (equal (plist-get hot :rank) 1))
              (should (equal (plist-get mild :rank) 2))
              (should (equal (plist-get ci :rank) 3))
              (should (equal (seq-take (plist-get hot :tags) 2)
                             '("RISK" "INCIDENT")))
              (should-not (member "RISK" (plist-get mild :tags)))
              ;; stable across calls with unchanged settings and caches
              (should (equal res (code-review-api-analysis "o/r" 1)))))
        (fset 'code-review-analysis--hunks orig)))))

(ert-deftest code-review-api-analysis/no-worktree-degrades ()
  (code-review-api-analysis-test--with-pr nil
    (let* ((res (code-review-api-analysis "o/r" 1))
           (lib (cdr (assoc "lib.py" res)))
           (ci (cdr (assoc ".github/workflows/ci.yml" res))))
      (should-not (code-review-api-analysis-available-p "o/r" 1))
      ;; CI-GAME is pure diff text; engine tags are absent
      (should (member "CI-GAME" (plist-get (cdr (car ci)) :tags)))
      (should-not (plist-get (cdr (car lib)) :tags))
      (should-not (plist-get (cdr (car lib)) :criteria))
      ;; ranks are diff order
      (should (equal (plist-get (cdr (car lib)) :rank) 1))
      (should (equal (code-review-api-analysis-test--all-ranks res)
                     '(1 2 3))))))

(ert-deftest code-review-api-analysis/file-without-hunks-and-duplicate-keys ()
  (code-review-test--with-db
    (code-review-db--pullreq-create
     (code-review-github-repo :owner "o" :repo "r" :number "1"))
    (code-review-db--pullreq-raw-diff-update
     (concat "diff --git a/img.png b/img.png\n"
             "Binary files a/img.png and b/img.png differ\n"
             "diff --git a/d.txt b/d.txt\n--- a/d.txt\n+++ b/d.txt\n"
             "@@ -1,1 +1,2 @@\n a\n+b\n"
             "@@ -1,1 +1,2 @@\n a\n+b\n"))
    (let ((res (code-review-api-analysis "o/r" 1)))
      ;; a file with no parseable hunk is just (PATH)
      (should (equal (assoc "img.png" res) '("img.png")))
      ;; a duplicate key keeps dense ranks
      (should (equal (code-review-api-analysis-test--all-ranks res) '(1 2))))))

(ert-deftest code-review-api-analysis/unknown-or-diffless-pr-is-nil ()
  (code-review-test--with-db
    (should (null (code-review-api-analysis "o/missing" 9)))
    (should-not (code-review-api-analysis-available-p "o/missing" 9))
    (code-review-db--pullreq-create
     (code-review-github-repo :owner "o" :repo "r" :number "2"))
    (should (null (code-review-api-analysis "o/r" 2)))
    (should-not (code-review-api-analysis-available-p "o/r" 2))))

(ert-deftest code-review-api-analysis/worktree-lookup-needs-existing-dir ()
  (code-review-test--with-db
    (let ((pr (code-review-github-repo :owner "o" :repo "r" :number "1"))
          (orig (symbol-function 'code-review-repo--find-existing)))
      (unwind-protect
          (progn
            (fset 'code-review-repo--find-existing (lambda (&rest _) nil))
            (should (null (code-review-api-analysis--worktree pr)))
            (fset 'code-review-repo--find-existing
                  (lambda (&rest _) temporary-file-directory))
            (should (null (code-review-api-analysis--worktree pr))))
        (fset 'code-review-repo--find-existing orig)))))

(provide 'code-review-api-analysis-test)
;;; code-review-api-analysis-test.el ends here
