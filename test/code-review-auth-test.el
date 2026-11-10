;;; code-review-auth-test.el --- ERT tests for gh authentication -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'code-review-auth)
(require 'code-review-utils)
(require 'code-review-github)

(defvar code-review-auth-login-marker)

(defmacro code-review-auth-test--with-gh (calls &rest body)
  "Run BODY with `gh' faked; CALLS is a symbol collecting arg lists."
  (declare (indent 1))
  `(let ((code-review-auth-backend 'gh)
         (code-review-auth-login-marker 'code-review)
         (code-review-gh-default-account nil)
         (code-review-gh-token-cache (make-hash-table :test #'equal)))
     (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/gh"))
               ((symbol-function 'call-process)
                (lambda (_prog _in dest _disp &rest args)
                  (push args ,calls)
                  (with-current-buffer (car dest)
                    (insert "gho_" (or (car (last args)) "active") "\n"))
                  0)))
       ,@body)))

(ert-deftest code-review-auth-test/normalize-host ()
  (should (equal (code-review-gh--normalize-host "api.github.com") "github.com"))
  (should (equal (code-review-gh--normalize-host nil) "github.com"))
  (should (equal (code-review-gh--normalize-host "https://github.com") "github.com"))
  (should (equal (code-review-gh--normalize-host "ghe.example.com/api/v3") "ghe.example.com")))

(ert-deftest code-review-auth-test/token-cached-per-account ()
  (let (calls)
    (code-review-auth-test--with-gh calls
      (should (equal (code-review-gh-auth-token "api.github.com" "alice") "gho_alice"))
      (should (equal (code-review-gh-auth-token "api.github.com" "alice") "gho_alice"))
      (should (= (length calls) 1))
      (should (equal (car calls) '("auth" "token" "-h" "github.com" "-u" "alice")))
      (should (equal (code-review-gh-auth-token "api.github.com" "bob") "gho_bob"))
      (should (= (length calls) 2))
      (code-review-auth-clear-cache)
      (code-review-gh-auth-token "api.github.com" "alice")
      (should (= (length calls) 3)))))

(ert-deftest code-review-auth-test/account-precedence ()
  (let (calls)
    (code-review-auth-test--with-gh calls
      (let ((code-review-gh-default-account "dflt"))
        (should (equal (code-review-gh-auth-token) "gho_dflt"))
        (with-temp-buffer
          (setq-local code-review-gh-account "local")
          (should (equal (code-review-gh-auth-token) "gho_local")))))))

(ert-deftest code-review-auth-test/no-implicit-account ()
  "With no account, signal an error and never call gh."
  (let (calls)
    (code-review-auth-test--with-gh calls
      (should-error (code-review-gh-auth-token) :type 'user-error)
      (should-error (code-review-auth-resolve 'github) :type 'user-error)
      (should-not calls))))

(ert-deftest code-review-auth-test/always-passes-user-and-never-switches ()
  (let (calls)
    (code-review-auth-test--with-gh calls
      (let ((code-review-gh-default-account "dflt"))
        (code-review-gh-auth-token)
        (should (equal (car calls) '("auth" "token" "-h" "github.com" "-u" "dflt")))
        (should-not (seq-some (lambda (c) (member "switch" c)) calls))))))

(ert-deftest code-review-auth-test/evict-scoped-to-failed-token ()
  (let ((code-review-gh-token-cache (make-hash-table :test #'equal)))
    (setf (map-elt code-review-gh-token-cache '("github.com" . "a")) "tok-a")
    (setf (map-elt code-review-gh-token-cache '("github.com" . "b")) "tok-b")
    (code-review-auth-evict-token "tok-a")
    (should-not (map-elt code-review-gh-token-cache '("github.com" . "a")))
    (should (equal (map-elt code-review-gh-token-cache '("github.com" . "b")) "tok-b"))))

(ert-deftest code-review-auth-test/errback-evicts-only-the-failed-request-token ()
  (require 'ghub)
  (let ((code-review-gh-token-cache (make-hash-table :test #'equal))
        (req (ghub--make-req :headers '(("Authorization" . "token tok-a")))))
    (setf (map-elt code-review-gh-token-cache '("github.com" . "a")) "tok-a")
    (setf (map-elt code-review-gh-token-cache '("github.com" . "b")) "tok-b")
    (code-review-github--evict-failed-token (list '(ghub-http-error 401) nil 401 req))
    (should-not (map-elt code-review-gh-token-cache '("github.com" . "a")))
    (should (map-elt code-review-gh-token-cache '("github.com" . "b")))
    (code-review-github--evict-failed-token '((error error 401 nil)))
    (should (zerop (hash-table-count code-review-gh-token-cache)))))

(ert-deftest code-review-auth-test/ensure-account-batch-does-not-prompt ()
  (let ((code-review-auth-backend 'gh)
        (code-review-gh-default-account nil))
    (with-temp-buffer
      (should-not (code-review-gh-ensure-account))
      (setq-local code-review-gh-account "x")
      (should (equal (code-review-gh-ensure-account) "x")))))

(ert-deftest code-review-auth-test/resolve-dispatch ()
  (let (calls)
    (code-review-auth-test--with-gh calls
      (should (equal (code-review-auth-resolve 'github nil "alice") "gho_alice"))
      (code-review-auth-resolve 'github "ghe.example.com/api/v3" "alice")
      (should (equal (car calls) '("auth" "token" "-h" "ghe.example.com" "-u" "alice")))
      (should (eq (code-review-auth-resolve 'gitlab) 'code-review))
      (let ((code-review-auth-backend 'auth-source))
        (should (eq (code-review-auth-resolve 'github) 'code-review)))
      (let ((code-review-auth-backend (lambda (_f _a) "custom")))
        (should (equal (code-review-auth-resolve 'github) "custom"))))))

(ert-deftest code-review-auth-test/gh-failure-and-empty-token ()
  (let ((code-review-gh-token-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/gh"))
              ((symbol-function 'call-process) (lambda (&rest _) 1)))
      (should-error (code-review-gh-auth-token "github.com" "x") :type 'user-error))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/gh"))
              ((symbol-function 'call-process) (lambda (&rest _) 0)))
      (should-error (code-review-gh-auth-token "github.com" "x") :type 'user-error)
      (should (zerop (hash-table-count code-review-gh-token-cache))))))

(ert-deftest code-review-auth-test/pr-account-survives-outside-buffer ()
  "A PR's account is used by requests run from timers/callbacks."
  (let (calls)
    (code-review-auth-test--with-gh calls
      (let ((code-review-gh--pr-accounts (make-hash-table :test #'equal))
            (pr (code-review-github-repo :owner "o" :repo "r" :number 7 :url "u" :id "x")))
        (with-temp-buffer
          (setq-local code-review-gh-account "alice")
          (code-review-gh-register-pr-account pr))
        (with-temp-buffer
          (should (equal (code-review-auth-resolve 'github nil pr) "gho_alice"))
          (should (equal (car calls) '("auth" "token" "-h" "github.com" "-u" "alice"))))
        (should (equal (code-review-gh-pr-account pr) "alice"))))))

(ert-deftest code-review-auth-test/pr-without-buffer-account-uses-default ()
  (let (calls)
    (code-review-auth-test--with-gh calls
      (let ((code-review-gh--pr-accounts (make-hash-table :test #'equal))
            (code-review-gh-default-account "dflt")
            (pr (code-review-github-repo :owner "o" :repo "r" :number 8 :url "u" :id "y")))
        (code-review-gh-register-pr-account pr)
        (should (equal (code-review-gh-pr-account pr) "dflt"))
        (should (equal (code-review-auth-resolve 'github nil pr) "gho_dflt"))))))

(ert-deftest code-review-auth-test/build-obj-from-url-returns-pr-and-registers-account ()
  "Regression: `code-review-db--pullreq-create' must return the PR object
so `code-review-utils-build-obj' can register the selected GitHub account."
  (require 'code-review-test-helpers)
  (code-review-test--with-db
    (let ((code-review-gh--pr-accounts (make-hash-table :test #'equal))
          (code-review-gh-default-account "tycho-casap"))
      (let ((pr (code-review-utils-build-obj-from-url
                 "https://github.com/casaphq/go-monorepo/pull/11423")))
        (should (code-review-github-repo-p pr))
        (should (equal (code-review-gh-pr-account pr) "tycho-casap"))))))

(ert-deftest code-review-auth-test/missing-gh ()
  (let ((code-review-auth-backend 'gh)
        (code-review-gh-token-cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
      (should-error (code-review-gh-auth-token "github.com" "x") :type 'user-error))))

(ert-deftest code-review-auth-test/errback-401-clears-cache ()
  (require 'code-review-github)
  (let ((code-review-gh-token-cache (make-hash-table :test #'equal)))
    (setf (map-elt code-review-gh-token-cache '("github.com" . "a")) "t")
    (cl-letf (((symbol-function 'message) #'ignore)
              ((symbol-function 'code-review-utils--log) #'ignore))
      (code-review-github-errback '(error . (error 401 nil))))
    (should (zerop (hash-table-count code-review-gh-token-cache)))))

;; Loading the leaf auth file alone must not need code-review.el.
(ert-deftest code-review-auth-test/marker-defined-by-auth-file ()
  (should (boundp 'code-review-auth-login-marker))
  (should (eq (default-value 'code-review-auth-login-marker) 'code-review)))

(ert-deftest code-review-auth-test/accounts-parses-gh-json ()
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/gh"))
            ((symbol-function 'call-process)
             (lambda (_p _i dest _d &rest args)
               (should (equal args '("auth" "status" "--json" "hosts")))
               (with-current-buffer (car dest)
                 (insert "{\"hosts\":{\"github.com\":[{\"login\":\"a\",\"active\":true},{\"login\":\"b\",\"active\":false}]}}"))
               0)))
    (should (equal (code-review-gh-accounts "api.github.com") '("a" "b")))
    (should-not (code-review-gh-accounts "ghe.example.com"))))

(ert-deftest code-review-auth-test/accounts-gh-failure ()
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/gh"))
            ((symbol-function 'call-process) (lambda (&rest _) 1)))
    (should-error (code-review-gh-accounts) :type 'user-error)))

(ert-deftest code-review-auth-test/select-account-is-local-and-registers-pr ()
  "Selecting sets only the buffer-local, registers the PR, never switches."
  (let ((code-review-gh--pr-accounts (make-hash-table :test #'equal))
        (code-review-db--pullreq-id "id")
        (pr (code-review-github-repo :owner "o" :repo "r" :number 9 :url "u" :id "id"))
        (called nil))
    (cl-letf (((symbol-function 'code-review-db-get-pullreq) (lambda () pr))
              ((symbol-function 'call-process) (lambda (&rest a) (push a called) 0))
              ((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (code-review-gh-select-account "alice")
        (should (equal code-review-gh-account "alice")))
      (should-not (default-value 'code-review-gh-account))
      (should (equal (code-review-gh-pr-account pr) "alice"))
      (should-not called))))

;; ghub looks up `git config github.user' when :username is nil, even
;; for a token string (verified live with ghub 20261101).
(ert-deftest code-review-auth-test/username-follows-backend ()
  (let ((code-review-gh-default-account "dflt"))
    (let ((code-review-auth-backend 'gh))
      (should (equal (code-review-auth-username) "dflt"))
      (should (equal (code-review-auth-username "bob") "bob")))
    (let ((code-review-auth-backend 'auth-source))
      (should-not (code-review-auth-username))))
  (let ((code-review-auth-backend 'gh)
        (code-review-gh-default-account nil))
    (should-error (code-review-auth-username) :type 'user-error)))

(ert-deftest code-review-auth-test/every-resolve-call-site-passes-username ()
  "Each `:auth (code-review-auth-resolve ...)' is followed by `:username',
and every call in `code-review-github*' passes the PR/account object."
  (let ((dir (file-name-directory (locate-library "code-review-auth"))))
    (seq-do
     (lambda (file)
       (with-temp-buffer
         (insert-file-contents file)
         (goto-char (point-min))
         (while (re-search-forward ":auth (code-review-auth-resolve 'github" nil t)
           (when (string-match-p "code-review-github" (file-name-nondirectory file))
             (should (looking-at-p " [^ \t\n)]+ \\([^ \t\n)]+\\))")))
           (forward-line 1)
           (should (looking-at-p "[ \t]*:username (code-review-auth-username")))))
     (directory-files dir t "\\`code-review-.*\\.el\\'"))))

(provide 'code-review-auth-test)
;;; code-review-auth-test.el ends here
