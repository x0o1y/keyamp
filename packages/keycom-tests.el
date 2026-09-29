;;; keycom-tests.el --- Tests for KEYCOM -*- lexical-binding: t; -*-

;; Run with:
;; /Applications/Emacs.app/Contents/MacOS/Emacs -Q --batch \
;;   --eval '(setq load-prefer-newer t)' \
;;   -L ~/.emacs.d/packages -l keycom-tests \
;;   --eval '(ert-run-tests-batch-and-exit "^keycom-test-")'

(require 'ert)
(require 'json)
(require 'keycom)

(defvar org-agenda-file-1)
(defvar org-agenda-file-2)
(defvar org-agenda-file-3)

(ert-deftest keycom-test-open-file-urls ()
  (dolist (sample '(("https://example.org/a" . "https://example.org/a")
                    ("http://example.org/a" . "http://example.org/a")
                    ("See (https://example.org/a), next" . "https://example.org/a")
                    ("See (https://example.org/a)." . "https://example.org/a")
                    ("[site](https://example.org/a_(b))." . "https://example.org/a_(b)")
                    ("https://example.org/a_(b)" . "https://example.org/a_(b)")))
    (let ((browse-url-ff-patterns nil)
          opened)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _) (setq opened url))))
        (with-temp-buffer
          (insert (car sample))
          (goto-char (point-min))
          (search-forward "example.org")
          (open-file)))
      (should (equal opened (cdr sample))))))

(ert-deftest keycom-test-open-file-selected-url ()
  (let ((browse-url-ff-patterns nil)
        opened)
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url &rest _) (setq opened url))))
      (with-temp-buffer
        (let ((transient-mark-mode t))
          (insert "https://example.org/selected unrelated")
          (goto-char (point-min))
          (push-mark (+ (point-min) (length "https://example.org/selected")) t t)
          (open-file))))
    (should (equal opened "https://example.org/selected"))))

(ert-deftest keycom-test-open-file-youtube ()
  (let (opened)
    (cl-letf (((symbol-function 'movie)
               (lambda (url) (setq opened url)))
              ((symbol-function 'browse-url)
               (lambda (&rest _) (ert-fail "YouTube URL opened in browser"))))
      (with-temp-buffer
        (insert "https://www.youtube.com/watch?v=example")
        (goto-char (point-min))
        (search-forward "youtube.com")
        (open-file)))
    (should (equal opened "https://www.youtube.com/watch?v=example"))))

(ert-deftest keycom-test-open-file-existing-path ()
  (let (opened)
    (cl-letf (((symbol-function 'file-exists-p)
               (lambda (path) (equal path "/tmp/report.txt")))
              ((symbol-function 'find-file)
               (lambda (path) (setq opened path))))
      (with-temp-buffer
        (insert "/tmp/report.txt")
        (goto-char (point-max))
        (open-file)))
    (should (equal opened "/tmp/report.txt"))))

(ert-deftest keycom-test-open-file-file-url ()
  (let (opened)
    (cl-letf (((symbol-function 'file-exists-p)
               (lambda (path) (equal path "/tmp/report.txt")))
              ((symbol-function 'find-file)
               (lambda (path) (setq opened path))))
      (with-temp-buffer
        (insert "file:///tmp/report.txt")
        (goto-char (point-max))
        (open-file)))
    (should (equal opened "/tmp/report.txt"))))

(ert-deftest keycom-test-open-file-home-path ()
  (let ((home (getenv "HOME"))
        opened)
    (cl-letf (((symbol-function 'file-exists-p)
               (lambda (path) (equal path (concat home "/report.txt"))))
              ((symbol-function 'find-file)
               (lambda (path) (setq opened path))))
      (with-temp-buffer
        (insert "$HOME/report.txt")
        (goto-char (point-max))
        (open-file)))
    (should (equal opened (concat home "/report.txt")))))

(ert-deftest keycom-test-open-file-tramp ()
  (let (opened)
    (cl-letf (((symbol-function 'find-file)
               (lambda (path) (setq opened path))))
      (with-temp-buffer
        (insert "/ssh:user@host:/home/user/report.txt")
        (goto-char (point-max))
        (open-file)))
    (should (equal opened "/ssh:user@host:/home/user/report.txt"))))

(ert-deftest keycom-test-open-file-line-number ()
  (let (opened line)
    (cl-letf (((symbol-function 'file-exists-p)
               (lambda (path) (equal path "/tmp/report.txt")))
              ((symbol-function 'find-file)
               (lambda (path)
                 (setq opened path)
                 (erase-buffer)
                 (insert "first\nsecond\nthird\n"))))
      (with-temp-buffer
        (insert "/tmp/report.txt:2:7")
        (goto-char (point-max))
        (open-file)
        (setq line (line-number-at-pos))))
    (should (equal opened "/tmp/report.txt"))
    (should (= line 2))))

(ert-deftest keycom-test-open-file-fragment ()
  (let (opened found)
    (cl-letf (((symbol-function 'file-exists-p)
               (lambda (path) (equal path "/tmp/report.txt")))
              ((symbol-function 'find-file)
               (lambda (path)
                 (setq opened path)
                 (erase-buffer)
                 (insert "before target after"))))
      (with-temp-buffer
        (insert "/tmp/report.txt#target")
        (goto-char (point-max))
        (open-file)
        (setq found (looking-back "target" (line-beginning-position)))))
    (should (equal opened "/tmp/report.txt"))
    (should found)))

(ert-deftest keycom-test-open-file-elisp-extension ()
  (let (opened)
    (cl-letf (((symbol-function 'file-exists-p)
               (lambda (path) (equal path "/tmp/helper.el")))
              ((symbol-function 'find-file)
               (lambda (path) (setq opened path))))
      (with-temp-buffer
        (insert "/tmp/helper")
        (goto-char (point-max))
        (open-file)))
    (should (equal opened "/tmp/helper.el"))))

(ert-deftest keycom-test-open-file-missing-path ()
  (let (fallback)
    (cl-letf (((symbol-function 'file-exists-p) (lambda (&rest _) nil))
              ((symbol-function 'describe-foo-at-point-error)
               (lambda () (setq fallback t))))
      (with-temp-buffer
        (insert "/tmp/missing")
        (goto-char (point-max))
        (open-file)))
    (should fallback)))

(ert-deftest keycom-test-open-file-dired ()
  (let (opened)
    (cl-letf (((symbol-function 'open-in-external-app)
               (lambda () (setq opened t))))
      (with-temp-buffer
        (setq major-mode 'dired-mode)
        (open-file)))
    (should opened)))

(ert-deftest keycom-test-json-pretty-compact ()
  (skip-unless (executable-find "fracjson"))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/keyamp.json")
    (insert "{\n  \"rules\": [\n    {\"from\": {\"key_code\": \"a\"}, \"to\": [{\"key_code\": \"b\"}]}\n  ]\n}\n")
    (let ((original (json-read-from-string (buffer-string))))
      (json-pretty-compact)
      (should (equal (json-read-from-string (buffer-string)) original))
      (should (string-match-p
               (regexp-quote "{ \"from\": {\"key_code\": \"a\"}")
               (buffer-string))))))

(ert-deftest keycom-test-json-pretty-compact-invalid ()
  (skip-unless (executable-find "fracjson"))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/keyamp.json")
    (insert "{invalid")
    (should-error (json-pretty-compact) :type 'user-error)
    (should (string-equal (buffer-string) "{invalid"))))

(ert-deftest keycom-test-json-pretty-compact-non-json ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/keyamp.txt")
    (insert "{}")
    (should-error (json-pretty-compact) :type 'user-error)
    (should (string-equal (buffer-string) "{}"))))

(ert-deftest keycom-test-bounds-of-block ()
  (with-temp-buffer
    (insert "one\n\ntwo\nthree\n\nfour")
    (goto-char 8)
    (should (equal (get-bounds-of-block) '(6 . 15)))))

(ert-deftest keycom-test-bounds-of-region ()
  (with-temp-buffer
    (let ((transient-mark-mode t))
      (insert "abcdef")
      (goto-char 2)
      (push-mark 5 t t)
      (should (equal (get-bounds-of-block-or-region) '(2 . 5))))))

(ert-deftest keycom-test-line-wrap ()
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (goto-char (point-min))
    (up-line-wrap)
    (should (= (line-number-at-pos) 4))
    (down-line-wrap #'forward-line 1)
    (should (= (point) (point-min)))))

(ert-deftest keycom-test-beg-of-line ()
  (with-temp-buffer
    (insert "  text")
    (goto-char (point-max))
    (beg-of-line)
    (should (= (current-column) 2))
    (beg-of-line)
    (should (bolp))
    (beg-of-line)
    (should (eolp))))

(ert-deftest keycom-test-end-of-lyne ()
  (with-temp-buffer
    (insert "  text")
    (goto-char (point-min))
    (end-of-lyne)
    (should (eolp))
    (end-of-lyne)
    (should (= (current-column) 2))))

(ert-deftest keycom-test-block-movement ()
  (with-temp-buffer
    (insert "one\n\ntwo\n\nthree")
    (goto-char (point-min))
    (forw-block)
    (should (looking-at "two"))
    (forw-block)
    (should (looking-at "three"))
    (back-block)
    (should (eolp))
    (should (= (line-number-at-pos) 4))))

(ert-deftest keycom-test-buffer-boundaries ()
  (with-temp-buffer
    (insert "one\ntwo\nthree\n")
    (goto-char 6)
    (beg-of-buf)
    (should (= (point) (point-min)))
    (beg-of-buf)
    (should (looking-at "three"))
    (goto-char (point-min))
    (end-of-buf)
    (should (looking-at "three"))
    (end-of-buf)
    (should (= (point) (point-min)))))

(ert-deftest keycom-test-punctuation-movement ()
  (with-temp-buffer
    (insert "one, two! three")
    (goto-char (point-min))
    (forward-punct)
    (should (eq (char-after) ?,))
    (forward-punct)
    (should (eq (char-after) ?!))
    (goto-char (point-max))
    (backward-punct)
    (should (eq (char-before) ?!))))

(ert-deftest keycom-test-bracket-movement ()
  (with-temp-buffer
    (insert "a (b [c]) d")
    (goto-char (point-max))
    (backward-bracket)
    (should (eq (char-after) ?\[))
    (goto-char (point-min))
    (forward-bracket)
    (should (eq (char-before) ?\]))))

(ert-deftest keycom-test-sort-lines-key-value ()
  (with-temp-buffer
    (insert "a: 3\nb: 1\nc: 2\n")
    (sort-lines-key-value (point-min) (point-max))
    (should (string-equal (buffer-string) "b: 1\nc: 2\na: 3\n"))))

(ert-deftest keycom-test-delete-spaces ()
  (with-temp-buffer
    (insert "one \t  two")
    (goto-char 6)
    (delete-spaces)
    (should (string-equal (buffer-string) "onetwo"))))

(ert-deftest keycom-test-delete-blank-lines ()
  (with-temp-buffer
    (insert "one\n\n\nthree")
    (goto-char 6)
    (delete-blank-lines)
    (should (string-equal (buffer-string) "onethree"))))

(ert-deftest keycom-test-newlines-to-one ()
  (with-temp-buffer
    (insert "one\n\n\nthree")
    (change-newline-chars-to-one (point-min) (point-max))
    (should (string-equal (buffer-string) "one\nthree"))))

(ert-deftest keycom-test-whitespaces-to-one-space ()
  (with-temp-buffer
    (insert "one \t\n  two")
    (reformat-whitespaces-to-one-space (point-min) (point-max))
    (should (string-equal (buffer-string) "one two"))))

(ert-deftest keycom-test-space-to-newline ()
  (with-temp-buffer
    (insert "one   two three")
    (space-to-newline)
    (should (string-equal (buffer-string) "one\ntwo\nthree"))))

(ert-deftest keycom-test-slash-conversions ()
  (with-temp-buffer
    (insert "a/b/c")
    (slash-to-backslash (point-min) (point-max))
    (should (string-equal (buffer-string) "a\\b\\c"))
    (backslash-to-slash (point-min) (point-max))
    (should (string-equal (buffer-string) "a/b/c"))
    (slash-to-double-backslash (point-min) (point-max))
    (should (string-equal (buffer-string) "a\\\\b\\\\c"))
    (double-backslash-to-slash (point-min) (point-max))
    (should (string-equal (buffer-string) "a/b/c"))))

(ert-deftest keycom-test-quote-lines ()
  (with-temp-buffer
    (insert "one\n  two")
    (quote-lines (point-min) (point-max) "\"" "\"" ",")
    (should (string-equal (buffer-string) "\"one\",\n  \"two\","))))

(ert-deftest keycom-test-escape-quotes ()
  (with-temp-buffer
    (insert "say \"hello\"")
    (escape-quotes (point-min) (point-max))
    (should (string-equal (buffer-string) "say \\\"hello\\\""))
    (unescape-quotes (point-min) (point-max))
    (should (string-equal (buffer-string) "say \"hello\""))))

(ert-deftest keycom-test-cycle-separators ()
  (with-temp-buffer
    (insert "one two")
    (let ((this-command 'cycle-hyphen-lowline-space)
          last-command)
      (put 'cycle-hyphen-lowline-space 'state 0)
      (cycle-hyphen-lowline-space (point-min) (point-max))
      (should (string-equal (buffer-string) "one-two"))
      (setq last-command this-command)
      (cycle-hyphen-lowline-space (point-min) (point-max))
      (should (string-equal (buffer-string) "one_two"))
      (cycle-hyphen-lowline-space (point-min) (point-max))
      (should (string-equal (buffer-string) "one two")))))

(ert-deftest keycom-test-string-capitalized ()
  (should (string-is-capitalized "ABC"))
  (should-not (string-is-capitalized "Abc")))

(ert-deftest keycom-test-toggle-case ()
  (with-temp-buffer
    (insert "hello")
    (goto-char 3)
    (let ((this-command 'toggle-case)
          last-command)
      (toggle-case)
      (should (string-equal (buffer-string) "Hello"))
      (setq last-command this-command)
      (toggle-case)
      (should (string-equal (buffer-string) "HELLO"))
      (toggle-case)
      (should (string-equal (buffer-string) "hello")))))

(ert-deftest keycom-test-title-case ()
  (with-temp-buffer
    (insert "the lord of the rings")
    (title-case-region-or-line (point-min) (point-max))
    (should (string-equal (buffer-string) "The Lord of the Rings"))))

(ert-deftest keycom-test-insert-bracket-pair ()
  (with-temp-buffer
    (insert "word")
    (goto-char (point-max))
    (insert-bracket-pair "(" ")")
    (should (string-equal (buffer-string) "word()"))
    (should (eq (char-after) (string-to-char ")")))))

(ert-deftest keycom-test-insert-bracket-region ()
  (with-temp-buffer
    (insert "word")
    (goto-char (point-min))
    (push-mark (point-max) t t)
    (insert-bracket-pair "[" "]")
    (should (string-equal (buffer-string) "[word]"))))

(ert-deftest keycom-test-select-line ()
  (with-temp-buffer
    (insert "one\ntwo")
    (goto-char 2)
    (select-line)
    (should
     (string-equal
      (buffer-substring-no-properties (region-beginning) (region-end))
      "one"))))

(ert-deftest keycom-test-select-word ()
  (with-temp-buffer
    (insert "one two")
    (goto-char 6)
    (let ((executing-kbd-macro t))
      (select-word)
      (should
       (string-equal
        (buffer-substring-no-properties
         (min (point) (mark)) (max (point) (mark)))
        "two")))))

(ert-deftest keycom-test-user-buffer ()
  (let ((org-agenda-file-1 "/tmp/keycom-agenda-1")
        (org-agenda-file-2 "/tmp/keycom-agenda-2")
        (org-agenda-file-3 "/tmp/keycom-agenda-3"))
    (with-temp-buffer
      (rename-buffer "keycom-test-user" t)
      (should (user-buffer-p)))
    (with-temp-buffer
      (rename-buffer "*keycom-test-internal*" t)
      (should-not (user-buffer-p)))))

(ert-deftest keycom-test-cur-word ()
  (with-temp-buffer
    (insert "one two-three four")
    (goto-char 8)
    (should (string-equal (cur-word) "two-three"))))

(ert-deftest keycom-test-terminal-input ()
  (with-temp-buffer
    (let ((kill-ring '("/tmp/work")))
      (insert "└ › pushd . && /tmp/work")
      (goto-char (point-max))
      (should (terminal-prompt "└ › pushd . && "))
      (should (terminal-input "└ › pushd . && "))
      (should (change-wd-p)))))

(ert-deftest keycom-test-vterm-capture-command ()
  (with-temp-buffer
    (insert "output\n└ › echo hello\n")
    (let ((vterm-last-command ""))
      (vterm-capture-command)
      (should (string-equal vterm-last-command "echo hello")))))

(ert-deftest keycom-test-one-windowp ()
  (cl-letf (((symbol-function 'window-total-height) (lambda (&rest _) 31))
            ((symbol-function 'one-window-p) (lambda (&rest _) nil)))
    (should (one-windowp)))
  (cl-letf (((symbol-function 'window-total-height) (lambda (&rest _) 20))
            ((symbol-function 'one-window-p) (lambda (&rest _) nil)))
    (should-not (one-windowp))))

(ert-deftest keycom-test-window-half-height ()
  (cl-letf (((symbol-function 'window-height) (lambda (&rest _) 50))
            ((symbol-function 'frame-height) (lambda (&rest _) 100)))
    (should (window-half-height-p)))
  (cl-letf (((symbol-function 'window-height) (lambda (&rest _) 40))
            ((symbol-function 'frame-height) (lambda (&rest _) 100)))
    (should-not (window-half-height-p))))

(defun keycom-tests-run ()
  "Run KEYCOM tests."
  (interactive)
  (ert "^keycom-test-"))

(provide 'keycom-tests)

;;; keycom-tests.el ends here
