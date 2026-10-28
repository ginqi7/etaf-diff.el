;;; etaf-diff.el --- Show diffs with ETAF -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `etaf-diff-json' runs diff(1) on two files and turns the unified diff into
;; diff2html JSON with `etaf-diff-highlight.js', a small Node script that uses
;; the diff2html library bundled with diff2html-cli.  Besides the lines, that
;; JSON carries the word highlight that diff2html's own renderer computes, so
;; the changed words match the diff2html page.  `etaf-diff-parse' turns the
;; JSON into Lisp data, and `etaf-diff-files' shows it as an ETAF application
;; buffer.
;;
;; The diff can be shown in two styles:
;;
;;   `line'  One column, old and new lines interleaved (diff2html "line").
;;   `side'  Two columns, old on the left and new on the right (diff2html "side").
;;
;; Entry points:
;;
;;   M-x etaf-diff-files   Ask for two files and a style, then show the diff.
;;   `etaf-diff-files'     Show the diff of two file names in STYLE.
;;   `etaf-diff-open'      Show the unified diff in a .diff FILE in STYLE.
;;   `etaf-diff-git'       Show the output of `git diff' with ARGS in STYLE.
;;   `etaf-diff-json'     Return the diff as a diff2html JSON string.

;;; Code:

(require 'json)
(require 'etaf)

(defgroup etaf-diff nil
  "Show diffs with ETAF."
  :group 'tools)

(defcustom etaf-diff-diff-program "diff"
  "Program that writes a unified diff."
  :type 'string)

(defcustom etaf-diff-diff2html-program "diff2html"
  "The diff2html-cli executable.
Its installed library, which `etaf-diff-highlight.js' loads, is found next
to this program."
  :type 'string)

(defcustom etaf-diff-git-program "git"
  "Program that writes the output of `git diff'."
  :type 'string)

(defcustom etaf-diff-node-program "node"
  "Node.js program that runs `etaf-diff-highlight.js'."
  :type 'string)

(defcustom etaf-diff-style 'side
  "Style of the diff shown by the commands that do not take one.
`line' shows one column; `side' shows old and new in two columns."
  :type '(choice (const :tag "Line" line)
                 (const :tag "Side by side" side)))

(defconst etaf-diff-buffer-name "*ETAF Diff*"
  "Buffer used by `etaf-diff-files'.")

(defconst etaf-diff--highlight-script
  (expand-file-name "etaf-diff-highlight.js"
                    (file-name-directory
                     (or load-file-name buffer-file-name default-directory)))
  "The Node script that prints diff2html JSON with word highlights.")

(defun etaf-diff--node-path ()
  "Return a NODE_PATH value that finds the diff2html library, or nil."
  (when-let* ((exe (executable-find etaf-diff-diff2html-program)))
    (let ((bin (file-name-directory (file-truename exe))))
      (mapconcat #'identity
                 (list (expand-file-name "../lib/node_modules/diff2html-cli/node_modules"
                                         bin)
                       (expand-file-name "../lib/node_modules" bin))
                 path-separator))))

(defun etaf-diff-json (old new)
  "Return the diff from file OLD to file NEW as a diff2html JSON string.
Each line of the JSON has a `runs' list of (TEXT CHANGED) pairs.  Signal an
error when diff(1) or the highlight script fails."
  (setq old (expand-file-name old)
        new (expand-file-name new))
  (with-temp-buffer
    (let ((status (call-process etaf-diff-diff-program nil t nil
                                "-u" old new)))
      ;; diff(1) exits with 1 when the files differ.
      (unless (memq status '(0 1))
        (error "diff failed for %s and %s: %s" old new (buffer-string))))
    (etaf-diff--highlight)))

(defun etaf-diff--highlight ()
  "Return the diff2html JSON of the unified diff in the current buffer.
An empty diff gives an empty list.  Signal an error when the highlight
script fails."
  (if (= (point-min) (point-max))
      "[]"
    ;; DELETE t replaces the diff text in this buffer with the JSON printed
    ;; on stdout.
    (let* ((node-path (etaf-diff--node-path))
           (process-environment
            (if node-path
                (cons (format "NODE_PATH=%s" node-path) process-environment)
              process-environment))
           (status (call-process-region
                    (point-min) (point-max) etaf-diff-node-program
                    t t nil
                    etaf-diff--highlight-script)))
      (unless (eq status 0)
        (error "etaf-diff-highlight failed: %s" (buffer-string)))
      (buffer-string))))

(defun etaf-diff-parse (json)
  "Parse the diff2html JSON string JSON into a list of file plists.
Each file has :oldName, :newName, :addedLines, :deletedLines and :blocks.
Each block has :header and :lines; each line has :type (\"context\",
\"insert\" or \"delete\"), :content, :oldNumber, :newNumber and :runs, a
list of (TEXT CHANGED) where CHANGED is non-nil for a changed word."
  (json-parse-string json
                     :object-type 'plist
                     :array-type 'list
                     :null-object nil
                     :false-object nil))

(defun etaf-diff--side-rows (lines)
  "Pair diff2html LINES into side-by-side rows.
Return a list of (OLD-LINE . NEW-LINE) conses; either side is nil when
that column has no line in the row.  A run of deletions followed by a
run of insertions is paired line by line."
  (let (rows)
    (while lines
      (if (equal (plist-get (car lines) :type) "context")
          (progn
            (push (cons (car lines) (car lines)) rows)
            (setq lines (cdr lines)))
        (let (dels ins)
          (while (and lines (equal (plist-get (car lines) :type) "delete"))
            (push (pop lines) dels))
          (while (and lines (equal (plist-get (car lines) :type) "insert"))
            (push (pop lines) ins))
          (setq dels (nreverse dels)
                ins (nreverse ins))
          (while (or dels ins)
            (push (cons (pop dels) (pop ins)) rows)))))
    (nreverse rows)))

(defun etaf-diff--run-view (text changed kind)
  "Return the View of TEXT in a diff line of KIND.
CHANGED runs get the stronger background of their kind, like diff2html's
word highlight.  KIND is \"insert\", \"delete\" or nil for context."
  (pcase kind
    ("insert"
     (etaf-view (text :color "#1A5E20"
                      :bgcolor (if changed "#A7E3B0" "#DFF5E1")
                      (expr text))))
    ("delete"
     (etaf-view (text :color "#8B1E24"
                      :bgcolor (if changed "#F3AEB5" "#FBE3E4")
                      (expr text))))
    (_ (etaf-view (text (expr text))))))

(defun etaf-diff--row-view (line number-text)
  "Return the View of diff2html LINE, prefixed by NUMBER-TEXT.
A nil LINE gives an empty row."
  (let ((kind (and line (plist-get line :type)))
        (runs (and line (plist-get line :runs))))
    (etaf-view
     (flex :flex-flow '(row nowrap)
           (expr (etaf-diff--run-view number-text nil kind))
           (expr (mapcar (lambda (run)
                           (etaf-diff--run-view (car run) (cadr run) kind))
                         runs))))))

(defun etaf-diff--line-view (line)
  "Return the View row for diff2html LINE in line style."
  (etaf-diff--row-view
   line
   (format "%5s %5s "
           (or (plist-get line :oldNumber) "")
           (or (plist-get line :newNumber) ""))))

(defun etaf-diff--side-row-view (row)
  "Return the View of one side-by-side ROW of (OLD-LINE . NEW-LINE)."
  (let ((left (etaf-diff--row-view
               (car row)
               (if (car row)
                   (format "%5s " (or (plist-get (car row) :oldNumber) ""))
                 "")))
        (right (etaf-diff--row-view
                (cdr row)
                (if (cdr row)
                    (format "%5s " (or (plist-get (cdr row) :newNumber) ""))
                  ""))))
    (etaf-view
     (flex :flex-flow '(row nowrap)
      (box :flex-basis '(px 0) :flex-grow 1 :flex-shrink 1 :overflow 'hidden
       (expr left))
      (box :width '(px 24))
      (box :flex-basis '(px 0) :flex-grow 1 :flex-shrink 1 :overflow 'hidden
       (expr right))))))

(defun etaf-diff--block-view (block style)
  "Return the View for one hunk BLOCK of a file diff in STYLE."
  (let ((header (plist-get block :header))
        (lines (plist-get block :lines)))
    (etaf-view
     (column
      (text :color "#6B5B95" (expr header))
      (expr (if (eq style 'side)
                (mapcar #'etaf-diff--side-row-view
                        (etaf-diff--side-rows lines))
              (mapcar #'etaf-diff--line-view lines)))))))

(defun etaf-diff--file-name (name)
  "Return NAME without the timestamp diff(1) appends after a tab."
  (car (split-string name "\t")))

(defun etaf-diff--file-view (file style)
  "Return the View for one file FILE of a parsed diff in STYLE."
  (let ((title (format "%s -> %s  (+%d -%d)"
                       (etaf-diff--file-name (plist-get file :oldName))
                       (etaf-diff--file-name (plist-get file :newName))
                       (plist-get file :addedLines)
                       (plist-get file :deletedLines))))
    (etaf-view
     (column
      (box :height '(lh 1))
      (text :font-weight 'bold (expr title))
      (expr (mapcar (lambda (block) (etaf-diff--block-view block style))
                    (plist-get file :blocks)))))))

(defun etaf-diff-view (files &optional style)
  "Return the View that shows the parsed diff FILES in STYLE.
STYLE is `line' (the default) or `side'.  Press q to close the buffer."
  (let ((style (or style etaf-diff-style)))
    (unless (memq style '(line side))
      (error "Unknown etaf-diff style: %S" style))
    (if (null files)
        (etaf-view (text "No differences."))
      (etaf-view
       (column :keys (list (list "q" #'etaf-app-close "Close diff")
                           (list "s" #'etaf-diff-toggle-style "Toggle line/side")
                           (list "n" #'etaf-diff-next-block "Next block")
                           (list "p" #'etaf-diff-previous-block "Previous block")
                           (list "j" #'etaf-diff-next-line "Next changed line")
                           (list "k" #'etaf-diff-previous-line "Previous changed line"))
        (expr (mapcar (lambda (file) (etaf-diff--file-view file style))
                      files)))))))

;;;###autoload
(defun etaf-diff-files (old new &optional style)
  "Show the diff from file OLD to file NEW in an ETAF buffer.
STYLE is `line' or `side'; it defaults to `etaf-diff-style'."
  (interactive
   (list (read-file-name "Old file: " nil nil t)
         (read-file-name "New file: " nil nil t)))
  (etaf-diff--show old new (etaf-diff-parse (etaf-diff-json old new)) style))

;;;###autoload
(defun etaf-diff-open (file &optional style)
  "Show the unified diff in FILE, a .diff file, in an ETAF buffer.
STYLE is `line' or `side'; it defaults to `etaf-diff-style'.  The diff has no
source files, so visiting a line does nothing."
  (interactive
   (list (read-file-name "Diff file: " nil nil t nil
                         (lambda (name)
                           (or (file-directory-p name)
                               (string-suffix-p ".diff" name))))))
  (let ((json (with-temp-buffer
                (insert-file-contents file)
                (etaf-diff--highlight))))
    (etaf-diff--show nil nil (etaf-diff-parse json) style)))

;;;###autoload
(defun etaf-diff-git (args &optional style)
  "Show the output of `git diff' with ARGS in an ETAF buffer.
ARGS is a string of git diff arguments, such as \"--cached\" or \"HEAD~1\".
It is run in `default-directory'.  STYLE is `line' or `side'; it defaults to
`etaf-diff-style'.  Visiting a line of the new side opens the working tree
file; the old side has no source."
  (interactive
   (list (read-string "git diff args: ")))
  (let ((json (with-temp-buffer
                (let ((status (apply #'call-process etaf-diff-git-program nil t nil
                                     "--no-pager" "diff" "--no-color" "--no-ext-diff"
                                     (split-string-and-unquote args))))
                  (unless (eq status 0)
                    (error "git diff failed: %s" (buffer-string))))
                (etaf-diff--highlight)))
        (root (with-temp-buffer
                (unless (eq 0 (call-process etaf-diff-git-program nil t nil
                                            "rev-parse" "--show-toplevel"))
                  (error "Not in a git repository: %s" (buffer-string)))
                (string-trim (buffer-string)))))
    (etaf-diff--show nil nil (etaf-diff-parse json) style root)))

(defun etaf-diff--show (old new files style &optional root)
  "Show the parsed diff FILES of OLD and NEW in STYLE in the diff buffer.
STYLE defaults to `etaf-diff-style'.  OLD and NEW are nil for a diff read
from a .diff file.  ROOT is the git repository root of a git diff.  The
paths, files and style are kept as the application state, so a style switch
can redraw the same diff and a jump can open the source files."
  (let ((style (or style etaf-diff-style)))
    (etaf-app-open etaf-diff-buffer-name
                   (etaf-diff-view files style)
                   :state (list :old (and old (expand-file-name old))
                                :new (and new (expand-file-name new))
                                :root root
                                :files files
                                :style style))))

(defun etaf-diff--state ()
  "Return the state of the current diff buffer."
  (or (etaf-app-state)
      (user-error "Not in an ETAF diff buffer")))

;;;###autoload
(defun etaf-diff-toggle-style ()
  "Switch the diff in the current diff buffer between line and side style."
  (interactive)
  (let ((state (etaf-diff--state)))
    (etaf-diff--show (plist-get state :old) (plist-get state :new)
                     (plist-get state :files)
                     (if (eq (plist-get state :style) 'side) 'line 'side)
                     (plist-get state :root))))

(defun etaf-diff--anchors (files style)
  "Return one anchor plist per buffer line of the diff shown in STYLE.
An anchor has :old and :new, the line numbers its row shows in the old and
the new file, or nil.  Spacer and title rows have neither.  The order
matches the rows of `etaf-diff-view'."
  (let (anchors)
    (dolist (file files)
      (push nil anchors)                ; spacer above the file title
      (push nil anchors)                ; file title
      (dolist (block (plist-get file :blocks))
        (push (list :old (plist-get block :oldStartLine)
                    :new (plist-get block :newStartLine)
                    :name (etaf-diff--file-name (plist-get file :newName)))
              anchors)
        (dolist (entry (if (eq style 'side)
                           (etaf-diff--side-rows (plist-get block :lines))
                         (mapcar (lambda (line) (cons line line))
                                 (plist-get block :lines))))
          (push (list :old (and (car entry) (plist-get (car entry) :oldNumber))
                      :new (and (cdr entry) (plist-get (cdr entry) :newNumber))
                      :name (etaf-diff--file-name (plist-get file :newName))
                      :changed (or (equal (plist-get (car entry) :type) "delete")
                                   (equal (plist-get (cdr entry) :type) "insert")))
                anchors))))
    (nreverse anchors)))

(defun etaf-diff--move-to-change (forward)
  "Move point to the next changed line when FORWARD, else the previous one.
A changed line is a row with a deleted or an inserted line; context rows
are skipped."
  (let* ((state (etaf-diff--state))
         (anchors (etaf-diff--anchors (plist-get state :files)
                                      (plist-get state :style)))
         (current (line-number-at-pos))
         (changed (cl-loop for anchor in anchors
                           for number from 1
                           when (plist-get anchor :changed)
                           collect number))
         (target (if forward
                     (cl-find-if (lambda (number) (> number current)) changed)
                   (cl-find-if (lambda (number) (< number current))
                               (reverse changed)))))
    (unless target
      (user-error "No more changed lines"))
    (goto-char (point-min))
    (forward-line (1- target))))

;;;###autoload
(defun etaf-diff-next-line ()
  "Move point to the next changed line of the diff."
  (interactive)
  (etaf-diff--move-to-change t))

;;;###autoload
(defun etaf-diff-previous-line ()
  "Move point to the previous changed line of the diff."
  (interactive)
  (etaf-diff--move-to-change nil))

(defun etaf-diff--move-to-block (forward)
  "Move point to the next block header when FORWARD, else the previous one.
The header of the current line is skipped in both directions."
  (let ((start (point)))
    (if forward (end-of-line) (beginning-of-line))
    (if (if forward
            (re-search-forward "^@@ " nil t)
          (re-search-backward "^@@ " nil t))
        (beginning-of-line)
      (goto-char start)
      (user-error "No more blocks"))))

;;;###autoload
(defun etaf-diff-next-block ()
  "Move point to the header of the next diff block."
  (interactive)
  (etaf-diff--move-to-block t))

;;;###autoload
(defun etaf-diff-previous-block ()
  "Move point to the header of the previous diff block."
  (interactive)
  (etaf-diff--move-to-block nil))

(defun etaf-diff--target (anchor side-left)
  "Return (FILE . LINE) for the source that ANCHOR shows.
SIDE-LEFT, non-nil in side style, prefers the old file; otherwise the new
file is preferred.  The other side is used when the preferred one is
empty.  Return nil when ANCHOR has no source line or the diff has no source
file for it, as with a .diff file.
A git diff has a :root in the state; its new side is the working tree file
named by the anchor's :name under that root, and its old side is not
supported.  A deleted file has no working tree file, so it has no target."
  (let* ((state (etaf-diff--state))
         (root (plist-get state :root))
         (name (plist-get anchor :name))
         (file (lambda (key)
                 (if (and root name)
                     (and (eq key :new)
                          (let ((path (expand-file-name name root)))
                            (and (file-regular-p path) path)))
                   (plist-get state key))))
         (key (cl-find-if (lambda (k) (and (plist-get anchor k)
                                           (funcall file k)))
                          (if side-left '(:old :new) '(:new :old)))))
    (when key
      (cons (funcall file key)
            (plist-get anchor key)))))

;;;###autoload
(defun etaf-diff-visit-source ()
  "Open the source file at the line shown on the current diff line.
In side style, a point in the left half visits the old file and a point in
the right half visits the new file.  Block headers visit the start of the
block in the new file."
  (interactive)
  (let* ((state (etaf-diff--state))
         (style (plist-get state :style))
         (anchor (nth (1- (line-number-at-pos))
                      (etaf-diff--anchors (plist-get state :files) style)))
         (side-left (and (eq style 'side)
                         (< (current-column) (/ (window-body-width) 2))))
         (target (and anchor (etaf-diff--target anchor side-left))))
    (unless target
      (user-error "No source line for this row"))
    (find-file-other-window (car target))
    (goto-char (point-min))
    (forward-line (1- (cdr target)))))

(provide 'etaf-diff)

;;; etaf-diff.el ends here
