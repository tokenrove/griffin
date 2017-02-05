;;; griffin.el --- a static blog generator à la Jekyll -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2012 Julian Squires.
;;
;; Author: Julian Squires <julian@cipht.net>
;; Created: June 28, 2012
;; Keywords: outlines, hypermedia, wp
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for the GNU General Public License.
;;
;;; Install:
;;
;; Ensure griffin.el is in your load-path and add the following to your init:
;;     (autoload 'griffin "griffin" "Compile static blog" t)
;;
;;; Commentary:
;;
;;
;; Template parameters live in .griffin.el and the header sexp.
;; Contents within {{ }} in your templates are eval'd.  Don't put this
;; in front of users.
;;
;; Caveats (aside from the big one):
;; - contents of template must be a single read expression.  Anything
;;   beyond the first read is ignored.
;; - this will get slow if the environment gets huge, since no pruning
;;   takes place.
;;
;; Intentional simplifications:
;; - no nested layouts
;; - no grouping of posts
;;
;; Bugs:
;; - should extract org-mode meta information instead of requiring the
;;   griffin sexp.
;; - better documentation is required.
;; - the initials are HG but it's designed for git.
;; - we require CL.
;;
;;; Code:

(eval-when-compile
  (require 'cl))
(require 'ox)

(eval-and-compile
  (autoload 'union "cl")
  (autoload 'reduce "cl"))

(defgroup griffin nil "Hawley Griffin blog engine"
  :version "24.2" :group 'applications)
(defcustom griffin-template-file-regexp "\\.\\(org\\|html\\|xml\\)"
  "Names of templatable files.

We only look at files whose name matches this regexp, to avoid
trying to template arbitrary binary files."
  :type '(regexp) :group 'griffin)
(defcustom griffin-template-regexp "{{\\([\0-\377[:nonascii:]]*?\\)}}"
  "The regexp used to match blocks to be expanded in templates.

The first match data is the sexp to evaluate."
  :type '(regexp) :group 'griffin)
(defcustom griffin-post-regexp "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)-\\(.*?\\)\\.\\([A-Za-z]+\\)$"
  "The regular expression used to match post filenames.

The first match is the date, the second the slug, and the third
the format."
  :type '(regexp) :group 'griffin)
(defcustom griffin-output-directory "_site"
  "The directory where the compiled site should go."
  :type '(directory) :group 'griffin)
(defcustom griffin-environment nil
  "The base environment for templating."
  :type '(alist)
  :group 'griffin)

(defvar griffin-conversion-buffer-name "*griffin-conversion-buffer*")
(defvar griffin-base-directory)

(defun griffin-lookup-template (string env)
  (eval
   `(let (,@(loop for (k . v) in env
                  collect (list k (if (and (listp v) (not (eq 'quote (car v)))) (list 'quote v) v))))
      ,(car (read-from-string string)))
   t))

(defun griffin-present-value (v) (with-output-to-string (princ v)))

(defun griffin-templatify (string env)
  (griffin-present-value (griffin-lookup-template string env)))

(defun griffin-expand-template-string (string environment)
  (griffin-replace-regexp-in-string griffin-template-regexp
                                    (lambda (s) (griffin-templatify s environment))
                                    string t t))

(defun griffin-env-union (&rest rest) (reduce (lambda (a b) (union a b :key #'car)) rest))
(defun griffin-read-default-env ()
  (with-current-buffer (find-file-noselect (concat griffin-base-directory "/.griffin.el"))
    (save-excursion
      (goto-char (point-min))
      (ignore-errors
        (let ((x (read (current-buffer))))
          (when (eq :griffin (car x)) (cdr x)))))))

(defun griffin-input-filename (file)
  (if (file-name-absolute-p file) file (concat griffin-base-directory "/" file)))

(defun griffin-output-filename (file)
  (when (file-name-absolute-p file) (setf file (file-relative-name file griffin-base-directory)))
  (concat griffin-output-directory "/" file))

;;;###autoload
(defun griffin (base-directory)
  "Compiles a static blog.

Run griffin on the BASE-DIRECTORY of the blog.  Files that start
with an s-expression of the form (:griffin <alist entries>) are
processed specially.  The alist entries form the environment in
which the file's templates are processed.

First, anything in the _posts directory matching
GRIFFIN-POST-REGEXP is processed as a blog post and written out
to GRIFFIN-OUTPUT-DIRECTORY/yyyy/mm/dd/slug.html.  In the current
version, these should be `org-mode' files with the :griffin sexp
header.  These are processed using layout post, which means
_layouts/post.html is used as the template around them.

The rest of the files are processed, either being copied into the
output directory if they do not contain a :griffin sexp header,
or being processed as templates if they do.

The file .griffin.el in the root of the base directory is read as
the default environment included in all others."
  (interactive "DBase directory: ")
  (save-some-buffers)
  (let ((griffin-base-directory base-directory)
        (griffin-output-directory (if (file-name-absolute-p griffin-output-directory)
                                      griffin-output-directory
                                    (concat base-directory "/" griffin-output-directory))))
    (make-directory griffin-output-directory t)
    (let ((env (griffin-env-union
                `((base-directory . ,base-directory)
                  (posts . ,(griffin-generate-posts)))
                (griffin-read-default-env))))
      (griffin-walk-others (list base-directory) env))))

(defun griffin-walk-others (directories environment)
  (while directories
    (dolist (file (directory-files (pop directories) t "^[^._].*[^~]$\\|^[^.~]$" t))
      (if (file-directory-p file)
          (push file directories)
        (griffin-maybe-template-file file environment)))))

(defun griffin-time<-iso8601-date (date-string)
  (apply #'encode-time (mapcar (lambda (x) (or x 0)) (nbutlast (parse-time-string date-string) 3))))

(defun griffin-escape-xml (string)
  (replace-regexp-in-string "[&<>]"
                            (lambda (x) (cond ((string= x "&") "&amp;")
                                              ((string= x "<") "&lt;")
                                              ((string= x ">") "&gt;")
                                              (t x)))
                            string t t))

(defun griffin-generate-posts ()
  (let ((posts nil))
    (dolist (file (directory-files (concat griffin-base-directory "/_posts") nil "[^~]$") posts)
      (when (string-match griffin-post-regexp file)
        (push (griffin-process-post file
                                    (griffin-time<-iso8601-date (match-string 1 file))
                                    (match-string 2 file)
                                    (match-string 3 file))
              posts)))))

(defun griffin-process-post (file date slug format)
  (let* ((url (format "%s/%s.html" (format-time-string "/%Y/%m/%d" date) slug))
         (in (griffin-input-filename (concat "_posts/" file)))
         (out (griffin-output-filename (substring url 1)))
         (env
          (griffin-env-union
           (griffin-read-default-env)
           `((date . ,date)
             (slug . ,slug)
             (url . ,url))
           (cond ((string= format "html") (griffin-read-file in))
                 ((string= format "org") (griffin-convert-org in))
                 (t (message "Don't know how to convert format `%s'; ignoring post %s" format file)
                    (throw 'unknown-format nil))))))
    ;; feed html through templating to produce _site/yyyy/mm/dd/post.html
    (make-directory (file-name-directory out) t)
    (with-temp-file out
      (insert (griffin-apply-layout "post" env)))
    env))

(defun griffin-read-leading-sexp ()
  (goto-char 2)
  (let ((sexp (ignore-errors (read (current-buffer)))))
    (when (and (listp sexp) (eq :griffin (first sexp)))
      (goto-char (point-min))
      (forward-line 1)
      (rest sexp))))

(defun griffin-convert-org (file)
  (griffin-visit-file
   file
   (lambda ()
     (let ((env (griffin-read-leading-sexp))
           (griffin-conversion-buffer-name "*Org HTML Export*")
           (org-html-preamble nil)
           (org-html-postamble nil)
           (org-src-fontify-natively t))
       (unless env (throw 'not-a-template-file nil))
       (prog1
           (with-current-buffer (org-html-export-as-html)
             (cons (cons 'content (buffer-substring-no-properties (point-min) (point-max))) env))
         (kill-buffer griffin-conversion-buffer-name))))))

(defun griffin-maybe-template-file (file environment)
  (let ((in (griffin-input-filename file)) (out (griffin-output-filename file)))
    (unless (and (string-match griffin-template-file-regexp file)
                 (catch 'not-a-template-file
                   (griffin-expand-template-file file environment)))
      (make-directory (file-name-directory out) t)
      (copy-file in out t))))

(defun griffin-visit-file (file fn)
  (let* ((was-visiting-p (find-buffer-visiting file)) buffer)
    (prog1
        (with-current-buffer (find-file-noselect file)
          (setf buffer (current-buffer))
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (funcall fn))))
      (unless was-visiting-p (kill-buffer buffer)))))

(defun griffin-read-file (file)
  (griffin-visit-file
   file
   (lambda ()
     (let ((env (griffin-read-leading-sexp)))
       (unless env (throw 'not-a-template-file nil))
       (cons (cons 'content (buffer-substring-no-properties (point) (point-max))) env)))))

(defun griffin-expand-template-file (file environment)
  "Merges values from FILE-LOCAL-VARIABLES-ALIST into ENVIRONMENT."
  (let ((env (griffin-env-union environment
                                (griffin-read-file (griffin-input-filename file))
                                `((file . ,file)
                                  (url . ,(concat "/" (file-relative-name file griffin-base-directory)))))))
    (let ((content (assq 'content env))
          (layout (assq 'layout env))
          output)
      (setf (cdr content)
            (griffin-expand-template-string (cdr content) env))
      (setf output (cdr content))
      (when (and layout (cdr layout))
        (setf output (griffin-apply-layout (substring (symbol-name (cdr layout)) 1) env)))
      (with-temp-file (griffin-output-filename file)
        (insert output))
      t)))

(defun griffin-apply-layout (layout env)
  (let* ((layout-file (concat griffin-base-directory "/_layouts/" layout ".html"))
         (layout-template (griffin-visit-file layout-file (lambda () (buffer-string)))))
    (griffin-expand-template-string layout-template env)))


(defun griffin-join (str list) (reduce (lambda (x y) (concat x str y)) list :key #'symbol-name))

(defun griffin-replace-regexp-in-string (regexp rep string &optional
                                                fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING.

As per replace-regexp-in-string, except REP is passed
match-string 1 instead of 0.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function, it is called with the actual text of each
match, and its value is used as the replacement text.  When REP is called,
the match data are the result of matching REGEXP against a substring
of STRING.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\\\(foo\\\\).*\\\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\""

  ;; To avoid excessive consing from multiple matches in long strings,
  ;; don't just call `replace-match' continually.  Walk down the
  ;; string looking for matches of REGEXP and building up a (reversed)
  ;; list MATCHES.  This comprises segments of STRING which weren't
  ;; matched interspersed with replacements for segments that were.
  ;; [For a `large' number of replacements it's more efficient to
  ;; operate in a temporary buffer; we can't tell from the function's
  ;; args whether to choose the buffer-based implementation, though it
  ;; might be reasonable to do so for long enough STRING.]
  (let ((l (length string))
        (start (or start 0))
        matches str mb me)
    (save-match-data
      (while (and (< start l) (string-match regexp string start))
        (setq mb (match-beginning 0)
              me (match-end 0))
        ;; If we matched the empty string, make sure we advance by one char
        (when (= me mb) (setq me (min l (1+ mb))))
        ;; Generate a replacement for the matched substring.
        ;; Operate only on the substring to minimize string consing.
        ;; Set up match data for the substring for replacement;
        ;; presumably this is likely to be faster than munging the
        ;; match data directly in Lisp.
        (string-match regexp (setq str (substring string mb me)))
        (setq matches
              (cons (replace-match (if (stringp rep)
                                       rep
                                     (funcall rep (match-string 1 str)))
                                   fixedcase literal str subexp)
                    (cons (substring string start mb) ; unmatched prefix
                          matches)))
        (setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))

(provide 'griffin)
(run-hooks 'griffin-run-hook)

;; griffin.el ends here


(provide 'griffin)

;;; griffin.el ends here
