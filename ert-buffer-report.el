;;; ert-buffer-report.el --- Create Org report for ert-buffer test

;; Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
;; Version: 1.0
;; URL: https://github.com/tj64/outorg

;;;; MetaData
;;   :PROPERTIES:
;;   :copyright: Thorsten Jolitz
;;   :copyright-years: 2014+
;;   :version:  1.0
;;   :licence:  GPL 2 or later (free software)
;;   :licence-url: http://www.gnu.org/licenses/
;;   :part-of-emacs: no
;;   :author: Thorsten Jolitz
;;   :author_emails: <tjolitz AT gmail DOT com
;;   :inspiration:  org-src
;;   :keywords: emacs org-mode comment-editing
;;   :git-repo: https://github.com/tj64/outorg
;;   :git-clone: git://github.com/tj64/outorg.git
;;   :END:

;;;; Commentary

;; Load this file after loading 'ert-buffer.el', it will then redefine
;; `ert--compare-test-with-buffer' such that it not only does its
;; usual work, but produces an Org-mode report file with the test
;; results as side-effect. Adapt `ert-buffer-report-temp-dir' to your
;; file-system to make this work for you. If 'diff' isn't your default
;; diff executable, you need to adapt
;; `ert-buffer-report-diff-executable' too.

;;;; Credits

;; The original author of `ert--compare-test-with-buffer' is of course
;; the author of `ert-buffer.el', Stefan Merten, and all his original
;; code is still contained in the overwritten function.

;;; Variables

;;;; Consts

(defconst ert-buffer-report-temp-dir "~/junk/tmp-ert/")
(defconst ert-buffer-report-diff-executable "diff")

;;;; Vars

(defvar ert-buffer-report-insert-buffer-strings-p nil)

;;; Functions

;;;; Helper Function

(defun tj/ert-buffer-report-toggle-insert-buffer-strings ()
  (interactive)
  (if ert-buffer-report-insert-buffer-strings-p
      (setq ert-buffer-report-insert-buffer-strings-p nil)
    (setq ert-buffer-report-insert-buffer-strings-p t))
  (message "ERT-report insert buffer strings is: %s"
	   ert-buffer-report-insert-buffer-strings-p))

;;;; Overwritten Funtion

(defun ert--compare-test-with-buffer (result buf ignore-return exp-return)
  "Compare RESULT of test with expected buffer BUF.
RESULT is a return value from `ert--run-test-with-buffer'.
Return a list of booleans where t stands for a successful test of
this kind:

* Content of output buffer
* Point in output buffer
* Return value

IGNORE-RETURN, EXP-RETURN are described in `ert--equal-buffer'."
  (let ((act-return (car result))
	(act-buf (cdr result)))
    (let* ((temporary-file-directory ert-buffer-report-temp-dir)
	   (ert-report (make-temp-file "ert-report" nil ".org"))
	   (diff-strg
	    (shell-command-to-string
	     (format "%s %s %s"
		     ert-buffer-report-diff-executable
		     (let ((tmp-file-buf (make-temp-file "buf")))
		       (with-current-buffer
			   (find-file-noselect tmp-file-buf)
			 (insert (ert-Buf-content buf))
			 (save-buffer)
			 (kill-buffer))
		       (chmod tmp-file-buf 438)
		       tmp-file-buf)
		     (let ((tmp-file-act-buf
			    (make-temp-file "act-buf")))
		       (with-current-buffer
			   (find-file-noselect tmp-file-act-buf)
			 (insert (ert-Buf-content act-buf))
			 (save-buffer)
			 (kill-buffer))
		       (chmod tmp-file-act-buf 438)
		       tmp-file-act-buf)))))
      (chmod ert-report 438)
      (with-current-buffer
	  (find-file-noselect ert-report)
	(org-mode)
	;; 1st Level *
	(org-insert-heading)
	(insert "ERT Test Report ")
	(org-insert-time-stamp nil t)
	(org-end-of-meta-data-and-drawers)
	(org-insert-heading '(4))
	;; 2nd Level **
	(insert "Point, Mark and Content Lenght")
	(org-demote-subtree)
	(org-end-of-meta-data-and-drawers)
	(newline)
	(insert
	 (format
	  (concat
	   " - Point position :: %s -> %s\n"
	   " - Mark position :: %s -> %s\n"
	   " - Content length :: %d -> %d\n")
	  (ert-Buf-point buf) (ert-Buf-point act-buf)
	  (ert-Buf-mark buf) (ert-Buf-mark act-buf)
	  (length (ert-Buf-content buf))
	  (length (ert-Buf-content act-buf))))
	(newline)
	;; 2nd Level **
 	(org-insert-heading '(4))
	(insert "Content DIFF")
	(org-end-of-meta-data-and-drawers)
	(if (org-string-nw-p diff-strg)
	    (insert
	     (format
	      "#+begin_quote\n%s\n#+end_quote\n"
	      diff-strg))
	  (insert "   [no-diffs]"))
	(newline)
	;; 2nd Level **
	(org-insert-heading '(4))
	(insert "Buffer Strings")
	(org-end-of-meta-data-and-drawers)
	;; 3rd Level ***
	(org-insert-heading '(4))
	(insert "Buffer String BEFORE")
	(org-demote-subtree)
	(org-end-of-meta-data-and-drawers)
	(newline)
	(if (not ert-buffer-report-insert-buffer-strings-p)
	    (insert "   [buffer-string omitted]")
	  (insert "#+begin_quote\n\n")
	  (insert (ert-Buf-string buf))
	  (insert "\n#+end_quote\n"))
	(newline 2)
	;; 3rd Level ***
	(org-insert-heading '(4))
	(insert "Buffer String AFTER")
	(org-end-of-meta-data-and-drawers)
	(if (not ert-buffer-report-insert-buffer-strings-p)
	    (insert "   [buffer-string omitted]")
	  (insert "#+begin_quote\n\n")
	  (insert (ert-Buf-string act-buf))
	  (insert "\n#+end_quote\n"))
	(newline)))
    (list
     (or (not buf)
	 (equal (ert-Buf-content act-buf) (ert-Buf-content buf)))
     (or
      (not buf)
      (not (ert-Buf-point buf))
      (equal (ert-Buf-point act-buf) (ert-Buf-point buf)))
     (or ignore-return
	 (equal act-return exp-return)))))


;;; Run Hooks and Provide

(provide 'ert-buffer-report)

;; ert-buffer-report.el ends here
