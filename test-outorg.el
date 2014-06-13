;;; outorg-test.el --- ERT suite for outorg.el

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
;;   :author_email: tjolitz AT gmail DOT com
;;   :inspiration:  test-org-element.el
;;   :keywords: emacs org-mode comment-editing
;;   :END:


;;; Requires

(require 'ert-buffer)

;;; Dependencies

(unless (featurep 'outorg)
  (signal 'missing-test-dependency "outorg"))
(unless (featurep 'ert-buffer)
  (signal 'missing-test-dependency "ert-buffer"))

;;; Variables

(defvar outorg-test-saved-org-cmd ()
  "Org command to be used in ERT test.")

(defvar outorg-test-saved-major-mode nil
  "Major mode to be used in ERT test.")

(defvar outorg-test-saved-prefix-arg nil
  "Prefix arg to be used in ERT test.")

;;; Non-interactive Functions
;;;; Insert Test Templates

;; FIXME
(defun outorg-get-preceeding-test-number ()
  "Return number of preceeding test or 0."
  (save-excursion
      (if (re-search-backward
	      (concat
	       "\\(^\(ert-deftest outorg-test-\\)"
	       "\\([[:digit:]]+\\)"
	       "\\( ()$\\)") (point-min) 'NOERROR)
	  (string-to-number (match-string 2)) 0)))
	  
;; FIXME
(defun outorg-change-ert-test-numbers (&optional op step beg end)
  "Change test-number with OP by STEP for next tests or in BEG END."
  (let ((incop (or op '+))
	(incstep (or step 1))
	(maxpos (or end (point-max))))
    (save-excursion
      (when beg (goto-char beg))
      (while (re-search-forward
	      (concat
	       "\\(^\(ert-deftest outorg-test-\\)"
	       "\\([[:digit:]]+\\)"
	       "\\( ()$\\)") maxpos 'NOERROR)
	(replace-match
	  (eval
	   `(number-to-string
	     (,incop (string-to-number (match-string 2))
		     ,incstep)))
	   nil nil nil 2)))))

;; FIXME
(defun outorg-insert-ert-test-and-renumber ()
  "Insert ert-test template at point.
Make test number 1 or (1+ number of preceeding test). Increase
test number of all following tests by 1."
  (interactive)
  (insert
   (format "%s%d%s\n%S\n%s\n%s\n%S%s\n"
	   '\(ert-deftest\ outorg-test-
	   (1+ (outorg-get-preceeding-test-number))
	   '\ \(\)
	   "See docstring of `outorg-test-1' for more info."
	   '\(should\ \(equal
	   '(outorg \"foo\")
	   "foo"
	   '\)\)\)))
  (indent-region (save-excursion (backward-sexp) (point)) (point))
  (outorg-change-ert-test-numbers))

;;;; Setup Function

(defun outorg-test-cmd ()
  (interactive)
  (let ((pref-arg '(4))
	saved-undo-tree)
    (funcall outorg-test-saved-major-mode)
    (outorg-edit-as-org
     outorg-test-saved-prefix-arg)
    (undo-tree-mode t)
    (call-interactively
     outorg-test-saved-org-cmd)
    ;; necessary (?) HACK to fill buffer-undo-tree
    (undo-tree-visualize)
    (undo-tree-visualizer-quit)
    (setq saved-undo-tree buffer-undo-tree)
    (outorg-copy-edits-and-exit)
    (outorg-edit-as-org 
     outorg-test-saved-prefix-arg)
    (undo-tree-mode t)
    (org-set-local 'buffer-undo-tree saved-undo-tree)
    (undo-tree-undo (undo-tree-count buffer-undo-tree))
    (outorg-copy-edits-and-exit)))


;;; Commands 
;;;; Run ERT Tests

(defun outorg-test-run-ert (org-cmd)
  "Prepare and run ERT tests."
  (interactive "P\nCOrg Command: ")
  (let ((old-buf (current-buffer))
	(maj-mode (outorg-get-buffer-mode)))
    ;; necessary (?) HACK
    (setq outorg-test-saved-org-cmd org-cmd)
    (setq outorg-test-saved-major-mode maj-mode)    
    (setq outorg-test-saved-prefix-arg current-prefix-arg)
    (save-restriction
      (widen)
      (with-current-buffer
	  (get-buffer-create "*outorg-test-buffer*")
	(erase-buffer)
	(insert-buffer-substring old-buf)
	(funcall maj-mode)
	(call-interactively 'ert-run-tests-interactively)))))

;;; Tests

;;;; Test Position of Point

;;;;; Conversion to Org

;; (defun my-forward-back ()
;;   (interactive)
;;   (undo-tree-mode 1)
;;   (save-excursion
;;     (goto-char 318)
;;     (newline)
;;     (forward-line -1)
;;     (insert "foo")
;;     (goto-char (point-at-bol))
;;     (kill-line)
;;     (kill-line)
;;     ;; (deactivate-mark 'FORCE)
;;     ))

(ert-deftest outorg-test-conversion ()
  "Test outorg conversion to and from Org.

This test assumes that it is called via user command
`outorg-test-run-ert' with point in the original programming
language buffer to be converted to Org-mode, and with the prefix
argument that should be used for `outorg-edit-as-org'. It further
relies on the `ert-buffer' library for doing its work.

Since outorg is about editing (and thus modifying) a buffer in
Org-mode, defining the expected outcome manually would be bit
cumbersome. Therefore so called 'do/undo' tests (invented and
named by the author) are introduced:

 - do :: convert to org, save original state before editing, edit
         in org, produce and save the diffs between original and
         final state, convert back from org

 - undo :: convert to org again, undo the saved diffs, convert
           back from org

After such an do/undo cyle the test buffer should be in exactly
the same state as before the test, i.e.

 - buffer content after the test should be string-equal to buffer
   content before

 - point should be in the same position

 - the mark should be in the same position

These are actually the three criteria checked by the 'ert-buffer'
library, and when one or more of the checks returns nil, the ert test
fails.

This test is a one-size-fits-all test for outorg, since it
allows, when called via command `outorg-test-run-ert', to execute
arbitrary Org-mode commands in the *outorg-edit-buffer* and undo
the changes later on, checking for any undesired permanent side
effects of the conversion process per se."
  (let ((curr-buf-initial-state
	 (with-current-buffer "*outorg-test-buffer*"
	   ;; (deactivate-mark 'FORCE)
	   (ert-Buf-from-buffer))))
    ;; (cmd outorg-test-cmd))
    (should
     (ert-equal-buffer
      (outorg-test-cmd)
      curr-buf-initial-state
      t))))

;; (ert-deftest outorg-test-2 ()
;;   "Test the conversion to and from Org via outorg.

;;       (should (equal (current-buffer) "outorg-elisp-test.el")))



;; ;;;;; Conversion from Org

;; ;;;; Do/Undo Tests

;; ;;  1. Call `outorg-edit-as-org'
;; ;;  2. Edit org buffer
;; ;;  3. Call `outorg-copy-edits-and-exit'
;; ;;  4. Call `outorg-edit-as-org' again
;; ;;  5. Undo edits from (2)
;; ;;  6. Call `outorg-copy-edits-and-exit' again
;; ;;  7. Test equality of buffers:
;; ;;     - buffer-size
;; ;;     - compare-buffer-substrings


;; ;;;;; Unedited 

;; ;;  8. Test if `buffer-modified-p' is nil

;; ;;;;; Edited

;;; Run hooks and provide
;;; outorg-test.el ends here
