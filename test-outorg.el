;;; test-outorg.el --- ERT suite for outorg.el

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


;;; Dependencies

(unless (featurep 'outorg)
  (signal 'missing-test-dependency "outorg"))
(unless (featurep 'ert-buffer)
  (signal 'missing-test-dependency "ert-buffer"))

;;; Convenience Functions for inserting Tests

(defun outorg-get-preceeding-test-number ()
  "Return number of preceeding test or 0."
  (save-excursion
      (if (re-search-backward
	      (concat
	       "\\(^\(ert-deftest outorg-test-\\)"
	       "\\([[:digit:]]+\\)"
	       "\\( ()$\\)") (point-min) 'NOERROR)
	  (string-to-number (match-string 2)) 0)))
	  
;; fixme
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

;; copied and adapted from undo-tree.el
;; (defun outorg-restore-state-from-register (register)
;;   "Restore undo-tree state from REGISTER.
;; The state must be saved using `undo-tree-save-state-to-register'.
;; Argument is a character, naming the register."
;;   (interactive "*cRestore undo-tree state from register: ")
;;   ;; throw error if undo is disabled in buffer, or if register doesn't contain
;;   ;; an undo-tree node
;;   (let ((data (registerv-data (get-register register))))
;;     (aset data 0 (current-buffer))
;;     (cond
;;      ((eq buffer-undo-list t)
;;       (user-error "No undo information in this buffer"))
;;      ((not (undo-tree-register-data-p data))
;;       (user-error "Register doesn't contain undo-tree state"))
;;      ((not (eq (current-buffer) (undo-tree-register-data-buffer data)))
;;       (message "Register contains undo-tree state for a different buffer")))
;;     ;; transfer entries accumulated in `buffer-undo-list' to `buffer-undo-tree'
;;     (undo-list-transfer-to-tree)
;;     ;; restore buffer state corresponding to saved node
;;     (undo-tree-set (undo-tree-register-data-node data))))


;;; Setup Function

(defun outorg-run-ert-test (fun &optional arg &rest
				function-args)
  (message "current-buf: %s" (current-buffer))
  (let (;; (utree-before (progn
	;; 		(undo-tree-mode t)
	;; 		(undo-tree-visualize)
	;; 		(undo-tree-visualizer-quit)
	;; 		(undo-tree-count buffer-undo-tree)))
	(pref-arg (or arg '(4)))
        ;; utree-after
	saved-undo-tree)
    "Run do/undo ERT test for outorg and return buffer string."
    (when (bufferp "*OUTORG-BEFORE-TEST*")
      (kill-buffer "*OUTORG-BEFORE-TEST*"))
    (when (bufferp "*OUTORG-AFTER-TEST*")
      (kill-buffer "*OUTORG-AFTER-TEST*"))
    (save-restriction
      (widen)
      (copy-to-buffer
       (get-buffer-create "*OUTORG-BEFORE-TEST*")
       (if (member pref-arg '((4)))
	   (point-min)
	 (save-excursion
	   (outline-previous-heading)
	   (point)))
       (if (member pref-arg '((4)))
	   (point-max)
	 (save-excursion
	   (outline-end-of-heading)
	   (point))))
      (outorg-edit-as-org pref-arg)
      (undo-tree-mode t)
      (if function-args
	  (funcall fun function-args)
	(call-interactively fun))
      ;; HACK (otherwise buffer-undo-tree is nil)
      (undo-tree-visualize)
      (undo-tree-visualizer-quit)
      (setq saved-undo-tree buffer-undo-tree)
      (outorg-copy-edits-and-exit)
      (outorg-edit-as-org pref-arg)
      (undo-tree-mode t)
      (org-set-local 'buffer-undo-tree saved-undo-tree)
      (undo-tree-undo (undo-tree-count buffer-undo-tree))
      (outorg-copy-edits-and-exit)
      (copy-to-buffer
       (get-buffer-create "*OUTORG-AFTER-TEST*")
       (if (member pref-arg '((4)))
	   (point-min)
	 (save-excursion
	   (outline-previous-heading)
	   (point)))
       (if (member pref-arg '((4)))
	   (point-max)
	 (save-excursion
	   (outline-end-of-heading)
	   (point)))))))
      ;; (setq utree-after
      ;; 	    (undo-tree-count buffer-undo-tree))
      ;; (when (> utree-after utree-before)
      ;; 	(undo-tree-undo (- utree-after utree-before))))))
      ;; (buffer-substring-no-properties (point-min) (point-max)))))

;;; Tests

;;;; Test Position of Point

;;;;; Conversion to Org

(ert-deftest outorg-test-1 ()
  (should
   (ert-equal-buffer (insert "foo")
		     (insert (buffer-name))
		     '("foo"))))


		     ;; (with-current-buffer "outorg-elisp-test.el"
		     ;;   (buffer-substring-no-properties
		     ;; 	(point-min) (point-max)))
		     ;; (with-current-buffer "outorg-elisp-test.el"
		     ;;   (buffer-substring-no-properties 
		     ;; 	(point-min) (point-max))))))


(ert-deftest outorg-test-2 ()
  "Test the conversion to and from Org via outorg.

For simplicity all outorg tests assume that point is in the
original programming language buffer to be converted to Org-mode
for text editing, exporting, or whatever.

Since outorg is about editing (and thus modifying) a buffer in
Org-mode, defining the expected outcome manually would be bit
cumbersome. Therefore so called 'do/undo' tests (invented and
named by the author) are introduced:

 - do :: convert to org, save original state before editing, edit in
         org, produce and save the diffs between original and final
         state, convert back from org
 - undo :: convert to org again, undo the saved diffs, convert back
           from org

After such an do/undo cyle `compare-buffer-substrings' of
'before' and 'after' state of original buffer. They should be
equal.

Further test topics are point-position and marker existance and
position. "
  ;; (message "current-buf: %s" (current-buffer))
  ;; (let ((case-fold-search t)
  ;; 	;; (old-buf (current-buffer))
  ;; 	)
  ;; 	;; (maj-mode (outorg-get-buffer-mode)))
  ;;   ;; (with-temp-buffer
  ;;   ;;   (insert-buffer-substring old-buf)
  ;;   ;;   (funcall maj-mode)
  ;;     (outorg-run-ert-test 'outline-insert-heading)
  ;;     (should (equal
  ;; 	       (compare-buffer-substrings
  ;; 		"*OUTORG-BEFORE-TEST*" (point-min) (point-max)
  ;; 		"*OUTORG-AFTER-TEST*" (point-min) (point-max))
  ;; 	       0))));)
      (should (equal (current-buffer) "outorg-elisp-test.el")))



;;;;; Conversion from Org

;;;; Do/Undo Tests

;;  1. Call `outorg-edit-as-org'
;;  2. Edit org buffer
;;  3. Call `outorg-copy-edits-and-exit'
;;  4. Call `outorg-edit-as-org' again
;;  5. Undo edits from (2)
;;  6. Call `outorg-copy-edits-and-exit' again
;;  7. Test equality of buffers:
;;     - buffer-size
;;     - compare-buffer-substrings


;;;;; Unedited 

;;  8. Test if `buffer-modified-p' is nil

;;;;; Edited
