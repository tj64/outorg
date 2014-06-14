;;; outorg-test.el --- ERT suite for outorg.el

;; Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
;; Version: 0.0
;; URL: https://github.com/tj64/outorg

;;;; MetaData
;;   :PROPERTIES:
;;   :copyright: Thorsten Jolitz
;;   :copyright-years: 2014+
;;   :version:  0.9
;;   :licence:  GPL 2 or later (free software)
;;   :licence-url: http://www.gnu.org/licenses/
;;   :part-of-emacs: no
;;   :author: Thorsten Jolitz
;;   :author_email: tjolitz AT gmail DOT com
;;   :inspiration:  test-org-element.el
;;   :keywords: emacs org-mode ert buffer
;;   :git-repo: https://github.com/tj64/outorg
;;   :git-clone: git://github.com/tj64/outorg.git
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

;;; Functions

(defun outorg-test-cmd ()
  "Command to be used inside `ert-deftest'"
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


(defun outorg-test-run-ert (org-cmd &optional arg)
  "Prepare and run ERT test.

This command records the major-mode of current-buffer in global
variable `outorg-test-saved-major-mode', the given
prefix-argument in `outorg-test-saved-prefix-arg' and the given
ORG-CMD in `outorg-test-saved-org-cmd', and it copies the content
of current buffer into a temporary *outorg-test-buffer* and sets its major-mode.

After this preparation it calls ERT test `outorg-test-conversion'
that makes use of the *outorg-test-buffer* and the global
variables mentioned above."
  (interactive "COrg Command: \nP")
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
	;; (call-interactively 'ert-run-tests-interactively)
	(funcall 'ert-run-tests-interactively
		 "outorg-test-conversion")
	))))

;;; Tests

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

 - the mark should be in the same position (or nil)

These are actually the three criteria checked by the 'ert-buffer'
library, and when one or more of the checks returns nil, the ert
test fails.

This test is a one-size-fits-all test for outorg, since it
allows, when called via command `outorg-test-run-ert', to execute
arbitrary Org-mode commands in the *outorg-edit-buffer* and undo
the changes later on, checking for any undesired permanent side
effects of the conversion process per se."
  (let ((curr-buf-initial-state
	 (with-current-buffer "*outorg-test-buffer*"
	   (ert-Buf-from-buffer))))
    (should
     (ert-equal-buffer
      (outorg-test-cmd)
      curr-buf-initial-state
      t))))

;;; Run hooks and provide

(provide 'outorg-test)

;;; outorg-test.el ends here
