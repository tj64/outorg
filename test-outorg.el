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

;;; Tests

;;;; Test Position of Point

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
