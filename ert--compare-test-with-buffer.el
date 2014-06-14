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
    (message
     (concat "\nBEFORE: \n\npoint: %s \n\nmark: %s"
	     "\nAFTER:  \n\npoint: %s \n\nmark: %s")
     (ert-Buf-point buf)
     (ert-Buf-mark buf)
     (ert-Buf-point act-buf)
     (ert-Buf-mark act-buf))
    (message
     (concat "\nBEFORE:\n\ntxt: \n%S"
	     "\nAFTER:\n\ntxt: \n%S")
     (ert-Buf-string buf)
     (ert-Buf-string act-buf))
    (message
     (concat "\nBEFORE:lenght: %d"
	     "\nAFTER: length: %d")
     (length (ert-Buf-string buf))
     (length (ert-Buf-string act-buf)))
    (list
     (or (not buf)
	 (equal (ert-Buf-content act-buf) (ert-Buf-content buf)))
     (or
      (not buf)
      (not (ert-Buf-point buf))
      (equal (ert-Buf-point act-buf) (ert-Buf-point buf)))
     (or ignore-return
	 (equal act-return exp-return)))))
