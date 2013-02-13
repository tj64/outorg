;; * outorg2.el --- Org-style comment editing

;; ** Copyright

;; Copyright (C) 2013 Thorsten Jolitz
;; This file is not (yet) part of GNU Emacs

;; Author: Thorsten Jolitz  (format "tjolitz%sgmail%s" "@" ".com")

;; ** Credits

;; This library is inspired by the way source-blocks can be edited in temporary
;; edit files in Eric Schulte's and Dan Davidson's 'Org-babel'. 
;; (http://orgmode.org/worg/org-contrib/babel/).

;; ** Commentary

;; `outorg2' is like "reverse Org-babel": editing of comment-sections from source
;; code files in temporary Org-mode buffers instead of editing of Org-mode
;; source-blocks in temporary source-code buffers. 

;; ** Emacs Version

;; `outorg2.el' works with [GNU Emacs 24.2.1 (x86_64-unknown-linux-gnu, GTK+
;; Version 3.6.4) of 2013-01-20 on eric]. No attempts of testing with older
;; versions or other types of Emacs have be made (yet).

;; ** Installation

;; Insert
;; (require 'outorg2)
;; in your .emacs file to install.  If you want a different prefix
;; key, insert first
;; (defvar outline-minor-mode-prefix "\C-c")
;; or whatever.  The prefix can only be changed before outline (minor)
;; mode is loaded.

;; ** ChangeLog

;; | date            | author(s)       | version |
;; |-----------------+-----------------+---------|
;; | <2013-02-11 Mo> | Thorsten Jolitz |     0.9 |

;; ** Bugs

;; `outorg2' is line-based, it only works with 'one-line' comments, i.e. with
;; comment-sections like those produced by `comment-region' (a command that
;; comments or uncomments each line in the region). Those special multi-line
;; comments found in many programming languages are not recognized and lead to
;; undefined behaviour.

;; ** Emacs Version

;; `outorg2.el' works with [GNU Emacs 24.2.1 (x86_64-unknown-linux-gnu, GTK+
;; Version 3.6.4) of 2013-01-20 on eric]. No attempts of testing with older
;; versions or other types of Emacs have be made (yet).

;; ** Installation

;; Insert
  ;; (require 'outorg2)
;; in your .emacs file to install. 

;; ** ChangeLog

;; | date            | author(s)       | version |
;; |-----------------+-----------------+---------|
;; | <2013-02-13 Mi> | Thorsten Jolitz |     0.9 |

;; ** Bugs

;; `outorg2' is line-based, it only works with 'one-line' comments, i.e. with
;; comment-sections like those produced by `comment-region' (a command that
;; comments or uncomments each line in the region). Those special multi-line
;; comments found in many programming languages are not recognized and lead to
;; undefined behaviour.

;; * Requires

(require 'outline)
(require 'org)

;; * Variables

;; ** Consts

(defconst outorg2-version "0.9"
  "outorg2 version number.")

;; ** Vars

(defvar outline-minor-mode-prefix "\C-c"
  "New outline-minor-mode prefix.")

(defvar outorg2-edit-whole-buffer-p nil
  "Non-nil if the whole code-buffer is edited.")

(defvar outorg2-initial-window-config nil
  "Initial window-configuration when editing as Org.") 

;; ** Hooks

(defvar outorg2-hook nil
  "Functions to run after `outorg2' is loaded.")

;; ** Customs

;; *** Custom Groups

;; (defgroup outorg2 nil
;;   "Library for outline navigation and Org-mode editing in Lisp buffers."
;;   :prefix "outorg2-"
;;   :group 'lisp 'outlines
;;   :link '(url-link "http://emacswiki.org/emacs/OutlineMinorMode"))


;; *** Custom Vars

;; * Functions

;; ** Non-interactive Functions

;; *** Get buffer major mode

(defun outorg2-get-buffer-mode (buffer-or-string)
  "Return major mode of BUFFER-OR-STRING."
  (with-current-buffer buffer-or-string
     major-mode))


;; ;; *** Calculate the outline-regexp

;; (defun outorg2-calc-outline-regexp ()
;;   "Calculate the outline regexp for the current mode."
;;   (let* ((comment-start-no-space
;;           (replace-regexp-in-string
;;            "[[:space:]]+" "" comment-start))
;;          (comment-start-region
;;           (if (and
;;                comment-end
;;                (not (string-equal "" comment-end)))
;;               comment-start-no-space
;;             (concat
;;              comment-start-no-space comment-start-no-space))))
;;     ;; the "^" not needed by outline, but by outorg2 (?)
;;     (concat "^" comment-start-region " [*]+ ")))

;; ;; *** Calculate the outline-level

;; (defun outorg2-calc-outline-level ()
;;   "Calculate the right outline level for the outorg2-outline-regexp"
;;   (save-excursion
;;     (save-match-data
;;       (let ((len (- (match-end 0) (match-beginning 0))))
;;         (- len (+ 2 (* 2 (length (format "%s" comment-start))))))))) 


;; ;; *** Set outline-regexp und outline-level

;; (defun outorg2-set-local-outline-regexp-and-level (regexp &optional fun)
;;    "Set `outline-regexp' locally to REGEXP and `outline-level' to FUN."
;; 	(make-local-variable 'outline-regexp)
;; 	(setq outline-regexp regexp)
;; 	(and fun
;;              (make-local-variable 'outline-level)
;;              (setq outline-level fun)))

;; ;; *** Outorg2 hook-functions

;; (defun outorg2-hook-function ()
;;   "Add this function to outline-minor-mode-hook"
;;   (let ((out-regexp (outorg2-calc-outline-regexp)))
;;     (outorg2-set-local-outline-regexp-and-level
;;      out-regexp 'outorg2-calc-outline-level)))

;; (add-hook 'outline-minor-mode-hook 'outorg2-hook-function)

;; ** Commands

;; *** Edit as Org 

(defun outorg2-edit-as-org (arg)
  "Convert and copy to temporary Org buffer
With ARG, edit the whole buffer, otherwise the current subtree."
  (interactive "P")
  (setq outorg2-code-buffer-marker (point-marker))
  (and arg (setq outorg2-edit-whole-buffer-p t))
  (setq outorg2-initial-window-config
        (current-window-configuration))
  (outorg2-copy-and-convert))

(defun outorg2-save-edits ()
  "Replace code-buffer content with (converted) edit-buffer content and
  kill edit-buffer"
  (interactive)
  (widen)
  (funcall
   (outorg2-get-buffer-mode
    (marker-buffer outorg2-code-buffer-marker)))
  (outorg2-convert-back-to-code)
  (outorg2-replace-code-with-edits)
  (kill-buffer
   (marker-buffer outorg2-edit-buffer-marker))
  (set-window-configuration
   outorg2-initial-window-config)
  ;; (switch-to-buffer
  ;;  (marker-buffer outorg2-code-buffer-marker))
  ;; (goto-char
  ;;  (marker-position outorg2-code-buffer-marker))
  (outorg2-reset-global-vars))

;; *** Edit as Org-file

(defun outorg2-copy-and-convert ()
  "Copy code buffer content to tmp-buffer and convert it to Org syntax.
If WHOLE-BUFFER-P is non-nil, copy the whole buffer, otherwise
  the current subtree."
  (let* ((edit-buffer
          (get-buffer-create "*outorg2-edit-buffer*")))
    (save-restriction
      (with-current-buffer edit-buffer (erase-buffer))
      (widen)
      ;; copy code buffer content
      (copy-to-buffer
       edit-buffer
       (if outorg2-edit-whole-buffer-p
           (point-min)
         (save-excursion
           (outline-back-to-heading 'INVISIBLE-OK)
           (point)))
       (if outorg2-edit-whole-buffer-p
           (point-max)
         (save-excursion
           (outline-end-of-subtree)
           (point)))))
    ;; switch to edit buffer
    (if (one-window-p) (split-window-sensibly (get-buffer-window)))
    (switch-to-buffer-other-window edit-buffer)
    (and outorg2-edit-whole-buffer-p
         (goto-char
          (marker-position outorg2-code-buffer-marker)))
    (setq outorg2-edit-buffer-marker (point-marker)))
  ;; activate programming language major mode and convert to org
  (funcall (outorg2-get-buffer-mode
            (marker-buffer outorg2-code-buffer-marker)))
  (outorg2-convert-to-org)
  ;; change major mode to org-mode
  (org-mode)
  (if outorg2-edit-whole-buffer-p
      (progn
        (org-first-headline-recenter)
        (hide-sublevels 3)
        (goto-char
         (marker-position outorg2-edit-buffer-marker))
        (show-subtree))
    (goto-char
     (marker-position outorg2-edit-buffer-marker))
    (show-all)))

(defun outorg2-convert-to-org ()
  "Convert file content to Org Syntax"
  (let* ((last-line-comment-p nil)
         (mode-name
          (format
           "%S" (with-current-buffer
                    (marker-buffer outorg2-code-buffer-marker)
                  major-mode)))
         (splitted-mode-name
          (split-string mode-name "-mode"))
         (language-name
          (if (> (length splitted-mode-name) 1)
              (car splitted-mode-name)
            (car (split-string mode-name "\\."))))
         (in-org-babel-load-languages-p
          (assq
           (intern language-name)
           org-babel-load-languages)))
    (goto-char (point-min))
    (outorg2-remove-trailing-blank-lines)
    (while (not (eobp))
      (cond
       ;; empty line (do nothing)
       ((looking-at "^[[:space:]]*$"))
       ;; comment line after comment line or at
       ;; beginning of buffer
       ((and
         (save-excursion
           (eq (comment-on-line-p) (point-at-bol)))
         (or (bobp) last-line-comment-p))
        (uncomment-region (point-at-bol) (point-at-eol))
        (setq last-line-comment-p t))
       ;; line of code after comment line
       ((and
         (save-excursion
           (not (eq (comment-on-line-p) (point-at-bol))))
         last-line-comment-p)
        (newline)
        (forward-line -1)
        (insert
         (if in-org-babel-load-languages-p
             (concat "#+begin_src " language-name)
           "#+begin_example"))
        (forward-line)
        (setq last-line-comment-p nil))
       ;; comment line after line of code
       ((and
         (save-excursion
           (eq (comment-on-line-p) (point-at-bol)))
         (not last-line-comment-p))
        (uncomment-region (point-at-bol) (point-at-eol))
        (save-excursion
          (forward-line -1)
          (unless (looking-at "^[[:space:]]*$")
            (newline))
          (if in-org-babel-load-languages-p
              (insert "#+end_src")
            (insert "#+end_example"))
          (newline))
        (setq last-line-comment-p t))
       ;; last line after line of code
       ((and
         (eq (line-number-at-pos)
             (1- (count-lines (point-min) (point-max))))
         (not last-line-comment-p))
        ;; (unless (looking-at "^[[:space:]]*$")
        (forward-line)
        (newline)
        (if in-org-babel-load-languages-p
            (insert "#+end_src")
          (insert "#+end_example"))
        (newline))
       ;; line of code after line of code
       (t (setq last-line-comment-p nil)))
      (forward-line))))

(defun outorg2-convert-back-to-code ()
  "Convert edit-buffer content back to programming language syntax.
Assume that edit-buffer major-mode has been set back to the
  programming-language major-mode of the associated code-buffer
  before this function is called."
  (let* ((inside-code-or-example-block-p nil))
    (goto-char (point-min))
    (while (not (eobp))
      (cond
       ;; empty line (do nothing)
       ((looking-at "^[[:space:]]*$"))
       ;; begin code/example block
       ((looking-at "^[ \t]*#\\+begin_?")
        (kill-whole-line)
        (forward-line -1)
        (setq inside-code-or-example-block-p t))
       ;; end code/example block
       ((looking-at "^[ \t]*#\\+end_?")
        (kill-whole-line)
        (forward-line -1)
        (setq inside-code-or-example-block-p nil))
       ;; line inside code/example block (do nothing)
       (inside-code-or-example-block-p)
       ;; not-empty line outside code/example block
       (t (comment-region (point-at-bol) (point-at-eol))))
      (forward-line))))

(defun outorg2-replace-code-with-edits ()
  "Replace code-buffer contents with edits."
  (let* ((edit-buf (marker-buffer outorg2-edit-buffer-marker))
         (code-buf (marker-buffer outorg2-code-buffer-marker))
         (edit-buf-point-min
          (with-current-buffer edit-buf
            (point-min)))
         (edit-buf-point-max
          (with-current-buffer edit-buf
            (goto-char (point-max))
            (unless (and (bolp) (looking-at "^$"))
              (newline))
            (point))))
    (with-current-buffer code-buf
      (if outorg2-edit-whole-buffer-p
          (progn
            (erase-buffer)
            (insert-buffer-substring-no-properties
             edit-buf edit-buf-point-min edit-buf-point-max)
            ;; (goto-char (marker-position outorg2-edit-buffer-marker))
            )
        (save-restriction
          (narrow-to-region
           (save-excursion
             (outline-back-to-heading 'INVISIBLE-OK)
             (point))
           (save-excursion
             (outline-end-of-subtree)
             (point)))
          (delete-region (point-min) (point-max))
          (insert-buffer-substring-no-properties
           edit-buf edit-buf-point-min edit-buf-point-max)))
      ;; (save-buffer) 
      )))

(defun outorg2-reset-global-vars ()
  "Reset some global vars defined by outorg2 to initial values."
  (set-marker outorg2-code-buffer-marker nil)
  (set-marker outorg2-edit-buffer-marker nil)
  (setq outorg2-edit-whole-buffer-p nil)
  (setq outorg2-initial-window-config nil))

;; inspired by `article-remove-trailing-blank-lines' in `gnus-art.el'
(defun outorg2-remove-trailing-blank-lines ()
  "Remove all trailing blank lines from buffer."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (delete-region
       (point)
       (progn
	 (while (and (not (bobp))
		     (looking-at "^[ \t]*$"))
	   (forward-line -1))
	 (forward-line 1)
	 (point))))))


;; * Keybindings.

;; ** Outline Minor Mode

;; ;; We provide bindings for all keys.
;; ;; FIXME: very old stuff from `out-xtra' - still necesary?

;; (if (fboundp 'eval-after-load)
;;     ;; FSF Emacs 19.
;;     (eval-after-load "outline"
;;       '(let ((map (lookup-key outline-minor-mode-map
;; 			      outline-minor-mode-prefix)))
;;           ;; TODO differentiate between called in code or edit buffer
;;          (define-key map "'" 'outorg2-edit-as-org)
;;          ;; TODO add these keybindings to org-mode keymap (all?)
;;          ;; (define-key map "\C-s" 'outorg2-save-edits)
;;          ;; (define-key map "\C-c" 'outorg2-save-edits)
;;          ;; (define-key map "'" 'outorg2-save-edits)

;; 	 ;; (if (fboundp 'update-power-keys)
;; 	 ;;     (update-power-keys outline-minor-mode-map))
;;          ))

;;   (if (string-match "Lucid" emacs-version)
;;       (progn				;; Lucid Emacs 19
;; 	(defconst outline-menu
;; 	  '(
;; 	    ;; ["Hide Sublevels" outline-hide-sublevels t]
;;             ))

;;         (defun outline-add-menu ()
;; 	  (set-buffer-menubar (copy-sequence current-menubar))
;; 	  (add-menu nil "Outline" outline-menu))

;; 	(add-hook 'outline-minor-mode-hook 'outline-add-menu)
;; 	(add-hook 'outline-mode-hook 'outline-add-menu)
;; 	(add-hook 'outline-minor-mode-off-hook
;;                   (function (lambda () (delete-menu-item '("Outline")))))))


;; * Run hooks and provide

(run-hooks 'outorg2-hook)

(provide 'outorg2)

;; Local Variables:
;; coding: utf-8
;; ispell-local-dictionary: "en_US"
;; End:

;; outorg2.el ends here
