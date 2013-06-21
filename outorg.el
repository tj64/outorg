;; * outorg.el --- Org-style comment editing
;;   :PROPERTIES:
;;   :copyright: Thorsten Jolitz
;;   :copyright-years: 2013
;;   :version:  1.0
;;   :licence:  GPL 2 or later (free software)
;;   :licence-url: http://www.gnu.org/licenses/
;;   :part-of-emacs: no
;;   :author: Thorsten Jolitz
;;   :author_email: tjolitz AT gmail DOT com
;;   :inspiration:  org-src
;;   :keywords: emacs org-mode comment-editing
;;   :END:

;; ** Commentary
;; *** About outorg

;; `outorg' is like "reverse Org-Babel": editing of comment-sections from source
;; code files in temporary Org-mode buffers instead of editing of Org-mode
;; source-blocks in temporary source-code buffers.

;; It should be used together with `outline-minor-mode' and `outshine.el'.
;; Keep in mind, that `outorg' only works with outshine-style headlines like
;; those produced by calling `comment-region' on Org-mode style headlines in a
;; source-code buffer. Take this file as an example for suitable outline
;; headlines in an Emacs Lisp buffer. In other major-modes, the
;; `comment-start' character ';' of Emacs Lisp would be replaced by that of
;; the respective major-mode, e.g. '#' in PicoLisp mode or '%' in LaTeX mode.

;; `outorgs' main command is

;;  ,---------------------------
;;  | C-c ' (outorg-edit-as-org)
;;  `---------------------------

;; used in source-code buffers where `outline-minor-mode' is activated with
;; `outshine' extensions. The Org-mode edit-buffer popped up by this command
;; has `outorg-edit-minor-mode' activated, a minor-mode with only 2 commands:

;; ,----------------------------------------
;; | M-# (outorg-copy-edits-and-exit)
;; | C-x C-s (outorg-save-edits-to-tmp-file)
;; `----------------------------------------

;; If you want to insert Org-mode source-code or example blocks in
;; comment-sections, simply outcomment them in the outorg-edit buffer before
;; calling `outorg-copy-edits-and-exit'.

;; *** Installation

;; Insert

;; ;; #+begin_src emacs-lisp
;; ;;  (require 'outorg)
;; ;; #+end_src

;; in your .emacs.

;; *** Bugs and Shortcomings

;; `outorg' is line-based, it only works with 'one-line' comments, i.e. with
;; comment-sections like those produced by `comment-region' (a command that
;; comments or uncomments each line in the region). Those special multi-line
;; comments found in many programming languages are not recognized and lead to
;; undefined behaviour.

;; *** Emacs Version

;; `outorg.el' works with [GNU Emacs 24.2.1 (x86_64-unknown-linux-gnu, GTK+
;; Version 3.6.4) of 2013-01-20 on eric]. No attempts of testing with older
;; versions or other types of Emacs have been made (yet).


;; ** ChangeLog

;; | date            | author(s)       | version |
;; |-----------------+-----------------+---------|
;; | <2013-05-03 Fr> | Thorsten Jolitz |     1.0 |
;; | <2013-02-11 Mo> | Thorsten Jolitz |     0.9 |

;; * Requires

(require 'outline)
(require 'org)

;; * Mode and Exporter Definitions
;; ** Outorg Edit minor-mode

(define-minor-mode outorg-edit-minor-mode
   "Minor mode for Org-mode buffers generated by outorg.
There is a mode hook, and two commands:
\\[outorg-copy-edits-and-exit] outorg-copy-edits-and-exit
\\[outorg-save-edits-to-tmp-file] outorg-save-edits-to-tmp-file"
  :lighter " Outorg")

;; ** Derived Exporters
;; *** LaTeX

  ;; (defun outorg-latex-property-drawer (drawer contents info)
  ;;   (concat "\\begin{example}\n"
  ;;           (org-element-interpret-data drawer)
  ;;           "\\end{example}"))

  ;; (org-export-define-derived-backend 'outorg-latex 'latex
  ;;   :translate-alist '((property-drawer . my-latex-property-drawer)))

;; *** HTML
;; *** ASCII

;; * Variables
;; ** Consts

(defconst outorg-version "1.0"
  "outorg version number.")

;; ** Vars

(defvar outline-minor-mode-prefix "\C-c"
  "New outline-minor-mode prefix.")

(defvar outorg-edit-whole-buffer-p nil
  "Non-nil if the whole code-buffer is edited.")

(defvar outorg-initial-window-config nil
  "Initial window-configuration when editing as Org.")

(defvar outorg-code-buffer-read-only-p nil
  "Remember if code-buffer was read only before editing")

;; copied and adapted from ob-core.el
(defvar outorg-temporary-directory)    ; FIXME why this duplication?
(unless (or noninteractive (boundp 'outorg-temporary-directory))
  (defvar outorg-temporary-directory
    (or (and (boundp 'outorg-temporary-directory)
	     (file-exists-p outorg-temporary-directory)
	     outorg-temporary-directory)
	(make-temp-file "outorg-" t))
    "Directory to hold temporary files created to edit code blocks.
Used by `org-babel-temp-file'.  This directory will be removed on
Emacs shutdown."))

(defvar outorg-oldschool-elisp-headers-p nil
  "Non-nil if an Emacs Lisp file uses oldschool headers ';;;+'")

;; ** Hooks

(defvar outorg-hook nil
  "Functions to run after `outorg' is loaded.")

(defvar outorg-edit-minor-mode-hook nil
  "Hook run after `outorg' switched a source code file or subtree to
  Org-mode.")

;; ** Customs

;; *** Custom Groups

(defgroup outorg nil
  "Library for outline navigation and Org-mode editing in Lisp buffers."
  :prefix "outorg-"
  :group 'lisp
  :link '(url-link
          "http://orgmode.org/worg/org-tutorials/org-outside-org.html"))

;; *** Custom Vars

;; inspired by 'org-src.el'
(defcustom outorg-edit-buffer-persistent-message t
  "Non-nil means show persistent exit help message while in edit-buffer.
The message is shown in the header-line, which will be created in the
first line of the window showing the editing buffer."
  :group 'outorg
  :type 'boolean)

(defcustom outorg-unindent-active-source-blocks-p t
  "Non-nil means common indentation (e.g. 2 spaces) in the active
source-blocks of the *outorg-edit-buffer* (i.e. those in the
language of the associated source-code buffer, and only in those)
is removed before converting back from Org to source-code."
  :group 'outorg
  :type 'boolean)

;; * Functions
;; ** Non-interactive Functions
;; *** Get Source Buffer Mode

;; copied from http://www.emacswiki.org/emacs/basic-edit-toolkit.el
(defun outorg-comment-on-line-p ()
  "Whether have comment part on current line.
If have comment return COMMENT-START, otherwise return nil."
  (save-excursion
    (beginning-of-line)
    (comment-normalize-vars)
    (comment-search-forward (line-end-position) t)))

(defun outorg-get-buffer-mode (buffer-or-string)
  "Return major mode of BUFFER-OR-STRING."
  (with-current-buffer buffer-or-string
     major-mode))

;; *** Configure Edit Buffer

;; copied and adapted from org-src.el
(defun outorg-edit-configure-buffer ()
  "Configure edit buffer"
  (let ((msg
         (concat "[ "
                 (buffer-name
                  (marker-buffer outorg-code-buffer-point-marker))
                 " ] "
                 "Exit with M-# (Meta-Key and #)")))
    (org-add-hook 'kill-buffer-hook
                  'outorg-save-edits-to-tmp-file nil 'local)
    ;; (setq buffer-offer-save t)
    (and outorg-edit-buffer-persistent-message
         (org-set-local 'header-line-format msg))
    ;; (setq buffer-file-name
    ;;       (concat (buffer-file-name
    ;; (marker-buffer outorg-code-buffer-point-marker))
    ;;               "[" (buffer-name) "]"))
    (if (featurep 'xemacs)
        (progn
          (make-variable-buffer-local
           'write-contents-hooks) ; needed only for 21.4
          (setq write-contents-hooks
                '(outorg-save-edits-to-tmp-file)))
      (setq write-contents-functions
            '(outorg-save-edits-to-tmp-file)))
    ;; (setq buffer-read-only t) ; why?
    ))


;; (org-add-hook 'outorg-edit-minor-mode-hook 'outorg-edit-minor-mode)
(org-add-hook 'outorg-edit-minor-mode-hook
              'outorg-edit-configure-buffer)

;; *** Backup Edit Buffer

;; copied and adapted from ob-core.el
(defun outorg-temp-file (prefix &optional suffix)
  "Create a temporary file in the `outorg-temporary-directory'.
Passes PREFIX and SUFFIX directly to `make-temp-file' with the
value of `temporary-file-directory' temporarily set to the value
of `outorg-temporary-directory'."
  (let ((temporary-file-directory
	 (if (file-remote-p default-directory)
	     (concat (file-remote-p default-directory) "/tmp")
	   (or (and (boundp 'outorg-temporary-directory)
		    (file-exists-p outorg-temporary-directory)
		    outorg-temporary-directory)
	       temporary-file-directory))))
      (make-temp-file prefix nil suffix)))

(defun outorg-save-edits-to-tmp-file ()
  "Save edit-buffer in temporary file"
  (interactive)
  (let ((tmp-file
         (outorg-temp-file
          (file-name-sans-extension
           (file-name-nondirectory
            (buffer-file-name
             (marker-buffer
              outorg-code-buffer-point-marker)))))))
    (write-region nil nil tmp-file)))

;; copied and adapted from ob-core.el
(defun outorg-remove-temporary-directory ()
  "Remove `outorg-temporary-directory' on Emacs shutdown."
  (when (and (boundp 'outorg-temporary-directory)
	     (file-exists-p outorg-temporary-directory))
    ;; taken from `delete-directory' in files.el
    (condition-case nil
	(progn
	  (mapc (lambda (file)
		  ;; This test is equivalent to
		  ;; (and (file-directory-p fn) (not (file-symlink-p fn)))
		  ;; but more efficient
		  (if (eq t (car (file-attributes file)))
		      (delete-directory file)
		    (delete-file file)))
		;; We do not want to delete "." and "..".
		(directory-files outorg-temporary-directory 'full
				 "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*"))
	  (delete-directory outorg-temporary-directory))
      (error
       (message "Failed to remove temporary outorg directory %s"
		(if (boundp 'outorg-temporary-directory)
		    outorg-temporary-directory
		  "[directory not defined]"))))))

(add-hook 'kill-emacs-hook 'outorg-remove-temporary-directory)

;; *** Reset Global Vars

;; TODO better use buffer-local variables instead?
(defun outorg-reset-global-vars ()
  "Reset some global vars defined by outorg to initial values."
  (ignore-errors
    (set-marker outorg-code-buffer-point-marker nil)
    (set-marker outorg-code-buffer-beg-of-subtree-marker nil)
    (set-marker outorg-edit-buffer-marker nil)
    (setq outorg-edit-whole-buffer-p nil)
    (setq outorg-initial-window-config nil)
    (setq outorg-code-buffer-read-only-p nil)
    (setq outorg-oldschool-elisp-headers-p nil)))

;; *** Remove Trailing Blank Lines

;; inspired by `article-remove-trailing-blank-lines' in `gnus-art.el'
(defun outorg-remove-trailing-blank-lines ()
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


;; *** Copy and Convert

(defun outorg-convert-org-to-outshine
  (&optional mode infile outfile BATCH)
  "Convert an existing Org-mode file into an Outshine buffer.

If MODE is non-nil, the Outshine buffer will be put in this
major-mode, otherwise the major-mode of the language of the first
source-code block in the Org-mode buffer will be used.

If INFILE is non-nil, the specified Org-mode file will be
visited, otherwise the current buffer will be used (i.e. the
buffer content will be copied to a temporary *outorg-edit-buffer*
for further processing).

If OUTFILE is non-nil, the converted Outshine buffer will be
saved in this file. Its the user's responsability to make sure
that OUTFILE's file-extension is suited for the major-mode of the
Outshine buffer to be saved. When in doubt, consult variable
`auto-mode-alist' for associations between file-extensions and
major-modes.

If BATCH is non-nil (and OUTFILE is non-nil, otherwise it makes
no sense), the new Outshine file is saved and its buffer
deleted."
  (let* ((org-buffer (if infile
                         (if (and (file-exists-p infile)
                                  (string-equal
                                   (file-name-extension infile) "org"))
                             (find-file (expand-file-name infile))
                           (error
                            "Infile doesn't exist or is not an Org file"))
                       (current-buffer)))
         (maj-mode (or mode
                       (with-current-buffer org-buffer
                         (save-excursion
                           (goto-char (point-min))
                           (or
                            ;; major-mode of first src-block
                            (ignore-errors
                              (org-next-block
                               nil nil org-babel-src-block-regexp)
                              (format
                               "%s-mode"
                               (car (org-babel-get-src-block-info 'LIGHT))))
                            ;; default case emacs-lisp-mode
                            "emacs-lisp-mode"))))))
    (with-current-buffer (get-buffer-create
                          (generate-new-buffer-name "tmp"))
      (setq outorg-code-buffer-point-marker (point-marker))
      (funcall (intern maj-mode))
      (and outfile
           ;; ;; FIXME does not really avoid confirmation prompts
           ;; (add-to-list 'revert-without-query (expand-file-name outfile))
           (if BATCH
               (write-file (expand-file-name outfile))
             (write-file (expand-file-name outfile) 'CONFIRM))))
    (setq outorg-edit-whole-buffer-p t)
    (setq outorg-initial-window-config
          (current-window-configuration))
    (with-current-buffer (get-buffer-create "*outorg-edit-buffer*")
      (erase-buffer)
      (insert-buffer-substring org-buffer)
      (outorg-copy-edits-and-exit))
    ;; ;; FIXME ugly hack
    ;; (funcall major-mode)
    ;; (funcall major-mode)
    ;; (fontify-keywords)
    (when outfile
      (save-buffer)
      ;; (revert-buffer t t)
      ;; (remove
      ;;  (expand-file-name outfile)
      ;;  revert-without-query)
      (and BATCH (kill-buffer)))))

(defun outorg-prepare-message-mode-buffer-for-editing ()
  "Prepare an unsent-mail in a message-mode buffer for outorg.

This function assumes that '--text follows this line--' is the
first line below the message header, is always present, and never
modified by the user. It turns this line into an `outshine'
headline and out-comments all text below this line - if any."
    (goto-char (point-min))
    (re-search-forward "--text follows this line--" nil 'NOERROR)
    (replace-match "* \\&")
    (beginning-of-line)
    (let ((start-body (point)))
      (comment-region start-body (point-max))
      (narrow-to-region start-body (point-max))
      (forward-line)))

(defun outorg-prepare-message-mode-buffer-for-sending ()
  "Prepare an unsent-mail edited via `outorg-edit' for sending.

This function assumes that '* --text follows this line--' is the
first line below the message header and is, like all lines below
it, out-commented with `comment-region'. It deletes the leading
star and uncomments the line and all text below it - if any."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (concat
      "\\(" (regexp-quote "* ") "\\)"
      "--text follows this line--")
     nil 'NOERROR)
    (replace-match "" nil nil nil 1)
    (beginning-of-line)
    (let ((start-body (point)))
      (uncomment-region start-body (point-max))
      (widen))))

(defun outorg-copy-and-convert ()
  "Copy code buffer content to tmp-buffer and convert it to Org syntax.
If `outorg-edit-whole-buffer' is non-nil, copy the whole buffer, otherwise
  the current subtree."
  (let* ((edit-buffer
          (get-buffer-create "*outorg-edit-buffer*")))
    (save-restriction
      (with-current-buffer edit-buffer
        (erase-buffer))
      ;; make outorg respect narrowing
      ;; (widen)
      ;; copy code buffer content
      (copy-to-buffer
       edit-buffer
       (if outorg-edit-whole-buffer-p
           (point-min)
         (save-excursion
           (outline-back-to-heading 'INVISIBLE-OK)
           (point)))
       (if outorg-edit-whole-buffer-p
           (point-max)
         (save-excursion
           (outline-end-of-subtree)
           (point)))))
    ;; switch to edit buffer
    (if (one-window-p) (split-window-sensibly (get-buffer-window)))
    (switch-to-buffer-other-window edit-buffer)
    ;; set point
    (goto-char
     (if outorg-edit-whole-buffer-p
         (marker-position outorg-code-buffer-point-marker)
       (1+ (- (marker-position outorg-code-buffer-point-marker)
          (marker-position outorg-code-buffer-beg-of-subtree-marker)))))
    ;; activate programming language major mode and convert to org
    (let ((mode (outorg-get-buffer-mode
              (marker-buffer outorg-code-buffer-point-marker))))
      ;; special case R-mode
      (if (eq mode 'ess-mode)
          (funcall 'R-mode)
        (funcall mode)))
    (outorg-convert-to-org)
    ;; change major mode to org-mode
    (org-mode)
    ;; activate minor mode outorg-edit-minor-mode
    (outorg-edit-minor-mode)
    ;; set outline visibility
    (if (not outorg-edit-whole-buffer-p)
        (show-all)
      (hide-sublevels 3)
      (show-subtree))))

(defun outorg-convert-to-org ()
  "Convert file content to Org Syntax"
  (let* ((last-line-comment-p nil)
         (mode-name
          (format
           "%S" (with-current-buffer
                    (marker-buffer outorg-code-buffer-point-marker)
                  major-mode)))
         (splitted-mode-name
          (split-string mode-name "-mode"))
         (language-name
          (if (> (length splitted-mode-name) 1)
              (car splitted-mode-name)
            (car (split-string mode-name "\\."))))
         (in-org-babel-load-languages-p
          (assq
           (intern
            (if (string-equal language-name "ess") "R" language-name))
           org-babel-load-languages)))
    (save-excursion
      (goto-char (point-min))
      (outorg-remove-trailing-blank-lines)
      (while (not (eobp))
        (cond
         ;; empty line (do nothing)
         ((looking-at "^[[:space:]]*$"))
         ;; comment line after comment line or at
         ;; beginning of buffer
         ((and
           (save-excursion
             (eq (outorg-comment-on-line-p) (point-at-bol)))
           (or (bobp) last-line-comment-p))
          (if (and outorg-oldschool-elisp-headers-p
                   (looking-at "^;;;+ "))
              ;; deal with oldschool elisp headers
              (let ((elisp-header-level
                     (- (length (match-string-no-properties 0)) 3)))
                (uncomment-region (point-at-bol) (point-at-eol))
                (save-excursion
                  (dotimes (i elisp-header-level) (insert "*"))
                  (insert " ")))
            ;; orgmode-style headers
            (uncomment-region (point-at-bol) (point-at-eol)))
          (setq last-line-comment-p t))
         ;; line of code after comment line
         ((and
           (save-excursion
             (not (eq (outorg-comment-on-line-p) (point-at-bol))))
           last-line-comment-p)
          (newline)
          (forward-line -1)
          (insert
           (if in-org-babel-load-languages-p
               (concat
                "#+begin_src "
                (if (string-equal language-name "ess") "R" language-name))
             "#+begin_example"))
          (forward-line)
          (setq last-line-comment-p nil))
         ;; comment line after line of code
         ((and
           (save-excursion
             (eq (outorg-comment-on-line-p) (point-at-bol)))
           (not last-line-comment-p))
          (if (and outorg-oldschool-elisp-headers-p
                   (looking-at "^;;;+ "))
              ;; deal with oldschool elisp headers
              (let ((elisp-header-level
                     (- (length (match-string-no-properties 0)) 3)))
                (uncomment-region (point-at-bol) (point-at-eol))
                (save-excursion
                  (dotimes (i elisp-header-level) (insert "*"))
                  (insert " ")))
            ;; orgmode-style headers
            (uncomment-region (point-at-bol) (point-at-eol)))
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
          (goto-char (point-max))
          (unless (looking-at "^[[:space:]]*$")
            (newline))
          (if in-org-babel-load-languages-p
              (insert "#+end_src")
            (insert "#+end_example"))
          (newline))
         ;; line of code after line of code
         (t
          (setq last-line-comment-p nil)))
        ;; continue loop
        (progn
          (forward-line)
          (and (eobp)
               (looking-at "^[[:space:]]*$")
               (not last-line-comment-p)
               (save-excursion
                 (forward-line -1)
                 (not (looking-at "^[ \t]*#\\+end_?")))
               (if in-org-babel-load-languages-p
                   (insert "#+end_src")
                 (insert "#+end_example"))))))))

(defun outorg-unindent-active-source-blocks (src-block-lang)
  "Remove common indentation from active source-blocks. 

While editing in the *outorg-edit-buffer*, the source-code of the
source-blocks with language LANG (which should be the major-mode
language of the associated source-code buffer) might be indented
consciously or by accident. The latter happens e.g. when the
source-blocks are edited with `org-edit-special' (C-c '), and
variable `org-edit-src-content-indentation' has a value > 0.

This function removes the introduced common indentation (e.g. 2
spaces) in these source-blocks (and only in them) before
converting back from Org to source-code if customizable variable
`outorg-unindent-active-source-blocks-p' is non-nil."
  (let ((language (if (string-equal src-block-lang "ess")
                      "R" src-block-lang)))
    (save-excursion
      ;; ;; FIXME necessary?
      ;; (goto-char (point-min))
      (org-babel-map-src-blocks nil
        ;; language given as argument equal to lang of processed block?
        (and (string-equal language lang)
             (org-babel-mark-block)
                   (save-restriction
                     (narrow-to-region
                      (car (region-or-buffer-limits))
                      (cadr (region-or-buffer-limits)))
                     (org-do-remove-indentation)))))))

(defun outorg-convert-back-to-code ()
  "Convert edit-buffer content back to programming language syntax.
Assume that edit-buffer major-mode has been set back to the
  programming-language major-mode of the associated code-buffer
  before this function is called."
  (let* ((inside-code-or-example-block-p nil)
         (comment-style "plain")
         (mode-name
          (format "%S" major-mode))
         (splitted-mode-name
          (split-string mode-name "-mode"))
         (language-name
          (if (> (length splitted-mode-name) 1)
              (car splitted-mode-name)
            (car (split-string mode-name "\\."))))
         (in-org-babel-load-languages-p
          (assq
           (intern
            (if (string-equal language-name "ess") "R" language-name))
           org-babel-load-languages)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ;; empty line (do nothing)
         ((looking-at "^[[:space:]]*$"))
         ;; begin code/example block
         ((looking-at "^[ \t]*#\\+begin_?")
          (if (or (and (not in-org-babel-load-languages-p)
                       (looking-at "^[ \t]*#\\+begin_example"))
                  (and in-org-babel-load-languages-p
                       (looking-at
                        (format "^[ \t]*#\\+begin_src %s"
                                (if (string-equal language-name "ess")
                                    "R"
                                  language-name)))))
              (progn
                (kill-whole-line)
                (forward-line -1)
                (setq inside-code-or-example-block-p t))
            (save-excursion
              (insert "!!!")
              (comment-region (point-at-bol) (point-at-eol))
              (beginning-of-line)
              (and (looking-at "\\(^.+\\)\\(!!![[:space:]]*#\\+\\)")
                   (replace-match "#+" nil nil nil 2)))))
         ;; end code/example block
         ((looking-at "^[ \t]*#\\+end_?")
          (if (or (and (not in-org-babel-load-languages-p)
                       (looking-at "^[ \t]*#\\+end_example"))
                  (and in-org-babel-load-languages-p
                       inside-code-or-example-block-p
                       (looking-at
                        (format "^[ \t]*#\\+end_src"))))
              (progn
                (kill-whole-line)
                (forward-line -1)
                (setq inside-code-or-example-block-p nil))
            (save-excursion
              (insert "!!!")
              (comment-region (point-at-bol) (point-at-eol))
              (beginning-of-line)
              (and (looking-at "\\(^.+\\)\\(!!![[:space:]]*#\\+\\)")
                   (replace-match "#+" nil nil nil 2)))))
        ;; line inside code/example block (do nothing)
        (inside-code-or-example-block-p)
        ;; not-empty line outside code/example block
        (t
         (if (and outorg-oldschool-elisp-headers-p
                  (looking-at "^[*]+ "))
             ;; deal with oldschool elisp headers (;;;+ )
             (let* ((org-header-level
                     (1- (length (match-string-no-properties 0))))
                    (replacement-string
                     (let ((strg ";"))
                       (dotimes (i (1- org-header-level) strg)
                         (setq strg (concat strg ";"))))))
               (comment-region (point-at-bol) (point-at-eol))
               (and
                (looking-at "\\(;;\\)\\( [*]+\\)\\( \\)")
                (replace-match replacement-string nil nil nil 2)))
           (comment-region (point-at-bol) (point-at-eol)))))
      (forward-line)))))

(defun outorg-replace-code-with-edits ()
  "Replace code-buffer contents with edits."
  (let* ((edit-buf (marker-buffer outorg-edit-buffer-marker))
         (code-buf (marker-buffer outorg-code-buffer-point-marker))
         (edit-buf-point-min
          (with-current-buffer edit-buf
            (point-min)))
         (edit-buf-point-max
          (with-current-buffer edit-buf
            (save-excursion
              (goto-char (point-max))
              (unless (and (bolp) (looking-at "^[ \t]*$"))
                (newline))
              (point)))))
    (with-current-buffer code-buf
      (if outorg-edit-whole-buffer-p
          (progn
            (if (buffer-narrowed-p)
                (delete-region (point-min) (point-max))
              (erase-buffer))
            (insert-buffer-substring-no-properties
             edit-buf edit-buf-point-min edit-buf-point-max))
        (goto-char (marker-position outorg-code-buffer-point-marker))
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
           edit-buf edit-buf-point-min edit-buf-point-max))
        ;; (save-buffer)
        ))))


;; ** Commands

(defun outorg-edit-as-org (&optional arg)
  "Convert and copy to temporary Org buffer
With ARG, edit the whole buffer, otherwise the current subtree."
  (interactive "P")
  (and buffer-file-read-only
       (error "Cannor edit read-only buffer-file"))
  (and buffer-read-only
       (if (not (y-or-n-p "Buffer is read-only - make writable "))
           (error "Cannot edit read-only buffer")
         (setq inhibit-read-only t)
         (setq outorg-code-buffer-read-only-p t)))
  (and (eq major-mode 'message-mode)
       (outorg-prepare-message-mode-buffer-for-editing))
  (setq outorg-code-buffer-point-marker (point-marker))
  (save-excursion
    (outline-back-to-heading 'INVISIBLE-OK)
    (setq outorg-code-buffer-beg-of-subtree-marker (point-marker)))
  (and arg (setq outorg-edit-whole-buffer-p t))
  (and outshine-enforce-no-comment-padding-p
       (setq outorg-oldschool-elisp-headers-p t))
  (setq outorg-initial-window-config
        (current-window-configuration))
  (outorg-copy-and-convert))

(defun outorg-copy-edits-and-exit ()
  "Replace code-buffer content with (converted) edit-buffer content and
  kill edit-buffer"
  (interactive)
  (widen)
  (let ((mode (outorg-get-buffer-mode
               (marker-buffer outorg-code-buffer-point-marker))))
    (and outorg-unindent-active-source-blocks-p
         (outorg-unindent-active-source-blocks
          (car (split-string
                (symbol-name mode) "-mode" 'OMIT-NULLS))))
    ;; special case R-mode
    (if (eq mode 'ess-mode)
        (funcall 'R-mode)
      (funcall mode)))
  ;; (funcall
  ;;  (outorg-get-buffer-mode
  ;;   (marker-buffer outorg-code-buffer-point-marker)))
  (setq outorg-edit-buffer-marker (point-marker))
  (outorg-convert-back-to-code)
  (outorg-replace-code-with-edits)
  (set-window-configuration
   outorg-initial-window-config)
  (if outorg-edit-whole-buffer-p
      (goto-char (marker-position outorg-edit-buffer-marker))
    (goto-char (1- (+ (marker-position
                       outorg-edit-buffer-marker)
                      (marker-position
                       outorg-code-buffer-beg-of-subtree-marker)))))
  ;; avoid confirmation prompt when killing the edit buffer
  (with-current-buffer (marker-buffer outorg-edit-buffer-marker)
    (set-buffer-modified-p nil))
  (kill-buffer
   (marker-buffer outorg-edit-buffer-marker))
  ;; (switch-to-buffer
  ;;  (marker-buffer outorg-code-buffer-point-marker))
  ;; (goto-char
  ;;  (marker-position outorg-code-buffer-point-marker))
  (and outorg-code-buffer-read-only-p
       (setq inhibit-read-only nil))
  (and (eq major-mode 'message-mode)
       (outorg-prepare-message-mode-buffer-for-sending))
  (outorg-reset-global-vars))

(defun outorg-replace-source-blocks-with-results
  (&optional arg &rest languages)
  "Replace source-blocks with their results.

Only source-blocks with ':export results' in their header
arguments will be mapped.

If LANGUAGES is non-nil, only those source-blocks with a
language found in the list are mapped.

If LANGUAGES is nil but a prefix-argument ARG is given, only the
languages read from the mini-buffer (separated by blanks) are mapped.

Otherwise, all languages found in `org-babel-load-languages' are mapped."
  (interactive "P\n")
  (let ((langs (or languages
                   (and arg
                        (split-string
                         (read-string
                          (concat "Org Babel languages separated by blanks: "))
                         " " 'OMIT-NULLS))
                   (mapcar
                    (lambda (X) (symbol-name (car X)))
                    org-babel-load-languages))))
    (org-babel-map-src-blocks nil
      (and
       (string-equal
        (cdr
         (assoc
          :exports
          (org-babel-parse-header-arguments header-args)))
        "results")
       (member lang langs)
       (org-babel-execute-src-block)
       (let* ((block-start (org-babel-where-is-src-block-head))
              (results-head (org-babel-where-is-src-block-result))
              (results-body
               (save-excursion
                 (goto-char results-head)
                 (forward-line)
                 (point))))
         (delete-region block-start results-body))))))


;; * Menus and Keys
;; ** Menus

(defvar outorg-edit-menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [outorg-copy-edits-and-exit]
      '(menu-item "Copy and Exit" outorg-copy-edits-and-exit
                  :help "Copy edits to original-buffer
                  and exit outorg"))
    (define-key map [outorg-save-edits-to-tmp-file]
      '(menu-item "Save" outorg-save-edits-to-tmp-file
                  :help "Save edit buffer to temporary
                  file in the OS tmp directory"))
    map))

;; ** Keys

;; *** Mode Keys

(defvar outorg-edit-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-#"
      'outorg-copy-edits-and-exit)
    (define-key map "\C-x\C-s"
      'outorg-save-edits-to-tmp-file)
    (define-key map [menu-bar outorg-edit]
      (cons (purecopy "Outorg") outorg-edit-menu-map))
    map))

(add-to-list 'minor-mode-map-alist
             (cons 'outorg-edit-minor-mode
                   outorg-edit-minor-mode-map))

;; * Run hooks and provide

(run-hooks 'outorg-hook)

(provide 'outorg)

;; Local Variables:
;; coding: utf-8
;; ispell-local-dictionary: "en_US"
;; End:

;; outorg.el ends here
