;; * outxxtra.el --- Org-style outline navigation and comment editing

;; ** Copyright

;; Copyright (C) 2013 Thorsten Jolitz
;; This file is not (yet) part of GNU Emacs

;; Author: Thorsten Jolitz  (format "tjolitz%sgmail%s" "@" ".com")

;; ** Credits

;; This library is based on, or rather an extension of, Per Abrahamsen's
;; 'out-xtra.el' (http://tinyurl.com/aql9p97), and may replace it many cases.
;; Some new ideas were taken from Fabrice Niessen's '.emacs'
;; (http://www.mygooglest.com/fni/dot-emacs.html#sec-2).

;; ** Commentary

;; This file provides (almost) the same nice extra features for outline minor
;; mode like Per Abrahamsen's 'out-xtra':

;; - Change default minor mode key prefix to `C-c'.
;; - Complete keybindings and menu support.
;; - Add command to show top level headers.
;; - Add command to hide all other entries than the one containing point.

;; `outxxtra' follows a different idea than `out-xtra': it consists of generic
;; functionality that that calculates the adequate outline-regexp and
;; outline-level for the active major-mode, rather than defining several blocks
;; of major-mode specific functionality.

;; New features of `outxxtra' are:

;; 1. Generic functionality that should work whereever `comment-region' and
;; `uncomment-region' work. 

;; 2. Fontification of headlines (copied from Fabrice Niessen's
;; '.emacs')

;; It is highly recommended to use `outxxtra' together with `outline-magic' for
;; the Org-style `outline-cycle' command.

;; ** Emacs Version

;; `outxxtra.el' works with [GNU Emacs 24.2.1 (x86_64-unknown-linux-gnu, GTK+
;; Version 3.6.4) of 2013-01-20 on eric]. No attempts of testing with older
;; versions or other types of Emacs have be made (yet).

;; ** Installation

;; Insert
;; (require 'outxxtra)
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

;; `outxxtra' uses a fixed format `outline-regexp' (and `outline-level'), the
;; one produced when applying `comment-region' on an Org-mode style headline
;; in the active programming-language major-mode (e.g. ';; ** Entry' in an
;; Emacs Lisp buffer, or '## ** Entry' in an PicoLisp buffer).

;; This could be make more flexible (and customizable), albeit at the price
;; that `outorg.el' would not work anymore with `outxxtra'. And - these
;; Org-style heades look good, why change them?

;; * Requires

(require 'outline)

;; * Variables
;; ** Consts

(defconst outxxtra-version "0.9"
  "outxxtra version number.")

;; copied from org-source.el
(defconst outxxtra-level-faces
  '(outxxtra-level-1 outxxtra-level-2 outxxtra-level-3 outxxtra-level-4
                     outxxtra-level-5 outxxtra-level-6 outxxtra-level-7
                     outxxtra-level-8))


;; ** Vars

(defvar outline-minor-mode-prefix "\C-c"
  "New outline-minor-mode prefix.")

;; ** Hooks

(defvar outxxtra-hook nil
  "Functions to run after `outxxtra' is loaded.")

;; ** Customs

;; *** Custom Groups

(defgroup outxxtra nil
  "Enhanced library for outline navigation in source code buffers."
  :prefix "outxxtra-"
  :group 'lisp)

(defgroup outxxtra-faces nil
  "Faces in Outxxtra."
  :tag "Outxxtry Faces"
  :group 'outxxtra)

;; *** Faces

;; copied from 'org-compat.el'
(defun outxxtra-compatible-face (inherits specs)
  "Make a compatible face specification.
If INHERITS is an existing face and if the Emacs version supports it,
just inherit the face.  If INHERITS is set and the Emacs version does
not support it, copy the face specification from the inheritance face.
If INHERITS is not given and SPECS is, use SPECS to define the face.
XEmacs and Emacs 21 do not know about the `min-colors' attribute.
For them we convert a (min-colors 8) entry to a `tty' entry and move it
to the top of the list.  The `min-colors' attribute will be removed from
any other entries, and any resulting duplicates will be removed entirely."
  (when (and inherits (facep inherits) (not specs))
    (setq specs (or specs
		    (get inherits 'saved-face)
		    (get inherits 'face-defface-spec))))
  (cond
   ((and inherits (facep inherits)
	 (not (featurep 'xemacs))
	 (>= emacs-major-version 22)
	 ;; do not inherit outline faces before Emacs 23
	 (or (>= emacs-major-version 23)
	     (not (string-match "\\`outline-[0-9]+"
				(symbol-name inherits)))))
    (list (list t :inherit inherits)))
   ((or (featurep 'xemacs) (< emacs-major-version 22))
    ;; These do not understand the `min-colors' attribute.
    (let (r e a)
      (while (setq e (pop specs))
	(cond
	 ((memq (car e) '(t default)) (push e r))
	 ((setq a (member '(min-colors 8) (car e)))
	  (nconc r (list (cons (cons '(type tty) (delq (car a) (car e)))
			       (cdr e)))))
	 ((setq a (assq 'min-colors (car e)))
	  (setq e (cons (delq a (car e)) (cdr e)))
	  (or (assoc (car e) r) (push e r)))
	 (t (or (assoc (car e) r) (push e r)))))
      (nreverse r)))
   (t specs)))
(put 'outxxtra-compatible-face 'lisp-indent-function 1)

;; The following face definitions have been copied from 'org-faces.el'
(defface outxxtra-level-1 ;; originally copied from font-lock-function-name-face
  (outxxtra-compatible-face 'outline-1
    '((((class color) (min-colors 88) (background light)) (:foreground "Blue1"))
      (((class color) (min-colors 88) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 16) (background light)) (:foreground "Blue"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSkyBlue"))
      (((class color) (min-colors 8)) (:foreground "blue" :bold t))
      (t (:bold t))))
  "Face used for level 1 headlines."
  :group 'outxxtra-faces)

(defface outxxtra-level-2 ;; originally copied from font-lock-variable-name-face
  (outxxtra-compatible-face 'outline-2
    '((((class color) (min-colors 16) (background light)) (:foreground "DarkGoldenrod"))
      (((class color) (min-colors 16) (background dark))  (:foreground "LightGoldenrod"))
      (((class color) (min-colors 8)  (background light)) (:foreground "yellow"))
      (((class color) (min-colors 8)  (background dark))  (:foreground "yellow" :bold t))
      (t (:bold t))))
  "Face used for level 2 headlines."
  :group 'outxxtra-faces)

(defface outxxtra-level-3 ;; originally copied from font-lock-keyword-face
  (outxxtra-compatible-face 'outline-3
    '((((class color) (min-colors 88) (background light)) (:foreground "Purple"))
      (((class color) (min-colors 88) (background dark))  (:foreground "Cyan1"))
      (((class color) (min-colors 16) (background light)) (:foreground "Purple"))
      (((class color) (min-colors 16) (background dark))  (:foreground "Cyan"))
      (((class color) (min-colors 8)  (background light)) (:foreground "purple" :bold t))
      (((class color) (min-colors 8)  (background dark))  (:foreground "cyan" :bold t))
      (t (:bold t))))
  "Face used for level 3 headlines."
  :group 'outxxtra-faces)

(defface outxxtra-level-4   ;; originally copied from font-lock-comment-face
  (outxxtra-compatible-face 'outline-4
    '((((class color) (min-colors 88) (background light)) (:foreground "Firebrick"))
      (((class color) (min-colors 88) (background dark))  (:foreground "chocolate1"))
      (((class color) (min-colors 16) (background light)) (:foreground "red"))
      (((class color) (min-colors 16) (background dark))  (:foreground "red1"))
      (((class color) (min-colors 8) (background light))  (:foreground "red" :bold t))
      (((class color) (min-colors 8) (background dark))   (:foreground "red" :bold t))
      (t (:bold t))))
  "Face used for level 4 headlines."
  :group 'outxxtra-faces)

(defface outxxtra-level-5 ;; originally copied from font-lock-type-face
  (outxxtra-compatible-face 'outline-5
    '((((class color) (min-colors 16) (background light)) (:foreground "ForestGreen"))
      (((class color) (min-colors 16) (background dark)) (:foreground "PaleGreen"))
      (((class color) (min-colors 8)) (:foreground "green"))))
  "Face used for level 5 headlines."
  :group 'outxxtra-faces)

(defface outxxtra-level-6 ;; originally copied from font-lock-constant-face
  (outxxtra-compatible-face 'outline-6
    '((((class color) (min-colors 16) (background light)) (:foreground "CadetBlue"))
      (((class color) (min-colors 16) (background dark)) (:foreground "Aquamarine"))
      (((class color) (min-colors 8)) (:foreground "magenta"))))
  "Face used for level 6 headlines."
  :group 'outxxtra-faces)

(defface outxxtra-level-7 ;; originally copied from font-lock-builtin-face
  (outxxtra-compatible-face 'outline-7
    '((((class color) (min-colors 16) (background light)) (:foreground "Orchid"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSteelBlue"))
      (((class color) (min-colors 8)) (:foreground "blue"))))
  "Face used for level 7 headlines."
  :group 'outxxtra-faces)

(defface outxxtra-level-8 ;; originally copied from font-lock-string-face
  (outxxtra-compatible-face 'outline-8
    '((((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
      (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
      (((class color) (min-colors 8)) (:foreground "green"))))
  "Face used for level 8 headlines."
  :group 'outxxtra-faces)

;; *** Custom Vars

;; copied form org.el
(defcustom outxxtra-fontify-whole-heading-line nil
  "Non-nil means fontify the whole line for headings.
This is useful when setting a background color for the
outxxtra-level-* faces."
  :group 'outxxtra
  :type 'boolean)

;; * Functions

;; ** Non-interactive Functions

;; *** Calculate the outline-regexp

(defun outxxtra-calc-outline-regexp ()
  "Calculate the outline regexp for the current mode."
  (let* ((comment-start-no-space
          (replace-regexp-in-string
           "[[:space:]]+" "" comment-start))
         (comment-start-region
          (if (and
               comment-end
               (not (string-equal "" comment-end)))
              comment-start-no-space
            (concat
             comment-start-no-space comment-start-no-space))))
    ;; the "^" not needed by outline, but by outxxtra (?)
    (concat "^" comment-start-region " [*]+ ")))

;; *** Calculate the outline-level

(defun outxxtra-calc-outline-level ()
  "Calculate the right outline level for the outxxtra-outline-regexp"
  (save-excursion
    (save-match-data
      (let ((len (- (match-end 0) (match-beginning 0))))
        (- len (+ 2 (* 2 (length (format "%s" comment-start))))))))) 


;; *** Fontify the headlines

;; Org-style highlighting of the headings
(defun outxxtra-fontify-headlines (outline-regexp)
  ;; (interactive)
  ;; (setq outline-regexp (tj/outline-regexp))

  ;; highlight the headings
  ;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html
  ;; use `M-x customize-apropos-faces' to customize faces
  ;; to find the corresponding face for each outline level, see
  ;; `org-faces.el'

  ;; Added `\n?', after having read the following chunk of code (from org.el):
  ;; `(,(if org-fontify-whole-heading-line
  ;;        "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)"
  ;;      "^\\(\\**\\)\\(\\* \\)\\(.*\\)")

  (let ((outxxtra-fontify-whole-heading-line "") ; "\n?")
        (heading-1-regexp
         (concat (substring outline-regexp 0 -1) 
                 "\\{1\\} \\(.*" outxxtra-fontify-whole-heading-line "\\)"))
        (heading-2-regexp
         (concat (substring outline-regexp 0 -1)
                 "\\{2\\} \\(.*" outxxtra-fontify-whole-heading-line "\\)"))
        (heading-3-regexp
         (concat (substring outline-regexp 0 -1)
                 "\\{3\\} \\(.*" outxxtra-fontify-whole-heading-line "\\)"))
        (heading-4-regexp
         (concat (substring outline-regexp 0 -1)
                 "\\{4,\\} \\(.*" outxxtra-fontify-whole-heading-line "\\)"))
         (heading-5-regexp
         (concat (substring outline-regexp 0 -1)
                 "\\{5\\} \\(.*" outxxtra-fontify-whole-heading-line "\\)"))
        (heading-6-regexp
         (concat (substring outline-regexp 0 -1)
                 "\\{6,\\} \\(.*" outxxtra-fontify-whole-heading-line "\\)"))
        (heading-7-regexp
         (concat (substring outline-regexp 0 -1)
                 "\\{7,\\} \\(.*" outxxtra-fontify-whole-heading-line "\\)"))
        (heading-8-regexp
         (concat (substring outline-regexp 0 -1)
                 "\\{8,\\} \\(.*" outxxtra-fontify-whole-heading-line "\\)")))
    (font-lock-add-keywords
     nil
     `((,heading-1-regexp 1 'outxxtra-level-1 t)
       (,heading-2-regexp 1 'outxxtra-level-2 t)
       (,heading-3-regexp 1 'outxxtra-level-3 t)
       (,heading-4-regexp 1 'outxxtra-level-4 t)
       (,heading-5-regexp 1 'outxxtra-level-5 t)
       (,heading-6-regexp 1 'outxxtra-level-6 t)
       (,heading-7-regexp 1 'outxxtra-level-7 t)
       (,heading-8-regexp 1 'outxxtra-level-8 t)))))

;; *** Set outline-regexp und outline-level

(defun outxxtra-set-local-outline-regexp-and-level (regexp &optional fun)
   "Set `outline-regexp' locally to REGEXP and `outline-level' to FUN."
	(make-local-variable 'outline-regexp)
	(setq outline-regexp regexp)
	(and fun
             (make-local-variable 'outline-level)
             (setq outline-level fun)))

;; *** Outxxtra hook-functions

;; attempt to load a feature/library, failing silently
;; copied from http://www.mygooglest.com/fni/dot-emacs.html
(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
      (progn
        (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature))
     nil)))

;; TODO coordinate outxxtra, outorg and orgstruct
(defun outxxtra-hook-function ()
  "Add this function to outline-minor-mode-hook"
  (let ((out-regexp (outxxtra-calc-outline-regexp)))
    (outxxtra-set-local-outline-regexp-and-level
     out-regexp 'outxxtra-calc-outline-level)
    (outxxtra-fontify-headlines out-regexp)
    (try-require 'outorg2)))

;; ;; add this to your .emacs
;; (add-hook 'outline-minor-mode-hook 'outxxtra-hook-function)

;; ** Commands

;; *** Additional outline commands (from `out-xtra').

(defun outline-hide-sublevels (keep-levels)
  "Hide everything except the first KEEP-LEVEL headers."
  (interactive "p")
  (if (< keep-levels 1)
      (error "Must keep at least one level of headers"))
  (setq keep-levels (1- keep-levels))
  (save-excursion
    (goto-char (point-min))
    (hide-subtree)
    (show-children keep-levels)
    (condition-case err
      (while (outline-get-next-sibling)
	(hide-subtree)
	(show-children keep-levels))
      (error nil))))

(defun outline-hide-other ()
  "Hide everything except for the current body and the parent headings."
  (interactive)
  (outline-hide-sublevels 1)
  (let ((last (point))
	(pos (point)))
    (while (save-excursion
	     (and (re-search-backward "[\n\r]" nil t)
		  (eq (following-char) ?\r)))
      (save-excursion
	(beginning-of-line)
	(if (eq last (point))
	    (progn
	      (outline-next-heading)
	      (outline-flag-region last (point) ?\n))
	  (show-children)
	  (setq last (point)))))))

;; * Keybindings.

;; We provide bindings for all keys.
;; FIXME: very old stuff from `out-xtra' - still necesary?

(if (fboundp 'eval-after-load)
    ;; FSF Emacs 19.
    (eval-after-load "outline"
      '(let ((map (lookup-key outline-minor-mode-map
			      outline-minor-mode-prefix)))
	 (define-key map "\C-t" 'hide-body)
	 (define-key map "\C-a" 'show-all)
	 (define-key map "\C-c" 'hide-entry)
	 (define-key map "\C-e" 'show-entry)
	 (define-key map "\C-l" 'hide-leaves)
	 (define-key map "\C-k" 'show-branches)
	 (define-key map "\C-q" 'outline-hide-sublevels)
	 (define-key map "\C-o" 'outline-hide-other)
         ;; TODO move this to outorg2.el
         ;; TODO differentiate between called in code or edit buffer
         (define-key map "'" 'outorg2-edit-as-org)
         ;; TODO add these keybindings to org-mode keymap (all?)
         ;; (define-key map "\C-s" 'outxxtra-save-edits)
         ;; (define-key map "\C-c" 'outxxtra-save-edits)
         ;; (define-key map "'" 'outxxtra-save-edits)

	 (define-key outline-minor-mode-map [menu-bar hide hide-sublevels]
	   '("Hide Sublevels" . outline-hide-sublevels))
	 (define-key outline-minor-mode-map [menu-bar hide hide-other]
	   '("Hide Other" . outline-hide-other))
	 (if (fboundp 'update-power-keys)
	     (update-power-keys outline-minor-mode-map))))

  (if (string-match "Lucid" emacs-version)
      (progn				;; Lucid Emacs 19
	(defconst outline-menu
	  '(["Up" outline-up-heading t]
	    ["Next" outline-next-visible-heading t]
	    ["Previous" outline-previous-visible-heading t]
	    ["Next Same Level" outline-forward-same-level t]
	    ["Previous Same Level" outline-backward-same-level t]
	    "---"
	    ["Show All" show-all t]
	    ["Show Entry" show-entry t]
	    ["Show Branches" show-branches t]
	    ["Show Children" show-children t]
	    ["Show Subtree" show-subtree t]
	    "---"
	    ["Hide Leaves" hide-leaves t]
	    ["Hide Body" hide-body t]
	    ["Hide Entry" hide-entry t]
	    ["Hide Subtree" hide-subtree t]
	    ["Hide Other" outline-hide-other t]
	    ["Hide Sublevels" outline-hide-sublevels t]))

        (defun outline-add-menu ()
	  (set-buffer-menubar (copy-sequence current-menubar))
	  (add-menu nil "Outline" outline-menu))

	(add-hook 'outline-minor-mode-hook 'outline-add-menu)
	(add-hook 'outline-mode-hook 'outline-add-menu)
	(add-hook 'outline-minor-mode-off-hook
                  (function (lambda () (delete-menu-item '("Outline")))))))

  ;; Lucid Emacs or Emacs 18.
  (require 'outln-18)
  (let ((map (lookup-key outline-minor-mode-map outline-minor-mode-prefix)))
    ;; Should add a menu here.
    (define-key map "\C-t" 'hide-body)
    (define-key map "\C-a" 'show-all)
    (define-key map "\C-c" 'hide-entry)
    (define-key map "\C-e" 'show-entry)
    (define-key map "\C-l" 'hide-leaves)
    (define-key map "\C-k" 'show-branches)
    (define-key map "\C-q" 'outline-hide-sublevels)
    (define-key map "\C-o" 'outline-hide-other)))


;; * Run hooks and provide

(run-hooks 'outxxtra-hook)

(provide 'outxxtra)

;; Local Variables:
;; coding: utf-8
;; ispell-local-dictionary: "en_US"
;; End:

;; outxxtra.el ends here
