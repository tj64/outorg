- [outorg.el &#x2014; Org-style comment editing](#outorg.el-&#x2014;-org-style-comment-editing)
  - [MetaData](#metadata)
  - [Commentary](#commentary)
    - [About outorg](#about-outorg)
    - [Installation](#installation)
    - [Bugs and Shortcomings](#bugs-and-shortcomings)
    - [Emacs Version](#emacs-version)
  - [ChangeLog](#changelog)

# outorg.el &#x2014; Org-style comment editing

Author: Thorsten Jolitz <tjolitz AT gmail DOT com>
Version: 1.0
URL: <https://github.com/tj64/outorg>

## MetaData

## Commentary

### About outorg

[NOTE: For the sake of adding this library to MELPA, headlines
had to be converted back from 'Org-mode style' to 'oldschool',
and a few extra lines of required information had to be added on
top of the MetaData section - just to comply with the required
file formatting. All outshine, outorg and navi-mode functionality
still works with this file. See my
[iOrg](https://github.com/tj64/iorg) repository for examples of
Emacs-Lisp and PicoLisp files structured 'the outshine way'.]

\`outorg' is like "reverse Org-Babel": editing of comment-sections
from source-code files in temporary Org-mode buffers instead of
editing of Org-mode source-blocks in temporary source-code
buffers.

It should be used together with \`outline-minor-mode' and
\`outshine.el'.  Keep in mind, that \`outorg' only works with
outshine-style headlines like those produced by calling
\`comment-region' on Org-mode style headlines in a source-code
buffer. Take this file as an example for suitable outline
headlines in an Emacs Lisp buffer. In other major-modes, the
\`comment-start' character ';' of Emacs Lisp would be replaced by
that of the respective major-mode, e.g. '#' in PicoLisp mode or
'%' in LaTeX mode.

\`outorgs' main command is accessible via two different
keybindings

1.  with outline-minor-mode-prefix 'C-c'
    
    ,,&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;
    ,| C-c ' (outorg-edit-as-org)
    ,\`&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;
    
    1.  with outline-minor-mode-prefix 'M-#'
    
    ,,&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;
    ,| M-# M-# (outorg-edit-as-org)
    ,\`&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;

used in source-code buffers where \`outline-minor-mode' is
activated with \`outshine' extensions. The Org-mode edit-buffer
popped up by this command has \`outorg-edit-minor-mode' activated,
a minor-mode with only 2 commands:

,,-&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;
,| M-# (outorg-copy-edits-and-exit)
,| C-x C-s (outorg-save-edits-to-tmp-file)
,\`-&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;&#x2014;

If you want to insert Org-mode source-code or example blocks in
comment-sections, simply outcomment them in the outorg-edit
buffer before calling \`outorg-copy-edits-and-exit'.

### Installation

Insert

;; #+begin<sub>src</sub> emacs-lisp
;;  (require 'outorg)
;; #+end<sub>src</sub>

in your .emacs. Since outshine.el has a soft dependency on outorg
and navi-mode.el has a strong dependency on outshine (and a soft
dependency on outorg), it may be sufficient to just require
navi-mode in case you use all three libraries (as long as Emacs
can find all of them).

### Bugs and Shortcomings

\`outorg' is line-based, it only works with 'one-line' comments,
i.e. with comment-sections like those produced by
\`comment-region' (a command that comments or uncomments each line
in the region). Those special multi-line comments found in many
programming languages are not recognized and lead to undefined
behaviour.

There is no comprehensive set of ERT tests defined for outorg
yet, which would be important given that outorg transforms often
important user files back and forth between two representations.

### Emacs Version

\`outorg.el' works with [GNU Emacs 24.2.1
(x86<sub>64</sub>-unknown-linux-gnu, GTK+ Version 3.6.4) of 2013-01-20 on
eric]. No attempts of testing with older versions or other types
of Emacs have been made (yet).

## ChangeLog

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">date</th>
<th scope="col" class="left">author(s)</th>
<th scope="col" class="right">version</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2013-05-03 Fr&gt;</span></span></td>
<td class="left">Thorsten Jolitz</td>
<td class="right">1.0</td>
</tr>


<tr>
<td class="left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2013-02-11 Mo&gt;</span></span></td>
<td class="left">Thorsten Jolitz</td>
<td class="right">0.9</td>
</tr>
</tbody>
</table>
