;;; pulsar.el --- Pulse highlight on demand or after select functions -*- lexical-binding: t -*-

;; Copyright (C) 2022-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/pulsar
;; Version: 1.3.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience, pulse, highlight

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is a small Emacs package that automatically highlights the
;; current line after certain functions are invoked.  It can also
;; highlight a line or region on demand.  The idea is to make it
;; easier to find where the point is, what was affected, and also to
;; bring attention to something in a buffer.
;;
;; Why the name "pulsar"?  It sounds like "pulse" and is a
;; recognisable word.  Though if you need a backronym, consider
;; "Pulsar Unquestionably Luminates, Strictly Absent the Radiation".

;;; Code:

(require 'pulse)

(defgroup pulsar ()
  "Pulse highlight line on demand or after running select functions.
Extension of `pulse.el'."
  :group 'editing)

;;;; User options

(make-obsolete-variable 'pulsar-pulse nil "1.3.0")

(defcustom pulsar-delay 0.05
  "Delay between increments of a pulse.
Together with `pulsar-iterations' control the overall duration of
a pulse.  Only applies when `pulsar-pulse' is non-nil."
  :type 'number
  :package-version '(pulsar . "0.1.0")
  :group 'pulsar)

(defcustom pulsar-iterations pulse-iterations
  "Number of iterations in a pulse highlight.
Together with `pulsar-delay' control the overall duration of a
pulse.  Only applies when `pulsar-pulse' is non-nil."
  :type 'number
  :package-version '(pulsar . "0.1.0")
  :group 'pulsar)

(defcustom pulsar-pulse-functions
  '(ace-window
    backward-page
    bookmark-jump
    delete-other-windows
    delete-window
    dired-maybe-insert-subdir
    dired-up-directory
    dired-goto-file
    dired-next-dirline
    dired-prev-dirline
    evil-goto-first-line
    evil-goto-line
    evil-scroll-down
    evil-scroll-line-to-bottom
    evil-scroll-line-to-center
    evil-scroll-line-to-top
    evil-scroll-page-down
    evil-scroll-page-up
    evil-scroll-up
    forward-page
    goto-line
    handle-switch-frame
    imenu
    logos-backward-page-dwim
    logos-forward-page-dwim
    handle-select-window
    move-to-window-line-top-bottom
    narrow-to-defun
    narrow-to-page
    narrow-to-region
    next-buffer
    next-error
    next-error-recenter
    next-multiframe-window
    occur-mode-goto-occurrence
    org-backward-heading-same-level
    org-forward-heading-same-level
    org-next-visible-heading
    org-previous-visible-heading
    other-window
    outline-backward-same-level
    outline-forward-same-level
    outline-next-visible-heading
    outline-previous-visible-heading
    outline-up-heading
    previous-buffer
    previous-error
    quit-window
    recenter-top-bottom
    reposition-window
    scroll-down-command
    scroll-up-command
    tab-close
    tab-new
    tab-next
    tab-previous
    widen
    windmove-down
    windmove-left
    windmove-right
    windmove-swap-states-down
    windmove-swap-states-left
    windmove-swap-states-right
    windmove-swap-states-up
    windmove-up)
  "Functions that call `pulsar-pulse-line' after invocation.
This only takes effect when `pulsar-mode' (buffer-local) or
`pulsar-global-mode' is enabled."
  :type '(repeat function)
  :package-version '(pulsar . "1.1.0")
  :group 'pulsar)

(make-obsolete-variable
 'pulsar-pulse-region-common-functions
 "the list of functions it defined is now the default for `pulsar-pulse-region-functions'"
 "1.3.0")

(defcustom pulsar-pulse-region-functions
  '(append-next-kill
    delete-region
    kill-line
    kill-paragraph backward-kill-paragraph
    kill-rectangle
    kill-region
    kill-ring-save
    kill-sentence backward-kill-sentence
    kill-sexp backward-kill-sexp
    kill-visual-line
    kill-whole-line
    kill-word backward-kill-word
    open-rectangle
    undo
    yank
    yank-rectangle)
  "Functions that highlight the affected region after invocation.
When the value is nil, no pulsing is in effect.  Otherwise, the value is
a list of functions that operate on the region.

This only takes effect when `pulsar-mode' (buffer-local) or
`pulsar-global-mode' is enabled."
  :type '(choice
          (const :tag "Do not pulse the region" nil)
          (repeat function))
  :package-version '(pulsar . "1.2.0")
  :group 'pulsar)

(defcustom pulsar-resolve-pulse-function-aliases t
  "When non-nil, resolve function aliases in pulsar function lists.
The affected lists are `pulsar-pulse-functions' and
`pulsar-pulse-region-functions'.  This allows pulsar to respect,
e.g., `tab-new' \"parent,\" `tab-bar-new-tab', and vice-versa,
enabling Pulsar to respect `tab-bar-new-tab' alias `tab-new'."
  :type 'boolean
  :package-version '(pulsar . "1.1.0")
  :group 'pulsar)

(defcustom pulsar-inhibit-hidden-buffers t
  "When non-nil, `pulsar-mode' will not be enabled in hidden buffers.
Hidden buffers are those whose name starts with a space character.  They
are not meant to be touched by the user, so pulsing in them is not
necessary.  This option is provided in case there is some scenario where
pulsing makes sense."
  :type 'boolean
  :package-version '(pulsar . "1.2.0")
  :group 'pulsar)

(defcustom pulsar-pulse-on-window-change nil
  "When non-nil, enable pulsing on every window change.
This covers all commands or functions that affect the current
window.  Users who prefer to trigger a pulse only after select
functions (e.g. only after `other-window') are advised to set
this variable to nil and update the `pulsar-pulse-functions'
accordingly."
  :type 'boolean
  :package-version '(pulsar . "1.2.0")
  :group 'pulsar)

(defconst pulsar--face-choice-widget
  '(radio :tag "Select another face"
          (face :tag "Generic pulse.el face" pulsar-generic)
          (face :tag "Red style" pulsar-red)
          (face :tag "Green style" pulsar-green)
          (face :tag "Yellow style" pulsar-yellow)
          (face :tag "Blue style" pulsar-blue)
          (face :tag "Magenta style" pulsar-magenta)
          (face :tag "Cyan style" pulsar-cyan)
          (face :tag "Other face (must have a background)"))
  "Widget for `defcustom' type to select a face.")

(defconst pulsar--face-with-default-and-choice-widget
  `(choice :tag "Choose a face"
           (variable :tag "Use the `pulsar-face'" pulsar-face)
           ,pulsar--face-choice-widget)
  "Like `pulsar--face-choice-widget' plus the `pulsar-face' option.")

(defcustom pulsar-face 'pulsar-generic
  "Face of the regular pulse line effect (`pulsar-pulse-line').
The default is `pulsar-generic' which reuses the standard face
from the underlying pulse library.  Users can select one among
`pulsar-red', `pulsar-green', `pulsar-yellow', `pulsar-blue',
`pulsar-magenta', `pulsar-cyan', or any other face that has a
background attribute."
  :type pulsar--face-choice-widget
  :package-version '(pulsar . "0.2.0")
  :group 'pulsar)

(defcustom pulsar-highlight-face 'pulsar-face
  "Face used by temporary or permanent static highlights.
These are done by commands such as `pulsar-highlight-temporarily'
and `pulsar-highlight-permanently'."
  :type pulsar--face-with-default-and-choice-widget
  :package-version '(pulsar . "1.3.0")
  :group 'pulsar)

(defcustom pulsar-region-face pulsar-face
  "Face to pulse a region that has note changed."
  :type pulsar--face-with-default-and-choice-widget
  :package-version '(pulsar . "1.2.0")
  :group 'pulsar)

(defcustom pulsar-region-change-face pulsar-face
  "Face to pulse a region that has changed (added or removed)."
  :type pulsar--face-with-default-and-choice-widget
  :package-version '(pulsar . "1.2.0")
  :group 'pulsar)

(defcustom pulsar-window-change-face pulsar-face
  "Face to indicate the current position on window changes."
  :type pulsar--face-with-default-and-choice-widget
  :package-version '(pulsar . "1.2.0")
  :group 'pulsar)

;;;; Faces

(defgroup pulsar-faces ()
  "Faces for `pulsar.el'."
  :group 'pulsar)

(defface pulsar-generic
  '((t :inherit pulse-highlight-start-face :extend t))
  "Default value of `pulsar-face'."
  :group 'pulsar-faces)

(defface pulsar-red
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffcccc")
    (((class color) (min-colors 88) (background dark))
     :background "#77002a")
    (t :inverse-video t))
  "Alternative red face for `pulsar-face'."
  :group 'pulsar-faces)

(defface pulsar-green
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#aceaac")
    (((class color) (min-colors 88) (background dark))
     :background "#00422a")
    (t :inverse-video t))
  "Alternative green face for `pulsar-face'."
  :group 'pulsar-faces)

(defface pulsar-yellow
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#fff29a")
    (((class color) (min-colors 88) (background dark))
     :background "#693200")
    (t :inverse-video t))
  "Alternative yellow face for `pulsar-face'."
  :group 'pulsar-faces)

(defface pulsar-blue
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8fcfff")
    (((class color) (min-colors 88) (background dark))
     :background "#242679")
    (t :inverse-video t))
  "Alternative blue face for `pulsar-face'."
  :group 'pulsar-faces)

(defface pulsar-magenta
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffccff")
    (((class color) (min-colors 88) (background dark))
     :background "#71206a")
    (t :inverse-video t))
  "Alternative magenta face for `pulsar-face'."
  :group 'pulsar-faces)

(defface pulsar-cyan
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8eecf4")
    (((class color) (min-colors 88) (background dark))
     :background "#004065")
    (t :inverse-video t))
  "Alternative cyan face for `pulsar-face'."
  :group 'pulsar-faces)

;;;; Pulse functions

(defun pulsar--indentation-only-line-p ()
  "Return non-nil if current line has only indentation."
  (save-excursion
    (goto-char (line-beginning-position))
    (and (not (bobp))
         (or (beginning-of-line 1) t)
         (save-match-data
           (looking-at "^[\s\t]+")))))

(defun pulsar--buffer-end-p ()
  "Return non-nil if point is at the end of the buffer."
  (unless (pulsar--indentation-only-line-p)
    (or (eobp) (eq (point) (point-max)))))

(defun pulsar--start ()
  "Return appropriate line start.
When in the minibuffer, return the `point-min', which includes
the text of the prompt.  This way, a pulse will be visible even
if the minibuffer has no initial text (e.g. `M-x' with the
default completion setup)."
  (cond
   ((minibufferp)
    (save-excursion
      (let ((inhibit-field-text-motion t))
        (goto-char (minibuffer-prompt-end))
        (line-beginning-position))))
   ((and (pulsar--buffer-end-p) (eq (char-before) ?\n))
    (line-beginning-position 0))
   (t
    (line-beginning-position))))

(defun pulsar--end ()
  "Return appropriate line end."
  (if (and (pulsar--buffer-end-p) (eq (char-before) ?\n))
      (line-beginning-position 1)
    (line-beginning-position 2)))

(defun pulsar--get-line-boundaries ()
  "Return cons cell of line beginning and end positions.
If possible, make the end be 1+ its value, so that the highlight can be
extended to the edge of the window."
  (cons (pulsar--start) (pulsar--end)))

(defun pulsar--get-region-boundaries ()
  "Return cons cell with region boundaries."
  (cons (region-beginning) (region-end)))

(defun pulsar--get-line-or-region-boundaries ()
  "Return cons cell of the active region boundaries or current line."
  (if (region-active-p)
      (pulsar--get-region-boundaries)
    (pulsar--get-line-boundaries)))

(defun pulsar--create-pulse (locus face)
  "Create a pulse spanning the LOCUS using FACE.
LOCUS is a cons cell with two buffer positions."
  (let ((overlay (make-overlay (car locus) (cdr locus))))
    (overlay-put overlay 'pulse-delete t)
    (overlay-put overlay 'window (frame-selected-window))
    (pulse-momentary-highlight-overlay overlay face)))

(define-obsolete-function-alias
  'pulsar-pulse-region
  'pulsar-highlight-pulse
  "1.3.0")

;;;###autoload
(defun pulsar-pulse-line ()
  "Create a pulse highlight for the current line.
Also see `pulsar-highlight-pulse'."
  (interactive)
  (let ((pulse-delay pulsar-delay)
        (pulse-flag t)
        (pulse-iterations pulsar-iterations))
    (pulsar--create-pulse (pulsar--get-line-boundaries) pulsar-face)))

;;;###autoload
(defun pulsar-highlight-pulse (&optional locus)
  "Highlight the current LOCUS by pulsing it.
To pulse is to add a colour and then gradually fade it away.  The pulse
is subject to `pulsar-delay' and `pulsar-iterations'.

When the region is active, LOCUS covers the region boundaries.
Otherwise, LOCUS spans the current line.

For highlights without a pulse, see `pulsar-highlight-temporarily' and
`pulsar-highlight-permanently'."
  (interactive (list (pulsar--get-line-or-region-boundaries)))
  (let ((pulse-flag t)
        (pulse-delay pulsar-delay)
        (pulse-iterations pulsar-iterations))
    (pulsar--create-pulse locus pulsar-face)))

(define-obsolete-function-alias
  'pulsar-highlight-line
  'pulsar-highlight-temporarily
  "1.3.0")

(define-obsolete-function-alias
  'pulsar-highlight-dwim
  'pulsar-highlight-temporarily
  "1.3.0")

;;;###autoload
(defun pulsar-highlight-temporarily (locus)
  "Temporarily highlight the current LOCUS.
Unlike `pulsar-highlight-pulse', never pulse the current line.  Keep the
highlight in place until another command is invoked.  This is what makes
the highlight temporary.

For a permanent highlight, see `pulsar-highlight-permanently'."
  (interactive (list (pulsar--get-line-or-region-boundaries)))
  (let ((pulse-flag nil)
        (pulse-delay nil)
        (pulse-iterations nil))
    (pulsar--create-pulse locus pulsar-highlight-face)))

;;;###autoload
(defun pulsar-highlight-permanently (locus)
  "Set a permanent highlight to the current LOCUS.
When the region is active, LOCUS is a cons cell of positions
corresponding to the region boundaries.  Otherwise it is a cons cell of
positions corresponding to the beginning and end of the current line.

Remove the highlight with `pulsar-highlight-permanently-remove' or
toggle it with `pulsar-highlight-permanently'.

For a temporary highlight use `pulsar-highlight-temporarily' and
related."
  (interactive (list (pulsar--get-line-or-region-boundaries)))
  (pcase-let* ((`(,beg . ,end) locus)
               (overlay (make-overlay beg end)))
    (overlay-put overlay 'face pulsar-highlight-face)
    (overlay-put overlay 'pulsar-permanent t)))

(defun pulsar--permanent-p (locus)
  "Return overlays if LOCUS has overlays with `pulsar-permanent' property.
LOCUS is a cons cell with two buffer positions."
  (seq-filter
   (lambda (overlay)
     (overlay-get overlay 'pulsar-permanent))
   (overlays-in (car locus) (cdr locus))))

(defun pulsar-highlight-permanently-remove (locus)
  "Remove the permanent highlight from the current LOCUS.
If the region is active, operate on its boundaries.  If the highlight
was a region-wide one (i.e. was created for the region boundaries rather
than the line boundaries), then remove it even if the region is not
active.

With LOCUS as a prefix argument, remove all such highlights in the
current buffer.  In this case, whether the region is active or not is
irrelevant.

When called from Lisp LOCUS is a cons cell with two buffer positions.

Set a permanent highlight with `pulsar-highlight-permanently'."
  (interactive
   (list
    (cond
     (current-prefix-arg
      (cons (point-min) (point-max)))
     ((region-active-p)
      (cons (region-beginning) (region-end)))
     (t
      (pulsar--get-line-boundaries)))))
  (if-let* ((overlays (pulsar--permanent-p locus)))
      (dolist (overlay overlays) (delete-overlay overlay))
    (user-error "No Pulsar permanent highlights found")))

;;;###autoload
(defun pulsar-highlight-permanently-dwim (locus)
  "Do-What-I-Mean with a permanent highlighting of the current LOCUS.
When there is a highlight, remove it, else add it.

If the region is active, LOCUS corresponds to its boundaries.  If there
is no region, then LOCUS corresponds to the boundaries of the current
line."
  (interactive
   (list
    (cond
     ((region-active-p)
      (cons (region-beginning) (region-end)))
     (t
      (pulsar--get-line-boundaries)))))
  (if-let* ((overlays (pulsar--permanent-p locus)))
      (dolist (overlay overlays) (delete-overlay overlay))
    (pulsar-highlight-permanently locus)))

;;;;; Convenience functions

(define-obsolete-function-alias
  'pulsar-pulse-with-face
  'pulsar-define-pulse-with-face
  "1.0.0")

;;;###autoload
(defmacro pulsar-define-pulse-with-face (face)
  "Produce function to pulse the current line with FACE.
If FACE starts with the `pulsar-' prefix, remove it and keep only
the remaining text.  The assumption is that something like
`pulsar-red' will be convered to `red', thus deriving a function
named `pulsar-pulse-line-red'.  Any other FACE is taken as-is."
  (declare (indent function))
  (let* ((face-string (symbol-name face))
         (face-name (if (string-prefix-p "pulsar-" face-string)
                        (replace-regexp-in-string "pulsar-" "" face-string)
                      face-string)))
    `(defun ,(intern (format "pulsar-pulse-line-%s" face-name)) ()
       ,(format "Like `pulsar-pulse-line' but uses the `%s' face.
The idea with this is to run it in special hooks or contexts
where you need a different color than what Pulsar normally
uses (per the user option `pulsar-face')" face)
       (interactive)
       (pulsar--create-pulse (pulsar--get-line-boundaries) ',face))))

(pulsar-define-pulse-with-face pulsar-red)
(pulsar-define-pulse-with-face pulsar-green)
(pulsar-define-pulse-with-face pulsar-yellow)
(pulsar-define-pulse-with-face pulsar-blue)
(pulsar-define-pulse-with-face pulsar-magenta)
(pulsar-define-pulse-with-face pulsar-cyan)

;;;; Mode setup

(define-minor-mode pulsar-mode
  "Set up pulsar for each function in pulsar functions lists.
The effective lists are `pulsar-pulse-functions' and
`pulsar-pulse-region-functions'.  This is a buffer-local mode.
Also check `pulsar-global-mode'."
  :global nil
  (if pulsar-mode
      (progn
        (when pulsar-resolve-pulse-function-aliases
          (pulsar-resolve-function-aliases))
        (add-hook 'post-command-hook #'pulsar-post-command-pulse nil 'local)
        (add-hook 'after-change-functions #'pulsar--after-change-function nil 'local)
        (when pulsar-pulse-on-window-change
          (add-hook 'window-buffer-change-functions #'pulsar--pulse-on-window-change nil 'local)
          (add-hook 'window-selection-change-functions #'pulsar--pulse-on-window-change nil 'local)))
    (remove-hook 'post-command-hook #'pulsar-post-command-pulse 'local)
    (remove-hook 'after-change-functions #'pulsar--after-change-function 'local)
    (remove-hook 'window-buffer-change-functions #'pulsar--pulse-on-window-change 'local)
    (remove-hook 'window-selection-change-functions #'pulsar--pulse-on-window-change 'local)))

(defun pulsar--on ()
  "Enable `pulsar-mode'."
  (unless (or pulsar-mode
              (minibufferp)
              (and pulsar-inhibit-hidden-buffers (string-prefix-p " " (buffer-name))))
    (let (inhibit-quit)
      (pulsar-mode 1))))

;;;###autoload
(define-globalized-minor-mode pulsar-global-mode pulsar-mode pulsar--on)

(defun pulsar--pulse-on-window-change (window)
  "Run `pulsar-pulse-line' on WINDOW change."
  (when (and pulsar-mode
             pulsar-pulse-on-window-change
             (eq (frame-selected-window) window)
             (not (minibufferp))
             ;; Avoid double pulsing when both
             ;; pulsar-pulse-on-window-change and
             ;; pulsar-pulse-functions are in effect.
             (not (memq this-command pulsar-pulse-functions))
             (not (memq real-this-command pulsar-pulse-functions)))
    (pulsar--create-pulse (pulsar--get-line-boundaries) pulsar-window-change-face)))

(defvar-local pulsar--pulse-region-changes nil)

;; This feature is heavily inspired by Daniel Mendler's `goggles' package.
(defun pulsar--after-change-function (beg end len)
  "Provide `after-change-functions' hook to accumulate buffer edits.
Changes are defined by BEG, END, LEN:

- BEG and END mark the region of text.
- LEN is zero for insertions.
- LEN is the extent of deletions and BEG==END."
  (when (or (memq this-command pulsar-pulse-region-functions)
            (memq real-this-command pulsar-pulse-region-functions))
    (when (and (zerop len) (= beg end)) ; In case of a deletion
      (when (> beg (buffer-size))
        (setq beg (1- beg)))
      (setq end (1+ beg)))
    (push (cons (copy-marker beg) (copy-marker end)) pulsar--pulse-region-changes)))

(defun pulsar-post-command-pulse ()
  "Pulse current line, accumulated edits, or selected region."
  (cond
   ((or (memq this-command pulsar-pulse-functions)
        (memq real-this-command pulsar-pulse-functions))
    (pulsar--create-pulse (pulsar--get-line-boundaries) pulsar-face))
   ;; Extract the outer limits of the affected region from
   ;; accumulated changes. NOTE: Non-contiguous regions such as
   ;; rectangles will pulse their contiguous bounds.
   (pulsar--pulse-region-changes
    (let ((beg (apply #'min (mapcar #'car pulsar--pulse-region-changes)))
          (end (apply #'max (mapcar #'cdr pulsar--pulse-region-changes))))
      (setq pulsar--pulse-region-changes nil)
      (pulsar--create-pulse
       (if (eq beg end)
           (cons beg (+ beg 1))
         (cons beg end))
       pulsar-region-change-face)))
   ;; Pulse the selected region for commands that did not cause
   ;; buffer changes; e.g., kill-ring-save.
   ((or (memq this-command pulsar-pulse-region-functions)
        (memq real-this-command pulsar-pulse-region-functions))
    (pulsar--create-pulse (pulsar--get-line-or-region-boundaries) pulsar-region-face))))

;; TODO 2024-11-26: Deprecate this at some point to prefer Emacs core.
(defun pulsar--function-alias-p (func &optional _noerror)
  "Return nil if FUNC is not a function alias.
If FUNC is a function alias, return the function alias chain.

This is a copy of `function-alias-p' for backward-compatibility and will
be deleted from Pulsar in future versions of Emacs."
  (declare (advertised-calling-convention (func) "30.1")
           (side-effect-free error-free))
  (let ((chain nil))
    (while (and (symbolp func)
                (setq func (symbol-function func))
                (symbolp func))
      (push func chain))
    (nreverse chain)))

(defun pulsar--find-fn-aliases (fns)
  "Return a list of aliases for the FNS symbols.
This is essentially the inverse of `pulsar--function-alias-p' for a list
of function symbols."
  (let ((aliases))
    (mapatoms (lambda (sym)
                (when (and
                       (commandp sym)
                       (memq (symbol-function sym) fns))
                  (push sym aliases))))
    aliases))

(defun pulsar--find-union (functions)
  "Find the union of FUNCTIONS and their aliases."
  (seq-union functions
             (seq-union (pulsar--find-fn-aliases functions)
                        (flatten-list (mapcar #'pulsar--function-alias-p functions)))))

(defun pulsar-resolve-function-aliases ()
  "Amend registered functions to respect function aliases.
Functions are registered in `pulsar-pulse-functions' and
`pulsar-pulse-region-functions'.  This is called automatically
when `pulsar-resolve-pulse-function-aliases' is non-nil.

You may also call this manually in your configuration after setting
`pulsar-pulse-functions'.  In that case, you would prefer
`pulsar-resolve-pulse-function-aliases' to be nil."
  (setq pulsar-pulse-functions (pulsar--find-union pulsar-pulse-functions))
  (setq pulsar-pulse-region-functions (pulsar--find-union pulsar-pulse-region-functions)))

;;;; Recentering commands

(defun pulsar-recenter-top ()
  "Reposition point at the top of the window and pulse line."
  (interactive)
  (recenter 0)
  (call-interactively 'pulsar-highlight-pulse))

(defun pulsar-recenter-center ()
  "Reposition point at the center of the window and pulse line."
  (interactive)
  (recenter nil)
  (call-interactively 'pulsar-highlight-pulse))

(defalias 'pulsar-recenter-middle 'pulsar-recenter-center
  "Alias for `pulsar-recenter-center'.")

;;;; Reveal contents of Org or Outline headings

(declare-function org-at-heading-p "org" (&optional _))
(declare-function org-show-entry "org")
(declare-function outline-on-heading-p "outline" (&optional invisible-ok))
(declare-function outline-show-entry "outline")

(defun pulsar-reveal-entry ()
  "Reveal Org or Outline entry.
Use this in combination with `pulsar-recenter-top' or
`pulsar-recenter-center'."
  (cond
   ((and (eq major-mode 'org-mode)
         (org-at-heading-p))
    (org-show-entry))
   ((and (or (eq major-mode 'outline-mode)
             (bound-and-true-p outline-minor-mode))
         (outline-on-heading-p))
    (outline-show-entry))))

(provide 'pulsar)

;;; pulsar.el ends here
