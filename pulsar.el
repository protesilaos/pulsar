;;; pulsar.el --- Pulse highlight on demand or after select functions -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Pulsar Development <~protesilaos/pulsar@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/pulsar
;; Mailing-List: https://lists.sr.ht/~protesilaos/pulsar
;; Version: 1.0.1
;; Package-Requires: ((emacs "27.1"))
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
;; This is a small package that temporarily highlights the current line
;; after a given function is invoked.  The affected functions are
;; defined in the user option `pulsar-pulse-functions' and the effect
;; takes place when either `pulsar-mode' (buffer-local) or
;; `pulsar-global-mode' is enabled.
;;
;; The overall duration of the highlight is determined by a combination
;; of `pulsar-delay' and `pulsar-iterations'.  The latter determines the
;; number of blinks in a pulse, while the former sets their delay in
;; seconds before they fade out.  The applicable face is specified in
;; `pulsar-face'.
;;
;; To disable the pulse but keep the temporary highlight, set the user
;; option `pulsar-pulse' to nil.  The current line will remain
;; highlighted until another command is invoked.
;;
;; To highlight the current line on demand, use the `pulsar-pulse-line'
;; command.  When `pulsar-pulse' is non-nil (the default), its highlight
;; will pulse before fading away.  Whereas the `pulsar-highlight-line'
;; command never pulses the line: the highlight stays in place as if
;; `pulsar-pulse' is nil.
;;
;; To help users differentiate between the pulse and highlight effects,
;; the user option `pulsar-highlight-face' controls the presentation of
;; the `pulsar-highlight-line' command.  By default, this variable is
;; the same as `pulsar-face'.
;;
;; Pulsar depends on the built-in `pulse.el' library.
;;
;; Why the name "pulsar"?  It sounds like "pulse" and is a recognisable
;; word.  Though if you need a backronym, consider "Pulsar Unquestionably
;; Luminates, Strictly Absent the Radiation".

;;; Code:

(require 'pulse)

(defgroup pulsar ()
  "Pulse highlight line on demand or after running select functions.
Extension of `pulse.el'."
  :group 'editing)

;;;; User options

(defcustom pulsar-pulse-functions
  '(recenter-top-bottom
    move-to-window-line-top-bottom
    reposition-window
    bookmark-jump
    other-window
    delete-window
    delete-other-windows
    forward-page
    backward-page
    scroll-up-command
    scroll-down-command
    next-buffer
    previous-buffer
    windmove-right
    windmove-left
    windmove-up
    windmove-down
    windmove-swap-states-right
    windmove-swap-states-left
    windmove-swap-states-up
    windmove-swap-states-down
    tab-new
    tab-close
    tab-next
    org-next-visible-heading
    org-previous-visible-heading
    org-forward-heading-same-level
    org-backward-heading-same-level
    outline-backward-same-level
    outline-forward-same-level
    outline-next-visible-heading
    outline-previous-visible-heading
    outline-up-heading)
  "Functions that `pulsar-pulse-line' after invocation.
This only takes effect when `pulsar-mode' (buffer-local) or
`pulsar-global-mode' is enabled."
  :type '(repeat function)
  :package-version '(pulsar . "1.0.0")
  :group 'pulsar)

(make-obsolete 'pulsar-pulse-on-window-change nil "0.5.0")

(defcustom pulsar-face 'pulsar-generic
  "Face of the regular pulse line effect (`pulsar-pulse-line').
The default is `pulsar-generic' which reuses the standard face
from the underlying pulse library.  Users can select one among
`pulsar-red', `pulsar-green', `pulsar-yellow', `pulsar-blue',
`pulsar-magenta', `pulsar-cyan', or any other face that has a
background attribute."
  :type '(radio (face :tag "Generic pulse.el face" pulsar-generic)
                (face :tag "Red style" pulsar-red)
                (face :tag "Green style" pulsar-green)
                (face :tag "Yellow style" pulsar-yellow)
                (face :tag "Blue style" pulsar-blue)
                (face :tag "Magenta style" pulsar-magenta)
                (face :tag "Cyan style" pulsar-cyan)
                (face :tag "Other face (must have a background)"))
  :package-version '(pulsar . "0.2.0")
  :group 'pulsar)

(defcustom pulsar-highlight-face 'pulsar-face
  "Face used in `pulsar-highlight-line'."
  :type '(choice (variable pulsar-face)
                 (radio (face :tag "Generic pulse.el face" pulsar-generic)
                        (face :tag "Red style" pulsar-red)
                        (face :tag "Green style" pulsar-green)
                        (face :tag "Yellow style" pulsar-yellow)
                        (face :tag "Blue style" pulsar-blue)
                        (face :tag "Magenta style" pulsar-magenta)
                        (face :tag "Cyan style" pulsar-cyan)
                        (face :tag "Other face (must have a background)")))
  :package-version '(pulsar . "0.3.0")
  :group 'pulsar)

(defcustom pulsar-pulse t
  "When non-nil enable pulsing.
Otherwise the highlight stays on the current line until another
command is invoked."
  :type 'boolean
  :package-version '(pulsar . "0.2.0")
  :group 'pulsar)

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

(defun pulsar--pulse (&optional no-pulse face start end)
  "Highlight the current line.
With optional NO-PULSE keep the highlight until another command
is invoked.  Otherwise use whatever `pulsar-pulse' entails.

With optional FACE, use it instead of `pulsar-face'.

With optional START and END, highlight the region in-between
instead of the current line."
  (let* ((pulse-flag (if no-pulse nil pulsar-pulse))
         (pulse-delay pulsar-delay)
         (pulse-iterations pulsar-iterations)
         (f (if (facep face) face pulsar-face))
         (o (make-overlay (or start (pulsar--start)) (or end (pulsar--end)))))
    (overlay-put o 'pulse-delete t)
    (overlay-put o 'window (frame-selected-window))
    (pulse-momentary-highlight-overlay o f)))

;;;###autoload
(defun pulsar-pulse-line ()
  "Temporarily highlight the current line.
When `pulsar-pulse' is non-nil (the default) make the highlight
pulse before fading away.  The pulse effect is controlled by
`pulsar-delay' and `pulsar-iterations'.

Also see `pulsar-highlight-line' for a highlight without the
pulse effect."
  (interactive)
  (pulsar--pulse))

;;;###autoload
(defun pulsar-highlight-line ()
  "Temporarily highlight the current line.
Unlike `pulsar-pulse-line', never pulse the current line.  Keep
the highlight in place until another command is invoked.

Use `pulsar-highlight-face' (it is the same as `pulsar-face' by
default)."
  (interactive)
  (pulsar--pulse :no-pulse pulsar-highlight-face))

;;;;; Convenience functions

(define-obsolete-function-alias
  'pulsar-pulse-with-face
  'pulsar-define-pulse-with-face
  "1.0.0")

;;;###autoload
(defmacro pulsar-define-pulse-with-face (face)
  "Produce function to `pulsar--pulse' with FACE.
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
       (pulsar--pulse nil ',face))))

(pulsar-define-pulse-with-face pulsar-red)
(pulsar-define-pulse-with-face pulsar-green)
(pulsar-define-pulse-with-face pulsar-yellow)
(pulsar-define-pulse-with-face pulsar-blue)
(pulsar-define-pulse-with-face pulsar-magenta)
(pulsar-define-pulse-with-face pulsar-cyan)

;;;;; Highlight region

(defvar-local pulsar--rectangle-face-cookie nil
  "Cookie of remapped rectangle region face.")

(autoload 'face-remap-remove-relative "face-remap.el")

(defun pulsar--remove-face-remap ()
  "Remove `pulsar--rectangle-face-cookie'."
  (when pulsar--rectangle-face-cookie
    (face-remap-remove-relative pulsar--rectangle-face-cookie)))

(defvar rectangle-mark-mode)

;; When we highlight a region, it gets the `region' face.  The
;; `pulsar-highlight-dwim' overlays it with `pulsar-highlight-face'
;; using a standard pulse.el mechanism.  If the user tries to expand the
;; region further, it gets its original face.  This function ensures
;; that the rectangle behaves the same way (pulse.el does not handle
;; rectangular regions).
(defun pulsar--remove-rectangle-remap ()
  "Remove face remap from rectangle region when appropriate."
  (when (and (bound-and-true-p rectangle-mark-mode)
             (not (eq this-command 'pulsar-highlight-dwim)))
    (pulsar--remove-face-remap)))

(defun pulsar--highlight-rectangle ()
  "Remap `region' face and set `pulsar--remove-face-remap'."
  (setq pulsar--rectangle-face-cookie
        (face-remap-add-relative 'region pulsar-highlight-face))
  (add-hook 'post-command-hook #'pulsar--remove-rectangle-remap nil t)
  (add-hook 'deactivate-mark-hook #'pulsar--remove-face-remap nil t))

;;;###autoload
(defun pulsar-highlight-dwim ()
  "Temporarily highlight the current line or active region.
The region may also be a rectangle.

For lines, do the same as `pulsar-highlight-line'."
  (interactive)
  (cond
   ((bound-and-true-p rectangle-mark-mode)
    (pulsar--highlight-rectangle))
   ((use-region-p)
    (pulsar--pulse :no-pulse pulsar-highlight-face (region-beginning) (region-end)))
   (t
    (pulsar--pulse :no-pulse pulsar-highlight-face))))

;;;; Mode setup

(define-minor-mode pulsar-mode
  "Set up pulsar for each function in `pulsar-pulse-functions'.
This is a buffer-local mode.  Also check `pulsar-global-mode'."
  :global nil
  (if pulsar-mode
      (add-hook 'post-command-hook #'pulsar--post-command-pulse nil 'local)
    (remove-hook 'post-command-hook #'pulsar--post-command-pulse 'local)))

(defun pulsar--on ()
  "Enable `pulsar-mode'."
  (unless (minibufferp)
    (let (inhibit-quit)
      (pulsar-mode 1))))

;;;###autoload
(define-globalized-minor-mode pulsar-global-mode pulsar-mode pulsar--on)

(defun pulsar--post-command-pulse ()
  "Run `pulsar-pulse-line' for `pulsar-pulse-functions'."
  (when (and (or pulsar-mode pulsar-global-mode)
             (or (memq this-command pulsar-pulse-functions)
                 (memq real-this-command pulsar-pulse-functions)))
    (pulsar-pulse-line)))

(make-obsolete 'pulsar-setup nil "0.3.0")

;;;; Recentering commands

(defun pulsar-recenter-top ()
  "Reposition point at the top of the window and pulse line."
  (interactive)
  (recenter 0)
  (pulsar-pulse-line))

(defun pulsar-recenter-center ()
  "Reposition point at the center of the window and pulse line."
  (interactive)
  (recenter nil)
  (pulsar-pulse-line))

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
