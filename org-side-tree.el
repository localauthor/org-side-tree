;;; org-side-tree.el --- Navigate Org outlines in side window tree          -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: September 7, 2023
;; License: GPL-3.0-or-later
;; Version: 0.5
;; Homepage: https://github.com/localauthor/org-side-tree
;; Package-Requires: ((emacs "28.1"))

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Navigate Org headings via tree outline in a side window.

;; Inspired by and modeled on `org-sidebar-tree' from org-sidebar by Adam
;; Porter (@alphapapa) and `embark-live' from Embark by Omar Antolin
;; (@oantolin).

;; To install, place file on your load-path
;; and include this in your init file:
;; (require 'org-side-tree)

;; To use, Open an Org file and call M-x `org-side-tree'.

;; Regarding support for non-Org files:

;; This package is generally functional in buffers that use `outline-mode' or
;; `outline-minor-mode'. However, the depth and quality of
;; support/functionality in these modes is highly dependent on what the
;; buffer-local value of `outline-regexp' is. Therefore, individual
;; experience may vary. Use advisedly.

;; For example, in `emacs-lisp-mode', consider setting `outline-regexp' as
;; follows: (setq-local outline-regexp ";;;\\(;* [^ \t\n]\\)")

;; To set this automatically for every elisp buffer, add the following lines
;; to your init file:

;; (add-hook 'emacs-lisp-mode-hook
;;     (lambda () (setq-local outline-regexp ";;;\\(;* [^   \t\n]\\)")))
;; (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)


;;; Code:

(require 'org)
(require 'hl-line)

;;;; Customization

(defgroup org-side-tree nil
  "Navigate Org headings via sidebar tree."
  :group 'org
  :prefix "org-side-tree")

(defcustom org-side-tree-display-side 'left
  "Side of frame where Org-Side-Tree buffer will display."
  :type '(choice
	  (const :tag "Left" left)
	  (const :tag "Right" right)
          (const :tag "Bottom" bottom)))

(defcustom org-side-tree-width nil
  "When non-nil, sets width of side-tree window, in characters."
  :type 'integer)

(defcustom org-side-tree-persistent nil
  "When non-nil, use a single buffer for all trees.
When nil, each Org buffer will have its own tree-buffer."
  :type 'boolean)

(defcustom org-side-tree-timer-delay .3
  "Timer to update headings and cursor position.
Changes to this variable will not take effect if there are any
live tree buffers. Kill and reopen tree buffers to see effects."
  :type 'number)

(defcustom org-side-tree-recenter-position .25
  "Setting to determine heading position after `org-side-tree-jump'.
Top is `scroll-margin' lines from the true window top. Middle
redraws the frame and centers point vertically within the window.
Integer number moves current line to the specified absolute
window-line. Float number between 0.0 and 1.0 means the
percentage of the screen space from the top."
  :type '(choice
	  (const :tag "Top" top)
	  (const :tag "Middle" middle)
	  (integer :tag "Line number")
	  (float :tag "Percentage")))

(defcustom org-side-tree-narrow-on-jump nil
  "When non-nil, source buffer is narrowed to subtree."
  :type 'boolean)

(defcustom org-side-tree-enable-folding t
  "Enable folding in Org-Side-Tree buffers.
This feature can cause lag in large buffers. Try increasing
`org-side-tree-timer-delay' to .5 seconds. Or, folding can be toggled locally
with `org-side-tree-toggle-folding'."
  :type 'boolean)

(defcustom org-side-tree-enable-auto-update t
  "When non-nil, tree-buffers will automatically update.
Can be toggled locally by calling `org-side-tree-toggle-auto-update'."
  :type 'boolean)

(defcustom org-side-tree-add-overlays t
  "When non-nil, overlays are included in tree-buffer headings.
This includes `org-num' numbering."
  :type 'boolean)

(defcustom org-side-tree-fontify t
  "When non-nil, tree-buffer headings are fontified to match Org buffer.
When nil, headings are in `org-side-tree-heading-face'."
  :type 'boolean)

(defcustom org-side-tree-cursor t
  "Cursor to use in tree-window buffers.
Changes only take effect after toggling `org-side-tree-mode'.
See for `cursor-type' for possible settings."
  :type '(choice
          (const :tag "Frame default" t)
          (const :tag "Filled box" box)
          (const :tag "Hollow cursor" hollow)
          (const :tag "Vertical bar" bar)
          (const :tag "Horizontal bar" hbar)
          (const :tag "None" nil)))

(defcustom org-side-tree-no-delete-other-windows nil
  "When non-nil tree-window will have `no-delete-other-windows' parameter.
Prevents the side-tree window from closing when calling `delete-other-windows'."
  :type 'boolean)

;;;; Define Mode

(defvar org-side-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") #'push-button)
    (define-key map (kbd "RET") #'push-button)
    (define-key map (kbd "<mouse-1>") #'push-button)
    (define-key map (kbd "n") #'org-side-tree-next-heading)
    (define-key map (kbd "p") #'org-side-tree-previous-heading)
    (make-composed-keymap map special-mode-map))
  "Keymap for `org-side-tree-mode'.")

(define-derived-mode org-side-tree-mode tabulated-list-mode "Org-Side-Tree"
  "Mode for `org-side-tree'.

\\{org-side-tree-mode-map}"
  :group 'org-side-tree
  :interactive nil
  (hl-line-mode)
  (setq tabulated-list-format [("Tree" 100)])
  (org-side-tree-cursor-setup)
  (add-hook 'window-configuration-change-hook #'org-side-tree-window-config nil t))

(defun org-side-tree-cursor-setup ()
  "Set `cursor-type' in tree-buffer, according to `org-side-tree-cursor'."
  (if (bound-and-true-p org-side-tree-cursor)
      (setq-local cursor-type org-side-tree-cursor)
    (add-hook 'post-command-hook #'beginning-of-line nil t)
    (setq-local cursor-type nil)))

(defun org-side-tree-window-config ()
  "Added to `window-configuration-change-hook' in `org-side-tree-mode'."
  (set-window-parameter (selected-window) 'no-delete-other-windows org-side-tree-no-delete-other-windows)
  (set-window-fringes nil 1 1)
  (setq fringe-indicator-alist
        '((truncation nil nil))))

(define-button-type 'org-side-tree
  'action 'org-side-tree-jump
  'face 'org-side-tree-heading-face
  'keymap org-side-tree-mode-map
  'help-echo nil)

;;;; Faces

(defface org-side-tree-heading-face '((t (:inherit default)))
  "Face for headings when `org-side-tree-fontify' is nil.")

;;;; Internal variables

(defvar org-side-tree-timer nil
  "Timer to update headings and cursor position.")

(defvar-local org-side-tree-fold-state nil
  "Fold state of current buffer.")

(defvar org-side-tree-last-point nil
  "Cursor position from the last run of `post-command-hook'.")

;;;; Internal functions

;;;###autoload
(defun org-side-tree ()
  "Create or pop to Org-Side-Tree buffer."
  (interactive)
  (when (org-side-tree-buffer-p)
    (error "Don't tree a tree"))
  (unless (or (derived-mode-p 'org-mode)
              (derived-mode-p 'outline-mode)
              outline-minor-mode)
    (error "Not an outline buffer"))
  (let* ((tree-name (if (default-value 'org-side-tree-persistent)
                        "*Org-Side-Tree*"
                      (format "<tree>%s" (buffer-name))))
         (tree-buffer (get-buffer tree-name))
         (heading (org-side-tree-heading-number))
         (dd default-directory)
         (inv-spec buffer-invisibility-spec))
    (unless (buffer-live-p tree-buffer)
      (save-restriction
        (widen)
        (jit-lock-mode 1)
        (jit-lock-fontify-now))
      (setq tree-buffer (generate-new-buffer tree-name))
      (add-hook 'kill-buffer-hook #'org-side-tree-cleanup nil t)
      (let* ((headings (org-side-tree-get-headings))
             (tree-head-line (or (cadar (org-collect-keywords
                                         '("title")))
                                 "Org-Side-Tree"))
             (tree-mode-line (format "Org-Side-Tree - %s"
                                     (buffer-name))))
        (when (default-value 'org-side-tree-enable-folding)
          (setq-local org-side-tree-enable-folding t))
        (with-current-buffer tree-buffer
          (setq-local default-directory dd)
          (org-side-tree-mode)
          (setq tabulated-list-entries headings)
          (tabulated-list-print t t)
          (when (default-value 'org-side-tree-enable-folding)
            (setq-local org-side-tree-enable-folding t)
            ;; preserve org font-locking
            (setq-local outline-minor-mode-highlight nil)
            (outline-minor-mode 1))
          (setq buffer-invisibility-spec inv-spec)
          (setq header-line-format tree-head-line)
          (setq mode-line-format tree-mode-line))))
    (org-side-tree-set-timer)
    (switch-to-buffer (buffer-name))
    (display-buffer-in-side-window
     tree-buffer
     `((side . ,org-side-tree-display-side)
       (window-width . ,org-side-tree-width)))
    (when (default-value 'org-side-tree-persistent)
      (setq-local org-side-tree-persistent
                  (buffer-name))
      (org-side-tree-update))
    (pop-to-buffer tree-buffer)
    (setq window-size-fixed (when org-side-tree-width 'width))
    (pulse-momentary-highlight-one-line)
    (org-side-tree-go-to-heading heading)))

;;;;; Helpers

(defun org-side-tree-buffer-list ()
  "Return list of current Org-Side-Tree buffers."
  (delq nil (append
             (list (get-buffer "*Org-Side-Tree*"))
             (mapcar
              (lambda (buf)
                (org-side-tree-has-tree-p buf))
              (buffer-list)))))

(defun org-side-tree-buffer-p ()
  "Return t if current buffer is a tree-buffer."
  (when (or (equal (buffer-name) "*Org-Side-Tree*")
            (member (current-buffer) (org-side-tree-buffer-list)))
    t))

(defun org-side-tree-has-tree-p (&optional buffer)
  "Return tree-buffer associated with BUFFER or current buffer."
  (let ((buffer (or buffer
                    (current-buffer))))
    (get-buffer (format "<tree>%s" (buffer-name buffer)))))

(defun org-side-tree-cleanup ()
  "Kill Org-Side-Tree buffer associated with current buffer.
This is added to `'kill-buffer-hook' for each base-buffer."
  (when-let* ((tree-buffer (org-side-tree-has-tree-p)))
    (kill-buffer tree-buffer))
  (setq org-side-tree-persistent
        (default-value 'org-side-tree-persistent)))

;;;;; Headings

(defun org-side-tree-get-headings ()
  "Return a list of outline headings."
  (let* ((heading-regexp (concat "^\\(?:"
                                 outline-regexp
                                 "\\)"))
         (buffer (current-buffer))
         headings)
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward heading-regexp nil t)
          (let* ((beg (line-beginning-position))
                 (end (line-end-position))
                 (heading
                  (or (and (and org-side-tree-add-overlays
                                (derived-mode-p 'org-mode))
                           (org-side-tree-overlays-to-text beg end))
                      (org-side-tree-buffer-substring beg end))))
            (push (list
                   heading
                   (vector (cons
                            (if org-side-tree-fontify
                                heading
                              (substring-no-properties heading))
                            `(type org-side-tree
                                   buffer ,buffer
                                   marker ,(point-marker)))))
                  headings)
            (goto-char (1+ end))))))
    (if headings
        (nreverse headings)
      (list (list "" (vector (cons "[No headings]"
                                   `(type org-side-tree
                                          buffer ,buffer
                                          face default))))))))

(defun org-side-tree-heading-number ()
  "Return the number of the current heading."
  (let ((count 0)
        (end (save-excursion
               (unless (outline-on-heading-p)
                 (outline-previous-visible-heading 1))
               (point))))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        ;; account for headline on first line
        (when (and (bobp)
                   (outline-on-heading-p))
          (setq count (1+ count)))
        (while (and (outline-next-heading)
                    (<= (point) end))
          (setq count (1+ count)))))
    count))

(defun org-side-tree-go-to-heading (n)
  "Go to Nth heading."
  (goto-char (point-min))
  (dotimes (_x (1- n))
    (outline-next-heading))
  (when-let (ol (car (overlays-at (point))))
    (when (overlay-get ol 'invisible)
      (outline-previous-visible-heading 1)))
  (beginning-of-line)
  (hl-line-highlight))

(defun org-side-tree-buffer-substring (beg end)
  "Return visible portion of string from BEG to END."
  (let ((string "")
        (props '(org-fold--spec-org-link-description-global
                 org-fold--spec-org-link-global
                 help-echo htmlize-link font-lock-multiline
                 keymap mouse-face rear-nonsticky)))
    (while (/= beg end)
      (goto-char beg)
      (if (and (derived-mode-p 'org-mode)
               org-link-descriptive
               (re-search-forward org-link-any-re end t))
          (setq string (concat string
                               (buffer-substring beg
                                                 (match-beginning 0))
                               (or (match-string 3)
                                   (match-string 2)))
                beg (match-end 0))
        (setq string (concat string (buffer-substring beg end))
              beg end)))
    (remove-list-of-text-properties 0 (length string) props string)
    string))

(defun org-side-tree-overlays-to-text (beg end)
  "Return line from BEG to END with overlays as text."
  (let ((overlays (overlays-in beg end))
        text)
    (setq overlays (sort overlays (lambda (o1 o2)
                                    (< (overlay-start o1)
                                       (overlay-start o2)))))
    (mapc (lambda (o)
            (let ((t1 (org-side-tree-buffer-substring beg (overlay-start o)))
                  (t2 (overlay-get o 'before-string))
                  (t3 (or (overlay-get o 'display)
                          (org-side-tree-buffer-substring (overlay-start o)
                                                          (overlay-end o))))
                  (t4 (overlay-get o 'after-string))
                  (t5 (org-side-tree-buffer-substring (overlay-end o) end))
                  (inv (overlay-get o 'invisible)))
              (with-temp-buffer
                (insert t1)
                (unless inv
                  (when t2 (insert t2))
                  (insert t3)
                  (when t4 (insert t4)))
                (insert t5)
                (setq text (buffer-string)))))
          overlays)
    text))

;;;;; Auto-update

(defun org-side-tree-update ()
  "Update tree-buffer."
  (unless (equal org-side-tree-persistent
                 (buffer-name))
    (setq-local org-side-tree-persistent
                (buffer-name))
    (save-restriction
      (widen)
      (jit-lock-mode 1)
      (jit-lock-fontify-now)))
  (when-let* ((tree-buffer (get-buffer
                            (if (default-value 'org-side-tree-persistent)
                                "*Org-Side-Tree*"
                              (format "<tree>%s"
                                      (buffer-name)))))
              (tree-window (get-buffer-window tree-buffer))
              (buffer-ol-regexp outline-regexp)
              (heading (org-side-tree-heading-number))
              (headings (org-side-tree-get-headings))
              (tree-head-line (or (cadar (org-collect-keywords
                                          '("title")))
                                  "Org-Side-Tree"))
              (tree-mode-line (format "Org-Side-Tree - %s"
                                      (buffer-name)))
              (dd default-directory))
    (with-selected-window tree-window
      (setq-local outline-regexp buffer-ol-regexp)
      (setq-local default-directory dd)
      (when org-side-tree-enable-folding
        (org-side-tree-get-fold-state))
      (setq header-line-format tree-head-line)
      (setq mode-line-format tree-mode-line)
      (setq tabulated-list-entries headings)
      (tabulated-list-print t t)
      (when org-side-tree-enable-folding
        (setq-local outline-minor-mode-highlight nil)
        (outline-minor-mode 1)
        (unless (equal (length headings)
                       (length (cdr org-side-tree-fold-state)))
          (org-side-tree-restore-fold-state)))
      (goto-char (point-min))
      (org-side-tree-go-to-heading heading))))

(defun org-side-tree-set-timer ()
  "Set `org-side-tree-timer-function'."
  (unless (or org-side-tree-timer
              (not org-side-tree-enable-auto-update))
    (setq org-side-tree-timer
          (run-with-idle-timer
           org-side-tree-timer-delay t
           #'org-side-tree-timer-function))))

(defun org-side-tree-timer-function ()
  "Timer for `org-side-tree-update'."
  (if (not (org-side-tree-buffer-list))
      (progn
        (cancel-timer org-side-tree-timer)
        (setq org-side-tree-timer nil))
    (when (or (and org-side-tree-enable-auto-update
                   (not (equal (point) org-side-tree-last-point))
                   (or (derived-mode-p 'org-mode)
                       (derived-mode-p 'outline-mode)
                       outline-minor-mode)
                   (not (org-side-tree-buffer-p))
                   (or (and (default-value 'org-side-tree-persistent)
                            (get-buffer-window "*Org-Side-Tree*"))
                       (org-side-tree-has-tree-p)))
              (member last-command '(outline-demote
                                     outline-promote
                                     outline-move-subtree-up
                                     outline-move-subtree-down
                                     org-metaleft
                                     org-metaright
                                     org-shiftleft
                                     org-shiftright
                                     org-shiftmetaright
                                     org-shiftmetaleft
                                     org-shiftup
                                     org-shiftdown)))
      (org-side-tree-update)
      (setq org-side-tree-last-point (point)))))

;;;;; Folding

(defun org-side-tree-get-fold-state ()
  "Register fold state of tree-buffer in `org-side-tree-fold-state'."
  (hl-line-mode -1)
  (setq org-side-tree-fold-state nil)
  (save-excursion
    (goto-char (point-max))
    (let ((total (line-number-at-pos)))
      (goto-char (point-min))
      (while (< (line-number-at-pos) total)
        (end-of-line)
        (if-let (ol (car (overlays-at (point))))
            (if (overlay-get ol 'invisible)
                (progn
                  (push 1 org-side-tree-fold-state)
                  (outline-next-visible-heading 1))
              (push 0 org-side-tree-fold-state)
              (forward-line))
          (push 0 org-side-tree-fold-state)
          (forward-line))))
    (setq org-side-tree-fold-state (nreverse org-side-tree-fold-state))
    (push (buffer-name) org-side-tree-fold-state)
    (hl-line-mode 1)))

(defun org-side-tree-restore-fold-state ()
  "Restore fold state of tree-buffer."
  (when (equal (buffer-name) (car org-side-tree-fold-state))
    (outline-show-all)
    (goto-char (point-min))
    (dolist (x (cdr org-side-tree-fold-state))
      (if (= x 1)
          (progn
            (outline-hide-subtree)
            (outline-next-visible-heading 1))
        (forward-line)))
    (goto-char (point-min))))

;;;;; Jump

(defun org-side-tree-jump (&optional _)
  "Jump to headline."
  (interactive)
  (let ((tree-window (selected-window))
        (buffer (get-text-property (point) 'buffer))
        (marker (get-text-property (point) 'marker))
        (recenter-positions (list org-side-tree-recenter-position)))
    (if (null marker)
        (pop-to-buffer buffer)
      (unless (buffer-live-p buffer)
        (when (yes-or-no-p
               "Base buffer has been killed. Kill org-side-tree window?")
          (kill-buffer-and-window))
        (keyboard-quit))
      (pop-to-buffer buffer)
      (org-side-tree-goto-marker marker)
      (outline-show-subtree)
      (beginning-of-line)
      (recenter-top-bottom)
      (pulse-momentary-highlight-one-line nil 'highlight)
      (when org-side-tree-narrow-on-jump
        (cond ((or
                (derived-mode-p 'org-mode)
                (derived-mode-p 'outline-mode))
               (org-narrow-to-element))
              (outline-minor-mode
               (org-narrow-to-subtree))))
      (when (member this-command '(org-side-tree-previous-heading
                                   org-side-tree-next-heading))
        (select-window tree-window)))))

(defun org-side-tree-goto-marker (marker)
  "Go to MARKER, widen if necessary."
  (when (or (> marker (point-max))
            (< marker (point-min)))
    (widen))
  (goto-char marker)
  (outline-show-subtree))

;;;; Commands

;;;;; Toggles

;;;###autoload
(defun org-side-tree-toggle ()
  "Toggle side-tree window."
  (interactive)
  (let* ((tree-buffer (get-buffer
                       (if (default-value 'org-side-tree-persistent)
                           "*Org-Side-Tree*"
                         (format "<tree>%s"
                                 (buffer-name)))))
         (tree-window (get-buffer-window tree-buffer)))
    (if tree-window
        (delete-window tree-window)
      (org-side-tree))))

;;;###autoload
(defun org-side-tree-toggle-auto-update ()
  "Toggle `org-side-tree-enable-auto-update' for the current buffer."
  (interactive)
  (let ((base-buffer (or (save-excursion
                           (goto-char (point-min))
                           (get-text-property (point) 'buffer))
                         (current-buffer))))
    (if (org-side-tree-buffer-p)
        (with-current-buffer base-buffer
          (org-side-tree-toggle-auto-update))
      (if (bound-and-true-p org-side-tree-enable-auto-update)
          (progn
            (setq-local org-side-tree-enable-auto-update nil)
            (message "Auto-update disabled locally"))
        (setq-local org-side-tree-enable-auto-update t)
        (org-side-tree-set-timer)
        (message "Auto-update enabled locally")))))

;;;###autoload
(defun org-side-tree-toggle-folding ()
  "Toggle `org-side-tree-enable-folding' for the current buffer."
  (interactive)
  (let ((base-buffer (or (save-excursion
                           (goto-char (point-min))
                           (get-text-property (point) 'buffer))
                         (current-buffer))))
    (cond
     ((and (org-side-tree-buffer-p)
           (bound-and-true-p org-side-tree-enable-folding))
      (progn
        (setq-local org-side-tree-enable-folding nil)
        (outline-minor-mode -1)
        (with-current-buffer base-buffer
          (setq-local org-side-tree-enable-folding nil))
        (message "Folding disabled locally")))
     ((and (org-side-tree-buffer-p)
           (not org-side-tree-enable-folding))
      (progn
        (setq-local org-side-tree-enable-folding t)
        (setq-local outline-minor-mode-highlight nil)
        (outline-minor-mode 1)
        (with-current-buffer base-buffer
          (setq-local org-side-tree-enable-folding t))
        (message "Folding enabled locally")))
     (t
      (if (default-value 'org-side-tree-persistent)
          (with-current-buffer (get-buffer "*Org-Side-Tree*")
            (org-side-tree-toggle-folding))
        (with-current-buffer (get-buffer (org-side-tree-has-tree-p))
          (org-side-tree-toggle-folding)))))))

;;;###autoload
(defun org-side-tree-toggle-narrow-on-jump ()
  "Toggle `org-side-tree-narrow-on-jump' for the current buffer."
  (interactive)
  (let ((base-buffer (or (save-excursion
                           (goto-char (point-min))
                           (get-text-property (point) 'buffer))
                         (current-buffer))))
    (if (org-side-tree-buffer-p)
        (with-current-buffer base-buffer
          (org-side-tree-toggle-narrow-on-jump))
      (if (bound-and-true-p org-side-tree-narrow-on-jump)
          (progn
            (setq-local org-side-tree-narrow-on-jump nil)
            (message "Narrow-on-jump disabled locally"))
        (setq-local org-side-tree-narrow-on-jump t)
        (message "Narrow-on-jump enabled locally")))))

;;;;; Movement

;;;###autoload
(defun org-side-tree-next-heading ()
  "Move to next heading."
  (interactive)
  (if (org-side-tree-buffer-p)
      (progn
        (if org-side-tree-enable-folding
            (outline-next-visible-heading 1)
          (forward-line 1))
        (push-button nil t))
    (widen)
    (outline-next-visible-heading 1)
    (org-side-tree-update)
    (when org-side-tree-narrow-on-jump
      (org-narrow-to-subtree))))

;;;###autoload
(defun org-side-tree-previous-heading ()
  "Move to previous heading."
  (interactive)
  (if (org-side-tree-buffer-p)
      (progn
        (if org-side-tree-enable-folding
            (outline-previous-visible-heading 1)
          (forward-line -1))
        (push-button nil t))
    (widen)
    (outline-previous-visible-heading 1)
    (org-side-tree-update)
    (when org-side-tree-narrow-on-jump
      (unless (org-before-first-heading-p)
        (org-narrow-to-subtree)))))

;;;;; Emulate macro

(defmacro org-side-tree-emulate (name doc fn arg error-fn)
  "Define function NAME to perform function FN in base-buffer.
DOC is a doc string. ERROR-FN is the body of a `condition-case'
handler. ARG can be non-nil for special cases."
  `(defun ,(intern (symbol-name name)) ,(when arg `(&optional ARG))
     ,doc
     (interactive,(when arg "p"))
     (let ((tree-window (selected-window))
           (org-side-tree-narrow-on-jump nil))
       (beginning-of-line)
       (push-button nil t)
       (condition-case nil
           ,fn
         (user-error ,error-fn))
       (sit-for .3)
       (org-side-tree-update)
       (select-window tree-window))))

(org-side-tree-emulate
 org-side-tree-move-subtree-down
 "Move the current subtree down past ARG headlines of the same level."
 (outline-move-subtree-down ARG) t
 (message "Cannot move past superior level or buffer limit"))

(org-side-tree-emulate
 org-side-tree-move-subtree-up
 "Move the current subtree up past ARG headlines of the same level."
 (outline-move-subtree-up ARG) t
 (message "Cannot move past superior level or buffer limit"))

(org-side-tree-emulate
 org-side-tree-promote-subtree
 "Promote the entire subtree."
 (cond ((or (derived-mode-p 'outline-mode)
            outline-minor-mode)
        (outline-promote 'subtree))
       ((derived-mode-p 'org-mode)
        (org-promote-subtree)))
 nil (message "Cannot promote to level 0"))

(org-side-tree-emulate
 org-side-tree-demote-subtree
 "Demote the entire subtree."
 (cond ((or (derived-mode-p 'outline-mode)
            outline-minor-mode)
        (outline-demote 'subtree))
       ((derived-mode-p 'org-mode)
        (org-demote-subtree)))
 nil nil)

(org-side-tree-emulate
 org-side-tree-promote
 "Promote the current heading higher up the tree."
 (cond ((or (derived-mode-p 'outline-mode)
            outline-minor-mode)
        (outline-promote))
       ((derived-mode-p 'org-mode)
        (org-do-promote)))
 nil nil)

(org-side-tree-emulate
 org-side-tree-demote
 "Demote the current heading lower down the tree."
 (cond ((or (derived-mode-p 'outline-mode)
            outline-minor-mode)
        ;; BUG: match-data returns next level heading
        ;; HACK: reset match-data
        (outline-previous-visible-heading 1)
        (outline-next-visible-heading 1)
        (outline-demote))
       ((derived-mode-p 'org-mode)
        (org-do-demote)))
 nil nil)

(org-side-tree-emulate
 org-side-tree-next-todo
 "Change the TODO state of heading."
 (when (derived-mode-p 'org-mode)
   (org-todo 'right))
 nil nil)

(org-side-tree-emulate
 org-side-tree-previous-todo
 "Change the TODO state of heading."
 (when (derived-mode-p 'org-mode)
   (org-todo 'left))
 nil nil)

(org-side-tree-emulate
 org-side-tree-priority-up
 "Change the priority state of heading."
 (when (derived-mode-p 'org-mode)
   (org-priority-up))
 nil nil)

(org-side-tree-emulate
 org-side-tree-priority-down
 "Change the priority state of heading."
 (when (derived-mode-p 'org-mode)
   (org-priority-down))
 nil nil)

(provide 'org-side-tree)
;;; org-side-tree.el ends here
