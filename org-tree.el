;;; org-tree.el --- Navigate Org outlines in side window tree          -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: September 7, 2023
;; License: GPL-3.0-or-later
;; Version: 0.4
;; Homepage: https://github.com/localauthor/org-tree
;; Package-Requires: ((emacs "27.2"))

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

;; Inspired by and modeled on `org-sidebar-tree' from org-sidebar by
;; @alphapapa and `embark-live' from Embark by @oantolin.


;; To install, place file on your load-path
;; and include this in your init file:
;; (require 'org-tree)

;; To use, open and Org file and call M-x `org-tree'.


;;; Code:

(require 'org)
(require 'hl-line)

(defvar org-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") #'push-button)
    (define-key map (kbd "RET") #'push-button)
    (define-key map (kbd "<mouse-1>") #'push-button)
    (define-key map (kbd "n") #'org-tree-next-heading)
    (define-key map (kbd "p") #'org-tree-previous-heading)
    (make-composed-keymap map special-mode-map))
  "Keymap for `org-tree-mode'.")

(define-derived-mode org-tree-mode tabulated-list-mode "Org-Tree"
  "Mode for `org-tree'.

\\{org-tree-mode-map}"
  :group 'org-tree
  :interactive nil
  (hl-line-mode)
  (setq-local cursor-type 'bar)
  (setq tabulated-list-format [("Tree" 100)])
  (set-window-fringes (selected-window) 1)
  (setq fringe-indicator-alist
        '((truncation nil nil))))

(defgroup org-tree nil
  "Navigate Org headings via sidebar tree."
  :group 'org
  :prefix "org-tree")

(defcustom org-tree-display-side 'left
  "Side of frame where Org-Tree buffer will display."
  :type '(choice
	  (const :tag "Left" left)
	  (const :tag "Right" right)
          (const :tag "Bottom" bottom)))

(defcustom org-tree-narrow-on-jump nil
  "When non-nil, source buffer is narrowed to subtree."
  :type 'boolean)

(defcustom org-tree-timer-delay .1
  "Timer to update headings and cursor position.
Changes to this variable will not take effect if there are any
live tree buffers. Kill and reopen tree buffers to see effects."
  :type 'number)

(defcustom org-tree-persistent nil
  "When non-nil, use a single buffer for all trees.
When nil, each Org buffer will have its own tree-buffer."
  :type 'boolean)

   (defcustom org-tree-recenter-position .25
     "Setting to determine heading position after `org-tree-jump'.
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

   (defcustom org-tree-enable-folding t
     "Enable folding in Org-Tree buffers.
This feature can cause lag in large buffers. Try increasing
`org-tree-timer-delay' to .5 seconds. Or, folding can be toggled locally
with `org-tree-toggle-folding'."
     :type 'boolean)

   (defcustom org-tree-enable-auto-update t
     "When non-nil, tree-buffers will automatically update.
Can be toggled locally by calling `org-tree-toggle-auto-update'."
     :type 'boolean)

(defcustom org-tree-add-overlays t
  "When non-nil, overlays are included in tree-buffer headings.
This includes `org-todo' heads and `org-num' numbering."
  :type 'boolean)

(defvar org-tree-timer nil
  "Timer to update headings and cursor position.")

(defvar-local org-tree-fold-state nil
  "Fold state of current buffer.")

(defvar-local org-tree-last-point 0
  "Cursor position from the last run of `post-command-hook'.")

(define-button-type 'org-tree
  'action 'org-tree-jump
  'help-echo nil);;;###autoload
(defun org-tree ()
  "Create or pop to Org-Tree buffer."
  (interactive)
  (when (org-tree-buffer-p)
    (error "Don't tree a tree"))
  (unless (derived-mode-p 'org-mode)
    (error "Not an org buffer"))
  (let* ((tree-name (if org-tree-persistent
                        "*Org-Tree*"
                      (format "<tree>%s" (buffer-name))))
         (tree-buffer (get-buffer tree-name))
         (heading (org-tree-heading-number)))
    (unless (buffer-live-p tree-buffer)
      (save-restriction
        (widen)
        (jit-lock-mode 1)
        (jit-lock-fontify-now))
      (setq tree-buffer (generate-new-buffer tree-name))
      (add-hook 'kill-buffer-hook #'org-tree-cleanup nil t)
      (let* ((headings (org-tree-get-headings))
             (tree-head-line (or (cadar (org-collect-keywords
                                         '("title")))
                                 "Org-Tree"))
             (tree-mode-line (format "Org-Tree - %s"
                                     (file-name-nondirectory
                                      buffer-file-name))))
        (when (default-value org-tree-enable-folding)
          (setq-local org-tree-enable-folding t))
        (with-current-buffer tree-buffer
          (org-tree-mode)
          (setq tabulated-list-entries headings)
          (tabulated-list-print t t)
          (when (default-value org-tree-enable-folding)
            (setq-local org-tree-enable-folding t)
            ;; preserve org font-locking
            (setq-local outline-minor-mode-highlight nil)
            (outline-minor-mode 1))
          (setq header-line-format tree-head-line)
          (setq mode-line-format tree-mode-line))))
    (when org-tree-persistent
      (org-tree-update))
    (org-tree-set-timer)
    (pop-to-buffer tree-buffer
                   (display-buffer-in-side-window
                    tree-buffer
                    `((side . ,org-tree-display-side))))
    (set-window-fringes (get-buffer-window tree-buffer) 1 1)
    (org-tree-go-to-heading heading)
    (beginning-of-line)
    (hl-line-highlight)))

(defun org-tree-get-headings ()
  "Return a list of outline headings."
  (let* ((heading-regexp (concat "^\\(?:"
                                 org-outline-regexp
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
                 (line (org-tree-overlays-to-text beg end)))
            (push (list
                   (org-get-heading)
                   (vector (cons (if (and org-tree-add-overlays
                                          line)
                                     line
                                   (buffer-substring beg end))
                                 `(type org-tree
                                        buffer ,buffer
                                        pos ,(point-marker)
                                        keymap org-tree-mode-map))))
                  headings)
            (goto-char (1+ end))))))
    (if headings
        (nreverse headings)
      (list (list "" (vector "[No headings]"))))))

(defun org-tree-overlays-to-text (beg end)
  "Return line from BEG to END with overlays as text."
  (let ((overlays (overlays-in beg end))
        text)
    (setq overlays (sort overlays (lambda (o1 o2)
                                    (< (overlay-start o1)
                                       (overlay-start o2)))))
    (mapc (lambda (o)
            (let ((t1 (buffer-substring beg (overlay-start o)))
                  (t2 (overlay-get o 'before-string))
                  (t3 (or (overlay-get o 'display)
                          (buffer-substring (overlay-start o) (overlay-end o))))
                  (t4 (overlay-get o 'after-string))
                  (t5 (buffer-substring (overlay-end o) end))
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

(defun org-tree-set-timer ()
  "Set `org-tree-timer-function'."
  (unless (or org-tree-timer
              (not org-tree-enable-auto-update))
    (setq org-tree-timer
          (run-with-idle-timer
           org-tree-timer-delay t
           #'org-tree-timer-function))))

(defun org-tree-timer-function ()
  "Timer for `org-tree-update'."
  (if (not (org-tree-buffer-list))
      (progn
        (cancel-timer org-tree-timer)
        (setq org-tree-timer nil))
    (unless (or (minibufferp)
                (unless (and org-tree-persistent
                             (derived-mode-p 'org-mode)
                             (get-buffer-window "*Org-Tree*"))
                  (not (org-tree-has-tree-p)))
                (and (equal (point) org-tree-last-point)
                     (not (member last-command '(org-metaleft
                                                 org-metaright
                                                 org-shiftleft
                                                 org-shiftright
                                                 org-shiftmetaright
                                                 org-shiftmetaleft
                                                 org-shiftup
                                                 org-shiftdown)))))
      (org-tree-update)
      (setq org-tree-last-point (point)))))

(defun org-tree-toggle-auto-update ()
  "Toggle `org-tree-enable-auto-update' for the current buffer."
  (interactive)
  (cond
   ((and (org-tree-has-tree-p))
    (if (bound-and-true-p org-tree-enable-auto-update)
        (progn
          (setq-local org-tree-enable-auto-update nil)
          (message "Auto-update disabled locally"))
      (setq-local org-tree-enable-auto-update t)
      (org-tree-set-timer)
      (message "Auto-update enabled locally")))
   ((and (org-tree-buffer-p))
    (with-current-buffer (substring (buffer-name) 6)
      (org-tree-toggle-auto-update)))))

(defun org-tree-update ()
  "Update tree-buffer."
  (when-let* ((tree-buffer (get-buffer
                            (if org-tree-persistent
                                "*Org-Tree*"
                              (format "<tree>%s"
                                      (buffer-name)))))
              (heading (org-tree-heading-number))
              (headings (org-tree-get-headings))
              (tree-head-line (or (cadar (org-collect-keywords
                                          '("title")))
                                  "Org-Tree"))
              (tree-mode-line (format "Org-Tree - %s"
                                      (file-name-nondirectory
                                       buffer-file-name))))
    (when org-tree-persistent
      (save-restriction
        (widen)
        (jit-lock-mode 1)
        (jit-lock-fontify-now)))
    (with-current-buffer tree-buffer
      (when org-tree-enable-folding
        (org-tree-get-fold-state))
      (setq header-line-format tree-head-line)
      (setq mode-line-format tree-mode-line)
      (setq tabulated-list-entries headings)
      (tabulated-list-print t t)
      (when org-tree-enable-folding
        (setq-local outline-minor-mode-highlight nil)
        (outline-minor-mode 1)
        (org-tree-restore-fold-state))
      (goto-char (point-min))
      (org-tree-go-to-heading heading)
      (beginning-of-line)
      (hl-line-highlight))))

(defun org-tree-cleanup ()
  "Kill Org-Tree buffer associated with current buffer.
This is added to `'kill-buffer-hook' for each base-buffer."
  (when-let* ((tree-buffer (org-tree-has-tree-p)))
    (kill-buffer tree-buffer)))

(defun org-tree-buffer-list ()
  "Return list of current Org-Tree buffers."
  (delq nil (append
             (list (get-buffer "*Org-Tree*"))
             (mapcar
              (lambda (buf)
                (org-tree-has-tree-p buf))
              (buffer-list)))))

(defun org-tree-buffer-p ()
  "Return t if current buffer is a tree-buffer."
  (when (or (equal (buffer-name) "*Org-Tree*")
            (member (current-buffer) (org-tree-buffer-list)))
    t))

(defun org-tree-has-tree-p (&optional buffer)
  "Return tree-buffer associated with BUFFER or current buffer."
  (let ((buffer (or buffer
                    (current-buffer))))
    (get-buffer (format "<tree>%s" (buffer-name buffer)))))

(defun org-tree-heading-number ()
  "Return the number of the current heading."
  (let ((count 0)
        (end (save-excursion
               (unless (org-at-heading-p)
                 (org-previous-visible-heading 1))
               (point))))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (while (and (outline-next-heading)
                    (<= (point) end))
          (setq count (1+ count)))))
    count))

(defun org-tree-go-to-heading (n)
  "Go to Nth heading."
  (goto-char (point-min))
  (dotimes (_x (1- n))
    (outline-next-heading))
  (when-let (ol (car (overlays-at (point))))
    (when (overlay-get ol 'invisible)
      (outline-previous-visible-heading 1))))

(defun org-tree-get-fold-state ()
  "Register fold state of tree-buffer in `org-tree-fold-state'."
  (hl-line-mode -1)
  (setq org-tree-fold-state nil)
  (save-excursion
    (goto-char (point-max))
    (let ((total (line-number-at-pos)))
      (goto-char (point-min))
      (while (< (line-number-at-pos) total)
        (end-of-line)
        (if-let (ol (car (overlays-at (point))))
            (if (overlay-get ol 'invisible)
                (progn
                  (push 1 org-tree-fold-state)
                  (outline-next-visible-heading 1))
              (push 0 org-tree-fold-state)
              (forward-line))
          (push 0 org-tree-fold-state)
          (forward-line))))
    (setq org-tree-fold-state (nreverse org-tree-fold-state))
    (hl-line-mode 1)))

(defun org-tree-restore-fold-state ()
  "Restore fold state of tree-buffer."
  (outline-show-all)
  (goto-char (point-min))
  (dolist (x org-tree-fold-state)
    (if (= x 1)
        (progn
          (outline-hide-subtree)
          (outline-next-visible-heading 1))
      (forward-line)))
  (goto-char (point-min)))

(defun org-tree-toggle-folding ()
  "Toggle `org-tree-enable-folding' for the current buffer."
  (interactive)
  (cond
   ((and (org-tree-buffer-p)
         (bound-and-true-p org-tree-enable-folding))
    (progn
      (setq-local org-tree-enable-folding nil)
      (outline-minor-mode -1)
      (with-current-buffer (substring (buffer-name) 6)
        (setq-local org-tree-enable-folding nil))
      (message "Folding disabled locally")))
   ((and (org-tree-buffer-p)
         (not org-tree-enable-folding))
    (progn
      (setq-local org-tree-enable-folding t)
      (setq-local outline-minor-mode-highlight nil)
      (outline-minor-mode 1)
      (with-current-buffer (substring (buffer-name) 6)
        (setq-local org-tree-enable-folding t))
      (message "Folding enabled locally")))
   ((org-tree-has-tree-p)
    (with-selected-window (get-buffer-window (org-tree-has-tree-p))
      (org-tree-toggle-folding)))))

(defun org-tree-jump (&optional _)
  "Jump to headline."
  (interactive)
  (let ((tree-window (selected-window))
        (buffer (get-text-property (point) 'buffer))
        (pos (get-text-property (point) 'pos))
        (recenter-positions (list org-tree-recenter-position)))
    (unless (buffer-live-p buffer)
      (when (yes-or-no-p
             "Base buffer has been killed. Kill org-tree window?")
        (kill-buffer-and-window))
      (keyboard-quit))
    (pop-to-buffer buffer)
    (widen)
    (org-fold-show-all)
    (org-fold-hide-drawer-all)
    (goto-char pos)
    (beginning-of-line)
    (recenter-top-bottom)
    (pulse-momentary-highlight-one-line nil 'highlight)
    (when org-tree-narrow-on-jump
      (org-narrow-to-element))
    (when (member this-command '(org-tree-previous-heading
                                 org-tree-next-heading))
      (select-window tree-window))))

(defun org-tree-next-heading ()
  "Move to next heading."
  (interactive)
  (if (org-tree-buffer-p)
      (progn
        (if org-tree-enable-folding
            (outline-next-visible-heading 1)
          (forward-line 1))
        (push-button nil t))
    (widen)
    (org-next-visible-heading 1)
    (org-tree-update)
    (if org-tree-narrow-on-jump
        (org-narrow-to-subtree))))

(defun org-tree-previous-heading ()
  "Move to previous heading."
  (interactive)
  (if (org-tree-buffer-p)
      (progn
        (if org-tree-enable-folding
            (outline-previous-visible-heading 1)
          (forward-line -1))
        (push-button nil t))
    (widen)
    (org-previous-visible-heading 1)
    (org-tree-update)
    (when org-tree-narrow-on-jump
      (unless (org-before-first-heading-p)
        (org-narrow-to-subtree)))))

(defmacro org-tree-emulate (name doc fn arg error-fn)
  "Define function NAME to emulate Org-Mode function FN.
DOC is a doc string. ERROR-FN is the body of a `condition-case'
handler. ARG can be non-nil for special cases."
  `(defun ,(intern (symbol-name name)) ,(when arg `(&optional ARG))
     ,doc
     (interactive,(when arg "p"))
     (let ((tree-window (selected-window)))
       (push-button nil t)
       (condition-case nil
           ,fn
         (user-error ,error-fn))
       (sit-for .3)
       (org-tree-update)
       (select-window tree-window))))

(org-tree-emulate
 org-tree-move-subtree-down
 "Move the current subtree down past ARG headlines of the same level."
 (org-move-subtree-down ARG) t
 (message "Cannot move past superior level or buffer limit"))

(org-tree-emulate
 org-tree-move-subtree-up
 "Move the current subtree up past ARG headlines of the same level."
 (org-move-subtree-up ARG) t
 (message "Cannot move past superior level or buffer limit"))

(org-tree-emulate
 org-tree-next-todo
 "Change the TODO state of heading."
 (org-todo 'right) nil nil)

(org-tree-emulate
 org-tree-previous-todo
 "Change the TODO state of heading."
 (org-todo 'left) nil nil)

(org-tree-emulate
 org-tree-priority-up
 "Change the priority state of heading."
 (org-priority-up) nil nil)

(org-tree-emulate
 org-tree-priority-down
 "Change the priority state of heading."
 (org-priority-down) nil nil)

(org-tree-emulate
 org-tree-promote-subtree
 "Promote the entire subtree."
 (org-promote-subtree) nil
 (message "Cannot promote to level 0"))

(org-tree-emulate
 org-tree-demote-subtree
 "Demote the entire subtree."
 (org-demote-subtree) nil nil)

(org-tree-emulate
 org-tree-do-promote
 "Promote the current heading higher up the tree."
 (org-do-promote) nil nil)

(org-tree-emulate
 org-tree-do-demote
 "Demote the current heading lower down the tree."
 (org-do-demote) nil nil)

(provide 'org-tree)
;;; org-tree.el ends here
