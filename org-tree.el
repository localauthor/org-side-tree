;;; org-tree.el --- Navigate Org headings via tree outline           -*- lexical-binding: t; -*-

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

;; ADD: org-move-subtree-up -down

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
    (define-key map (kbd "C-S-<up>") #'org-tree-move-subtree-up)
    (define-key map (kbd "C-S-<down>") #'org-tree-move-subtree-down)
    (make-composed-keymap map special-mode-map))
  "Keymap for `org-tree-mode'.")

(define-derived-mode org-tree-mode tabulated-list-mode "Org-Tree"
  "Mode for `org-tree'.

\\{org-tree-mode-map}"
  (hl-line-mode)
  (setq-local cursor-type 'bar)
  (setq tabulated-list-format [("Tree" 100)])
  (set-window-fringes (selected-window) 1)
  (setq fringe-indicator-alist
        '((truncation nil nil))))

(defcustom org-tree-narrow-on-jump nil
  "When non-nil, source buffer is narrowed to subtree."
  :group 'org-tree
  :type 'boolean)

(defcustom org-tree-timer-delay .1
  "Timer to update headings and cursor position."
  :group 'org-tree
  :type 'number)

(defcustom org-tree-add-overlays t
  "When non-nil, overlays are included in tree-buffer headings.
This includes `org-todo' heads and `org-num' numbering."
  :group 'org-tree
  :type 'boolean)

(defvar org-tree-timer nil
  "Timer to update headings and cursor position.")

(defvar org-tree-last-point 0
  "Cursor position from the last run of `post-command-hook'.")

(make-variable-buffer-local 'org-tree-last-point)

(define-button-type 'org-tree
  'action 'org-tree-jump
  'help-echo nil)

;;;###autoload
(defun org-tree ()
  "Create Org-Tree buffer."
  (interactive)
  (when (org-tree-buffer-p)
    (error "Don't tree a tree"))
  (unless (derived-mode-p 'org-mode)
    (error "Not an org buffer"))
  (let* ((tree-name (format "<tree>%s" (buffer-name)))
         (tree-buffer (get-buffer tree-name))
         (heading (org-tree-heading-number)))
    (unless (buffer-live-p tree-buffer)
      (setq tree-buffer (generate-new-buffer tree-name))
      (add-hook 'kill-buffer-hook #'org-tree-cleanup nil t)
      (save-restriction
        (widen)
        (jit-lock-mode 1)
        (jit-lock-fontify-now))
      (let* ((headings (org-tree-get-headings))
             (tree-head-line (or (org-get-title)
                                 "Org-Tree"))
             (tree-mode-line (format "Org-Tree - %s"
                                     (file-name-nondirectory
                                      buffer-file-name))))
        (with-current-buffer tree-buffer
          (org-tree-mode)
          (setq tabulated-list-entries headings)
          (tabulated-list-print t t)
          (setq header-line-format tree-head-line)
          (setq mode-line-format tree-mode-line))))
    (org-tree-set-timer)
    (pop-to-buffer tree-buffer)
    (set-window-fringes (get-buffer-window tree-buffer) 1 1)
    (org-tree-go-to-heading heading)
    (beginning-of-line)
    (hl-line-highlight)))

(defun org-tree-get-headings ()
  "Return a list of outline headings."
  (interactive)
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
            (goto-char (1+ (line-end-position)))))))
    (unless headings
      (user-error "No headings"))
    (nreverse headings)))

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
  (unless org-tree-timer
    (setq org-tree-timer
          (run-with-idle-timer
           org-tree-timer-delay t
           #'org-tree-timer-function))))

(defun org-tree-timer-function ()
  "Timer for `org-tree-update-line'."
  (if (not (org-tree-buffer-list))
      (progn
        (cancel-timer org-tree-timer)
        (setq org-tree-timer nil))
    (unless (or (minibufferp)
                (not (org-tree-has-tree-p))
                (equal (point) org-tree-last-point))
      (org-tree-update-line)
      (setq org-tree-last-point (point)))))

(defun org-tree-update-line ()
  "Refresh cursor position in tree-buffer."
  (when-let* ((tree-buffer (get-buffer
                            (format "<tree>%s"
                                    (buffer-name))))
              (tree-window (get-buffer-window tree-buffer))
              (heading (org-tree-heading-number))
              (headings (org-tree-get-headings))
              (tree-head-line (or (org-get-title)
                                  "Org-Tree"))
              (tree-mode-line (format "Org-Tree - %s"
                                      (file-name-nondirectory
                                       buffer-file-name))))
    (with-selected-window tree-window
      (setq header-line-format tree-head-line)
      (setq mode-line-format tree-mode-line)
      (setq tabulated-list-entries headings)
      (tabulated-list-print t t)
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
  (delq nil
        (mapcar
         (lambda (buf)
           (org-tree-has-tree-p buf))
         (buffer-list))))

(defun org-tree-buffer-p ()
  "Return t if current buffer is a tree-buffer."
  (when (member (current-buffer) (org-tree-buffer-list))
    t))

(defun org-tree-has-tree-p (&optional buffer)
  "Return tree-buffer associated with BUFFER or current buffer."
  (let ((buffer (or buffer
                    (current-buffer))))
    (get-buffer (format "<tree>%s" (buffer-name buffer)))))

(defun org-tree-heading-number ()
  "Return the number of the current heading."
  (interactive)
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
    (outline-next-heading)))

(defun org-tree-jump (&optional _)
  "Jump to headline."
  (interactive)
  (let ((tree-window (selected-window))
        (buffer (get-text-property (point) 'buffer))
        (pos (get-text-property (point) 'pos)))
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
    (when org-tree-narrow-on-jump
      (org-narrow-to-element))
    (when (or (eq this-command 'org-tree-next-heading)
              (eq this-command 'org-tree-previous-heading))
      (select-window tree-window))))

(defun org-tree-next-heading ()
  "Move to next heading."
  (interactive)
  (if (org-tree-buffer-p)
      (progn
        (forward-line 1)
        (push-button nil t))
    (widen)
    (org-next-visible-heading 1)
    (org-tree-update-line)
    (if org-tree-narrow-on-jump
        (org-narrow-to-subtree))))

(defun org-tree-previous-heading ()
  "Move to previous heading."
  (interactive)
  (if (org-tree-buffer-p)
      (progn
        (forward-line -1)
        (push-button nil t))
    (widen)
    (org-previous-visible-heading 1)
    (org-tree-update-line)
    (when org-tree-narrow-on-jump
      (unless (org-before-first-heading-p)
        (org-narrow-to-subtree)))))

(defun org-tree-move-subtree-down (&optional ARG)
  "Move the current subtree down past ARG headlines of the same level."
  (interactive "p")
  (let ((tree-window (selected-window)))
    (push-button nil t)
    (condition-case nil
        (org-move-subtree-down ARG)
      ('user-error (message "Cannot move past superior level or buffer limit")))
    (org-tree-update-line)
    (select-window tree-window)))

(defun org-tree-move-subtree-up (&optional ARG)
  "Move the current subtree up past ARG headlines of the same level."
  (interactive "p")
  (let ((tree-window (selected-window)))
    (push-button nil t)
    (condition-case nil
        (org-move-subtree-up ARG)
      ('user-error (message "Cannot move past superior level or buffer limit")))
    (org-tree-update-line)
    (select-window tree-window)))

(provide 'org-tree)
;;; org-tree.el ends here
