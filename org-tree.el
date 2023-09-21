;;; org-tree.el --- Navigate Org outlines via side window           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: September 7, 2023
;; License: GPL-3.0-or-later
;; Version: 0.2
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

;; Navigate Org buffer via tree-outline in a side window.

;; Inspired by and modeled on org-sidebar by @alphapapa. Implemented with
;; code borrowings from `embark-live' in Embark by @oantolin and from Consult
;; by @minad.

;;; Code:

(require 'org)

(defvar org-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<return>") #'push-button)
    (define-key map (kbd "<mouse-1>") #'push-button)
    (define-key map (kbd "n") #'org-tree-next-heading)
    (define-key map (kbd "p") #'org-tree-previous-heading)
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

(define-button-type 'org-tree
  'action 'org-tree-jump
  'help-echo nil)

(defcustom org-tree-narrow-on-jump t
  "When non-nil, source buffer is narrowed to subtree."
  :group 'org-tree
  :type 'boolean)

;;;###autoload
(defun org-tree ()
  "Create Org-Tree buffer."
  (interactive)
  (cond
   ((org-tree-buffer-p)
    (error "Don't tree a tree"))
   ((not (derived-mode-p 'org-mode))
    (error "Not an org buffer"))
   ((not (buffer-live-p (get-buffer (format "<tree>%s" (buffer-name)))))
    (jit-lock-mode 1)
    (jit-lock-fontify-now)
    (let* ((headings (org-tree--headings))
           (tree-name (format "<tree>%s" (buffer-name)))
           (tree-buffer (generate-new-buffer tree-name))
           (tree-mode-line (format "Org-Tree - %s"
                                   (file-name-nondirectory buffer-file-name))))
      (add-hook 'after-save-hook #'org-tree--update nil t)
      (with-current-buffer tree-buffer
        (org-tree-mode)
        (setq tabulated-list-entries headings)
        (tabulated-list-print t t)
        (set-window-fringes (get-buffer-window tree-buffer) 1 1)
        (setq mode-line-format tree-mode-line))
      (pop-to-buffer tree-buffer)))
   (t
    (let ((tree-buffer (get-buffer
                        (format "<tree>%s" (buffer-name))))
          (heading (org-no-properties (org-get-heading t))))
      (pop-to-buffer tree-buffer)
      (goto-char (point-min))
      (re-search-forward heading)
      (beginning-of-line)
      (hl-line-highlight)))))

  (defun org-tree--headings ()
    "Return a list of outline headings."
    (interactive)
    (let* ((heading-regexp (concat "^\\(?:"
                                   org-outline-regexp
                                   "\\)"))
           (buffer (current-buffer))
           narrow-beg
           narrow-end
           headings)
      (when (org-buffer-narrowed-p)
        (setq narrow-beg (point-min-marker)
              narrow-end (point-max-marker))
        (widen))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward heading-regexp nil t)
          (push (list
                 (org-get-heading t)
                 (vector (cons (buffer-substring (line-beginning-position)
                                                 (line-end-position))
                               `(type org-tree
                                      buffer ,buffer
                                      pos ,(point-marker)
                                      keymap org-tree-mode-map))))
                headings)
          (goto-char (1+ (line-end-position)))))
      (when narrow-beg
        (save-excursion
          (narrow-to-region narrow-beg narrow-end)))
      (unless headings
        (user-error "No headings"))
      (nreverse headings)))

(defun org-tree--update ()
  "Update Org-Tree buffer."
  (setq this-command 'org-tree--update)
  (let ((tree-buffer (get-buffer
                      (format "<tree>%s"
                              (buffer-name))))
        (heading (org-get-heading t))
        headings)
    (if tree-buffer
        (progn
          (setq headings (org-tree--headings))
          (with-current-buffer tree-buffer
            (setq tabulated-list-entries headings)
            (tabulated-list-print t t)
            (goto-char (point-min))
            (re-search-forward heading)
            (beginning-of-line)
            (hl-line-highlight)))
      (remove-hook 'after-save-hook #'org-tree--update t))))

(defun org-tree-jump (&optional _)
  "Jump to headline."
  (interactive)
  (let ((tree-window (selected-window))
        (buffer (get-text-property (point) 'buffer))
        ;; point isn't accurate; use marker instead?
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
    (recenter-top-bottom 'top)
    (when org-tree-narrow-on-jump
      (org-narrow-to-element))
    (when (or (eq this-command 'org-tree-next-heading)
              (eq this-command 'org-tree-previous-heading))
      (select-window tree-window))))

(defun org-tree-buffer-p (&optional buffer)
  "Is this BUFFER a tree-buffer?"
  (interactive)
  (let ((buffer (or buffer (buffer-name))))
    (string-match "^<tree>.*" buffer)))

(defun org-tree-next-heading ()
  "Move to next heading."
  (interactive)
  (if (org-tree-buffer-p)
      (progn
        (forward-line 1)
        (push-button nil t))
    (widen)
    (org-next-visible-heading 1)
    (let* ((tree-buffer (get-buffer (concat
                                     "<tree>"
                                     (buffer-name))))
           (tree-window (get-buffer-window tree-buffer))
           (base-window (selected-window))
           (heading (org-no-properties (org-get-heading t))))
      (when tree-window
        (select-window tree-window)
        (goto-char (point-min))
        (re-search-forward heading)
        (beginning-of-line)
        (hl-line-highlight)
        (select-window base-window)))
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
    (let* ((tree-buffer (get-buffer (concat
                                     "<tree>"
                                     (buffer-name))))
           (tree-window (get-buffer-window tree-buffer))
           (base-window (selected-window))
           (heading (org-no-properties (org-get-heading t))))
      (when tree-window
        (select-window tree-window)
        (goto-char (point-min))
        (re-search-forward heading)
        (beginning-of-line)
        (hl-line-highlight)
        (select-window base-window)))
    (when org-tree-narrow-on-jump
      (unless (org-before-first-heading-p)
        (org-narrow-to-subtree)))))

(provide 'org-tree)
;;; org-tree.el ends here
