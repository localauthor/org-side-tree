;;; org-tree.el --- Navigate Org outlines via side window           -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Grant Rosson

;; Author: Grant Rosson <https://github.com/localauthor>
;; Created: September 7, 2023
;; License: GPL-3.0-or-later
;; Version: 0.1
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

;; Simplified version of "tree view" feature from org-sidebar.el by @alphapapa.

;;; Code:

(require 'org)
(require 'hl-line)

(defvar org-tree-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'org-tree-next)
    (define-key map (kbd "p") #'org-tree-previous)
    (define-key map (kbd "<return>") #'org-tree-jump)
    (define-key map (kbd "i") #'org-tree-cycle)
    (define-key map (kbd "C-i") #'org-tree-cycle)
    (define-key map (kbd "<tab>") #'org-tree-cycle)
    (define-key map (kbd "<backtab>") #'org-tree-unfold)
    (define-key map (kbd "q") #'quit-window)
    (make-composed-keymap map)))

;;;###autoload
(defun org-tree ()
  "Create `org-tree' buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (error "Not an org buffer"))
  (when (string-match "^<tree>.*" (buffer-name (current-buffer)))
    (error "Don't tree a tree"))
  (let* ((buffer (current-buffer))
         (tree-buffer (concat "<tree>" (buffer-name buffer)))
         (file-name (file-name-nondirectory buffer-file-name))
         (inhibit-message t)
         min)
    (unless (get-buffer tree-buffer)
      (clone-indirect-buffer-other-window tree-buffer nil 'norecord))
    (with-current-buffer tree-buffer
      (when (org-buffer-narrowed-p)
        (org-toggle-narrow-to-subtree))
      (read-only-mode 1)
      (hl-line-mode 1)
      (use-local-map org-tree-map)
      (setq-local cursor-type 'bar)
      (toggle-truncate-lines 1)
      (setq fringe-indicator-alist
            '((truncation nil nil)))
      (setq mode-line-format (format "Org-Tree - %s" file-name))
      (goto-char (point-min))
      (when (org-before-first-heading-p)
        (org-next-visible-heading 1))
      (setq min (point))
      (narrow-to-region min (point-max))
      (org-show-branches-buffer))
    (pop-to-buffer tree-buffer
                   '(display-buffer-in-side-window
                     tree-buffer
                     (side . left)))))

(defun org-tree-jump ()
  "Jump to heading in base buffer."
  (interactive)
  (let* ((pos (progn (unless (org-at-heading-p)
                       (org-previous-visible-heading 1))
                     (point)))
         (tree-buffer (current-buffer))
         (tree-window (get-buffer-window tree-buffer))
         (base-buffer (buffer-base-buffer))
         (base-window (get-buffer-window base-buffer)))
    (if base-window
        (select-window base-window)
      (switch-to-buffer base-buffer))
    (when (org-buffer-narrowed-p)
      (org-toggle-narrow-to-subtree))
    (goto-char pos)
    (org-fold-show-entry)
    (org-fold-show-subtree)
    (org-narrow-to-element)
    (when (or (eq this-command 'org-tree-next)
              (eq this-command 'org-tree-previous))
      (select-window tree-window))))

(defun org-tree-next ()
  "Move to next heading."
  (interactive)
  (if (string-match "^<tree>.*"
                    (buffer-name (current-buffer)))
      (progn
        (org-speed-move-safe 'org-next-visible-heading)
        (org-tree-jump))
    (org-toggle-narrow-to-subtree)
    (org-next-visible-heading 1)
    (org-narrow-to-subtree)))

(defun org-tree-previous ()
  "Move to previous heading."
  (interactive)
  (if (string-match "^<tree>.*"
                    (buffer-name (current-buffer)))
      (progn
        (org-speed-move-safe 'org-previous-visible-heading)
        (org-tree-jump))
    (org-toggle-narrow-to-subtree)
    (if (org-on-heading-p)
        (org-previous-visible-heading 1)
      (org-previous-visible-heading 2))
    (org-narrow-to-subtree)))

(defun org-tree-cycle ()
  "Cycle folding."
  (interactive)
  (let ((init-level (org-outline-level))
        (org-cycle-separator-lines 0))
    (if (save-excursion
          (forward-visible-line 1)
          (and (org-at-heading-p t)
               (< init-level (org-outline-level))))
        (progn
          (org-cycle)
          (org-hide-entry))
      (org-fold-show-branches))))

(defun org-tree-unfold ()
  "Unfold all branches."
  (interactive)
  (org-show-branches-buffer))

(defun org-tree-fold ()
  "Fold all branches."
  (interactive)
  (let ((org-cycle-separator-lines 0))
    (org-cycle '(4))))

(provide 'org-tree)
;;; org-tree.el ends here
