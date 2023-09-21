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


;;; Code:

(require 'org)
(require 'tabulated-list)
(require 'hl-line)

(defvar-keymap org-tree-mode-map
  :parent special-mode-map
  :doc "Keymap for `org-tree-mode'."
  "<return>" #'push-button
  "n" #'org-tree-next-heading
  "p" #'org-tree-previous-heading)

(define-derived-mode org-tree-mode tabulated-list-mode "Org-Tree"
  "Mode for `org-tree'."
  ;;\\{org-tree-mode-map}"
  (let ((columns [("Tree" 100)]))
    ;;(read-only-mode)
    (hl-line-mode)
    (setq-local cursor-type 'bar)
    (tabulated-list-mode)
    (hl-line-mode)
    (use-local-map org-tree-mode-map)
    (setq tabulated-list-format columns)
    (setq fringe-indicator-alist
          '((truncation nil nil)))))

(defcustom org-tree-narrow-on-jump t
  "When non-nil, source buffer is narrowed to subtree.")

(define-button-type 'org-tree
  'action 'org-tree-button-action
  'help-echo nil)

;;;###autoload
(defun org-tree ()
  "Create Org-Tree buffer."
  (interactive)
  (when (string-match "^<tree>.*" (buffer-name))
    (error "Don't tree a tree"))
  (unless (derived-mode-p 'org-mode)
    (error "Not an org buffer"))
  (jit-lock-mode 1)
  (jit-lock-fontify-now)
  (let* ((headings (org-tree--headings))
         (tree-name (format "<tree>%s" (buffer-name)))
         (tree-buffer (if (get-buffer tree-name)
                          (get-buffer tree-name)
                        (generate-new-buffer tree-name)))
         (tree-mode-line (format "Org-Tree - %s"
                                 (file-name-nondirectory buffer-file-name))))
    (add-hook 'after-save-hook 'org-tree--update nil t)
    ;; remove hook when tree-buffer closed
    (with-current-buffer-window tree-buffer
        '(display-buffer-in-side-window
          tree-buffer
          (side . left))
        nil
      (org-tree-mode)
      (setq tabulated-list-entries headings)
      (tabulated-list-print t t)
      (set-window-fringes (get-buffer-window tree-buffer) 1 1)
      (setq mode-line-format tree-mode-line))
    (pop-to-buffer tree-buffer)))

(defun org-tree-button-action (_)
  (interactive)
  (let ((tree-window (selected-window))
        (buffer (get-text-property (point) 'buffer))
        (pos (get-text-property (point) 'pos)))
    (unless (buffer-live-p buffer)
      (error "Base buffer has been killed"))
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

(defun org-tree--headings ()
  "Return a list of outline headings."
  (interactive)
  (let* ((narrow (when (org-buffer-narrowed-p)
                   (save-excursion
                     (goto-char (point-min)))))
         (heading-regexp (concat "^\\(?:"
                                 org-outline-regexp
                                 "\\)"))
         (buffer (current-buffer))
         headings)
    (when narrow (widen))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward heading-regexp nil t)
        (push (list
               (buffer-substring (pos-bol) (pos-eol))
               (vector (cons (buffer-substring (pos-bol) (pos-eol))
                             `(type org-tree
                                    buffer ,buffer
                                    pos ,(point)
                                    keymap org-tree-mode-map
                                    ))))
              headings)
        (goto-char (1+ (pos-eol)))))
    (when narrow (save-excursion
                   (goto-char narrow)
                   (org-narrow-to-subtree)))
    (unless headings
      (user-error "No headings"))
    (nreverse headings)))

(defun org-tree--update ()
  ;; FIX: remove after-save-hook if tree buffer doesn't exist
  (let ((headings (org-tree--headings))
        (tree-buffer (get-buffer
                      (format "<tree>%s"
                              (buffer-name)))))
    (with-current-buffer tree-buffer
      (setq tabulated-list-entries headings)
      ;; TODO figure out why I can't restore point
      (tabulated-list-print t t))))

(defun org-tree-next-heading ()
  "Move to next heading."
  (interactive)
  (if (string-match "^<tree>.*"
                    (buffer-name))
      (progn
        (forward-line 1)
        (push-button nil t))
    (widen)
    (org-next-visible-heading 1)
    ;; FIX: move cursor in tree window
    ;; (let* ((tree-buffer (get-buffer (concat
    ;;                                  "<tree>"
    ;;                                  (buffer-name))))
    ;;        (tree-window (get-buffer-window tree-buffer))
    ;;        (base-buffer (buffer-base-buffer tree-buffer))
    ;;        (base-window (get-buffer-window base-buffer))
    ;;        (pos (point)))
    ;;   (when tree-window
    ;;     (select-window tree-window)
    ;;     (goto-char pos) FIX: get tabulated list id instead
    ;;     (hl-line-highlight)
    ;;     (select-window base-window)))
    (if org-tree-narrow-on-jump
        (org-narrow-to-subtree))))

(defun org-tree-previous-heading ()
  "Move to previous heading."
  (interactive)
  (if (string-match "^<tree>.*"
                    (buffer-name))
      (progn
        (forward-line -1)
        (push-button nil t))
    (widen)
    (org-previous-visible-heading 1)
    ;; FIX: move cursor in tree window
    ;; (let* ((tree-buffer (get-buffer (concat
    ;;                                  "<tree>"
    ;;                                  (buffer-name))))
    ;;        (tree-window (get-buffer-window tree-buffer))
    ;;        (base-buffer (buffer-base-buffer tree-buffer))
    ;;        (base-window (get-buffer-window base-buffer))
    ;;        (pos (point)))
    ;;   (when tree-window
    ;;     (select-window tree-window)
    ;;     (goto-char pos)
    ;;     (hl-line-highlight)
    ;;     (select-window base-window)))
    (when org-tree-narrow-on-jump
      (unless (org-before-first-heading-p)
        (org-narrow-to-subtree)))))

;; (defun org-tree-live ()
;;   "Update Org-Tree buffer."
;;   ;; slow
;;   ;; can't get the hook add/removal to work right
;;   (interactive)
;;   (let ((run-collect (make-symbol "run-collect"))
;;         (stop-collect (make-symbol "stop-collect"))
;;         timer)
;;     (setf (symbol-function stop-collect)
;;           (lambda ()
;;             (remove-hook 'after-change-functions run-collect t)))
;;     (setf (symbol-function run-collect)
;;           (lambda (_1 _2 _3)
;;             (unless timer
;;               (setq timer
;;                     (run-with-idle-timer
;;                      0.05 nil
;;                      (lambda ()
;;                        (let ((headings (org-tree--headings))
;;                              (tree-buffer (get-buffer
;;                                            (format "<tree>%s"
;;                                                    (buffer-name)))))
;;                          ;; (if (not (buffer-live-p tree-buffer))
;;                          ;;     (funcall stop-collect)
;;                          (with-current-buffer tree-buffer
;;                            (setq tabulated-list-entries headings)
;;                            ;; TODO figure out why I can't restore point
;;                            (tabulated-list-print t t))
;;                          (setq timer nil)
;;                          )))))))
;;     (add-hook 'after-change-functions run-collect nil t)))

(provide 'org-tree)
;;; org-tree.el ends here
