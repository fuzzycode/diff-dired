;;; diff-dired.el --- A Dired Utility to list changed files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Björn Larsson
;;
;; Author: Björn Larsson <develop@bjornlarsson.net>
;; Maintainer: Björn Larsson <develop@bjornlarsson.net>
;; Created: January 02, 2023
;; Modified: January 02, 2023
;; Version: 0.0.1
;; Keywords: convenience extensions files vc
;; Homepage: https://github.com/fuzzycode/diff-dired
;; Package-Requires: ((emacs "25.1") (magit "3.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; A package to list changed files between two branches in a Dired enabled buffer
;;
;; The listing makes use of git's --diff-filter option to provide different listings
;; of files that have in some way been modified between two different branches.
;;
;;; Code:

(require 'dired)
(require 'magit)

(defun diff-dired-sentinel (proc state)
  "Sentinel for \\[diff-dired] processes."
  (let ((buf (process-buffer proc)))
    (if (buffer-name buf)
        (with-current-buffer buf
          (save-excursion
            (setq mode-line-process (format ":%s" (process-status proc)))
            (delete-process proc)
            (force-mode-line-update))))))

(defun diff-dired-filter (proc string)
  "Filter for \\[diff-dired] processes."
  (let ((buf (process-buffer proc))
        (inhibit-read-only t))
    (if (buffer-name buf)
        (with-current-buffer buf
          (save-excursion
            (save-restriction
              (widen)
              (let ((buffer-read-only nil)
                    (beg (point-max)))
                (goto-char beg)
                (insert string)
                (goto-char beg)
                (or (looking-at "^")
                    (forward-line 1))
                (while (looking-at "^")
                  (insert "  ")
                  (forward-line 1))
                ;; Find all the complete lines in the unprocessed
                ;; output and process it to add text properties.
                (goto-char (point-max))
                (if (search-backward "\n" (process-mark proc) t)
                    (progn
                      (dired-insert-set-properties (process-mark proc)
                                                   (1+ (point)))
                      (move-marker (process-mark proc) (1+ (point)))))))))
      ;; The buffer has been killed.
      (delete-process proc))))

;;;###autoload
(defun diff-dired (filter base)
  "Calculate the diff between BASE and HEAD using FILTER."
  (let ((dired-buffers nil)
        (diff-dired-buffer-name "*Diff Dired*")
        (cmd (concat "git diff --name-only --no-color " (format "--diff-filter=%s" filter) " " base " | xargs gls -ldh --quoting-style=literal"))
        (root (magit-toplevel)))

    ;; Check that it's really a directory.
    (unless root
      (error "%s is not a git directory" root))

    (with-current-buffer (get-buffer-create diff-dired-buffer-name)
      ;; prepare buffer
      (switch-to-buffer (current-buffer))
      (widen)
      (kill-all-local-variables)
      (setq buffer-read-only nil)
      (erase-buffer)

      (setq default-directory root)
      ;; Start the process.
      (async-shell-command cmd (current-buffer))

      ;; enable Dired mode
      (dired-mode root)

      ;; provide a keybinding to kill the find process
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map (current-local-map))
        (use-local-map map))

      (set (make-local-variable 'dired-sort-inhibit) t)
      (set (make-local-variable 'revert-buffer-function)
           `(lambda (ignore-auto noconfirm)
              (diff-dired ,filter ,base)))

      (set (make-local-variable 'dired-subdir-alist)
           (list (cons default-directory (point-min-marker))))

      (setq buffer-read-only nil)
      ;; Subdir headlerline must come first because the first marker in
      ;; `subdir-alist' points there.
      (insert "  " root ":\n")

      ;; Make second line a ``find'' line in analogy to the ``total'' or
      ;; ``wildcard'' line.
      (let ((beg (point)))
        (insert "  " cmd "\n")
        (dired-insert-set-properties beg (point)))
      (setq buffer-read-only t)

      (let ((proc (get-buffer-process (current-buffer))))
        (set-process-filter proc (function diff-dired-filter))
        (set-process-sentinel proc (function diff-dired-sentinel))
        ;; Initialize the process marker; it is used by the filter.
        (move-marker (process-mark proc) 1 (current-buffer)))
      (setq mode-line-process '(":%s")))))


;;;###autoload
(defun diff-dired-list-added (base)
  "List added files between BASE and HEAD."
  (interactive (list (magit-read-branch "Base" (magit-main-branch))))
  (diff-dired "A" base))

;;;###autoload
(defun diff-dired-list-modified (base)
  "List modified files between BASE and HEAD."
  (interactive (list (magit-read-branch "Base" (magit-main-branch))))
  (diff-dired "M" base))

;;;###autoload
(defun diff-dired-list-renamed (base)
  "List renamed files between BASE and HEAD."
  (interactive (list (magit-read-branch "Base" (magit-main-branch))))
  (diff-dired "R" base))

;;;###autoload
(defun diff-dired-list-coppied (base)
  "List coppied files between BASE and HEAD."
  (interactive (list (magit-read-branch "Base" (magit-main-branch))))
  (diff-dired "C" base))

;;;###autoload
(defun diff-dired-list-type-changed (base)
  "List files who's type has changed between BASE and HEAD."
  (interactive (list (magit-read-branch "Base" (magit-main-branch))))
  (diff-dired "T" base))

;;;###autoload
(defun diff-dired-list-changed (base)
  "List all changed files except deleted ones between BASE and HEAD."
  (interactive (list (magit-read-branch "Base" (magit-main-branch))))
  (diff-dired "d" base))

(defun diff-dired-cleanup ()
  "Clean up diff-dired created temp buffers for multiple searching processes."
  (mapcar 'kill-buffer
          (seq-filter
           (lambda (buffer-name)
             (string-match-p "*Diff Dired*" buffer-name))
           (mapcar 'buffer-name (buffer-list)))))

;;;###autoload
(add-hook 'kill-emacs-hook #'diff-dired-cleanup)

(provide 'diff-dired)
;;; diff-dired.el ends here
