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
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Insert some commentary
;;
;;
;;; Code:

(require 'find-dired)
(require 'magit)

(defgroup diff-dired nil
  "diff-dired customize group."
  :prefix "diff-dired-"
  :group 'diff-dired)

;;;###autoload
(defun diff-dired (filter base compare)
  ""

  (let ((dired-buffers dired-buffers)
        (diff-dired-buffer-name "*Diff Dired*")
        (cmd (concat "git diff --name-only --no-color " (format "--diff-filter=%s" filter) " " base " " compare " | xargs gls -ldh --quoting-style=literal &"))
        (root (magit-toplevel)))

    ;; Check that it's really a directory.
    (or (file-directory-p root)
        (error "diff-dired needs a directory: %s" root))

    (with-current-buffer (get-buffer-create diff-dired-buffer-name)
      ;; prepare buffer
      (switch-to-buffer (current-buffer))
      (widen)
      (kill-all-local-variables)
      (setq buffer-read-only nil)
      (erase-buffer)

      (setq default-directory root)
      ;; Start the process.
      (shell-command cmd (current-buffer))

      ;; enable Dired mode
      (dired-mode root)

      ;; provide a keybinding to kill the find process
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map (current-local-map))
        (use-local-map map))

      (set (make-local-variable 'dired-sort-inhibit) t)
      (set (make-local-variable 'revert-buffer-function)
           `(lambda (ignore-auto noconfirm)
              (diff-dired ,filter ,base ,compare)))

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
        (set-process-filter proc (function find-dired-filter))
        (set-process-sentinel proc (function find-dired-sentinel))
        ;; Initialize the process marker; it is used by the filter.
        (move-marker (process-mark proc) 1 (current-buffer)))
      (setq mode-line-process '(":%s")))))


;;;###autoload
(defun diff-dired-list-added (base compare)
  ""
  (interactive (list (magit-read-branch "Base" (magit-main-branch))
                     (magit-read-branch "Compare" (magit-get-current-branch))))
  (diff-dired "A" base compare))

;;;###autoload
(defun diff-dired-list-modified (base compare)
  ""
  (interactive (list (magit-read-branch "Base" (magit-main-branch))
                     (magit-read-branch "Compare" (magit-get-current-branch))))
  (diff-dired "M" base compare))

;;;###autoload
(defun diff-dired-list-changed (base compare)
  ""
  (interactive (list (magit-read-branch "Base" (magit-main-branch))
                     (magit-read-branch "Compare" (magit-get-current-branch))))
  (diff-dired "C" base compare))

(defun diff-dired-cleanup ()
  "Clean up diff-dired created temp buffers for multiple searching processes."
  (mapcar 'kill-buffer
          (seq-filter
           (lambda (buffer-name)
             (string-match-p "*Diff Dired*" buffer-name))
           (mapcar 'buffer-name (buffer-list)))))

(add-hook 'kill-emacs-hook #'diff-dired-cleanup)

(provide 'diff-dired)
;;; diff-dired.el ends here
