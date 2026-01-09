;;; org-utils.el --- Utility functions for org-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Collection of custom utility functions for org-mode

;;; Code:

(defun deduplicate-org-headings ()
  "Find and delete duplicate org trees in all open org files.
Compares entire subtrees (heading + all content + all subheadings).
Only checks for duplicates within each file, not across files.
Keeps the first occurrence of each unique tree and deletes subsequent duplicates."
  (interactive)
  (let ((org-buffers (seq-filter (lambda (buf)
                                   (with-current-buffer buf
                                     (derived-mode-p 'org-mode)))
                                 (buffer-list)))
        (total-deleted 0))
    (if (not org-buffers)
        (message "No org-mode buffers are currently open")
      (dolist (buffer org-buffers)
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            (let ((seen-trees (make-hash-table :test 'equal))
                  (to-delete '()))
              ;; First pass: collect all trees and mark duplicates using markers
              (while (not (eobp))
                (when (org-at-heading-p)
                  (let* ((tree-start (point))
                         (tree-end (save-excursion
                                    (org-end-of-subtree t t)
                                    (point)))
                         (tree-content (buffer-substring-no-properties
                                      tree-start
                                      tree-end)))
                    (if (gethash tree-content seen-trees)
                        ;; This is a duplicate - use markers that will adjust
                        (let ((start-marker (copy-marker tree-start))
                              (end-marker (copy-marker tree-end)))
                          (push (cons start-marker end-marker) to-delete))
                      ;; First occurrence, remember it
                      (puthash tree-content t seen-trees))))
                ;; Move to next heading (at any level)
                (if (outline-next-heading)
                    nil
                  (goto-char (point-max))))
              ;; Second pass: delete duplicates in reverse order
              ;; (reverse ensures we delete from end to beginning)
              (dolist (region (reverse to-delete))
                (delete-region (marker-position (car region))
                             (marker-position (cdr region)))
                ;; Clean up markers
                (set-marker (car region) nil)
                (set-marker (cdr region) nil))
              (when to-delete
                (setq total-deleted (+ total-deleted (length to-delete)))
                (message "Deleted %d duplicate tree(s) in %s"
                        (length to-delete)
                        (buffer-name)))))))
      (if (> total-deleted 0)
          (message "Total: Deleted %d duplicate tree(s) across all org buffers" total-deleted)
        (message "No duplicate trees found in open org buffers")))))

(defun deduplicate-capture-file ()
  "Remove top-level headings from current buffer that exist in other open org files.
Useful for cleaning up a capture file after refiling entries elsewhere.
Compares tree content (heading + content + subheadings) regardless of org level,
so a heading at level 3 in another file will match a top-level heading here.
Only deletes from the current buffer, leaving other files untouched."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not in org-mode"))
  (let* ((current-buf (current-buffer))
         (other-org-buffers (seq-filter (lambda (buf)
                                          (and (not (eq buf current-buf))
                                               (with-current-buffer buf
                                                 (derived-mode-p 'org-mode))))
                                        (buffer-list))))
    (if (not other-org-buffers)
        (message "No other org-mode buffers open to compare against")
      ;; First, collect all unique trees from other buffers
      (let ((other-trees (make-hash-table :test 'equal)))
        ;; Scan all other org buffers and collect their trees
        (dolist (buffer other-org-buffers)
          (with-current-buffer buffer
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (when (org-at-heading-p)
                  (let* ((tree-start (point))
                         (tree-end (save-excursion
                                    (org-end-of-subtree t t)
                                    (point)))
                         ;; Normalize by removing leading stars and spaces from heading
                         (tree-content (buffer-substring-no-properties
                                      tree-start
                                      tree-end))
                         ;; Extract just the content after the stars and spaces
                         (normalized-content
                          (replace-regexp-in-string
                           "^\\*+ " "* "
                           tree-content)))
                    (puthash normalized-content t other-trees)))
                (if (outline-next-heading)
                    nil
                  (goto-char (point-max)))))))

        ;; Now check top-level headings in current buffer
        (let ((to-delete '())
              (deleted-count 0))
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (when (and (org-at-heading-p)
                        (= (org-outline-level) 1))  ; Only top-level headings
                (let* ((tree-start (point))
                       (tree-end (save-excursion
                                  (org-end-of-subtree t t)
                                  (point)))
                       (tree-content (buffer-substring-no-properties
                                    tree-start
                                    tree-end))
                       (normalized-content
                        (replace-regexp-in-string
                         "^\\*+ " "* "
                         tree-content)))
                  (when (gethash normalized-content other-trees)
                    ;; Found in another buffer - mark for deletion
                    (let ((start-marker (copy-marker tree-start))
                          (end-marker (copy-marker tree-end)))
                      (push (cons start-marker end-marker) to-delete)))))
              (if (outline-next-heading)
                  nil
                (goto-char (point-max)))))

          ;; Delete marked trees in reverse order
          (dolist (region (reverse to-delete))
            (delete-region (marker-position (car region))
                         (marker-position (cdr region)))
            (setq deleted-count (1+ deleted-count))
            ;; Clean up markers
            (set-marker (car region) nil)
            (set-marker (cdr region) nil))

          (if (> deleted-count 0)
              (message "Deleted %d top-level tree(s) from %s that exist in other org files"
                      deleted-count (buffer-name))
            (message "No duplicate trees found in current buffer")))))))

(provide 'org-utils)
;;; org-utils.el ends here
