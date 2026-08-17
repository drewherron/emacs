;;; ox-hugo-publish.el --- Export only non-draft ox-hugo subtrees -*- lexical-binding: t; -*-

;;; Commentary:
;; `C-c C-e H A' (export all subtrees) exports TODO headings too, just with
;; draft = true in the front matter.  This adds an "export all, but skip the
;; drafts" command for the drewherron.com source in content-org/*.org.
;;
;; Bound to `C-c p' in org-mode only.

;;; Code:

(require 'org)

(declare-function org-hugo-export-wim-to-md "ox-hugo")

(defun my/ox-hugo-draft-subtree-p ()
  "Return non-nil if the post subtree at point would export as a draft.
Mirrors `org-hugo--parse-draft-state': a not-done TODO keyword, or an
explicit EXPORT_HUGO_DRAFT property.  A heading with no TODO keyword at
all is not a draft."
  (let ((todo (org-entry-get (point) "TODO"))
        (hugo-draft (org-entry-get (point) "EXPORT_HUGO_DRAFT")))
    (or (and (stringp todo) (not (member todo org-done-keywords)))
        (and (stringp hugo-draft)
             (member (downcase hugo-draft) '("t" "true" "yes" "1"))))))

(defun my/ox-hugo-export-published ()
  "Export every non-draft post subtree in the current buffer.
Like `C-c C-e H A' but omits TODO/draft headings entirely instead of
exporting them with draft = true.  Each subtree is exported exactly the
way `C-c C-e H H' would export it."
  (interactive)
  (require 'ox-hugo)
  (let ((n 0))
    (org-with-wide-buffer
     (org-map-entries
      (lambda () (setq n (1+ n)) (org-hugo-export-wim-to-md))
      ;; Same match ox-hugo's own all-subtrees loop uses.
      "EXPORT_FILE_NAME<>\"\"" nil
      ;; Extra skip predicate; returning a position skips that entry.
      ;; Note `org-map-entries' let-binds `org-agenda-skip-function' from
      ;; this argument, so it has to be passed here rather than wrapped
      ;; around the call in a `let'.
      (lambda () (and (my/ox-hugo-draft-subtree-p) (line-end-position)))))
    (message "[ox-hugo] Exported %d non-draft subtree%s"
             n (if (= n 1) "" "s"))))

(define-key org-mode-map (kbd "C-c p") #'my/ox-hugo-export-published)

(with-eval-after-load 'which-key
  (which-key-add-major-mode-key-based-replacements 'org-mode
    "C-c p" "ox-hugo publish"))

(provide 'ox-hugo-publish)
;;; ox-hugo-publish.el ends here
