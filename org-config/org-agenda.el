;;; Org Agenda - Daily Dashboard System

;; ============================================================================
;; ORG AGENDA BASIC CONFIGURATION
;; ============================================================================

;; Org Agenda files
(setq org-agenda-files '("~/org/"
                         "~/org/act/"
                         "~/org/cap/"
                         "~/org/lib/"))

;; Org Refile targets
(setq org-refile-targets
      `((,(directory-files "~/org/lib/" t "\\.org$") :maxlevel . 2)
        ("~/org/act/todo.org" :maxlevel . 1)
        ("~/org/act/read.org" :maxlevel . 1)
        ("~/org/act/watch.org" :maxlevel . 1)
        ("~/org/act/write.org" :maxlevel . 1)
        ("~/org/act/reminders.org" :maxlevel . 1)
        ("~/org/act/maybe.org" :maxlevel . 1)
        ("~/org/act/projects.org" :maxlevel . 1)
        ("~/org/act/today.org" :maxlevel . 1)))

;; Refile and agenda settings
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-agenda-start-day "-3d"
      org-agenda-span 14
      org-agenda-start-on-weekday nil
      org-deadline-warning-days 0
      org-agenda-hide-tags-regexp "noexport\\|exampleforreference"
      org-agenda-show-future-repeats t
      org-list-allow-alphabetical t)

;; Global keybinding for org-agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;; ============================================================================
;; DAILY DASHBOARD CUSTOM AGENDA VIEW
;; ============================================================================

;; Custom agenda view: Daily Dashboard
(setq org-agenda-custom-commands
      '(("d" "Daily Dashboard"
         (;; TODAY'S PRIORITIES
          (tags-todo "TODAY"
                     ((org-agenda-overriding-header "\nTODAY'S PRIORITIES\n")
                      (org-agenda-files '("~/org/act/today.org"))
                      (org-agenda-sorting-strategy '(priority-down))))

          ;; DAILY HABITS
          (tags-todo "HABIT"
                     ((org-agenda-overriding-header "\nDAILY HABITS\n")
                      (org-agenda-files '("~/org/act/today.org"))))

          ;; AGENDA VIEW
          (agenda ""
                  ((org-agenda-overriding-header "\nAGENDA\n")
                   (org-agenda-start-day "-2d")
                   (org-agenda-span 8)
                   ;; Show all org-agenda-files except today.org in this section
                   (org-agenda-files (seq-filter
                                      (lambda (f) (not (string-match-p "today\\.org" f)))
                                      org-agenda-files))))))

        ;; Dashboard Reset - prepare for new day
        ("r" "Reset / Morning Setup" morning-setup)

        ;; Habit Copy/Review - evening tracking
        ("h" "Habit Copy/Review" copy-habits-to-log)))

;; ============================================================================
;; DAILY DASHBOARD HELPER FUNCTIONS
;; ============================================================================

(defun reset-today-habits ()
  "Reset all habits in today.org to TODO state and clear numeric tracking."
  (interactive)
  (save-excursion
    (with-current-buffer (find-file-noselect "~/org/act/today.org")
      (goto-char (point-min))
      (let ((binary-count 0)
            (numeric-count 0))
        ;; Find the "Daily Habits" section
        (when (re-search-forward "^\\* Daily Habits.*:HABIT:" nil t)
          (let ((section-start (point))
                (section-end (save-excursion
                              (if (re-search-forward "^\\* " nil t)
                                  (match-beginning 0)
                                (point-max)))))
            ;; Reset binary habits (DONE/CANCELLED → TODO)
            (goto-char section-start)
            (while (re-search-forward "^\\*\\* \\(DONE\\|CANCELLED\\) " section-end t)
              (org-todo "TODO")
              (setq binary-count (1+ binary-count)))
            ;; Reset numeric tracking lines (delete everything after colon)
            (goto-char section-start)
            (while (< (point) section-end)
              (when (looking-at "^\\*\\* \\([^*]+:\\).*$")
                ;; Only process if not a TODO/DONE line
                (unless (looking-at "^\\*\\* \\(TODO\\|DONE\\)")
                  (replace-match "** \\1")
                  (setq numeric-count (1+ numeric-count))))
              (forward-line 1))))
        (save-buffer)
        (message "Reset %d habit%s and %d tracking line%s"
                 binary-count (if (= binary-count 1) "" "s")
                 numeric-count (if (= numeric-count 1) "" "s"))))))

(defun archive-today-completed ()
  "Archive completed items from today.org priority tasks."
  (interactive)
  (save-excursion
    (with-current-buffer (find-file-noselect "~/org/act/today.org")
      (goto-char (point-min))
      (let ((count 0))
        ;; Find all DONE items under "Today's Priority Tasks"
        (when (re-search-forward "^\\* Today's Priority Tasks" nil t)
          (let ((section-start (point)))
            (when (re-search-forward "^\\* " nil t)
              (let ((section-end (match-beginning 0)))
                (goto-char section-start)
                (while (re-search-forward "^\\*\\* DONE " section-end t)
                  (org-archive-subtree)
                  (setq count (1+ count)))))))
        (save-buffer)
        (message "Archived %d completed item%s" count (if (= count 1) "" "s"))))))

(defun morning-setup (&optional arg)
  "Prepare today.org for a new day: reset habits and archive completed tasks.
ARG is ignored, present for compatibility with org-agenda-custom-commands."
  (interactive)
  (reset-today-habits)
  (archive-today-completed)
  (find-file "~/org/act/today.org")
  (goto-char (point-min))
  (when (re-search-forward "^\\* Today's Priority Tasks" nil t)
    (forward-line 1))
  (message "Morning setup complete!"))

(defun copy-habits-to-log (&optional arg)
  "Copy today's habit completions to habits.org for tracking.
Opens both files side-by-side with habits auto-populated.
ARG is ignored, present for compatibility with org-agenda-custom-commands."
  (interactive)
  (let ((today-date (format-time-string "%Y-%m-%d"))
        (year-month (format-time-string "%Y-%m"))
        (habits-text "")
        (total 0)
        (completed 0))

    ;; Extract habit completions from today.org
    (with-current-buffer (find-file-noselect "~/org/act/today.org")
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^\\* Daily Habits.*:HABIT:" nil t)
          (let ((section-start (point))
                (section-end (save-excursion
                              (if (re-search-forward "^\\* " nil t)
                                  (match-beginning 0)
                                (point-max))))
                (habit-lines '()))
            ;; Extract habit lines (both TODO/DONE and numeric tracking)
            (goto-char section-start)
            (while (re-search-forward "^\\*\\* \\(.*\\)$" section-end t)
              (let ((line-content (match-string 1)))
                ;; Check if it's a TODO/DONE line or numeric tracking line
                (cond
                 ;; Binary habit (TODO/DONE) - count it
                 ((string-match "^\\(TODO\\|DONE\\) \\(.*\\)$" line-content)
                  (let ((state (match-string 1 line-content))
                        (title (match-string 2 line-content)))
                    (push (format "**** %s %s" state title) habit-lines)
                    (setq total (1+ total))
                    (when (string= state "DONE")
                      (setq completed (1+ completed)))))
                 ;; Numeric tracking line (e.g., "Sleep: 7 hours") - don't count
                 ((string-match "^[^*].*:" line-content)
                  (push (format "**** %s" line-content) habit-lines)))))
            ;; Reverse to maintain original order and join with newlines
            (setq habits-text (mapconcat 'identity (nreverse habit-lines) "\n"))
            (unless (string-empty-p habits-text)
              (setq habits-text (concat habits-text "\n")))))))

    ;; Open today.org in left window
    (delete-other-windows)
    (find-file "~/org/act/today.org")
    ;; Split and open habits.org in right window
    (split-window-right)
    (other-window 1)
    (find-file "~/org/act/habits.org")

    ;; Jump to current month or create it
    (goto-char (point-min))
    (unless (re-search-forward (format "^\\*\\* %s$" year-month) nil t)
      (when (re-search-forward "^\\* Habit Completion Log" nil t)
        (forward-line 2)
        (insert (format "** %s\n\n" year-month))))

    ;; Check if today's entry already exists
    (goto-char (point-min))
    (if (re-search-forward (format "^\\*\\*\\* %s$" today-date) nil t)
        (progn
          ;; Entry exists - expand and position at Notes
          (org-show-subtree)
          (when (re-search-forward "^\\*\\*\\*\\* Notes:$" nil t)
            (forward-line 1)
            (recenter))
          (message "Entry for %s already exists - update notes if needed" today-date))
      ;; Create new entry with habits auto-populated
      (goto-char (point-min))
      (when (re-search-forward (format "^\\*\\* %s$" year-month) nil t)
        (forward-line 1)
        (insert (format "\n*** %s\n**** Completed: %d/%d (%.0f%%)\n%s\n**** Notes:\n\n"
                       today-date
                       completed
                       total
                       (if (> total 0) (* 100.0 (/ (float completed) total)) 0)
                       habits-text))
        ;; Position cursor under Notes: and expand the tree
        (goto-char (point-min))
        (when (re-search-forward (format "^\\*\\*\\* %s$" today-date) nil t)
          (org-show-subtree)
          (when (re-search-forward "^\\*\\*\\*\\* Notes:$" nil t)
            (forward-line 1)))))

    ;; Stay in right window (habits.org), cursor positioned for notes
    (message "Habits auto-copied! Add your notes below, then save")))

;; Note: All dashboard commands are accessible via C-c a (org-agenda dispatcher)
;; C-c a d - Daily Dashboard
;; C-c a r - Reset / Morning Setup
;; C-c a h - Habit Copy/Review
