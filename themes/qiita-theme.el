;;; qiita-theme.el --- A low contrast color theme for Emacs.

(deftheme qiita "The Qiita color theme")

(defgroup qiita-theme nil
  "Qiita theme."
  :prefix "qiita-theme-"
  :tag "Qiita theme")

;;;###autoload
(defcustom override-colors-alist '()
  "Place to override default theme colors.

You can override a subset of the theme's default colors by
defining them in this alist."
  :group 'qiita-theme
  :type '(alist
          :key-type (string :tag "Name")
          :value-type (string :tag " Hex")))

(defcustom use-variable-pitch nil
  "Use variable pitch face for some headings and titles."
  :type 'boolean
  :group 'qiita-theme
  :package-version '(qiita . "2.6"))

(defcustom height-minus-1 0.8
  "Font size -1."
  :type 'number
  :group 'qiita-theme
  :package-version '(qiita . "2.6"))

(defcustom height-plus-1 1.1
  "Font size +1."
  :type 'number
  :group 'qiita-theme
  :package-version '(qiita . "2.6"))

(defcustom height-plus-2 1.15
  "Font size +2."
  :type 'number
  :group 'qiita-theme
  :package-version '(qiita . "2.6"))

(defcustom height-plus-3 1.2
  "Font size +3."
  :type 'number
  :group 'qiita-theme
  :package-version '(qiita . "2.6"))

(defcustom height-plus-4 1.3
  "Font size +4."
  :type 'number
  :group 'qiita-theme
  :package-version '(qiita . "2.6"))

(defcustom scale-org-headlines nil
  "Whether `org-mode' headlines should be scaled."
  :type 'boolean
  :group 'qiita-theme
  :package-version '(qiita . "2.6"))

(defcustom scale-outline-headlines nil
  "Whether `outline-mode' headlines should be scaled."
  :type 'boolean
  :group 'qiita-theme
  :package-version '(qiita . "2.6"))

;;; Color Palette

(defvar default-colors-alist
  '(
    ("fg+1"     . "#FFFFEF")
    ("fg"       . "#DCDCCC")
    ("fg-1"     . "#bfbfbf")
    ("bg-2"     . "#000000")
    ("bg-1"     . "#2B2B2B")
    ("bg-05"    . "#383838")
    ("bg"       . "#000000")
    ("bg+05"    . "#494949")
    ("bg+1"     . "#4F4F4F")
    ("bg+2"     . "#5F5F5F")
    ("bg+3"     . "#6F6F6F")
    ("comment"  . "#9cabaf")
    ("region"   . "#4a708b")
    ("red+2"    . "#ff7f94")
    ("red+1"    . "#ff7f94")
    ("red"      . "#ff7f94")
    ("red-1"    . "#ff7f94")
    ("red-2"    . "#ff7f94")
    ("red-3"    . "#ff7f94")
    ("red-4"    . "#ff7f94")
    ("red-5"    . "#ff7f94")
    ("red-6"    . "#ff7f94")
    ("orange"   . "#ff7f94")
    ("yellow"   . "#ebd247")
    ("yellow-1" . "#e5d247")
    ("yellow-2" . "#e5d247")
    ("green-5"  . "#8bdf4c")
    ("green-4"  . "#8bdf4c")
    ("green-3"  . "#8bdf4c")
    ("green-2"  . "#8bdf4c")
    ("green-1"  . "#8bdf4c")
    ("green"    . "#8bdf4c")
    ("green+1"  . "#8bdf4c")
    ("green+2"  . "#8bdf4c")
    ("green+3"  . "#8bdf4c")
    ("green+4"  . "#8bdf4c")
    ("cyan"     . "#55d6c3")
    ("blue+3"   . "#41b7d7")
    ("blue+2"   . "#41b7d7")
    ("blue+1"   . "#41b7d7")
    ("blue"     . "#41b7d7")
    ("blue-1"   . "#41b7d7")
    ("blue-2"   . "#41b7d7")
    ("blue-3"   . "#41b7d7")
    ("blue-4"   . "#41b7d7")
    ("blue-5"   . "#41b7d7")
    ("magenta"  . "#A980F5")
    )
  "List of Qiita colors.
Each element has the form (NAME . HEX).

`+N' suffixes indicate a color is lighter.
`-N' suffixes indicate a color is darker.")

(defmacro with-color-variables (&rest body)
  "`let' bind all colors defined in `colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   (append default-colors-alist
                           override-colors-alist))
         (z-variable-pitch (if use-variable-pitch
                               'variable-pitch 'default)))
     ,@body))

;;; Theme Faces
(with-color-variables
  (custom-theme-set-faces
   'qiita
;;;; Built-in
;;;;; basic coloring
   '(button ((t (:underline t))))
   `(link ((t (:foreground ,yellow :underline t :weight bold))))
   `(link-visited ((t (:foreground ,yellow-2 :underline t :weight normal))))
   `(default ((t (:foreground ,fg :background ,bg))))
   `(cursor ((t (:foreground ,fg :background ,fg+1))))
   `(widget-field ((t (:foreground ,fg :background ,bg+3))))
   `(escape-glyph ((t (:foreground ,yellow :weight bold))))
   `(fringe ((t (:foreground ,fg :background ,bg+1))))
   `(header-line ((t (:foreground ,yellow
                                  :background ,bg-1
                                  :box (:line-width -1 :style released-button)))))
   `(highlight ((t (:background , bg-05))))
   `(success ((t (:foreground ,green :weight bold))))
   `(warning ((t (:foreground ,orange :weight bold))))
   `(tooltip ((t (:foreground ,fg :background ,bg+1))))
;;;;; compilation
   `(compilation-column-face ((t (:foreground ,yellow))))
   `(compilation-enter-directory-face ((t (:foreground ,green))))
   `(compilation-error-face ((t (:foreground ,red-1 :weight bold :underline t))))
   `(compilation-face ((t (:foreground ,fg))))
   `(compilation-info-face ((t (:foreground ,blue))))
   `(compilation-info ((t (:foreground ,green+4 :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,green))))
   `(compilation-line-face ((t (:foreground ,yellow))))
   `(compilation-line-number ((t (:foreground ,green))))
   `(compilation-message-face ((t (:foreground ,blue))))
   `(compilation-warning-face ((t (:foreground ,orange :weight bold :underline t))))
   `(compilation-mode-line-exit ((t (:foreground ,green+2 :weight bold))))
   `(compilation-mode-line-fail ((t (:foreground ,red :weight bold))))
   `(compilation-mode-line-run ((t (:foreground ,yellow :weight bold))))
;;;;; completions
   `(completions-annotations ((t (:foreground ,fg-1))))
;;;;; eww
   '(eww-invalid-certificate ((t (:inherit error))))
   '(eww-valid-certificate   ((t (:inherit success))))
;;;;; grep
   `(grep-context-face ((t (:foreground ,fg))))
   `(grep-error-face ((t (:foreground ,red-1 :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,blue))))
   `(grep-match-face ((t (:foreground ,orange :weight bold))))
   `(match ((t (:background ,bg-1 :foreground ,orange :weight bold))))
;;;;; hi-lock
   `(hi-blue    ((t (:background ,cyan    :foreground ,bg-1))))
   `(hi-green   ((t (:background ,green+4 :foreground ,bg-1))))
   `(hi-pink    ((t (:background ,magenta :foreground ,bg-1))))
   `(hi-yellow  ((t (:background ,yellow  :foreground ,bg-1))))
   `(hi-blue-b  ((t (:foreground ,blue    :weight     bold))))
   `(hi-green-b ((t (:foreground ,green+2 :weight     bold))))
   `(hi-red-b   ((t (:foreground ,red     :weight     bold))))
;;;;; info
   `(Info-quoted ((t (:inherit font-lock-constant-face))))
;;;;; isearch
   `(isearch ((t (:foreground ,yellow-2 :weight bold :background ,bg+2))))
   `(isearch-fail ((t (:foreground ,fg :background ,red-4))))
   `(lazy-highlight ((t (:foreground ,yellow-2 :weight bold :background ,bg-05))))

   `(menu ((t (:foreground ,fg :background ,"color-232"))))
   `(minibuffer-prompt ((t (:foreground ,yellow))))
   `(mode-line
     ((,class (:foreground ,green+1
                           :background ,bg-1
                           :box (:line-width -1 :style released-button)))
      (t :inverse-video t)))
   `(mode-line-buffer-id ((t (:foreground ,yellow :weight bold))))
   `(mode-line-inactive
     ((t (:foreground ,green-2
                      :background ,bg-05
                      :box (:line-width -1 :style released-button)))))
   `(region ((,class (:background ,region))
             (t :inverse-video t)))
   `(secondary-selection ((t (:background ,bg+2))))
   `(trailing-whitespace ((t (:background ,red))))
   `(vertical-border ((t (:foreground ,fg))))
;;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,fg :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comment))))
   `(font-lock-constant-face ((t (:foreground ,blue+3))))
   `(font-lock-doc-face ((t (:foreground ,green+2))))
   `(font-lock-function-name-face ((t (:foreground ,green))))
   `(font-lock-keyword-face ((t (:foreground ,yellow :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,yellow :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,blue+1))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,green :weight bold))))
   `(font-lock-string-face ((t (:foreground ,blue))))
   `(font-lock-type-face ((t (:foreground ,green-1))))
   `(font-lock-variable-name-face ((t (:foreground ,orange))))
   `(font-lock-warning-face ((t (:foreground ,yellow-2 :weight bold))))

   `(c-annotation-face ((t (:inherit font-lock-constant-face))))
;;;;; line numbers (Emacs 26.1 and above)
   `(line-number ((t (:foreground ,fg-1 :background ,bg-05))))
   `(line-number-current-line ((t (:inherit line-number :foreground ,green-2))))
;;;;; man
   '(Man-overstrike ((t (:inherit font-lock-keyword-face))))
   '(Man-underline  ((t (:inherit (font-lock-string-face underline)))))
;;;;; newsticker
   `(newsticker-date-face ((t (:foreground ,fg))))
   `(newsticker-default-face ((t (:foreground ,fg))))
   `(newsticker-enclosure-face ((t (:foreground ,green+3))))
   `(newsticker-extra-face ((t (:foreground ,bg+2 :height 0.8))))
   `(newsticker-feed-face ((t (:foreground ,fg))))
   `(newsticker-immortal-item-face ((t (:foreground ,green))))
   `(newsticker-new-item-face ((t (:foreground ,blue))))
   `(newsticker-obsolete-item-face ((t (:foreground ,red))))
   `(newsticker-old-item-face ((t (:foreground ,bg+3))))
   `(newsticker-statistics-face ((t (:foreground ,fg))))
   `(newsticker-treeview-face ((t (:foreground ,fg))))
   `(newsticker-treeview-immortal-face ((t (:foreground ,green))))
   `(newsticker-treeview-listwindow-face ((t (:foreground ,fg))))
   `(newsticker-treeview-new-face ((t (:foreground ,blue :weight bold))))
   `(newsticker-treeview-obsolete-face ((t (:foreground ,red))))
   `(newsticker-treeview-old-face ((t (:foreground ,bg+3))))
   `(newsticker-treeview-selection-face ((t (:background ,bg-1 :foreground ,yellow))))
;;;;; woman
   '(woman-bold   ((t (:inherit font-lock-keyword-face))))
   '(woman-italic ((t (:inherit (font-lock-string-face italic)))))
;;;; Third-party
;;;;; ace-jump
   `(ace-jump-face-background
     ((t (:foreground ,fg-1 :background ,bg :inverse-video nil))))
   `(ace-jump-face-foreground
     ((t (:foreground ,green+2 :background ,bg :inverse-video nil))))
;;;;; ace-window
   `(aw-background-face
     ((t (:foreground ,fg-1 :background ,bg :inverse-video nil))))
   `(aw-leading-char-face ((t (:inherit aw-mode-line-face))))
;;;;; android mode
   `(android-mode-debug-face ((t (:foreground ,green+1))))
   `(android-mode-error-face ((t (:foreground ,orange :weight bold))))
   `(android-mode-info-face ((t (:foreground ,fg))))
   `(android-mode-verbose-face ((t (:foreground ,green))))
   `(android-mode-warning-face ((t (:foreground ,yellow))))
;;;;; anzu
   `(anzu-mode-line ((t (:foreground ,cyan :weight bold))))
   `(anzu-mode-line-no-match ((t (:foreground ,red :weight bold))))
   `(anzu-match-1 ((t (:foreground ,bg :background ,green))))
   `(anzu-match-2 ((t (:foreground ,bg :background ,orange))))
   `(anzu-match-3 ((t (:foreground ,bg :background ,blue))))
   `(anzu-replace-to ((t (:inherit anzu-replace-highlight :foreground ,yellow))))
;;;;; auctex
   `(font-latex-bold-face ((t (:inherit bold))))
   `(font-latex-warning-face ((t (:foreground nil :inherit font-lock-warning-face))))
   `(font-latex-sectioning-5-face ((t (:foreground ,red :weight bold ))))
   `(font-latex-sedate-face ((t (:foreground ,yellow))))
   `(font-latex-italic-face ((t (:foreground ,cyan :slant italic))))
   `(font-latex-string-face ((t (:inherit ,font-lock-string-face))))
   `(font-latex-math-face ((t (:foreground ,orange))))
   `(font-latex-script-char-face ((t (:foreground ,orange))))
;;;;; agda-mode
   `(agda2-highlight-keyword-face ((t (:foreground ,yellow :weight bold))))
   `(agda2-highlight-string-face ((t (:foreground ,red))))
   `(agda2-highlight-symbol-face ((t (:foreground ,orange))))
   `(agda2-highlight-primitive-type-face ((t (:foreground ,blue-1))))
   `(agda2-highlight-inductive-constructor-face ((t (:foreground ,fg))))
   `(agda2-highlight-coinductive-constructor-face ((t (:foreground ,fg))))
   `(agda2-highlight-datatype-face ((t (:foreground ,blue))))
   `(agda2-highlight-function-face ((t (:foreground ,blue))))
   `(agda2-highlight-module-face ((t (:foreground ,blue-1))))
   `(agda2-highlight-error-face ((t (:foreground ,bg :background ,magenta))))
   `(agda2-highlight-unsolved-meta-face ((t (:foreground ,bg :background ,magenta))))
   `(agda2-highlight-unsolved-constraint-face ((t (:foreground ,bg :background ,magenta))))
   `(agda2-highlight-termination-problem-face ((t (:foreground ,bg :background ,magenta))))
   `(agda2-highlight-incomplete-pattern-face ((t (:foreground ,bg :background ,magenta))))
   `(agda2-highlight-typechecks-face ((t (:background ,red-4))))
;;;;; auto-complete
   `(ac-candidate-face ((t (:background ,bg+3 :foreground ,bg-2))))
   `(ac-selection-face ((t (:background ,blue-4 :foreground ,fg))))
   `(popup-tip-face ((t (:background ,yellow-2 :foreground ,bg-2))))
   `(popup-menu-mouse-face ((t (:background ,yellow-2 :foreground ,bg-2))))
   `(popup-summary-face ((t (:background ,bg+3 :foreground ,bg-2))))
   `(popup-scroll-bar-foreground-face ((t (:background ,blue-5))))
   `(popup-scroll-bar-background-face ((t (:background ,bg-1))))
   `(popup-isearch-match ((t (:background ,bg :foreground ,fg))))
;;;;; avy
   `(avy-background-face
     ((t (:foreground , :background , :inverse-video nil))))
   `(avy-lead-face-0
     ((t (:foreground ,bg :background ,magenta :inverse-video nil :weight bold))))
   `(avy-lead-face-1
     ((t (:foreground ,bg :background ,yellow :inverse-video nil :weight bold))))
   `(avy-lead-face-2
     ((t (:foreground ,bg :background ,red+1 :inverse-video nil :weight bold))))
   `(avy-lead-face
     ((t (:foreground ,bg :background ,magenta :inverse-video nil :weight bold))))
;;;;; company-mode
   `(company-tooltip ((t (:foreground ,fg :background ,bg+1))))
   `(company-tooltip-annotation ((t (:foreground ,orange :background ,bg+1))))
   `(company-tooltip-annotation-selection ((t (:foreground ,orange :background ,bg-1))))
   `(company-tooltip-selection ((t (:foreground ,fg :background ,bg-1))))
   `(company-tooltip-mouse ((t (:background ,bg-1))))
   `(company-tooltip-common ((t (:foreground ,green+2))))
   `(company-tooltip-common-selection ((t (:foreground ,green+2))))
   `(company-scrollbar-fg ((t (:background ,bg-1))))
   `(company-scrollbar-bg ((t (:background ,bg+2))))
   `(company-preview ((t (:background ,green+2))))
   `(company-preview-common ((t (:foreground ,green+2 :background ,bg-1))))
;;;;; bm
   `(bm-face ((t (:background ,yellow-1 :foreground ,bg))))
   `(bm-fringe-face ((t (:background ,yellow-1 :foreground ,bg))))
   `(bm-fringe-persistent-face ((t (:background ,green-2 :foreground ,bg))))
   `(bm-persistent-face ((t (:background ,green-2 :foreground ,bg))))
;;;;; calfw
   `(cfw:face-annotation ((t (:foreground ,red :inherit cfw:face-day-title))))
   `(cfw:face-day-title ((t nil)))
   `(cfw:face-default-content ((t (:foreground ,green))))
   `(cfw:face-default-day ((t (:weight bold))))
   `(cfw:face-disable ((t (:foreground ,fg-1))))
   `(cfw:face-grid ((t (:inherit shadow))))
   `(cfw:face-header ((t (:inherit font-lock-keyword-face))))
   `(cfw:face-holiday ((t (:inherit cfw:face-sunday))))
   `(cfw:face-periods ((t (:foreground ,cyan))))
   `(cfw:face-saturday ((t (:foreground ,blue :weight bold))))
   `(cfw:face-select ((t (:background ,blue-5))))
   `(cfw:face-sunday ((t (:foreground ,red :weight bold))))
   `(cfw:face-title ((t (:height 2.0 :inherit (variable-pitch font-lock-keyword-face)))))
   `(cfw:face-today ((t (:foreground ,cyan :weight bold))))
   `(cfw:face-today-title ((t (:inherit highlight bold))))
   `(cfw:face-toolbar ((t (:background ,blue-5))))
   `(cfw:face-toolbar-button-off ((t (:underline nil :inherit link))))
   `(cfw:face-toolbar-button-on ((t (:underline nil :inherit link-visited))))
;;;;; cider
   `(cider-result-overlay-face ((t (:background unspecified))))
   `(cider-enlightened-face ((t (:box (:color ,orange :line-width -1)))))
   `(cider-enlightened-local-face ((t (:weight bold :foreground ,green+1))))
   `(cider-deprecated-face ((t (:background ,yellow-2))))
   `(cider-instrumented-face ((t (:box (:color ,red :line-width -1)))))
   `(cider-traced-face ((t (:box (:color ,cyan :line-width -1)))))
   `(cider-test-failure-face ((t (:background ,red-4))))
   `(cider-test-error-face ((t (:background ,magenta))))
   `(cider-test-success-face ((t (:background ,green-2))))
   `(cider-fringe-good-face ((t (:foreground ,green+4))))
;;;;; circe
   `(circe-highlight-nick-face ((t (:foreground ,cyan))))
   `(circe-my-message-face ((t (:foreground ,fg))))
   `(circe-fool-face ((t (:foreground ,red+1))))
   `(circe-topic-diff-removed-face ((t (:foreground ,red :weight bold))))
   `(circe-originator-face ((t (:foreground ,fg))))
   `(circe-server-face ((t (:foreground ,green))))
   `(circe-topic-diff-new-face ((t (:foreground ,orange :weight bold))))
   `(circe-prompt-face ((t (:foreground ,orange :background ,bg :weight bold))))
;;;;; context-coloring
   `(context-coloring-level-0-face ((t :foreground ,fg)))
   `(context-coloring-level-1-face ((t :foreground ,cyan)))
   `(context-coloring-level-2-face ((t :foreground ,green+4)))
   `(context-coloring-level-3-face ((t :foreground ,yellow)))
   `(context-coloring-level-4-face ((t :foreground ,orange)))
   `(context-coloring-level-5-face ((t :foreground ,magenta)))
   `(context-coloring-level-6-face ((t :foreground ,blue+1)))
   `(context-coloring-level-7-face ((t :foreground ,green+2)))
   `(context-coloring-level-8-face ((t :foreground ,yellow-2)))
   `(context-coloring-level-9-face ((t :foreground ,red+1)))
;;;;; coq
   `(coq-solve-tactics-face ((t (:foreground nil :inherit font-lock-constant-face))))
;;;;; ctable
   `(ctbl:face-cell-select ((t (:background ,blue :foreground ,bg))))
   `(ctbl:face-continue-bar ((t (:background ,bg-05 :foreground ,bg))))
   `(ctbl:face-row-select ((t (:background ,cyan :foreground ,bg))))
;;;;; debbugs
   `(debbugs-gnu-done ((t (:foreground ,fg-1))))
   `(debbugs-gnu-handled ((t (:foreground ,green))))
   `(debbugs-gnu-new ((t (:foreground ,red))))
   `(debbugs-gnu-pending ((t (:foreground ,blue))))
   `(debbugs-gnu-stale ((t (:foreground ,orange))))
   `(debbugs-gnu-tagged ((t (:foreground ,red))))
;;;;; diff
   `(diff-added          ((t (:background ,green-5 :foreground ,green+2))))
   `(diff-changed        ((t (:background "#555511" :foreground ,yellow-1))))
   `(diff-removed        ((t (:background ,red-6 :foreground ,red+1))))
   `(diff-refine-added   ((t (:background ,green-4 :foreground ,green+3))))
   `(diff-refine-changed ((t (:background "#888811" :foreground ,yellow))))
   `(diff-refine-removed ((t (:background ,red-5 :foreground ,red+2))))
   `(diff-header ((,class (:background ,bg+2))
                  (t (:background ,fg :foreground ,bg))))
   `(diff-file-header
     ((,class (:background ,bg+2 :foreground ,fg :weight bold))
      (t (:background ,fg :foreground ,bg :weight bold))))
;;;;; diff-hl
   `(diff-hl-change ((,class (:foreground ,blue :background ,blue-2))))
   `(diff-hl-delete ((,class (:foreground ,red+1 :background ,red-1))))
   `(diff-hl-insert ((,class (:foreground ,green+1 :background ,green-2))))
;;;;; dim-autoload
   `(dim-autoload-cookie-line ((t :foreground ,bg+1)))
;;;;; dired+
   `(diredp-display-msg ((t (:foreground ,blue))))
   `(diredp-compressed-file-suffix ((t (:foreground ,orange))))
   `(diredp-date-time ((t (:foreground ,magenta))))
   `(diredp-deletion ((t (:foreground ,yellow))))
   `(diredp-deletion-file-name ((t (:foreground ,red))))
   `(diredp-dir-heading ((t (:foreground ,blue :background ,bg-1))))
   `(diredp-dir-priv ((t (:foreground ,cyan))))
   `(diredp-exec-priv ((t (:foreground ,red))))
   `(diredp-executable-tag ((t (:foreground ,green+1))))
   `(diredp-file-name ((t (:foreground ,blue))))
   `(diredp-file-suffix ((t (:foreground ,green))))
   `(diredp-flag-mark ((t (:foreground ,yellow))))
   `(diredp-flag-mark-line ((t (:foreground ,orange))))
   `(diredp-ignored-file-name ((t (:foreground ,red))))
   `(diredp-link-priv ((t (:foreground ,yellow))))
   `(diredp-mode-line-flagged ((t (:foreground ,yellow))))
   `(diredp-mode-line-marked ((t (:foreground ,orange))))
   `(diredp-no-priv ((t (:foreground ,fg))))
   `(diredp-number ((t (:foreground ,green+1))))
   `(diredp-other-priv ((t (:foreground ,yellow-1))))
   `(diredp-rare-priv ((t (:foreground ,red-1))))
   `(diredp-read-priv ((t (:foreground ,green-2))))
   `(diredp-symlink ((t (:foreground ,yellow))))
   `(diredp-write-priv ((t (:foreground ,magenta))))
;;;;; dired-async
   `(dired-async-failures ((t (:foreground ,red :weight bold))))
   `(dired-async-message ((t (:foreground ,yellow :weight bold))))
   `(dired-async-mode-message ((t (:foreground ,yellow))))
;;;;; diredfl
   `(diredfl-compressed-file-suffix ((t (:foreground ,orange))))
   `(diredfl-date-time ((t (:foreground ,magenta))))
   `(diredfl-deletion ((t (:foreground ,yellow))))
   `(diredfl-deletion-file-name ((t (:foreground ,red))))
   `(diredfl-dir-heading ((t (:foreground ,blue :background ,bg-1))))
   `(diredfl-dir-priv ((t (:foreground ,cyan))))
   `(diredfl-exec-priv ((t (:foreground ,red))))
   `(diredfl-executable-tag ((t (:foreground ,green+1))))
   `(diredfl-file-name ((t (:foreground ,blue))))
   `(diredfl-file-suffix ((t (:foreground ,green))))
   `(diredfl-flag-mark ((t (:foreground ,yellow))))
   `(diredfl-flag-mark-line ((t (:foreground ,orange))))
   `(diredfl-ignored-file-name ((t (:foreground ,red))))
   `(diredfl-link-priv ((t (:foreground ,yellow))))
   `(diredfl-no-priv ((t (:foreground ,fg))))
   `(diredfl-number ((t (:foreground ,green+1))))
   `(diredfl-other-priv ((t (:foreground ,yellow-1))))
   `(diredfl-rare-priv ((t (:foreground ,red-1))))
   `(diredfl-read-priv ((t (:foreground ,green-1))))
   `(diredfl-symlink ((t (:foreground ,yellow))))
   `(diredfl-write-priv ((t (:foreground ,magenta))))
;;;;; ediff
   `(ediff-current-diff-A ((t (:inherit diff-removed))))
   `(ediff-current-diff-Ancestor ((t (:inherit ediff-current-diff-A))))
   `(ediff-current-diff-B ((t (:inherit diff-added))))
   `(ediff-current-diff-C ((t (:foreground ,blue+2 :background ,blue-5))))
   `(ediff-even-diff-A ((t (:background ,bg+1))))
   `(ediff-even-diff-Ancestor ((t (:background ,bg+1))))
   `(ediff-even-diff-B ((t (:background ,bg+1))))
   `(ediff-even-diff-C ((t (:background ,bg+1))))
   `(ediff-fine-diff-A ((t (:inherit diff-refine-removed :weight bold))))
   `(ediff-fine-diff-Ancestor ((t (:inherit ediff-fine-diff-A))))
   `(ediff-fine-diff-B ((t (:inherit diff-refine-added :weight bold))))
   `(ediff-fine-diff-C ((t (:foreground ,blue+3 :background ,blue-4 :weight bold))))
   `(ediff-odd-diff-A ((t (:background ,bg+2))))
   `(ediff-odd-diff-Ancestor ((t (:inherit ediff-odd-diff-A))))
   `(ediff-odd-diff-B ((t (:inherit ediff-odd-diff-A))))
   `(ediff-odd-diff-C ((t (:inherit ediff-odd-diff-A))))
;;;;; egg
   `(egg-text-base ((t (:foreground ,fg))))
   `(egg-help-header-1 ((t (:foreground ,yellow))))
   `(egg-help-header-2 ((t (:foreground ,green+3))))
   `(egg-branch ((t (:foreground ,yellow))))
   `(egg-branch-mono ((t (:foreground ,yellow))))
   `(egg-term ((t (:foreground ,yellow))))
   `(egg-diff-add ((t (:foreground ,green+4))))
   `(egg-diff-del ((t (:foreground ,red+1))))
   `(egg-diff-file-header ((t (:foreground ,yellow-2))))
   `(egg-section-title ((t (:foreground ,yellow))))
   `(egg-stash-mono ((t (:foreground ,green+4))))
;;;;; elfeed
   `(elfeed-log-error-level-face ((t (:foreground ,red))))
   `(elfeed-log-info-level-face ((t (:foreground ,blue))))
   `(elfeed-log-warn-level-face ((t (:foreground ,yellow))))
   `(elfeed-search-date-face ((t (:foreground ,yellow-1 :underline t
                                              :weight bold))))
   `(elfeed-search-tag-face ((t (:foreground ,green))))
   `(elfeed-search-feed-face ((t (:foreground ,cyan))))
;;;;; emacs-w3m
   `(w3m-anchor ((t (:foreground ,yellow :underline t
                                 :weight bold))))
   `(w3m-arrived-anchor ((t (:foreground ,yellow-2
                                         :underline t :weight normal))))
   `(w3m-form ((t (:foreground ,red-1 :underline t))))
   `(w3m-header-line-location-title ((t (:foreground ,yellow
                                                     :underline t :weight bold))))
   '(w3m-history-current-url ((t (:inherit match))))
   `(w3m-lnum ((t (:foreground ,green+2 :background ,bg))))
   `(w3m-lnum-match ((t (:background ,bg-1
                                     :foreground ,orange
                                     :weight bold))))
   `(w3m-lnum-minibuffer-prompt ((t (:foreground ,yellow))))
;;;;; erc
   `(erc-action-face ((t (:inherit erc-default-face))))
   `(erc-bold-face ((t (:weight bold))))
   `(erc-current-nick-face ((t (:foreground ,blue :weight bold))))
   `(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
   `(erc-default-face ((t (:foreground ,fg))))
   `(erc-direct-msg-face ((t (:inherit erc-default-face))))
   `(erc-error-face ((t (:inherit font-lock-warning-face))))
   `(erc-fool-face ((t (:inherit erc-default-face))))
   `(erc-highlight-face ((t (:inherit hover-highlight))))
   `(erc-input-face ((t (:foreground ,yellow))))
   `(erc-keyword-face ((t (:foreground ,blue :weight bold))))
   `(erc-nick-default-face ((t (:foreground ,yellow :weight bold))))
   `(erc-my-nick-face ((t (:foreground ,red :weight bold))))
   `(erc-nick-msg-face ((t (:inherit erc-default-face))))
   `(erc-notice-face ((t (:foreground ,green))))
   `(erc-pal-face ((t (:foreground ,orange :weight bold))))
   `(erc-prompt-face ((t (:foreground ,orange :background ,bg :weight bold))))
   `(erc-timestamp-face ((t (:foreground ,green+4))))
   `(erc-underline-face ((t (:underline t))))
;;;;; eros
   `(eros-result-overlay-face ((t (:background unspecified))))
;;;;; ert
   `(ert-test-result-expected ((t (:foreground ,green+4 :background ,bg))))
   `(ert-test-result-unexpected ((t (:foreground ,red :background ,bg))))
;;;;; eshell
   `(eshell-prompt ((t (:foreground ,yellow :weight bold))))
   `(eshell-ls-archive ((t (:foreground ,red-1 :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,blue+1 :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,red+1 :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,yellow :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,cyan :weight bold))))
;;;;; flx
   `(flx-highlight-face ((t (:foreground ,green+2 :weight bold))))
;;;;; flycheck
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,red-1) :inherit unspecified))
      (t (:foreground ,red-1 :weight bold :underline t))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yellow) :inherit unspecified))
      (t (:foreground ,yellow :weight bold :underline t))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,cyan) :inherit unspecified))
      (t (:foreground ,cyan :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,red-1 :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,yellow :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,cyan :weight bold))))
;;;;; flymake
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,red)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,red-1 :weight bold :underline t))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,orange)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,orange :weight bold :underline t))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,green)
                   :inherit unspecified :foreground unspecified :background unspecified))
      (t (:foreground ,green-2 :weight bold :underline t))))
;;;;; flyspell
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,orange) :inherit unspecified))
      (t (:foreground ,orange :weight bold :underline t))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,red) :inherit unspecified))
      (t (:foreground ,red-1 :weight bold :underline t))))
;;;;; full-ack
   `(ack-separator ((t (:foreground ,fg))))
   `(ack-file ((t (:foreground ,blue))))
   `(ack-line ((t (:foreground ,yellow))))
   `(ack-match ((t (:foreground ,orange :background ,bg-1 :weight bold))))
;;;;; git-annex
   '(git-annex-dired-annexed-available ((t (:inherit success :weight normal))))
   '(git-annex-dired-annexed-unavailable ((t (:inherit error :weight normal))))
;;;;; git-commit
   `(git-commit-comment-action  ((,class (:foreground ,green+1 :weight bold))))
   `(git-commit-comment-branch  ((,class (:foreground ,blue+1  :weight bold)))) ; obsolete
   `(git-commit-comment-branch-local  ((,class (:foreground ,blue+1  :weight bold))))
   `(git-commit-comment-branch-remote ((,class (:foreground ,green  :weight bold))))
   `(git-commit-comment-heading ((,class (:foreground ,yellow  :weight bold))))
;;;;; git-gutter
   `(git-gutter:added ((t (:foreground ,green :weight bold ((t (:inverse-video t)))))))
   `(git-gutter:deleted ((t (:foreground ,red :weight bold ((t (:inverse-video t)))))))
   `(git-gutter:modified ((t (:foreground ,magenta :weight bold ((t (:inverse-video t)))))))
   `(git-gutter:unchanged ((t (:foreground ,fg :weight bold ((t (:inverse-video t)))))))
   ;; `(git-gutter:added ((t (:foreground ,green :weight bold :inverse-video t))))
   ;; `(git-gutter:deleted ((t (:foreground ,red :weight bold :inverse-video t))))
   ;; `(git-gutter:modified ((t (:foreground ,magenta :weight bold :inverse-video t))))
   ;; `(git-gutter:unchanged ((t (:foreground ,fg :weight bold :inverse-video t))))
;;;;; git-gutter-fr
   `(git-gutter-fr:added ((t (:foreground ,green  :weight bold))))
   `(git-gutter-fr:deleted ((t (:foreground ,red :weight bold))))
   `(git-gutter-fr:modified ((t (:foreground ,magenta :weight bold))))
;;;;; git-rebase
   `(git-rebase-hash ((t (:foreground, orange))))
;;;;; gnus
   `(gnus-group-mail-1 ((t (:weight bold :inherit gnus-group-mail-1-empty))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-news-1-empty))))
   `(gnus-group-mail-2 ((t (:weight bold :inherit gnus-group-mail-2-empty))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-news-2-empty))))
   `(gnus-group-mail-3 ((t (:weight bold :inherit gnus-group-mail-3-empty))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-news-3-empty))))
   `(gnus-group-mail-4 ((t (:weight bold :inherit gnus-group-mail-4-empty))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-news-4-empty))))
   `(gnus-group-mail-5 ((t (:weight bold :inherit gnus-group-mail-5-empty))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-news-5-empty))))
   `(gnus-group-mail-6 ((t (:weight bold :inherit gnus-group-mail-6-empty))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-news-6-empty))))
   `(gnus-group-mail-low ((t (:weight bold :inherit gnus-group-mail-low-empty))))
   `(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))
   `(gnus-group-news-1 ((t (:weight bold :inherit gnus-group-news-1-empty))))
   `(gnus-group-news-2 ((t (:weight bold :inherit gnus-group-news-2-empty))))
   `(gnus-group-news-3 ((t (:weight bold :inherit gnus-group-news-3-empty))))
   `(gnus-group-news-4 ((t (:weight bold :inherit gnus-group-news-4-empty))))
   `(gnus-group-news-5 ((t (:weight bold :inherit gnus-group-news-5-empty))))
   `(gnus-group-news-6 ((t (:weight bold :inherit gnus-group-news-6-empty))))
   `(gnus-group-news-low ((t (:weight bold :inherit gnus-group-news-low-empty))))
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-from ((t (:inherit message-header-to))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-header-newsgroups ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-server-opened ((t (:foreground ,green+2 :weight bold))))
   `(gnus-server-denied ((t (:foreground ,red+1 :weight bold))))
   `(gnus-server-closed ((t (:foreground ,blue :slant italic))))
   `(gnus-server-offline ((t (:foreground ,yellow :weight bold))))
   `(gnus-server-agent ((t (:foreground ,blue :weight bold))))
   `(gnus-summary-cancelled ((t (:foreground ,orange))))
   `(gnus-summary-high-ancient ((t (:foreground ,blue))))
   `(gnus-summary-high-read ((t (:foreground ,green :weight bold))))
   `(gnus-summary-high-ticked ((t (:foreground ,orange :weight bold))))
   `(gnus-summary-high-unread ((t (:foreground ,fg :weight bold))))
   `(gnus-summary-low-ancient ((t (:foreground ,blue))))
   `(gnus-summary-low-read ((t (:foreground ,green))))
   `(gnus-summary-low-ticked ((t (:foreground ,orange :weight bold))))
   `(gnus-summary-low-unread ((t (:foreground ,fg))))
   `(gnus-summary-normal-ancient ((t (:foreground ,blue))))
   `(gnus-summary-normal-read ((t (:foreground ,green))))
   `(gnus-summary-normal-ticked ((t (:foreground ,orange :weight bold))))
   `(gnus-summary-normal-unread ((t (:foreground ,fg))))
   `(gnus-summary-selected ((t (:foreground ,yellow :weight bold))))
   `(gnus-cite-1 ((t (:foreground ,blue))))
   `(gnus-cite-10 ((t (:foreground ,yellow-1))))
   `(gnus-cite-11 ((t (:foreground ,yellow))))
   `(gnus-cite-2 ((t (:foreground ,blue-1))))
   `(gnus-cite-3 ((t (:foreground ,blue-2))))
   `(gnus-cite-4 ((t (:foreground ,green+2))))
   `(gnus-cite-5 ((t (:foreground ,green+1))))
   `(gnus-cite-6 ((t (:foreground ,green))))
   `(gnus-cite-7 ((t (:foreground ,red))))
   `(gnus-cite-8 ((t (:foreground ,red-1))))
   `(gnus-cite-9 ((t (:foreground ,red-2))))
   `(gnus-group-news-1-empty ((t (:foreground ,yellow))))
   `(gnus-group-news-2-empty ((t (:foreground ,green+3))))
   `(gnus-group-news-3-empty ((t (:foreground ,green+1))))
   `(gnus-group-news-4-empty ((t (:foreground ,blue-2))))
   `(gnus-group-news-5-empty ((t (:foreground ,blue-3))))
   `(gnus-group-news-6-empty ((t (:foreground ,bg+2))))
   `(gnus-group-news-low-empty ((t (:foreground ,bg+2))))
   `(gnus-signature ((t (:foreground ,yellow))))
   `(gnus-x ((t (:background ,fg :foreground ,bg))))
   `(mm-uu-extract ((t (:background ,bg-05 :foreground ,green+1))))
;;;;; go-guru
   `(go-guru-hl-identifier-face ((t (:foreground ,bg-1 :background ,green+1))))
;;;;; guide-key
   `(guide-key/highlight-command-face ((t (:foreground ,blue))))
   `(guide-key/key-face ((t (:foreground ,green))))
   `(guide-key/prefix-command-face ((t (:foreground ,green+1))))
;;;;; hackernews
   '(hackernews-comment-count ((t (:inherit link-visited :underline nil))))
   '(hackernews-link          ((t (:inherit link         :underline nil))))
;;;;; helm
   `(helm-header
     ((t (:foreground ,green
                      :background ,bg
                      :underline nil
                      :box nil))))
   `(helm-source-header
     ((t (:foreground ,yellow
                      :background ,bg-1
                      :underline nil
                      :weight bold
                      :box (:line-width -1 :style released-button)))))
   `(helm-selection ((t (:background ,bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,bg+1))))
   `(helm-visible-mark ((t (:foreground ,bg :background ,yellow-2))))
   `(helm-candidate-number ((t (:foreground ,green+4 :background ,bg-1))))
   `(helm-separator ((t (:foreground ,red :background ,bg))))
   `(helm-time-zone-current ((t (:foreground ,green+2 :background ,bg))))
   `(helm-time-zone-home ((t (:foreground ,red :background ,bg))))
   `(helm-bookmark-addressbook ((t (:foreground ,orange :background ,bg))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,magenta :background ,bg))))
   `(helm-bookmark-info ((t (:foreground ,green+2 :background ,bg))))
   `(helm-bookmark-man ((t (:foreground ,yellow :background ,bg))))
   `(helm-bookmark-w3m ((t (:foreground ,magenta :background ,bg))))
   `(helm-buffer-not-saved ((t (:foreground ,red :background ,bg))))
   `(helm-buffer-process ((t (:foreground ,cyan :background ,bg))))
   `(helm-buffer-saved-out ((t (:foreground ,fg :background ,bg))))
   `(helm-buffer-size ((t (:foreground ,fg-1 :background ,bg))))
   `(helm-ff-directory ((t (:foreground ,cyan :background ,bg :weight bold))))
   `(helm-ff-file ((t (:foreground ,fg :background ,bg :weight normal))))
   `(helm-ff-executable ((t (:foreground ,green+2 :background ,bg :weight normal))))
   `(helm-ff-invalid-symlink ((t (:foreground ,red :background ,bg :weight bold))))
   `(helm-ff-symlink ((t (:foreground ,yellow :background ,bg :weight bold))))
   `(helm-ff-prefix ((t (:foreground ,bg :background ,yellow :weight normal))))
   `(helm-grep-cmd-line ((t (:foreground ,cyan :background ,bg))))
   `(helm-grep-file ((t (:foreground ,fg :background ,bg))))
   `(helm-grep-finish ((t (:foreground ,green+2 :background ,bg))))
   `(helm-grep-lineno ((t (:foreground ,fg-1 :background ,bg))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,red :background ,bg))))
   `(helm-match ((t (:foreground ,orange :background ,bg-1 :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,cyan :background ,bg))))
   `(helm-mu-contacts-address-face ((t (:foreground ,fg-1 :background ,bg))))
   `(helm-mu-contacts-name-face ((t (:foreground ,fg :background ,bg))))
;;;;; helm-swoop
   `(helm-swoop-target-line-face ((t (:foreground ,fg :background ,bg+1))))
   `(helm-swoop-target-word-face ((t (:foreground ,yellow :background ,bg+2 :weight bold))))
;;;;; hl-line-mode
   `(hl-line-face ((,class (:background ,bg-05))
                   (t :weight bold)))
   `(hl-line ((,class (:background ,bg-05)) ; old emacsen
              (t :weight bold)))
;;;;; hl-sexp
   `(hl-sexp-face ((,class (:background ,bg+1))
                   (t :weight bold)))
;;;;; hydra
   `(hydra-face-red ((t (:foreground ,red-1 :background ,bg))))
   `(hydra-face-amaranth ((t (:foreground ,red-3 :background ,bg))))
   `(hydra-face-blue ((t (:foreground ,blue :background ,bg))))
   `(hydra-face-pink ((t (:foreground ,magenta :background ,bg))))
   `(hydra-face-teal ((t (:foreground ,cyan :background ,bg))))
;;;;; info+
   `(info-command-ref-item ((t (:background ,bg-1 :foreground ,orange))))
   `(info-constant-ref-item ((t (:background ,bg-1 :foreground ,magenta))))
   `(info-double-quoted-name ((t (:inherit font-lock-comment-face))))
   `(info-file ((t (:background ,bg-1 :foreground ,yellow))))
   `(info-function-ref-item ((t (:background ,bg-1 :inherit font-lock-function-name-face))))
   `(info-macro-ref-item ((t (:background ,bg-1 :foreground ,yellow))))
   `(info-menu ((t (:foreground ,yellow))))
   `(info-quoted-name ((t (:inherit font-lock-constant-face))))
   `(info-reference-item ((t (:background ,bg-1))))
   `(info-single-quote ((t (:inherit font-lock-keyword-face))))
   `(info-special-form-ref-item ((t (:background ,bg-1 :foreground ,yellow))))
   `(info-string ((t (:inherit font-lock-string-face))))
   `(info-syntax-class-item ((t (:background ,bg-1 :foreground ,blue+1))))
   `(info-user-option-ref-item ((t (:background ,bg-1 :foreground ,red))))
   `(info-variable-ref-item ((t (:background ,bg-1 :foreground ,orange))))
;;;;; irfc
   `(irfc-head-name-face ((t (:foreground ,red :weight bold))))
   `(irfc-head-number-face ((t (:foreground ,red :weight bold))))
   `(irfc-reference-face ((t (:foreground ,blue-1 :weight bold))))
   `(irfc-requirement-keyword-face ((t (:inherit font-lock-keyword-face))))
   `(irfc-rfc-link-face ((t (:inherit link))))
   `(irfc-rfc-number-face ((t (:foreground ,cyan :weight bold))))
   `(irfc-std-number-face ((t (:foreground ,green+4 :weight bold))))
   `(irfc-table-item-face ((t (:foreground ,green+3))))
   `(irfc-title-face ((t (:foreground ,yellow
                                      :underline t :weight bold))))
;;;;; ivy
   `(ivy-confirm-face ((t (:foreground ,green :background ,bg))))
   `(ivy-current-match ((t (:foreground ,yellow :weight bold :underline t))))
   `(ivy-cursor ((t (:foreground ,bg :background ,fg))))
   `(ivy-match-required-face ((t (:foreground ,red :background ,bg))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,bg+1))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,green-2))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,green))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,green+1))))
   `(ivy-remote ((t (:foreground ,blue :background ,bg))))
   `(ivy-subdir ((t (:foreground ,yellow :background ,bg))))
;;;;; ido-mode
   `(ido-first-match ((t (:foreground ,yellow :weight bold))))
   `(ido-only-match ((t (:foreground ,orange :weight bold))))
   `(ido-subdir ((t (:foreground ,yellow))))
   `(ido-indicator ((t (:foreground ,yellow :background ,red-4))))
;;;;; iedit-mode
   `(iedit-occurrence ((t (:background ,bg+2 :weight bold))))
;;;;; jabber-mode
   `(jabber-roster-user-away ((t (:foreground ,green+2))))
   `(jabber-roster-user-online ((t (:foreground ,blue-1))))
   `(jabber-roster-user-dnd ((t (:foreground ,red+1))))
   `(jabber-roster-user-xa ((t (:foreground ,magenta))))
   `(jabber-roster-user-chatty ((t (:foreground ,orange))))
   `(jabber-roster-user-error ((t (:foreground ,red+1))))
   `(jabber-rare-time-face ((t (:foreground ,green+1))))
   `(jabber-chat-prompt-local ((t (:foreground ,blue-1))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,red+1))))
   `(jabber-chat-prompt-system ((t (:foreground ,green+3))))
   `(jabber-activity-face((t (:foreground ,red+1))))
   `(jabber-activity-personal-face ((t (:foreground ,blue+1))))
   `(jabber-title-small ((t (:height 1.1 :weight bold))))
   `(jabber-title-medium ((t (:height 1.2 :weight bold))))
   `(jabber-title-large ((t (:height 1.3 :weight bold))))
;;;;; js2-mode
   `(js2-warning ((t (:underline ,orange))))
   `(js2-error ((t (:foreground ,red :weight bold))))
   `(js2-jsdoc-tag ((t (:foreground ,green-2))))
   `(js2-jsdoc-type ((t (:foreground ,green+2))))
   `(js2-jsdoc-value ((t (:foreground ,green+3))))
   `(js2-function-param ((t (:foreground, orange))))
   `(js2-external-variable ((t (:foreground ,orange))))
;;;;; additional js2 mode attributes for better syntax highlighting
   `(js2-instance-member ((t (:foreground ,green-2))))
   `(js2-jsdoc-html-tag-delimiter ((t (:foreground ,orange))))
   `(js2-jsdoc-html-tag-name ((t (:foreground ,red-1))))
   `(js2-object-property ((t (:foreground ,blue+1))))
   `(js2-magic-paren ((t (:foreground ,blue-5))))
   `(js2-private-function-call ((t (:foreground ,cyan))))
   `(js2-function-call ((t (:foreground ,cyan))))
   `(js2-private-member ((t (:foreground ,blue-1))))
   `(js2-keywords ((t (:foreground ,magenta))))
;;;;; ledger-mode
   `(ledger-font-payee-uncleared-face ((t (:foreground ,red-1 :weight bold))))
   `(ledger-font-payee-cleared-face ((t (:foreground ,fg :weight normal))))
   `(ledger-font-payee-pending-face ((t (:foreground ,red :weight normal))))
   `(ledger-font-xact-highlight-face ((t (:background ,bg+1))))
   `(ledger-font-auto-xact-face ((t (:foreground ,yellow-1 :weight normal))))
   `(ledger-font-periodic-xact-face ((t (:foreground ,green :weight normal))))
   `(ledger-font-pending-face ((t (:foreground ,orange weight: normal))))
   `(ledger-font-other-face ((t (:foreground ,fg))))
   `(ledger-font-posting-date-face ((t (:foreground ,orange :weight normal))))
   `(ledger-font-posting-account-face ((t (:foreground ,blue-1))))
   `(ledger-font-posting-account-cleared-face ((t (:foreground ,fg))))
   `(ledger-font-posting-account-pending-face ((t (:foreground ,orange))))
   `(ledger-font-posting-amount-face ((t (:foreground ,orange))))
   `(ledger-occur-narrowed-face ((t (:foreground ,fg-1 :invisible t))))
   `(ledger-occur-xact-face ((t (:background ,bg+1))))
   `(ledger-font-comment-face ((t (:foreground ,green))))
   `(ledger-font-reconciler-uncleared-face ((t (:foreground ,red-1 :weight bold))))
   `(ledger-font-reconciler-cleared-face ((t (:foreground ,fg :weight normal))))
   `(ledger-font-reconciler-pending-face ((t (:foreground ,orange :weight normal))))
   `(ledger-font-report-clickable-face ((t (:foreground ,orange :weight normal))))
;;;;; linum-mode
   `(linum ((t (:foreground ,green+2 :background ,bg))))
;;;;; lispy
   `(lispy-command-name-face ((t (:background ,bg-05 :inherit font-lock-function-name-face))))
   `(lispy-cursor-face ((t (:foreground ,bg :background ,fg))))
   `(lispy-face-hint ((t (:inherit highlight :foreground ,yellow))))
;;;;; ruler-mode
   `(ruler-mode-column-number ((t (:inherit 'ruler-mode-default :foreground ,fg))))
   `(ruler-mode-fill-column ((t (:inherit 'ruler-mode-default :foreground ,yellow))))
   `(ruler-mode-goal-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-comment-column ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-tab-stop ((t (:inherit 'ruler-mode-fill-column))))
   `(ruler-mode-current-column ((t (:foreground ,yellow :box t))))
   `(ruler-mode-default ((t (:foreground ,green+2 :background ,bg))))

;;;;; lui
   `(lui-time-stamp-face ((t (:foreground ,blue-1))))
   `(lui-hilight-face ((t (:foreground ,green+2 :background ,bg))))
   `(lui-button-face ((t (:inherit hover-highlight))))
;;;;; macrostep
   `(macrostep-gensym-1
     ((t (:foreground ,green+2 :background ,bg-1))))
   `(macrostep-gensym-2
     ((t (:foreground ,red+1 :background ,bg-1))))
   `(macrostep-gensym-3
     ((t (:foreground ,blue+1 :background ,bg-1))))
   `(macrostep-gensym-4
     ((t (:foreground ,magenta :background ,bg-1))))
   `(macrostep-gensym-5
     ((t (:foreground ,yellow :background ,bg-1))))
   `(macrostep-expansion-highlight-face
     ((t (:inherit highlight))))
   `(macrostep-macro-face
     ((t (:underline t))))
;;;;; magit
;;;;;; headings and diffs
   `(magit-section-highlight           ((t (:background ,bg+05))))
   `(magit-section-heading             ((t (:foreground ,yellow :weight bold))))
   `(magit-section-heading-selection   ((t (:foreground ,orange :weight bold))))

   `(magit-diff-added ((t (:inherit diff-added))))
   `(magit-diff-added-highlight ((t (:inherit diff-refine-added))))
   `(magit-diff-removed ((t (:inherit diff-removed))))
   `(magit-diff-removed-highlight ((t (:inherit diff-refine-removed))))

   `(magit-diff-file-heading           ((t (:weight bold))))
   `(magit-diff-file-heading-highlight ((t (:background ,bg+05  :weight bold))))
   `(magit-diff-file-heading-selection ((t (:background ,bg+05
                                                        :foreground ,orange :weight bold))))
   `(magit-diff-hunk-heading           ((t (:background ,bg+1))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,bg+2))))
   `(magit-diff-hunk-heading-selection ((t (:background ,bg+2
                                                        :foreground ,orange))))
   `(magit-diff-lines-heading          ((t (:background ,orange
                                                        :foreground ,bg+2))))
   `(magit-diff-context-highlight      ((t (:background ,bg+05
                                                        :foreground "grey70"))))
   `(magit-diffstat-added   ((t (:foreground ,green+4))))
   `(magit-diffstat-removed ((t (:foreground ,red))))
;;;;;; popup
   `(magit-popup-heading             ((t (:foreground ,yellow  :weight bold))))
   `(magit-popup-key                 ((t (:foreground ,green-2 :weight bold))))
   `(magit-popup-argument            ((t (:foreground ,green   :weight bold))))
   `(magit-popup-disabled-argument   ((t (:foreground ,fg-1    :weight normal))))
   `(magit-popup-option-value        ((t (:foreground ,blue-2  :weight bold))))
;;;;;; process
   `(magit-process-ok    ((t (:foreground ,green  :weight bold))))
   `(magit-process-ng    ((t (:foreground ,red    :weight bold))))
;;;;;; log
   `(magit-log-author    ((t (:foreground ,orange))))
   `(magit-log-date      ((t (:foreground ,fg-1))))
   `(magit-log-graph     ((t (:foreground ,fg+1))))
;;;;;; sequence
   `(magit-sequence-pick ((t (:foreground ,yellow-2))))
   `(magit-sequence-stop ((t (:foreground ,green))))
   `(magit-sequence-part ((t (:foreground ,yellow))))
   `(magit-sequence-head ((t (:foreground ,blue))))
   `(magit-sequence-drop ((t (:foreground ,red))))
   `(magit-sequence-done ((t (:foreground ,fg-1))))
   `(magit-sequence-onto ((t (:foreground ,fg-1))))
;;;;;; bisect
   `(magit-bisect-good ((t (:foreground ,green))))
   `(magit-bisect-skip ((t (:foreground ,yellow))))
   `(magit-bisect-bad  ((t (:foreground ,red))))
;;;;;; blame
   `(magit-blame-heading ((t (:background ,bg-1 :foreground ,blue-2))))
   `(magit-blame-hash    ((t (:background ,bg-1 :foreground ,blue-2))))
   `(magit-blame-name    ((t (:background ,bg-1 :foreground ,orange))))
   `(magit-blame-date    ((t (:background ,bg-1 :foreground ,orange))))
   `(magit-blame-summary ((t (:background ,bg-1 :foreground ,blue-2
                                          :weight bold))))
;;;;;; references etc
   `(magit-dimmed         ((t (:foreground ,bg+3))))
   `(magit-hash           ((t (:foreground ,bg+3))))
   `(magit-tag            ((t (:foreground ,orange :weight bold))))
   `(magit-branch-remote  ((t (:foreground ,green  :weight bold))))
   `(magit-branch-local   ((t (:foreground ,blue   :weight bold))))
   `(magit-branch-current ((t (:foreground ,blue   :weight bold :box t))))
   `(magit-head           ((t (:foreground ,blue   :weight bold))))
   `(magit-refname        ((t (:background ,bg+2 :foreground ,fg :weight bold))))
   `(magit-refname-stash  ((t (:background ,bg+2 :foreground ,fg :weight bold))))
   `(magit-refname-wip    ((t (:background ,bg+2 :foreground ,fg :weight bold))))
   `(magit-signature-good      ((t (:foreground ,green))))
   `(magit-signature-bad       ((t (:foreground ,red))))
   `(magit-signature-untrusted ((t (:foreground ,yellow))))
   `(magit-signature-expired   ((t (:foreground ,orange))))
   `(magit-signature-revoked   ((t (:foreground ,magenta))))
   '(magit-signature-error     ((t (:inherit    magit-signature-bad))))
   `(magit-cherry-unmatched    ((t (:foreground ,cyan))))
   `(magit-cherry-equivalent   ((t (:foreground ,magenta))))
   `(magit-reflog-commit       ((t (:foreground ,green))))
   `(magit-reflog-amend        ((t (:foreground ,magenta))))
   `(magit-reflog-merge        ((t (:foreground ,green))))
   `(magit-reflog-checkout     ((t (:foreground ,blue))))
   `(magit-reflog-reset        ((t (:foreground ,red))))
   `(magit-reflog-rebase       ((t (:foreground ,magenta))))
   `(magit-reflog-cherry-pick  ((t (:foreground ,green))))
   `(magit-reflog-remote       ((t (:foreground ,cyan))))
   `(magit-reflog-other        ((t (:foreground ,cyan))))
;;;;; markup-faces
   `(markup-anchor-face ((t (:foreground ,blue+1))))
   `(markup-code-face ((t (:inherit font-lock-constant-face))))
   `(markup-command-face ((t (:foreground ,yellow))))
   `(markup-emphasis-face ((t (:inherit bold))))
   `(markup-internal-reference-face ((t (:foreground ,yellow-2 :underline t))))
   `(markup-list-face ((t (:foreground ,fg+1))))
   `(markup-meta-face ((t (:foreground ,yellow))))
   `(markup-meta-hide-face ((t (:foreground ,yellow))))
   `(markup-secondary-text-face ((t (:foreground ,yellow-1))))
   `(markup-title-0-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-1-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-2-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-3-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-title-4-face ((t (:inherit font-lock-function-name-face :weight bold))))
   `(markup-typewriter-face ((t (:inherit font-lock-constant-face))))
   `(markup-verbatim-face ((t (:inherit font-lock-constant-face))))
   `(markup-value-face ((t (:foreground ,yellow))))
;;;;; message-mode
   `(message-cited-text ((t (:inherit font-lock-comment-face))))
   `(message-header-name ((t (:foreground ,green+1))))
   `(message-header-other ((t (:foreground ,green))))
   `(message-header-to ((t (:foreground ,yellow :weight bold))))
   `(message-header-cc ((t (:foreground ,yellow :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,yellow :weight bold))))
   `(message-header-subject ((t (:foreground ,orange :weight bold))))
   `(message-header-xheader ((t (:foreground ,green))))
   `(message-mml ((t (:foreground ,yellow :weight bold))))
   `(message-separator ((t (:inherit font-lock-comment-face))))
;;;;; mew
   `(mew-face-header-subject ((t (:foreground ,orange))))
   `(mew-face-header-from ((t (:foreground ,yellow))))
   `(mew-face-header-date ((t (:foreground ,green))))
   `(mew-face-header-to ((t (:foreground ,red))))
   `(mew-face-header-key ((t (:foreground ,green))))
   `(mew-face-header-private ((t (:foreground ,green))))
   `(mew-face-header-important ((t (:foreground ,blue))))
   `(mew-face-header-marginal ((t (:foreground ,fg :weight bold))))
   `(mew-face-header-warning ((t (:foreground ,red))))
   `(mew-face-header-xmew ((t (:foreground ,green))))
   `(mew-face-header-xmew-bad ((t (:foreground ,red))))
   `(mew-face-body-url ((t (:foreground ,orange))))
   `(mew-face-body-comment ((t (:foreground ,fg :slant italic))))
   `(mew-face-body-cite1 ((t (:foreground ,green))))
   `(mew-face-body-cite2 ((t (:foreground ,blue))))
   `(mew-face-body-cite3 ((t (:foreground ,orange))))
   `(mew-face-body-cite4 ((t (:foreground ,yellow))))
   `(mew-face-body-cite5 ((t (:foreground ,red))))
   `(mew-face-mark-review ((t (:foreground ,blue))))
   `(mew-face-mark-escape ((t (:foreground ,green))))
   `(mew-face-mark-delete ((t (:foreground ,red))))
   `(mew-face-mark-unlink ((t (:foreground ,yellow))))
   `(mew-face-mark-refile ((t (:foreground ,green))))
   `(mew-face-mark-unread ((t (:foreground ,red-2))))
   `(mew-face-eof-message ((t (:foreground ,green))))
   `(mew-face-eof-part ((t (:foreground ,yellow))))
;;;;; mic-paren
   `(paren-face-match ((t (:foreground ,cyan :background ,bg :weight bold))))
   `(paren-face-mismatch ((t (:foreground ,bg :background ,magenta :weight bold))))
   `(paren-face-no-match ((t (:foreground ,bg :background ,red :weight bold))))
;;;;; mingus
   `(mingus-directory-face ((t (:foreground ,blue))))
   `(mingus-pausing-face ((t (:foreground ,magenta))))
   `(mingus-playing-face ((t (:foreground ,cyan))))
   `(mingus-playlist-face ((t (:foreground ,cyan ))))
   `(mingus-mark-face ((t (:bold t :foreground ,magenta))))
   `(mingus-song-file-face ((t (:foreground ,yellow))))
   `(mingus-artist-face ((t (:foreground ,cyan))))
   `(mingus-album-face ((t (:underline t :foreground ,red+1))))
   `(mingus-album-stale-face ((t (:foreground ,red+1))))
   `(mingus-stopped-face ((t (:foreground ,red))))
;;;;; nav
   `(nav-face-heading ((t (:foreground ,yellow))))
   `(nav-face-button-num ((t (:foreground ,cyan))))
   `(nav-face-dir ((t (:foreground ,green))))
   `(nav-face-hdir ((t (:foreground ,red))))
   `(nav-face-file ((t (:foreground ,fg))))
   `(nav-face-hfile ((t (:foreground ,red-4))))
;;;;; merlin
   `(merlin-type-face ((t (:inherit highlight))))
   `(merlin-compilation-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,orange)))
      (t
       (:underline ,orange))))
   `(merlin-compilation-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,red)))
      (t
       (:underline ,red))))
;;;;; mu4e
   `(mu4e-cited-1-face ((t (:foreground ,blue    :slant italic))))
   `(mu4e-cited-2-face ((t (:foreground ,green+2 :slant italic))))
   `(mu4e-cited-3-face ((t (:foreground ,blue-2  :slant italic))))
   `(mu4e-cited-4-face ((t (:foreground ,green   :slant italic))))
   `(mu4e-cited-5-face ((t (:foreground ,blue-4  :slant italic))))
   `(mu4e-cited-6-face ((t (:foreground ,green-2 :slant italic))))
   `(mu4e-cited-7-face ((t (:foreground ,blue    :slant italic))))
   `(mu4e-replied-face ((t (:foreground ,bg+3))))
   `(mu4e-trashed-face ((t (:foreground ,bg+3 :strike-through t))))
;;;;; mumamo
   `(mumamo-background-chunk-major ((t (:background nil))))
   `(mumamo-background-chunk-submode1 ((t (:background ,bg-1))))
   `(mumamo-background-chunk-submode2 ((t (:background ,bg+2))))
   `(mumamo-background-chunk-submode3 ((t (:background ,bg+3))))
   `(mumamo-background-chunk-submode4 ((t (:background ,bg+1))))
;;;;; neotree
   `(neo-banner-face ((t (:foreground ,blue+1 :weight bold))))
   `(neo-header-face ((t (:foreground ,fg))))
   `(neo-root-dir-face ((t (:foreground ,blue+1 :weight bold))))
   `(neo-dir-link-face ((t (:foreground ,blue))))
   `(neo-file-link-face ((t (:foreground ,fg))))
   `(neo-expand-btn-face ((t (:foreground ,blue))))
   `(neo-vc-default-face ((t (:foreground ,fg+1))))
   `(neo-vc-user-face ((t (:foreground ,red :slant italic))))
   `(neo-vc-up-to-date-face ((t (:foreground ,fg))))
   `(neo-vc-edited-face ((t (:foreground ,magenta))))
   `(neo-vc-needs-merge-face ((t (:foreground ,red+1))))
   `(neo-vc-unlocked-changes-face ((t (:foreground ,red :background ,blue-5))))
   `(neo-vc-added-face ((t (:foreground ,green+1))))
   `(neo-vc-conflict-face ((t (:foreground ,red+1))))
   `(neo-vc-missing-face ((t (:foreground ,red+1))))
   `(neo-vc-ignored-face ((t (:foreground ,fg-1))))
;;;;; org-mode
   `(org-agenda-date-today
     ((t (:foreground ,fg+1 :slant italic :weight bold))) t)
   `(org-agenda-structure
     ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,fg :weight bold))))
   `(org-checkbox ((t (:background ,bg+2 :foreground ,fg+1
                                   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,blue :underline t))))
   `(org-deadline-announce ((t (:foreground ,red-1))))
   `(org-done ((t (:weight bold :weight bold :foreground ,green+3))))
   `(org-formula ((t (:foreground ,yellow-2))))
   `(org-headline-done ((t (:foreground ,green+3))))
   `(org-hide ((t (:foreground ,bg))))
   `(org-level-1 ((t (:inherit ,z-variable-pitch :foreground ,orange
                               ,@(when scale-org-headlines
                                   (list :height height-plus-4))))))
   `(org-level-2 ((t (:inherit ,z-variable-pitch :foreground ,green+4
                               ,@(when scale-org-headlines
                                   (list :height height-plus-3))))))
   `(org-level-3 ((t (:inherit ,z-variable-pitch :foreground ,blue-1
                               ,@(when scale-org-headlines
                                   (list :height height-plus-2))))))
   `(org-level-4 ((t (:inherit ,z-variable-pitch :foreground ,yellow-2
                               ,@(when scale-org-headlines
                                   (list :height height-plus-1))))))
   `(org-level-5 ((t (:inherit ,z-variable-pitch :foreground ,cyan))))
   `(org-level-6 ((t (:inherit ,z-variable-pitch :foreground ,green+2))))
   `(org-level-7 ((t (:inherit ,z-variable-pitch :foreground ,red-4))))
   `(org-level-8 ((t (:inherit ,z-variable-pitch :foreground ,blue-4))))
   `(org-link ((t (:foreground ,yellow-2 :underline t))))
   `(org-scheduled ((t (:foreground ,green+4))))
   `(org-scheduled-previously ((t (:foreground ,red))))
   `(org-scheduled-today ((t (:foreground ,blue+1))))
   `(org-sexp-date ((t (:foreground ,blue+1 :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,green+2))))
   `(org-tag ((t (:weight bold :weight bold))))
   `(org-time-grid ((t (:foreground ,orange))))
   `(org-todo ((t (:weight bold :foreground ,red :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:weight bold :foreground ,red :weight bold :underline nil))))
   `(org-column ((t (:background ,bg-1))))
   `(org-column-title ((t (:background ,bg-1 :underline t :weight bold))))
   `(org-mode-line-clock ((t (:foreground ,fg :background ,bg-1))))
   `(org-mode-line-clock-overrun ((t (:foreground ,bg :background ,red-1))))
   `(org-ellipsis ((t (:foreground ,yellow-1 :underline t))))
   `(org-footnote ((t (:foreground ,cyan :underline t))))
   `(org-document-title ((t (:inherit ,z-variable-pitch :foreground ,blue
                                         :weight bold :height ,height-plus-4))))
   `(org-document-info ((t (:foreground ,blue))))
   `(org-habit-ready-face ((t :background ,green)))
   `(org-habit-alert-face ((t :background ,yellow-1 :foreground ,bg)))
   `(org-habit-clear-face ((t :background ,blue-3)))
   `(org-habit-overdue-face ((t :background ,red-3)))
   `(org-habit-clear-future-face ((t :background ,blue-4)))
   `(org-habit-ready-future-face ((t :background ,green-2)))
   `(org-habit-alert-future-face ((t :background ,yellow-2 :foreground ,bg)))
   `(org-habit-overdue-future-face ((t :background ,red-4)))
;;;;; org-ref
   `(org-ref-ref-face ((t :underline t)))
   `(org-ref-label-face ((t :underline t)))
   `(org-ref-cite-face ((t :underline t)))
   `(org-ref-glossary-face ((t :underline t)))
   `(org-ref-acronym-face ((t :underline t)))
;;;;; outline
   `(outline-1 ((t (:inherit ,z-variable-pitch :foreground ,orange
                             ,@(when scale-outline-headlines
                                 (list :height height-plus-4))))))
   `(outline-2 ((t (:inherit ,z-variable-pitch :foreground ,green+4
                             ,@(when scale-outline-headlines
                                 (list :height height-plus-3))))))
   `(outline-3 ((t (:inherit ,z-variable-pitch :foreground ,blue-1
                             ,@(when scale-outline-headlines
                                 (list :height height-plus-2))))))
   `(outline-4 ((t (:inherit ,z-variable-pitch :foreground ,yellow-2
                             ,@(when scale-outline-headlines
                                 (list :height height-plus-1))))))
   `(outline-5 ((t (:inherit ,z-variable-pitch :foreground ,cyan))))
   `(outline-6 ((t (:inherit ,z-variable-pitch :foreground ,green+2))))
   `(outline-7 ((t (:inherit ,z-variable-pitch :foreground ,red-4))))
   `(outline-8 ((t (:inherit ,z-variable-pitch :foreground ,blue-4))))
;;;;; p4
   `(p4-depot-added-face ((t :inherit diff-added)))
   `(p4-depot-branch-op-face ((t :inherit diff-changed)))
   `(p4-depot-deleted-face ((t :inherit diff-removed)))
   `(p4-depot-unmapped-face ((t :inherit diff-changed)))
   `(p4-diff-change-face ((t :inherit diff-changed)))
   `(p4-diff-del-face ((t :inherit diff-removed)))
   `(p4-diff-file-face ((t :inherit diff-file-header)))
   `(p4-diff-head-face ((t :inherit diff-header)))
   `(p4-diff-ins-face ((t :inherit diff-added)))
;;;;; c/perl
   `(cperl-nonoverridable-face ((t (:foreground ,magenta))))
   `(cperl-array-face ((t (:foreground ,yellow, :backgorund ,bg))))
   `(cperl-hash-face ((t (:foreground ,yellow-1, :background ,bg))))
;;;;; paren-face
   `(parenthesis ((t (:foreground ,fg-1))))
;;;;; perspective
   `(persp-selected-face ((t (:foreground ,yellow-2 :inherit mode-line))))
;;;;; powerline
   `(powerline-active1 ((t (:background ,bg-05 :inherit mode-line))))
   `(powerline-active2 ((t (:background ,bg+2 :inherit mode-line))))
   `(powerline-inactive1 ((t (:background ,bg+1 :inherit mode-line-inactive))))
   `(powerline-inactive2 ((t (:background ,bg+3 :inherit mode-line-inactive))))
;;;;; proofgeneral
   `(proof-active-area-face ((t (:underline t))))
   `(proof-boring-face ((t (:foreground ,fg :background ,bg+2))))
   `(proof-command-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-debug-message-face ((t (:inherit proof-boring-face))))
   `(proof-declaration-name-face ((t (:inherit font-lock-keyword-face :foreground nil))))
   `(proof-eager-annotation-face ((t (:foreground ,bg :background ,orange))))
   `(proof-error-face ((t (:foreground ,fg :background ,red-4))))
   `(proof-highlight-dependency-face ((t (:foreground ,bg :background ,yellow-1))))
   `(proof-highlight-dependent-face ((t (:foreground ,bg :background ,orange))))
   `(proof-locked-face ((t (:background ,blue-5))))
   `(proof-mouse-highlight-face ((t (:foreground ,bg :background ,orange))))
   `(proof-queue-face ((t (:background ,red-4))))
   `(proof-region-mouse-highlight-face ((t (:inherit proof-mouse-highlight-face))))
   `(proof-script-highlight-error-face ((t (:background ,red-2))))
   `(proof-tacticals-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,bg))))
   `(proof-tactics-name-face ((t (:inherit font-lock-constant-face :foreground nil :background ,bg))))
   `(proof-warning-face ((t (:foreground ,bg :background ,yellow-1))))
;;;;; racket-mode
   `(racket-keyword-argument-face ((t (:inherit font-lock-constant-face))))
   `(racket-selfeval-face ((t (:inherit font-lock-type-face))))
;;;;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,fg))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,green+4))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,yellow-2))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,cyan))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,green+2))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,blue+1))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,yellow-1))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,green+1))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,blue-2))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,blue-5))))
;;;;; rcirc
   `(rcirc-my-nick ((t (:foreground ,blue))))
   `(rcirc-other-nick ((t (:foreground ,orange))))
   `(rcirc-bright-nick ((t (:foreground ,blue+1))))
   `(rcirc-dim-nick ((t (:foreground ,blue-2))))
   `(rcirc-server ((t (:foreground ,green))))
   `(rcirc-server-prefix ((t (:foreground ,green+1))))
   `(rcirc-timestamp ((t (:foreground ,green+2))))
   `(rcirc-nick-in-message ((t (:foreground ,yellow))))
   `(rcirc-nick-in-message-full-line ((t (:weight bold))))
   `(rcirc-prompt ((t (:foreground ,yellow :weight bold))))
   `(rcirc-track-nick ((t (:inverse-video t))))
   `(rcirc-track-keyword ((t (:weight bold))))
   `(rcirc-url ((t (:weight bold))))
   `(rcirc-keyword ((t (:foreground ,yellow :weight bold))))
;;;;; re-builder
   `(reb-match-0 ((t (:foreground ,bg :background ,magenta))))
   `(reb-match-1 ((t (:foreground ,bg :background ,blue))))
   `(reb-match-2 ((t (:foreground ,bg :background ,orange))))
   `(reb-match-3 ((t (:foreground ,bg :background ,red))))
;;;;; realgud
   `(realgud-overlay-arrow1 ((t (:foreground ,green))))
   `(realgud-overlay-arrow2 ((t (:foreground ,yellow))))
   `(realgud-overlay-arrow3 ((t (:foreground ,orange))))
   `(realgud-bp-enabled-face ((t (:inherit error))))
   `(realgud-bp-disabled-face ((t (:inherit secondary-selection))))
   `(realgud-bp-line-enabled-face ((t (:box (:color ,red :style nil)))))
   `(realgud-bp-line-disabled-face ((t (:box (:color "grey70" :style nil)))))
   `(realgud-line-number ((t (:foreground ,yellow))))
   `(realgud-backtrace-number ((t (:foreground ,yellow, :weight bold))))
;;;;; regex-tool
   `(regex-tool-matched-face ((t (:background ,blue-4 :weight bold))))
;;;;; rpm-mode
   `(rpm-spec-dir-face ((t (:foreground ,green))))
   `(rpm-spec-doc-face ((t (:foreground ,green))))
   `(rpm-spec-ghost-face ((t (:foreground ,red))))
   `(rpm-spec-macro-face ((t (:foreground ,yellow))))
   `(rpm-spec-obsolete-tag-face ((t (:foreground ,red))))
   `(rpm-spec-package-face ((t (:foreground ,red))))
   `(rpm-spec-section-face ((t (:foreground ,yellow))))
   `(rpm-spec-tag-face ((t (:foreground ,blue))))
   `(rpm-spec-var-face ((t (:foreground ,red))))
;;;;; rst-mode
   `(rst-level-1-face ((t (:foreground ,orange))))
   `(rst-level-2-face ((t (:foreground ,green+1))))
   `(rst-level-3-face ((t (:foreground ,blue-1))))
   `(rst-level-4-face ((t (:foreground ,yellow-2))))
   `(rst-level-5-face ((t (:foreground ,cyan))))
   `(rst-level-6-face ((t (:foreground ,green-2))))
;;;;; sh-mode
   `(sh-heredoc     ((t (:foreground ,yellow :weight bold))))
   `(sh-quoted-exec ((t (:foreground ,red))))
;;;;; show-paren
   `(show-paren-mismatch ((t (:foreground ,red+1 :background ,bg+3 :weight bold))))
   `(show-paren-match ((t (:foreground ,fg :background ,bg+3 :weight bold))))
;;;;; smart-mode-line
   ;; use (setq sml/theme nil) to enable Qiita for sml
   `(sml/global ((,class (:foreground ,fg :weight bold))))
   `(sml/modes ((,class (:foreground ,yellow :weight bold))))
   `(sml/minor-modes ((,class (:foreground ,fg-1 :weight bold))))
   `(sml/filename ((,class (:foreground ,yellow :weight bold))))
   `(sml/line-number ((,class (:foreground ,blue :weight bold))))
   `(sml/col-number ((,class (:foreground ,blue+1 :weight bold))))
   `(sml/position-percentage ((,class (:foreground ,blue-1 :weight bold))))
   `(sml/prefix ((,class (:foreground ,orange))))
   `(sml/git ((,class (:foreground ,green+3))))
   `(sml/process ((,class (:weight bold))))
   `(sml/sudo ((,class  (:foreground ,orange :weight bold))))
   `(sml/read-only ((,class (:foreground ,red-2))))
   `(sml/outside-modified ((,class (:foreground ,orange))))
   `(sml/modified ((,class (:foreground ,red))))
   `(sml/vc-edited ((,class (:foreground ,green+2))))
   `(sml/charging ((,class (:foreground ,green+4))))
   `(sml/discharging ((,class (:foreground ,red+1))))
;;;;; smartparens
   `(sp-show-pair-mismatch-face ((t (:foreground ,red+1 :background ,bg+3 :weight bold))))
   `(sp-show-pair-match-face ((t (:background ,bg+3 :weight bold))))
;;;;; sml-mode-line
   '(sml-modeline-end-face ((t :inherit default :width condensed)))
;;;;; SLIME
   `(slime-repl-output-face ((t (:foreground ,red))))
   `(slime-repl-inputed-output-face ((t (:foreground ,green))))
   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,red)))
      (t
       (:underline ,red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,orange)))
      (t
       (:underline ,orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,yellow)))
      (t
       (:underline ,yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,green)))
      (t
       (:underline ,green))))
   `(slime-highlight-face ((t (:inherit highlight))))
;;;;; speedbar
   `(speedbar-button-face ((t (:foreground ,green+2))))
   `(speedbar-directory-face ((t (:foreground ,cyan))))
   `(speedbar-file-face ((t (:foreground ,fg))))
   `(speedbar-highlight-face ((t (:foreground ,bg :background ,green+2))))
   `(speedbar-selected-face ((t (:foreground ,red))))
   `(speedbar-separator-face ((t (:foreground ,bg :background ,blue-1))))
   `(speedbar-tag-face ((t (:foreground ,yellow))))
;;;;; sx
   `(sx-custom-button
     ((t (:background ,fg :foreground ,bg-1
          :box (:line-width 3 :style released-button) :height 0.9))))
   `(sx-question-list-answers
     ((t (:foreground ,green+3
          :height 1.0 :inherit sx-question-list-parent))))
   `(sx-question-mode-accepted
     ((t (:foreground ,green+3
          :height 1.3 :inherit sx-question-mode-title))))
   '(sx-question-mode-content-face ((t (:inherit highlight))))
   `(sx-question-mode-kbd-tag
     ((t (:box (:color ,bg-1 :line-width 3 :style released-button)
          :height 0.9 :weight semi-bold))))
;;;;; tabbar
   `(tabbar-button ((t (:foreground ,fg
                                    :background ,bg))))
   `(tabbar-selected ((t (:foreground ,fg
                                      :background ,bg
                                      :box (:line-width -1 :style pressed-button)))))
   `(tabbar-unselected ((t (:foreground ,fg
                                        :background ,bg+1
                                        :box (:line-width -1 :style released-button)))))
;;;;; term
   `(term-color-black ((t (:foreground ,bg
                                       :background ,bg-1))))
   `(term-color-red ((t (:foreground ,red-2
                                     :background ,red-4))))
   `(term-color-green ((t (:foreground ,green
                                       :background ,green+2))))
   `(term-color-yellow ((t (:foreground ,orange
                                        :background ,yellow))))
   `(term-color-blue ((t (:foreground ,blue-1
                                      :background ,blue-4))))
   `(term-color-magenta ((t (:foreground ,magenta
                                         :background ,red))))
   `(term-color-cyan ((t (:foreground ,cyan
                                      :background ,blue))))
   `(term-color-white ((t (:foreground ,fg
                                       :background ,fg-1))))
   '(term-default-fg-color ((t (:inherit term-color-white))))
   '(term-default-bg-color ((t (:inherit term-color-black))))
;;;;; undo-tree
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,fg+1 :weight bold))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,red-1 :weight bold))))
   `(undo-tree-visualizer-default-face ((t (:foreground ,fg))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,yellow))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,cyan))))
;;;;; visual-regexp
   `(vr/group-0 ((t (:foreground ,bg :background ,green :weight bold))))
   `(vr/group-1 ((t (:foreground ,bg :background ,orange :weight bold))))
   `(vr/group-2 ((t (:foreground ,bg :background ,blue :weight bold))))
   `(vr/match-0 ((t (:inherit isearch))))
   `(vr/match-1 ((t (:foreground ,yellow-2 :background ,bg-1 :weight bold))))
   `(vr/match-separator-face ((t (:foreground ,red :weight bold))))
;;;;; volatile-highlights
   `(vhl/default-face ((t (:background ,bg-05))))
;;;;; web-mode
   `(web-mode-builtin-face ((t (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((t (:inherit ,font-lock-constant-face))))
   `(web-mode-css-at-rule-face ((t (:foreground ,orange ))))
   `(web-mode-css-prop-face ((t (:foreground ,orange))))
   `(web-mode-css-pseudo-class-face ((t (:foreground ,green+3 :weight bold))))
   `(web-mode-css-rule-face ((t (:foreground ,blue))))
   `(web-mode-doctype-face ((t (:inherit ,font-lock-comment-face))))
   `(web-mode-folded-face ((t (:underline t))))
   `(web-mode-function-name-face ((t (:foreground ,blue))))
   `(web-mode-html-attr-name-face ((t (:foreground ,orange))))
   `(web-mode-html-attr-value-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-html-tag-face ((t (:foreground ,cyan))))
   `(web-mode-keyword-face ((t (:inherit ,font-lock-keyword-face))))
   `(web-mode-preprocessor-face ((t (:inherit ,font-lock-preprocessor-face))))
   `(web-mode-string-face ((t (:inherit ,font-lock-string-face))))
   `(web-mode-type-face ((t (:inherit ,font-lock-type-face))))
   `(web-mode-variable-name-face ((t (:inherit ,font-lock-variable-name-face))))
   `(web-mode-server-background-face ((t (:background ,bg))))
   `(web-mode-server-comment-face ((t (:inherit web-mode-comment-face))))
   `(web-mode-server-string-face ((t (:inherit web-mode-string-face))))
   `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))
   `(web-mode-warning-face ((t (:inherit font-lock-warning-face))))
   `(web-mode-whitespaces-face ((t (:background ,red))))
;;;;; enh-ruby-mode
   `(enh-ruby-op-face ((t (:foreground ,red))))
   `(enh-ruby-string-delimiter-face ((t (:foreground ,blue))))
;;;;; whitespace-mode
   `(whitespace-space ((t (:background ,bg+1 :foreground ,bg+1))))
   `(whitespace-hspace ((t (:background ,bg+1 :foreground ,bg+1))))
   `(whitespace-tab ((t (:background ,red-1))))
   `(whitespace-newline ((t (:foreground ,bg+1))))
   `(whitespace-trailing ((t (:background ,red))))
   `(whitespace-line ((t (:background ,bg :foreground ,magenta))))
   `(whitespace-space-before-tab ((t (:background ,orange :foreground ,orange))))
   `(whitespace-indentation ((t (:background ,yellow :foreground ,red))))
   `(whitespace-empty ((t (:background ,yellow))))
   `(whitespace-space-after-tab ((t (:background ,yellow :foreground ,red))))
;;;;; wanderlust
   `(wl-highlight-folder-few-face ((t (:foreground ,red-2))))
   `(wl-highlight-folder-many-face ((t (:foreground ,red-1))))
   `(wl-highlight-folder-path-face ((t (:foreground ,orange))))
   `(wl-highlight-folder-unread-face ((t (:foreground ,blue))))
   `(wl-highlight-folder-zero-face ((t (:foreground ,fg))))
   `(wl-highlight-folder-unknown-face ((t (:foreground ,blue))))
   `(wl-highlight-message-citation-header ((t (:foreground ,red-1))))
   `(wl-highlight-message-cited-text-1 ((t (:foreground ,red))))
   `(wl-highlight-message-cited-text-2 ((t (:foreground ,green+2))))
   `(wl-highlight-message-cited-text-3 ((t (:foreground ,blue))))
   `(wl-highlight-message-cited-text-4 ((t (:foreground ,blue+1))))
   `(wl-highlight-message-header-contents-face ((t (:foreground ,green))))
   `(wl-highlight-message-headers-face ((t (:foreground ,red+1))))
   `(wl-highlight-message-important-header-contents ((t (:foreground ,green+2))))
   `(wl-highlight-message-header-contents ((t (:foreground ,green+1))))
   `(wl-highlight-message-important-header-contents2 ((t (:foreground ,green+2))))
   `(wl-highlight-message-signature ((t (:foreground ,green))))
   `(wl-highlight-message-unimportant-header-contents ((t (:foreground ,fg))))
   `(wl-highlight-summary-answered-face ((t (:foreground ,blue))))
   `(wl-highlight-summary-disposed-face ((t (:foreground ,fg
                                                         :slant italic))))
   `(wl-highlight-summary-new-face ((t (:foreground ,blue))))
   `(wl-highlight-summary-normal-face ((t (:foreground ,fg))))
   `(wl-highlight-summary-thread-top-face ((t (:foreground ,yellow))))
   `(wl-highlight-thread-indent-face ((t (:foreground ,magenta))))
   `(wl-highlight-summary-refiled-face ((t (:foreground ,fg))))
   `(wl-highlight-summary-displaying-face ((t (:underline t :weight bold))))
;;;;; which-func-mode
   `(which-func ((t (:foreground ,green+4))))
;;;;; xcscope
   `(cscope-file-face ((t (:foreground ,yellow :weight bold))))
   `(cscope-function-face ((t (:foreground ,cyan :weight bold))))
   `(cscope-line-number-face ((t (:foreground ,red :weight bold))))
   `(cscope-mouse-face ((t (:foreground ,bg :background ,blue+1))))
   `(cscope-separator-face ((t (:foreground ,red :weight bold
                                            :underline t :overline t))))
;;;;; yascroll
   `(yascroll:thumb-text-area ((t (:background ,bg-1))))
   `(yascroll:thumb-fringe ((t (:background ,bg-1 :foreground ,bg-1))))
   ))

;;; Theme Variables
(with-color-variables
  (custom-theme-set-variables
   'qiita
;;;;; ansi-color
   `(ansi-color-names-vector [,bg ,red ,green ,yellow
                                          ,blue ,magenta ,cyan ,fg])
;;;;; company-quickhelp
   `(company-quickhelp-color-background ,bg+1)
   `(company-quickhelp-color-foreground ,fg)
;;;;; fill-column-indicator
   `(fci-rule-color ,bg-05)
;;;;; nrepl-client
   `(nrepl-message-colors
     '(,red ,orange ,yellow ,green ,green+4
       ,cyan ,blue+1 ,magenta))
;;;;; pdf-tools
   `(pdf-view-midnight-colors '(,fg . ,bg-05))
;;;;; vc-annotate
   `(vc-annotate-color-map
     '(( 20. . ,red-1)
       ( 40. . ,red)
       ( 60. . ,orange)
       ( 80. . ,yellow-2)
       (100. . ,yellow-1)
       (120. . ,yellow)
       (140. . ,green-2)
       (160. . ,green)
       (180. . ,green+1)
       (200. . ,green+2)
       (220. . ,green+3)
       (240. . ,green+4)
       (260. . ,cyan)
       (280. . ,blue-2)
       (300. . ,blue-1)
       (320. . ,blue)
       (340. . ,blue+1)
       (360. . ,magenta)))
   `(vc-annotate-very-old-color ,magenta)
   `(vc-annotate-background ,bg-1)
   ))

;;; Rainbow Support

(declare-function rainbow-mode 'rainbow-mode)
(declare-function rainbow-colorize-by-assoc 'rainbow-mode)

(defvar add-font-lock-keywords nil
  "Whether to add font-lock keywords for qiita color names.
In buffers visiting library `qiita-theme.el' the qiita
specific keywords are always added.  In all other Emacs-Lisp
buffers this variable controls whether this should be done.
This requires library `rainbow-mode'.")

(defvar colors-font-lock-keywords nil)

;; (defadvice rainbow-turn-on (after qiita activate)
;;   "Maybe also add font-lock keywords for qiita colors."
;;   (when (and (derived-mode-p 'emacs-lisp-mode)
;;              (or add-font-lock-keywords
;;                  (equal (file-name-nondirectory (buffer-file-name))
;;                         "qiita-theme.el")))
;;     (unless colors-font-lock-keywords
;;       (setq colors-font-lock-keywords
;;             `((,(regexp-opt (mapcar 'car colors-alist) 'words)
;;                (0 (rainbow-colorize-by-assoc colors-alist))))))
;;     (font-lock-add-keywords nil colors-font-lock-keywords)))

;; (defadvice rainbow-turn-off (after qiita activate)
;;   "Also remove font-lock keywords for qiita colors."
;;   (font-lock-remove-keywords nil colors-font-lock-keywords))

;;; Footer

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'qiita)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; qiita-theme.el ends here
