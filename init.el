;;===========================;;
;;  This Is MY emacs config  ;;
;;  There are many like it   ;;
;;  But this one is mine     ;;
;;===========================;;

;; Don't show the splash screen
(setq inhibit-startup-message t)

;;==========;;
;; Packages ;;
;;==========;;
(require 'package)

(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                        ("melpa"  . "https://melpa.org/packages/")
                        ("org"    . "https://orgmode.org/elpa/")))

;; Only uncomment when you change a package (slows every load)
(package-refresh-contents)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(package-initialize)

;; Company
(use-package company
  :commands (global-company-mode)
  :delight
  :init
  (global-company-mode))

(use-package helm-company
  :after helm company
  :bind (:map company-mode-map
         ("C-;" . helm-company)
         :map company-active-map
         ("C-;" . helm-company)))

;; darkroom
(use-package darkroom)

;; Elfeed
(use-package elfeed
  :custom
  (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-c f" . elfeed))

(use-package elfeed-org
  :after elfeed org
  :custom
  (rmh-elfeed-org-files (list "~/org/feeds.org"))
  :config
  (elfeed-org))

;; emms
(use-package emms
  :config
  (add-to-list 'load-path "~/.emacs.d/emms/lisp")
  (require 'emms-setup)
  (emms-standard)
  (emms-default-players)
  :custom
  (emms-browser-covers 'emms-browser-cache-thumbnail-async)
  (emms-player-list '(emms-player-mpv))
  (emms-source-file-default-directory "~/Music")
  (emms-source-playlist-default-format 'm3u)
  :bind
  ("<XF86AudioPrev>" . emms-previous)
  ("<XF86AudioNext>" . emms-next)
  ("<XF86AudioPlay>" . emms-pause)
  ("<XF86AudioStop>" . emms-stop))

(use-package helm-emms
  :bind
  ("<C-f6>" . helm-emms))

;; Go
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook (go-mode .
          go-enable-fmt-on-save))

;; Helm
(use-package helm
  :bind (("M-x"     . helm-M-x)
         ("C-x b"   . helm-mini)
         ("C-h a"   . helm-apropos)
         ("C-x C-f" . helm-find-files))
  :config
  (helm-adaptive-mode)
  (helm-mode))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))

(use-package helm-org
  :after (org helm)
  :custom
  (helm-org-headings-fontify t)
  (helm-org-format-outline-path t))

(use-package helm-gtags
  :after helm
  :init (helm-gtags-mode t)
  :diminish ""
  :hook ((dired-mode . helm-gtags-mode)
         (eshell-mode-hook . helm-gtags-mode)
         (c-mode-hook . helm-gtags-mode)
         (c++-mode . helm-gtags-mode)
         (asm-mode . helm-gtags-mode))
  :custom
  (helm-gtags-ignore-case t)
  (helm-gtags-auto-update t)
  (helm-gtags-use-input-at-cursor t)
  (helm-gtags-pulse-at-cursor t)
  (helm-gtags-prefix-key "\C-cg")
  (helm-autoresize-mode nil)
  (helm-gtags-suggested-key-mapping t)
  :bind (:map helm-gtags-mode-map
         ("C-j" . helm-gtags-select)
         ("M-." . helm-gtags-dwim)
         ("M-," . helm-gtags-pop-stack)
         ("C-c <" . helm-gtags-previous-history)
         ("C-c >" . helm-gtags-next-history)
         ("C-c g a" . helm-gtags-tags-in-this-function)))


;; Better completion ordering
(with-eval-after-load 'helm
  (setq helm-mode-fuzzy-match t
        helm-completion-in-region-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-M-x-fuzzy-match t))

;; highlight-indent-guides
(use-package highlight-indent-guides
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0)
  :hook
  (prog-mode . highlight-indent-guides-mode))

;; key-chord
;(use-package key-chord
;  :config
;  (setq key-chord-two-keys-delay 0.150)
;  (key-chord-mode)
;  (key-chord-define-global "kj" ctl-x-map)
;  (key-chord-define-global "df" 'null)
;  (define-key key-translation-map (kbd "<key-chord> df") (kbd "C-c"))
;  (key-chord-define-global "fd" 'null)
;  (define-key key-translation-map (kbd "<key-chord> fd") (kbd "C-c")))

;; use-package-chords
(use-package use-package-chords
  :config (key-chord-mode 1))

;; list-unicode-display
(use-package list-unicode-display)

;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-dispatch)
         :map dired-mode-map
         ("C-x g" . magit-dired-log)))

;; Markdown Mode
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . (lambda ()
                          (local-set-key (kbd "RET") 'newline))))

;; Markdown Preview Mode
(use-package markdown-preview-mode
  :after markdown-mode
  :commands markdown-preview-mode
  :custom
  (markdown-command "/bin/pandoc"))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)))

;; Org
(use-package org
  :pin gnu
  :custom
  (org-startup-folded t)
  (org-startup-indented t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (lisp . t)
     (C . t)
     (python . t)
     (shell . t))))

;; Python
(use-package python-mode
  :custom
  (python-shell-interpreter "python3"))

;; Rainbow-mode
(use-package rainbow-mode
  :delight
  :hook ((prog-mode text-mode) . rainbow-mode))

;; recentf
(use-package recentf
  :custom
  (recentf-max-saved-items 200)
  (recentf-max-menu-items 15)
  (recentf-auto-cleanup 'never)
  (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  :config
  (recentf-mode 1)
  ;; Save file list every 5 minutes
  (run-at-time nil (* 5 60) 'recentf-save-list))

;; Optional: Add helm-recentf binding if you want quick access through helm
(with-eval-after-load 'helm
  (global-set-key (kbd "C-c r") 'helm-recentf))

;; Smartparens
(use-package smartparens
  :config
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (setq sp-autowrap-region nil)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-show-pair-from-inside t)
  ;; Disable quote escaping
  (sp-pair "\"" nil :actions :rem)
  (sp-pair "\'" nil :actions :rem)
  ;; C-mode specific pairs
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                             ("* ||\n[i]" "RET")))))

;; Try
(use-package try)

;; undo
(use-package undo-tree
  :init (global-undo-tree-mode t)
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-visualizer-diff t)
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo" user-emacs-directory))))
  :bind
  ("C-x u" . undo-tree-visualize))

;; which-key
(use-package which-key
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode))

;;==========;;
;; Files    ;;
;;==========;;

;; Load config files
(load "~/.config/emacs/org-config/org-capture")

;; Backup files
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save" user-emacs-directory) t)))

;; Spaces
(setq-default indent-tabs-mode nil
              tab-width 4
              indent-line-function 'insert-tab)

;;=============;;
;; Environment ;;
;;=============;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(xterm-mouse-mode 1)

;; Tabs
(setq tab-bar-mode nil
      tab-bar-show 1
      tab-bar-close-button-show nil
      tab-bar-new-button-show nil)

(global-set-key (kbd "C-M-S-<left>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-M-S-<right>") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-M-S-<up>") 'tab-bar-new-tab)
(global-set-key (kbd "C-M-S-<down>") 'tab-bar-close-tab)

;; Clipboard
(setq x-select-enable-clipboard t
      x-select-enable-clipboard-manager t)

;; Display settings
(global-display-line-numbers-mode t)
(global-visual-line-mode t)
(setq-default left-margin-width 0 right-margin-width 2)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                org-agenda-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                vterm-mode-hook
                compilation-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode 0)
                   (let ((win (get-buffer-window)))
                     (when win
                       (with-selected-window win
                         (set-window-margins nil 1)))))))

(set-face-attribute 'region nil :extend nil)

;; Themes
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(load-theme 'aldalome t)

;; Fix colors in st
(add-to-list 'term-file-aliases
             '("st-256color" . "xterm-256color"))

;; Set default font
(set-face-attribute 'default nil
                    :family "CaskaydiaCove NFM"
                    :height 140
                    :weight 'normal
                    :width 'normal)

;;==========;;
;; Org Mode ;;
;;==========;;

(require 'org)
(define-key org-mode-map (kbd "RET") nil)
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-todo-keywords
      '((sequence "TODO" "NEXT" "WAITING" "|" "DONE")))

(setq org-log-done 'time
      org-directory "~/org/"
      org-latex-tables-centered nil)

;; Org Agenda
(setq org-agenda-files '("~/org/"
                        "~/org/gtd/"
                        "~/org/inbox/"
                        "~/org/ref/people.org"))

(setq org-refile-targets
      `((,(directory-files "~/org/ref/" t "\\.org$") :maxlevel . 2)
        ("~/org/gtd/todo.org" :maxlevel . 1)
        ("~/org/gtd/tickler.org" :maxlevel . 1)
        ("~/org/gtd/projects.org" :maxlevel . 1)))

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

;; Calendar
(define-key calendar-mode-map (kbd "RET") 'org-journal-display-entry)

;; LaTeX Export settings
(setq org-latex-compiler "lualatex")
(plist-put org-format-latex-options :scale 2.5)

;; Source blocks
(setq org-src-fontify-natively t)
(setq org-image-actual-width nil)

(defun org-insert-source-block (name language switches header)
  "Asks name, language, switches, header.
Inserts org-mode source code snippet"
  (interactive "sName: \nsLanguage: \nsSwitches: \nsHeader: ")
  (insert
   (if (string= name "")
       ""
     (concat "#+NAME: " name))
   (format "
#+BEGIN_SRC %s %s %s

#+END_SRC" language switches header))
  (forward-line -1)
  (goto-char (line-end-position)))

(define-key org-mode-map (kbd "C-c s") #'org-insert-source-block)

;;==================;;
;; Custom Functions ;;
;;==================;;

;; Open this file
(defun open-init-file ()
  "Open the Emacs config file."
  (interactive)
  (find-file "~/.config/emacs/init.el"))

;; 'brnm' from command line
(defun dired-rename-from-shell (dir)
  "Open dired in DIR for renaming, auto-close frame on finish."
  (let* ((use-sudo (string-prefix-p "sudo::" dir))
         (actual-dir (if use-sudo 
                         (substring dir 6)
                       dir))
         (tramp-dir (if use-sudo
                        (concat "/sudo::" actual-dir)
                      dir))
         (buf (dired tramp-dir)))
    (with-current-buffer buf
      (dired-toggle-read-only)
      (setq header-line-format
            '(:eval (propertize " Wdired: C-c C-c to apply & exit, C-c C-k to cancel & exit" 
                                'face '(:foreground "green" :weight bold))))
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (wdired-finish-edit)
                       (kill-buffer)
                       (delete-frame)))
      (local-set-key (kbd "C-c C-k")
                     (lambda ()
                       (interactive)
                       (wdired-abort-changes)
                       (kill-buffer)
                       (delete-frame))))))
;; Then in .zshrc
;dired() {
;    local dir="${1:-.}"
;    # Check if we can write to the directory
;    if [[ -w "$dir" ]]; then
;        emacsclient --tty --eval "(dired-rename-from-shell \"$dir\")"
;    else
;        echo "Directory not writable, using sudo..."
;        emacsclient --tty --eval "(dired-rename-from-shell \"/sudo::$(realpath $dir)\")"
;    fi
;}

(defun wrap-region-with-char-or-pair (open)
  "Wrap the region with the specified OPEN character or character pair."
  (interactive "cWrap with: ")
  (let* ((pairs '((?\( . ?\))
                  (?\' . ?\')
                  (?\" . ?\")
                  (?\{ . ?\})
                  (?\[ . ?\])
                  (?< . ?>)))
         (close (or (cdr (assoc open pairs)) open))
         (beg (region-beginning))
         (end (region-end)))
    (goto-char end)
    (insert close)
    (goto-char beg)
    (insert open)))

(global-set-key (kbd "C-x w") 'wrap-region-with-char-or-pair)

(defun org-agenda-kill-all-files ()
  "Close the buffers associated with 'org-agenda-files'."
  (interactive)
  (let* ((file-list (apply #'append (mapcar (lambda (x)
                                             (if (file-directory-p x)
                                                 (directory-files x t "\\.org\\'")
                                               (list x)))
                                           org-agenda-files)))
         (buffer-list (mapcar #'find-buffer-visiting file-list)))
    (dolist (buffer buffer-list)
      (when buffer
        (kill-buffer buffer)))))

(global-set-key (kbd "C-c k a") 'org-agenda-kill-all-files)
(which-key-add-key-based-replacements "C-c k" "kill")

;; Screenshot function
(defun org-scrot ()
  "Take a screenshot using `scrot', save it to .img directory, and insert a link at point."
  (interactive)
  (setq filename
        (concat (make-temp-name
                (concat (file-name-directory buffer-file-name)
                        ".img/"
                        (format-time-string "%Y%m%d_")))
                ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  (call-process "scrot" nil nil nil "-s" filename)
  (setq relative-filename (replace-regexp-in-string (file-name-directory buffer-file-name) "./" filename))
  (insert (concat "#+ATTR_HTML: :width 500px\n#+ATTR_LATEX: :width 12cm\n[[" relative-filename "]]")))

(define-key org-mode-map (kbd "C-M-S-s") 'org-scrot)

;; Audio recording
(defvar audio-recording-process nil)
(defvar audio-recording-filename nil)

(defun toggle-audio-recording ()
  "Record audio to a directory named .audio and add a link to file at point."
  (interactive)
  (if audio-recording-process
      (progn
        (delete-process audio-recording-process)
        (setq audio-recording-process nil)
        (insert (format "[[./%s]]" (file-relative-name audio-recording-filename)))
        (setq audio-recording-filename nil))
    (let* ((dirname (concat (file-name-directory buffer-file-name) ".audio/"))
           (filename (concat (make-temp-name (concat dirname (format-time-string "%Y%m%d_"))) ".mp3")))
      (make-directory dirname t)
      (setq audio-recording-process
            (start-process-shell-command "audio-recording" nil
                                       (format "arecord -f cd -t wav | lame - -b 192 %s" filename)))
      (setq audio-recording-filename filename))))

(define-key org-mode-map (kbd "C-M-S-a") 'toggle-audio-recording)

;; Play audio link at point
(defun emms-play-org-link-at-point ()
  "Play the audio file at point using EMMS."
  (interactive)
  (when (eq (org-element-type (org-element-context)) 'link)
    (let ((path (org-element-property :path (org-element-context))))
      (emms-play-file path))))

(define-key org-mode-map (kbd "C-M->") 'emms-play-org-link-at-point)

;; Window management
(defun transpose-windows ()
  "Transpose two windows.  If more or less than two windows are visible, error."
  (interactive)
  (unless (= 2 (count-windows))
    (error "There are not 2 windows"))
  (let* ((windows (window-list))
         (w1 (car windows))
         (w2 (nth 1 windows))
         (w1b (window-buffer w1))
         (w2b (window-buffer w2)))
    (set-window-buffer w1 w2b)
    (set-window-buffer w2 w1b)))

(global-set-key (kbd "C-x 9") 'transpose-windows)

;; IDE/Programming Settings
(setq c-default-style "bsd")
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c w") 'whitespace-mode)

(add-hook 'prog-mode-hook (lambda ()
                           (interactive)
                           (setq show-trailing-whitespace 1)))

(setq-default indent-tabs-mode nil
              tab-width 4
              c-basic-offset 4)

;; Custom scripts
(add-to-list 'load-path (expand-file-name "scripts" user-emacs-directory))

;(use-package stock-line
;  :ensure nil
;  :load-path "~/.config/emacs/scripts"
;  :custom
;  (stock-line-api-key "api_key_here")
;  (stock-line-tickers '("AAPL" "MSFT" "GOOG" "AMZN"))
;  (stock-line-update-interval 1800)
;  (stock-line-scroll-speed 1.0)
;  :config
;  (stock-line-mode nil))

;; Keep customizations in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
