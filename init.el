;;===========================;;
;;  This Is MY emacs config  ;;
;;  There are many like it   ;;
;;  But this one is mine     ;;
;;===========================;;

;; Don't show the splash screen
(setq inhibit-startup-message t)

(if (and (= emacs-major-version 26)
         (<= emacs-minor-version 1))
    (progn (message "Fixing GNU TLS algorithm")
           (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")))

;;==========;;
;; Packages ;;
;;==========;;
(require 'package)

(setq package-archives '(("gnu"    .  "https://elpa.gnu.org/packages/")
                         ("nongnu" .  "https://elpa.nongnu.org/nongnu/")
                         ("melpa"  .  "https://melpa.org/packages/")
                         ("org"    .  "https://orgmode.org/elpa/")))

;; Uncomment when you're updating this file less frequently
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
  :ensure t
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
(use-package darkroom
  :ensure t)

;; Elfeed
(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  :bind
    ("C-c f" . elfeed ))

(use-package elfeed-org
  :after elfeed org
  :init
  (setq rmh-elfeed-org-files (list "~/org/feeds.org"))
  :config
  (elfeed-org))

;; emms
(use-package emms
  :ensure t
  :pin "melpa"
  :config
  (add-to-list 'load-path "~/.emacs.d/emms/lisp")
  (require 'emms-setup)
  (emms-standard)
  (emms-default-players)
  (setq emms-browser-covers 'emms-browser-cache-thumbnail-async)
  (setq-default
    emms-player-list '(emms-player-mpv)
    emms-source-file-default-directory "~/Music"
    emms-source-playlist-default-format 'm3u)
  :bind
    ("<XF86AudioPrev>" . emms-previous)
    ("<XF86AudioNext>" . emms-next)
    ("<XF86AudioPlay>" . emms-pause)
    ("<XF86AudioStop>" . emms-stop))

;(use-package emms-info-mp3info)
;(use-package emms-info-ogginfo)
;(use-package emms-browser)

(use-package helm-emms
  :bind
  (("<C-f6>" . helm-emms)))

;; Helm
(use-package helm
  :ensure t
  :bind (("M-x"     . helm-M-x)
         ("C-x b"   . helm-mini)
         ("C-h a"   . helm-apropos)
         ("C-x C-f" . helm-find-files))
  :config
  (helm-adaptive-mode)
  (helm-mode)
  )

(add-to-list 'display-buffer-alist
                    `(,(rx bos "*helm" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.4)))

(use-package helm-org
  :after (org helm)
  :ensure t
  :custom ((helm-org-headings-fontify t)
           (helm-org-format-outline-path t)))

(use-package helm-gtags
  :after helm
  :ensure t
  :init (helm-gtags-mode t)
  :diminish ""
  :hook ((dired-mode . helm-gtags-mode)
         (eshell-mode-hook . helm-gtags-mode)
         (c-mode-hook . helm-gtags-mode)
         (c++-mode . helm-gtags-mode)
         (asm-mode . helm-gtags-mode))
  :custom ((helm-gtags-ignore-case t)
           (helm-gtags-auto-update t)
           (helm-gtags-use-input-at-cursor t)
           (helm-gtags-pulse-at-cursor t)
           (helm-gtags-prefix-key "\C-cg")
           (helm-autoresize-mode nil) ; Test without this after v28 update
           (helm-gtags-suggested-key-mapping t))
  :bind (:map helm-gtags-mode-map
        ("C-j" . helm-gtags-select) ; Change this or more of these?
        ("M-." . helm-gtags-dwim)   ; After you get used to gtags
        ("M-," . helm-gtags-pop-stack)
        ("C-c <" . helm-gtags-previous-history)
        ("C-c >" . helm-gtags-next-history)
        ("C-c g a" . helm-gtags-tags-in-this-function)))

;; highlight-indent-guides
(use-package highlight-indent-guides
  :ensure t
  :custom
  ((highlight-indent-guides-method 'character)
   (highlight-indent-guides-responsive 'top)
   (highlight-indent-guides-delay 0))
  :hook
  (prog-mode   .  highlight-indent-guides-mode))

;; key-chord
(use-package key-chord
  :ensure t
  :config
  (setq key-chord-two-keys-delay 0.150)
  (key-chord-mode))

; This will change, a lot
(key-chord-define-global "kj" ctl-x-map)
(key-chord-define-global "df" 'null)
(define-key key-translation-map (kbd "<key-chord> df")  (kbd "C-c"))
(key-chord-define-global "fd" 'null)
(define-key key-translation-map (kbd "<key-chord> fd")  (kbd "C-c"))

;; use-package-chords
(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))

;; list-unicode-display
(use-package list-unicode-display
  :ensure t)

;; Magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-dispatch)
         (:map dired-mode-map
          ("C-x g" . magit-dired-log))))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C->"     . mc/mark-all-like-this)))

;; Org
(use-package org
  :ensure t
  :pin gnu
  :custom
  (org-startup-folded t)
  (org-startup-indented t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (lisp       . t)
     (C          . t)
     (python     . t)
     (shell      . t)))
  )

;;;Org-Journal
;(use-package org-journal
;  :ensure t
;  :defer t
;  :custom
;  (org-journal-dir "~/org/log/")
;  (org-journal-file-format "%Y.org")
;  (org-journal-date-prefix "* ")
;  (org-journal-time-prefix "** %H:%M ")
;  (org-journal-date-format "%Y-%m-%d %A")
;  (org-journal-enable-agenda-integration t))
;
;; Org-Roam
;(use-package org-roam
;  :ensure t
;  :after org
;  :custom
;  (org-roam-directory "~/org/roam/")
;  (org-roam-db-location "~/org/roam/roam.db")
;  (org-roam-dailies-directory "~/org/log/")
;
;  :bind (("C-c n f" . org-roam-node-find)
;         ("C-c n r" . org-roam-node-random)
;         ("C-c n c" . org-roam-capture)
;         ("C-c n l" . org-roam-dailies-capture-today)
;         :map org-mode-map
;         ("C-c n b" . org-roam-buffer-toggle)
;         ("C-c n t" . org-roam-tag-add)
;         ("C-c n e" . org-roam-extract-subtree)
;         ("C-c n i" . org-roam-node-insert))
;  :config
;  (setq org-roam-completion-everywhere t)
;  (setq org-roam-completion-system 'ido)
;  (org-roam-db-autosync-mode t)
;  ;; (require 'org-roam-protocol)
;
;;(setq org-roam-dailies-capture-templates
;;  '(("d" "default" entry
;;     (file "~/org/log/template.org")
;;     :target (file+head "%<%Y-%m-%d>.org"
;;                        "#+title: %<%Y-%m-%d>\n"))))
;)

;; Rainbow-mode
(use-package rainbow-mode
  :ensure t
  :delight
  :hook ((prog-mode text-mode) . rainbow-mode))

;; Slime?
;(use-package slime
;  :init (setq inferior-lisp-program "sbcl"))

;; Smartparens
(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  (setq sp-autowrap-region nil)
  (setq sp-highlight-pair-overlay nil)
  (setq sp-show-pair-from-inside t))

;; when you press RET, the curly braces automatically
;; add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))
;; Try
(use-package try
  :ensure t)

;; undo
(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode t)
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-visualizer-diff t)
  :config
  (setq undo-tree-history-directory-alist `(("." . "~/.config/emacs/undo")))
  :bind  (("C-x u" . undo-tree-visualize)))

;; which-key
(use-package which-key
  :ensure t
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-setup-side-window-bottom)
  (which-key-mode))

;;==========;;
;; Files    ;;
;;==========;;

;; Load config files
;;(This section will grow)
(load "~/.config/emacs/org-config/org-capture")
;(load "~/.config/emacs/imp-mode/imp-mode")

;; Temp files to /tmp/
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;;=============;;
;; Environment ;;
;;=============;;

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

;(setq initial-scratch-message nil)

(setq x-select-enable-clipboard t)
(setq x-select-enable-clipboard-manager t)

;;;;;;;;;;;;;;;;;;;;;;;;;;; re-org by M-x customize groups?

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

;; Highlight current line
;(global-hl-line-mode 1)
;(set-face-background hl-line-face "#0c0c0c")

;; Themes
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(load-theme 'aldalome t)

;; Fix colors in st
(add-to-list 'term-file-aliases
	         '("st-256color" . "xterm-256color"))

;; set default font
(set-face-attribute 'default nil
                    :family "Cascadia Mono"
                    :height 140
                    :weight 'normal
                    :width 'normal)
;;==========;;
;; Org Mode ;;
;;==========;;

(require 'org)
(define-key org-mode-map (kbd "RET") nil)

(global-set-key (kbd "C-c a") 'org-agenda)
;(global-set-key (kbd "C-c r") 'org-refile)

(setq org-todo-keywords
	'((sequence "TODO" "NEXT" "WAITING" "|"  "DONE")))

(setq org-log-done 'time)

(setq org-directory "~/org/")

;; Org Agenda
(setq org-agenda-files '("~/org/"
                         "~/org/gtd/"
                         "~/org/inbox/"
                         "~/org/ref/people.org"
                         "~/Documents/CS/311/"
                         "~/Documents/CS/350/"
                         "~/Documents/CS/586/"))
;(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
;(setq org-agenda-files '("~/org/projects.org"
;			             "~/org/people.org"
;			             "~/org/todo.org"
;                         "~/org/tickler.org"))

;(setq org-refile-targets '(("~/org/ref/" :maxlevel . 2)
;                           ("~/org/gtd/todo.org" :maxlevel . 1)
;                           ("~/org/gtd/tickler.org" :maxlevel . 1)
;                           ("~/org/gtd/projects.org" :maxlevel . 2)
;                           ))

; This works, except maxlevels
(setq org-refile-targets
      `((,(directory-files "~/org/ref/" t "\\.org$") :maxlevel . 1)
        ("~/org/gtd/todo.org" :maxlevel . 1)
        ("~/org/gtd/tickler.org" :maxlevel . 1)
        ("~/org/gtd/projects.org" :maxlevel . 1)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-agenda-start-day "-3d")
(setq org-agenda-span 14)
(setq org-agenda-start-on-weekday nil)
(setq org-deadline-warning-days 0)

(setq org-agenda-hide-tags-regexp "noexport\\|exampleforreference")

(setq org-agenda-show-future-repeats t)

;; Calendar stuff
;(eval-after-load "calendar"
;  `(progn
;     (define-key calendar-mode-map (kbd "RET") 'org-journal-display-entry)
;    ; (define-key calendar-mode-map (kbd "M-[") 'calendar-backward-month)
;     ))
(define-key calendar-mode-map (kbd "RET") 'org-journal-display-entry)

;; Latex Export

;; Source blocks
(defun org-insert-source-block (name language switches header)
  "Asks name, language, switches, header.
Inserts org-mode source code snippet"
  (interactive "sName: 
sLanguage: 
sSwitches: 
sHeader: ")
  (insert
   (if (string= name "")
       ""
     (concat "#+NAME: " name) )
   (format "
#+BEGIN_SRC %s %s %s

#+END_SRC" language switches header
))
  (forward-line -1)
  (goto-char (line-end-position))
  )

(define-key org-mode-map (kbd "C-c s") #'org-insert-source-block)

;; Colors in source blocks
(setq org-src-fontify-natively t)

;; LaTeX
(plist-put org-format-latex-options :scale 2.5)

(setq org-latex-compiler "lualatex")

; Not sure about this yet
;(add-to-list 'org-latex-packages-alist '("" "booktabs" nil))
;(setq org-latex-tables-booktabs t)

;; other org settings
(setq org-image-actual-width nil)

;; imp mode
;(let ((default-directory "/imp-mode"))
;    (shell-command "hypercaps.sh"))
;(setq x-hyper-keysym 'hyper)

(defun open-init-file ()
  "Open the Emacs config file."
  (interactive)
  (find-file "~/.config/emacs/init.el"))

;; Custom wrap function
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

;; Insert scrot to org-mode
(defun org-scrot ()
  "Take a screenshot using `scrot', save it to .img directory, and insert a link at point."
  (interactive)
    (setq filename
        (concat (make-temp-name
                 (concat (file-name-directory buffer-file-name) ".img/" (format-time-string "%Y%m%d_") ) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  (call-process "scrot" nil nil nil "-s" filename)
  (setq relative-filename (replace-regexp-in-string (file-name-directory buffer-file-name) "./" filename))
  ;(insert (concat "[[" relative-filename "]]")))
  (insert (concat "#+ATTR_HTML: :width 500px\n#+ATTR_LATEX: :width 12cm\n[[" relative-filename "]]")))

(define-key org-mode-map (kbd "C-M-S-s") 'org-scrot)

;; An audio version?
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

;; Transpose windows
(defun transpose-windows ()
  "Transpose two windows.  If more or less than two windows are visible, error."
  (interactive)
  (unless (= 2 (count-windows))
    (error "There are not 2 windows."))
  (let* ((windows (window-list))
         (w1 (car windows))
         (w2 (nth 1 windows))
         (w1b (window-buffer w1))
         (w2b (window-buffer w2)))
    (set-window-buffer w1 w2b)
    (set-window-buffer w2 w1b)))

(global-set-key (kbd "C-x 9") 'transpose-windows)

;; IDE/Programming Stuff
(setq c-default-style "bsd") ; maybe k&r
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(add-hook 'prog-mode-hook (lambda ()
                            (interactive)
                            (setq show-trailing-whitespace 1)))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;;========;;
;;========;;
;;========;;
;;========;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(frame-brackground-mode 'dark)
  '(package-selected-packages
   '(helm-emms org-emms emms-browser emms-info-ogginfo emms-info-mp3info emms highlight-indent-guides list-unicode-display rainbow-mode helm-gtags helm-company helm-org helm multiple-cursors use-package-chords key-chord which-key smartparens try magit use-package undo-tree nyan-mode))
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
