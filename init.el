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
           (helm-gtags-suggested-key-mapping t))
  :bind (:map helm-gtags-mode-map
        ("C-j" . helm-gtags-select) ; Change this or more of these?
        ("M-." . helm-gtags-dwim)   ; After you get used to gtags
        ("M-," . helm-gtags-pop-stack)
        ("C-c <" . helm-gtags-previous-history)
        ("C-c >" . helm-gtags-next-history)
        ("C-c g a" . helm-gtags-tags-in-this-function)))


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

;; undo-tree
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
      `((,(directory-files "~/org/ref/" t "\\.org$") :maxlevel . 2)
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
 '(custom-safe-themes
   '("8ad09291741c32c9c9bafb2a6cf91d32f63304d9187fb64e003669a3ce593802" "28a34dd458a554d34de989e251dc965e3dc72bace7d096cdc29249d60f395a82" "79a51b6114f2f57ce6e96383c01c0a836210c9b23ed712e863b7ebad6010f3b1" "18624b2da7749af193a4eeaa7be1dc2abe94a97a8562ba69f5ee0f06d6dd156e" "6c2a850ca31ae416c0920cb44283aa8a8c8ec3f9a4e34dadd6b3afa76ce78c12" "76eb683894aa00c77e55f482a23233d0007d59cc47a5653a37ad2897f31330b0" "4363ac3323e57147141341a629a19f1398ea4c0b25c79a6661f20ffc44fdd2cb" "5fdc0f5fea841aff2ef6a75e3af0ce4b84389f42e57a93edc3320ac15337dc10" "062e6ec918ed89d5d9a342dbbefd99e8690c5514c6698a78fc25f259972e9242" "ac557db1a7c09e61798b356e0af7aa9cba281b95deb76c8fa0765de761dae5a8" "02591317120fb1d02f8eb4ad48831823a7926113fa9ecfb5a59742420de206e0" "ba4ab079778624e2eadbdc5d9345e6ada531dc3febeb24d257e6d31d5ed02577" "9584533e7ca091a59f88e7f2acc2f8ce9124753d7b82aad6d4526ccf77787975" "ae823a87daa15b90a193830aa7edd34454be68a0dfb52e8316d9280a5aeeb51a" "2db9c83380f626b24a0ba7a1dd9972b72ec3e5ce9e58892350d7188106e0e114" "4c7a1f0559674bf6d5dd06ec52c8badc5ba6e091f954ea364a020ed702665aa1" "47610f9d6af7e30fbfb52fffe6de4c7de299792a7f0d09192a5b2b593c18931b" "e068203104e27ac7eeff924521112bfcd953a655269a8da660ebc150c97d0db8" "0c860c4fe9df8cff6484c54d2ae263f19d935e4ff57019999edbda9c7eda50b8" "37c8c2817010e59734fe1f9302a7e6a2b5e8cc648cf6a6cc8b85f3bf17fececf" default))
 '(package-selected-packages
   '(highlight-indent-guides list-unicode-display rainbow-mode helm-gtags helm-company helm-org helm multiple-cursors use-package-chords key-chord which-key smartparens try magit use-package undo-tree nyan-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
