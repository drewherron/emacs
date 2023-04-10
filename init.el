;;===========================;;
;;  This Is MY emacs config  ;;
;;  There are many like it   ;;
;;  But this one is mine     ;;
;;  Below this block will go ;;
;;  only things I understand ;;
;;===========================;;

;; Don't show the splash screen
; I'll use this later:
;(setq inhibit-startup-message t)

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

;; Corfu
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.3)
  (corfu-preview-current 'insert)
  (corfu-on-exact-match nil)
  :bind
    (:map corfu-map
        ("TAB"     .  corfu-next)
        ([tab]     .  corfu-next)
        ("S-TAB"   .  corfu-previous)
        ([backtab] .  corfu-previous))
  :hook prog-mode)

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

;; Org
(use-package org
  :ensure t
  :pin gnu
  :custom
  (org-startup-folded t)
  (org-startup-indented t))

;; Org-Roam
(use-package org-roam
  :ensure t
  :after org
  :custom
  (org-roam-directory "~/org/roam/")
  (org-roam-db-location "~/org/roam/roam.db")
  (org-roam-dailies-directory "~/org/log/")

  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         ("C-c n c" . org-roam-capture)
         ("C-c n l" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n e" . org-roam-extract-subtree)
         ("C-c n i" . org-roam-node-insert))
  :config
  (setq org-roam-completion-everywhere t)
  (setq org-roam-completion-system 'ido) 
  (org-roam-db-autosync-mode t)
  ;; (require 'org-roam-protocol)

;(setq org-roam-dailies-capture-templates
;  '(("d" "default" entry
;     (file "~/org/log/template.org")
;     :target (file+head "%<%Y-%m-%d>.org"
;                        "#+title: %<%Y-%m-%d>\n"))))
)
    
;; Smartparens
(use-package smartparens
  :ensure t
  :config
  (setq smartparens-global-mode -1)
  ;(evil-smartparens-keybindings-mode t)
  (show-smartparens-global-mode t)
  (setq sp-show-pair-from-inside t))

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
(load "~/.config/emacs/org-config/org-capture-templates")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;; re-org by M-x customize groups?

(global-display-line-numbers-mode)

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
(global-set-key (kbd "C-c r") 'org-refile)

(setq org-todo-keywords
	'((sequence "TODO" "NEXT" "WAITING" "|"  "DONE")))

(setq org-log-done 'time)

(setq org-directory "~/org/")

;; Org Capture
(setq org-default-notes-file (concat org-directory "~/capture.org"))

(global-set-key (kbd "C-c c") 'org-capture)

;; Org Agenda
(setq org-agenda-files '("~/org/"
                         "~/org/log/"
                         "~/Documents/CS/311/CS311.org"
                         "~/Documents/CS/350/CS350.org"
                         "~/Documents/CS/586/CS586.org"))
;(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
;(setq org-agenda-files '("~/org/projects.org"
;			             "~/org/people.org"
;			             "~/org/todo.org"
;                         "~/org/tickler.org"))

(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
(setq org-refile-use-outline-path 'file)
;; or to specify files, or vary maxlevels:
                                        ;(setq org-refile-targets
                                        ;      '(("projects.org" :maxlevel . 1)
                                        ;        ("people.org" :maxlevel . 2)))

(setq org-agenda-start-day "-3d")
(setq org-agenda-span 10)
(setq org-agenda-start-on-weekday nil)
    
;; Org Journal
;; Moving to roam
;(unless (package-installed-p 'org-journal)
;  (package-install 'org-journal))
;(require 'org-journal)
;
;(setq org-journal-dir (concat org-directory "journal/"))
;(setq org-journal-file-type 'yearly)
;(setq org-journal-time-prefix "** ")
;(setq org-journal-date-format "%A, %B %d %Y")


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
(plist-put org-format-latex-options :scale 2)

;; Ido
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-show-dot-for-dired t)
(ido-mode t)


;; imp mode
;(let ((default-directory "/imp-mode")) 
;    (shell-command "hypercaps.sh"))
;(setq x-hyper-keysym 'hyper)

(defun open-init-file ()
  "Open the Emacs config file."
  (interactive)
  (find-file "~/.config/emacs/init.el"))
    
;;========;;
;;========;;
;;========;;
;;========;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(use-package-chords key-chord which-key org-roam smartparens try magit corfu use-package undo-tree nyan-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
