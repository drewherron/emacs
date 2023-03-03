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

(setq package-archives '(("melpa" .  "https://melpa.org/packages/")
                         ("org"   .  "https://orgmode.org/elpa/")
                         ("gnu"   .  "https://elpa.gnu.org/packages/")))

;; Uncomment when you're updating this file less frequently
;(package-refresh-contents)

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

;; Magit
(use-package magit
  :ensure t)

(use-package try
  :ensure t)

;;==========;;
;; Files    ;;
;;==========;;
 
;; Load config files
;;(This section will grow)
(load "~/.config/emacs/org-config/org-capture-templates")

;;===========;;
;; Imp Mode ;;
;;===========;;

;; Download Imp Mode
;(unless (package-installed-p 'imp-mode)
;  (package-install 'imp-mode))

;; Enable Imp Mode
;(require 'imp-mode)
;(imp-mode 1)

;;======;;
;; Undo ;;
;;======;;

(unless (package-installed-p 'undo-tree)
    (package-install 'undo-tree))
(global-undo-tree-mode)

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
(setq org-agenda-files (list "~/org/"))
;(setq org-agenda-files '("projects.org"
;			 "people.org"
;			 "todo.org"
;                         "tickler.org"))

(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
(setq org-refile-use-outline-path 'file)
;; or to specify files, or vary maxlevels:
                                        ;(setq org-refile-targets
                                        ;      '(("projects.org" :maxlevel . 1)
                                        ;        ("people.org" :maxlevel . 2)))

;; Org Journal
(unless (package-installed-p 'org-journal)
  (package-install 'org-journal))
(require 'org-journal)

(setq org-journal-dir (concat org-directory "journal/"))
(setq org-journal-file-type 'yearly)
;(setq org-journal-time-prefix "** ")
;(setq org-journal-date-format "%A, %B %d %Y")


;; Calendar stuff
;(eval-after-load "calendar"
;  `(progn
;     (define-key calendar-mode-map (kbd "RET") 'org-journal-display-entry)
;    ; (define-key calendar-mode-map (kbd "M-[") 'calendar-backward-month)
;     ))
(define-key calendar-mode-map (kbd "RET") 'org-journal-display-entry)

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
(ido-mode t)


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
        '(try magit corfu use-package undo-tree org-journal nyan-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
