;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
(setq use-package-ensure-function 'ignore)

(setq inhibit-startup-screen t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;;; Fonts
;; 5/30/22: https://www.reddit.com/r/emacs/comments/pc189c/fonts_in_emacs_daemon_mode/
;; 6/1/22: This setup correctly sets fonts on system boot
(defun jf/setup-font-faces ()
  "Setup all customacs font faces."
  ;; ;;re-disable GUI stuff we don't care about
  ;; (push '(menu-bar-lines . 0) default-frame-alist)
  ;; (push '(tool-bar-lines . 0) default-frame-alist)
  ;; (push '(vertical-scroll-bars) default-frame-alist)
  (defvar jf/default-font-size 100)
  (defvar jf/default-variable-font-size 100)
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font "Iosevka Custom" :height jf/default-font-size)
    (set-face-attribute 'fixed-pitch nil :font "Iosevka Custom" :height jf/default-font-size)
    ;; TODO Install Iosevka Aile
    ;; (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height jf/default-variable-font-size)
    ))

;; run this hook after we have initialized the first time
(add-hook 'after-init-hook 'jf/setup-font-faces)
;; re-run this hook if we create a new frame from daemonized Emacs
(add-hook 'server-after-make-frame-hook 'jf/setup-font-faces)

;; org-mode
(setq org-use-extra-keys t)

(use-package nix-mode
  :hook (before-save . nix-format-before-save))

;;; Inits
;; Code:
;; Heaviliy inspired by https://github.com/daviwil/emacs-from-scratch/blob/master/Emacs.org
;;;; Speedup Blub
;; https://www.reddit.com/r/emacs/comments/qw52na/emacs_271_earlyinit_file/hl0oo31/
(setq default-gc-threshold gc-cons-threshold
      default-gc-percentage gc-cons-percentage)

(setq gc-cons-threshold most-positive-fixnum
      default-gc-percentage 0.8)

(setq load-prefer-newer t)

(defun jf/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.4f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'jf/display-startup-time)

;;;; Packages + Repos
;; Initialize package sources

(setq use-package-always-ensure t)

;;;;; auto-package-update
;; (use-package auto-package-update
;;   :config
;;   (setq auto-package-update-delete-old-versions t)
;;   (setq auto-package-update-hide-results t)
;;   (setq auto-package-update-prompt-before-update t)
;;   (setq auto-package-update-show-preview t)
;;   (auto-package-update-maybe)
;;   (auto-package-update-at-time "14:00"))

;;;; Set up path
(use-package exec-path-from-shell
  :init ;; (when (memq window-system '(mac ns x pgtk))
	;;   (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)
    (setenv "DISPLAY" ":0.0") 		; used with emacsclients to open pdfs in zathura
    ))
;;;; auto-save
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;;;; yes-or-no QoL
(defalias 'yes-or-no-p 'y-or-n-p)

;;;; emacsclient QoL
(defun save-buffers-kill-terminal--safe ()
  "Based off 'save-buffers-kill-emacs'.
Wrapper around 'save-buffers-kill-terminal.
Meant to be used with emacsclient'"
  (interactive)
  (let ((confirm confirm-kill-emacs))
    (and (or (null confirm)
	     (funcall confirm "Exit this emacsclient?"))
	 (save-buffers-kill-terminal))))

(use-package files
  :ensure nil
  :init (setq confirm-kill-emacs 'y-or-n-p)
  :bind (("C-x C-c" . #'save-buffers-kill-terminal--safe)))


;;; Misc
(blink-cursor-mode -1)
(setq scroll-conservatively 101)
(delete-selection-mode 1)

(electric-pair-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(use-package emacs
  :ensure nil
  :config
  (setq initial-major-mode 'org-mode)

  (setq initial-scratch-message ";; This buffer is for text that is not saved, and for org-mode usage.

#+begin_src elisp
(+ 1 1)
#+end_src

"))

;;; Usability
;;;; Vertico
(defun jf/vertico--backward-updir ()
  "Delete char before or go up directory for file cagetory completions.
https://github.com/minad/vertico/issues/65#issuecomment-875094896"
  (interactive)
  (let ((metadata (completion-metadata (minibuffer-contents)
                                       minibuffer-completion-table
                                       minibuffer-completion-predicate)))
    (if (and (eq (char-before) ?/)
             (eq (completion-metadata-get metadata 'category) 'file))
        (let ((new-path (minibuffer-contents)))
          (delete-region (minibuffer-prompt-end) (point-max))
          (insert (abbreviate-file-name
                   (file-name-directory
                    (directory-file-name
                     (expand-file-name new-path))))))
      (call-interactively 'backward-delete-char))))
;; Make ESC quit prompts
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package vertico
  :bind (:map vertico-map
	      ("DEL" . jf/vertico--backward-updir))
  :init
  (setq enable-recursive-minibuffers t)
  (vertico-mode))

;;;; Savehist
(use-package savehist
  :init
  (savehist-mode))
;;;; Marginalia
(use-package marginalia
  :init
  (marginalia-mode))
;;;; Orderless
;; https://github.com/minad/vertico#configuration
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))
	)
  (setq orderless-matching-styles '(orderless-regexp)))

;;;; Embark
(use-package embark
  :bind (("C-." . embark-act)
	 ("C-;" . embark-dwim)
	 ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command)
  :config  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult))

;;;; Consult
(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)	   ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)		   ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-/" . consult-register-load)
         ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
         ("M-\"" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)     ;; orig. yank-pop
         ("<help> a" . consult-apropos) ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)   ;; orig. goto-line
         ("M-g M-g" . consult-goto-line) ;; orig. goto-line
         ("M-g o" . consult-outline)     ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
	 ("M-s t" . (lambda () (interactive) (consult-line (hl-todo--regexp))))
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
	 ("M-g f" . consult-flymake))

  :init

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Configure project.el (project-roots)
  ;; To return the project root directory.
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

;;;; Which-key
(use-package which-key
  :init
  (which-key-mode))
;;;; Completion: Corfu
(use-package corfu
  ;; All from README
  :custom
  (corfu-separator ?\s)	;; Orderless field separator
  (corfu-scroll-margin 1) ;; Use scroll margin

  :init
  (global-corfu-mode)
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
		(bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1))

;;; Window Management
;;;; Ace-window
(use-package ace-window
  :bind* (("M-o" . 'ace-window))
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil)
  (setq aw-display-mode-overlay nil)
  (setq aw-scope 'frame)
  (defun jf/ace-window-display--helper (left-delim right-delim)

    (let ((pred (and ace-window-display-mode (< 2 (count-windows)))))
      (when pred
	(concat left-delim (window-parameter (get-buffer-window) 'ace-window-path) right-delim ))))
  ;; (add-hook 'ace-window-display-mode-hook #'jf/ace-window-message--helper)
  )

(defun jf/window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows.
https://emacs.stackexchange.com/a/5372"
  (interactive)
  ;; 11/10/22
  ;; trying to preserve ratio of either height or width
  ;; split-window-vert/horiz can take a char count
  ;; use window-full-height-p to determine current split
  ;; (let* ((ww (window-width))
  ;; 	 (fw (frame-width))
  ;; 	 (wh (window-height))
  ;; 	 (fh (- (frame-height) 1))
  ;; 	 (ratio (cond ((= ww fw) (/ wh (float fh)))
  ;; 		      ((= wh fh) (/ ww (float fw)))
  ;; 		      (t (error "Window size issue."))))))
  (cond ((> (length (window-list)) 2)
	 (error "Can't toggle with more than 2 windows!"))
	((= (length (window-list)) 1)
	 (error "Can't toggle only 1 window!"))
	(t
	 (let ((func (if (window-full-height-p)
			 #'split-window-vertically
		       #'split-window-horizontally)))
	   (delete-other-windows)
	   (funcall func)
	   (save-selected-window
	     (other-window 1)
	     (switch-to-buffer (other-buffer)))))))

;;; Visual Interface
(use-package simple
  :ensure nil
  :init (setq-default fill-column 100)
  (global-visual-line-mode)
  :bind ("C-c j" . join-line))

;;;; Nano-modeline
(use-package nano-modeline
  :after (ace-window)
  :functions jf/ace-window-display--helper
  :init (setq nano-modeline-position 'nano-modeline-header)
  (ace-window-display-mode 1)

  :config
  ;; Color logic implemented in modus-themes section
  (defface nano-modeline-status-** '((t (:inherit nano-modeline-status))) "")
  (add-to-list 'nano-modeline-faces '(status-**-active . (nano-modeline-status-**)))
  (add-to-list 'nano-modeline-faces '(status-**-inactive . (nano-modeline-status-**)))

  (defface nano-modeline-status-RW `((t (:inherit nano-modeline-status))) "")
  (add-to-list 'nano-modeline-faces '(status-RW-active . (nano-modeline-status-RW)))
  (add-to-list 'nano-modeline-faces '(status-RW-inactive . (nano-modeline-status-RW)))

  (defun jf/nano-modeline-ace-display (left-delim right-delim)
    "Extract window selecter chracter and wrap in LEFT-DELIM and RIGHT-DELIM.
Discovered window-parameter from https://oremacs.com/2015/03/12/ace-window-display-mode/."
    (let ((pred (and ace-window-display-mode (< 2 (count-windows)))))
      (when pred
	(propertize
	 (concat left-delim
		 (substring-no-properties (window-parameter (selected-window) 'ace-window-path))
		 right-delim)
	 'face (nano-modeline-face 'name)))))

  (defun jf/nano-modeline-my-mode (&optional default)
    (funcall nano-modeline-position
             '((nano-modeline-buffer-status) " "
               (nano-modeline-buffer-name) " "
               (nano-modeline-git-info "") " "
	       (jf/nano-modeline-ace-display "{" "}"))
             '((nano-modeline-date nil "%a %y%m%d -") " "
               (nano-modeline-cursor-position)
	       (nano-modeline-window-dedicated))
             default))

  (jf/nano-modeline-my-mode t)
  (setq-default mode-line-format nil))

;;;; Modus-themes
(use-package modus-themes
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
	modus-themes-bold-constructs nil
	modus-themes-syntax '()
	modus-themes-prompts '()
	modus-themes-region '(bg-only no-extend)
	modus-themes-mode-line '()
	modus-themes-subtle-line-numbers t
	modus-themes-org-blocks 'tinted-background
	modus-themes-headings '((1 . (rainbow overline background))
				(t . (rainbow overline no-bold)))
	modus-themes-completions '((matches . (intense))
				   (selection . (accented intense))
				   (popup . (accented))))

  (defun jf/modus-themes-custom-faces ()
    ;; [ status | name (primary) secondary ]
    ;; 230101: I believe these inherit from mode-line-active
    (modus-themes-with-colors
      (custom-set-faces
       `(nano-modeline-active ((,c)))
       `(nano-modeline-status-** ((,c :background ,bg-red-intense)))
       `(nano-modeline-status-RW ((,c :background ,bg-blue-intense)))
       `(magit-diff-context-highlight ((,c :background ,bg-cyan-nuanced)))

       `(window-divider ((,c :background ,bg-main :foreground ,border)))
       `(fringe ((,c :background ,fringe :foreground ,fg-main))))

      `(setq hl-todo-keyword-faces
	     '(("TODO" . ,red-intense)
	       ("HOLD" . ,bg-yellow-intense)
	       ("DONE" . ,green-intense)
	       ("HACK" . ,blue-intense)
	       ("NEXT" . ,magenta-cooler)
	       ("NOTE" . ,cyan-intense)))
      `(setq org-todo-keyword-faces
	     '(("TODO" . (:foreground ,red-intense :weight bold))
	       ("HOLD" . (:foreground ,bg-yellow-intense :weight bold))
	       ("DONE" . (:foreground ,green-intense :weight bold))
	       ("NEXT" . (:foreground ,magenta-cooler :weight bold)))))

    (hl-todo-mode -1)
    (hl-todo-mode 1))

  ;; https://protesilaos.com/modus-themes/#h:1487c631-f4fe-490d-8d58-d72ffa3bd474
  (add-hook 'modus-themes-after-load-theme-hook #'jf/modus-themes-custom-faces)

  (modus-themes-load-theme 'modus-operandi)

  :bind
  ("<f5>" . #'modus-themes-toggle))

;;;; window-divider
(use-package frame
  :ensure nil
  :init (setq window-divider-default-places 'bottom-only)
  (setq window-divider-default-bottom-width 1)
  (window-divider-mode))
;;; Fonts
;; fonts are set in early-init

;;; Hydra
(use-package hydra
  :init
  ;; (defhydra hydra-text-scale (:timeout 4)
  ;;   "scale text"
  ;;   ("j" text-scale-increase "in")
  ;;   ("k" text-scale-decrease "out")
  ;;   ("f" nil "finished" :exit t))
  )

;;; Org-mode
(use-package engrave-faces)
(defun mk/ignore-builtin (pkg)
  (assq-delete-all pkg package--builtins)
  (assq-delete-all pkg package--builtin-versions))

(mk/ignore-builtin 'org)
(use-package org
  ;; https://github.com/jwiegley/use-package/issues/955
  ;; does not work for built-ins
  ;; :pin gnu

  ;; Putting accessory org packages (in /custom/) before the main org package
  ;; will autoload the older version required by org-roam Setting the load-path
  ;; forces the new one to load

  :load-path "elpa/org-9.6"

  :init
  ;; (add-hook 'org-mode-hook #'rainbow-delimiters-mode)

  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  :config ;; (setq org-ellipsis "﻿")
  (setq org-ellipsis " »")
  (setq org-edit-src-content-indentation 0)
  (setq org-preview-latex-default-process 'dvisvgm) ;https://stackoverflow.com/a/43943452
  (plist-put org-format-latex-options :scale 1.1)
  (plist-put org-format-latex-options :foreground nil)
  (plist-put org-format-latex-options :background nil)
  ;; 'auto currently inherits from hl-line
  ;; can add a hook to modus-themes-toggle

  ;; global version of:
  ;; :PROPERTIES:
  ;; :COOKIE_DATA: recursive todo
  ;; :END:
  (setq org-hierarchical-todo-statistics nil)

  ;; makes any internal file link open in same window
  ;; defualt value is find-file-other-window
  ;; changed for use in org-roam
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (setq org-return-follows-link t)

  ;; agenda
  ;; https://github.com/rougier/emacs-gtd
  (setq org-agenda-files (list "~/HarmonyAnalytics/Inbox.org"
			       "~/HarmonyAnalytics/Notes.org"
			       "~/HarmonyAnalytics/Projects.org"))
  (setq org-capture-templates
	`(("i" "Inbox" entry  (file "~/HarmonyAnalytics/Inbox.org")
           ,(concat "* TODO %?\n"
                    "/Entered on/ %U"))
	  ("I" "Inbox" entry  (file "~/HarmonyAnalytics/Inbox.org")
           ,(concat "* TODO Input \"%?\" Notes\n"
                    "/Entered on/ %U"))))
  (defun jf/org-capture-inbox ()
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "i"))
  (defun jf/org-capture-inbox-note ()
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "I"))
  (setq org-refile-targets `(("~/HarmonyAnalytics/Projects.org" :regexp . ,(regexp-opt '("Tasks")))))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)


  (setq org-use-fast-todo-selection 'expert)
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "DISC(z)")))
  ;; org-todo-keyword-faces set in jf/modus-themes-custom-faces
  (setq org-log-done 'time)

  (setq org-priority-highest 1
	org-priority-default 3
	org-priority-lowest 5)

  (setq org-agenda-span 5)
  (setq org-agenda-time-grid
	(quote
	 ((daily today require-timed)
          (600 800 1000 1200 1400)
          "  ⊢" "――――")))
  (setq org-agenda-sorting-strategy
	'((agenda habit-down time-up priority-down category-keep)
	  (todo priority-down category-keep)
	  (tags priority-down tag-up category-keep)
	  (search category-keep)))

  (setq org-agenda-custom-commands
	'(("g" "Get Things Done (GTD)"
           ((agenda ""
                    (;; (org-agenda-skip-function
                     ;;  '(org-agenda-skip-entry-if 'deadline))
                     (org-deadline-warning-days 0)))
            (todo "NEXT"
                  (;; (org-agenda-skip-function
                   ;;  '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nNext Tasks\n")))
	    (todo "HOLD"
                  (;; (org-agenda-skip-function
                   ;;  '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "On Hold\n")))
            ;; (agenda nil
            ;;         ((org-agenda-entry-types '(:deadline))
            ;;          ;; (org-agenda-format-date "")
            ;;          (org-deadline-warning-days 7)
	    ;; 	     ;; https://github.com/rougier/emacs-gtd/issues/21
	    ;; 	     (org-agenda-show-all-dates nil)
            ;;          (org-agenda-skip-function
            ;;           '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
            ;;          (org-agenda-overriding-header "\nDeadlines")))
            (tags-todo "projects"
                       ((org-agenda-skip-function
			 '(org-agenda-skip-entry-if 'todo '("NEXT" "HOLD")))
			(org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "\nProjects\n")))
	    (tags-todo "+inbox+TODO=\"TODO\""
                       ((org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "\nInbox\n")))
            ;; (tags "+CLOSED>=\"<today>\""
            ;;       ((org-agenda-overriding-header "\nCompleted today\n")))
	    ;; 230105: for some reason <today> isn't working
	    ;; https://www.reddit.com/r/emacs/comments/75nkj6/orgmode_tasks_closed_yesterday/do85ili/
	    (tags (concat "+TODO=\"DONE\"+CLOSED>="
			  (prin1-to-string (format-time-string "[%Y-%m-%d]" (current-time))))
                  ((org-agenda-overriding-header "\nCompleted today\n")))
	    ))
	  ("w" "Completed tasks from previous week"
	   ((agenda "" ((org-agenda-span 7)
			(org-agenda-start-day nil)
			(org-agenda-skip-function
			 '(org-agenda-skip-entry-if 'nottodo '("DONE")))
			(org-agenda-prefix-format "  %? s")
			(org-agenda-show-log t)))))))

  ;; make mastodon and org agenda bookmark-able to some degree
  ;; https://github.com/alphapapa/activities.el/issues/19#issuecomment-1925700528
  ;; alternatively, look into replacing agenda with org-ql
  (defun jf/agenda-bookmark (_bookmark)
    (org-agenda nil "g"))

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (setq-local bookmark-make-record-function
                          (lambda ()
                            `((handler . jf/agenda-bookmark))))))

  :bind
  (("C-c a" . #'org-agenda)
   ("C-c c" . #'org-capture)
   ("C-c i" . #'jf/org-capture-inbox)
   ("C-c I" . #'jf/org-capture-inbox-note)
   ;; View closed tasks from last Monday to now
   ;; superseded by org-agenda w
   ;; ("C-c w" . (lambda () (interactive)
   ;; 		(org-tags-view nil
   ;; 			       (concat "+TODO=\"DONE\"+CLOSED>="
   ;; 				       (prin1-to-string (substring
   ;; 							 (shell-command-to-string "date -dlast-monday +[%Y-%m-%d]") 0 -1))))))
   :map org-mode-map
   ("$" . #'math-delimiters-insert)
   ("C-c C-x d" . #'org-metadown)
   ;; org-agenda-mode-map is not loading on startup for some reason
   ;; :map org-agenda-mode-map
   ;; ("g" . #'org-agenda-redo)
   ;; ("r" . #'org-agenda-redo-all)
   )

  :hook
  (org-mode . org-indent-mode)
  (org-agenda-mode . (lambda () (buffer-face-set :height 120))))

(use-package org-fragtog
  :init
  (add-hook 'org-mode-hook 'org-fragtog-mode))

;; (use-package org-modern
;;   :config
;;   (setq org-modern-hide-stars 'leading
;; 	org-modern-horizontal-rule nil ;; not sure what this is
;; 	org-modern-internal-target '(" ↪ " nil " ")
;; 	org-modern-keyword "≽"
;; 	org-modern-priority nil
;; 	org-modern-progress nil
;; 	org-modern-radio-target nil
;; 	org-modern-star nil
;; 	org-modern-statistics nil
;; 	org-modern-table-horizontal 10 ;; this is not centered
;; 	org-modern-table-vertical 2
;; 	org-modern-tag nil
;; 	org-modern-todo nil)
;;   ;; :hook
;;   ;; (org-mode . org-modern-mode)
;;   (global-org-modern-mode)
;;   )

;;;; Org Export - LaTeX
;; https://blog.tecosaur.com/tmio/2022-05-31-folding.html#-engraved-source

;; using https://wiki.archlinux.org/title/TeX_Live#tllocalmgr

;; Check on alternative: https://www.reddit.com/r/emacs/comments/lbkmmz/the_best_syntax_highlighting_in_a_pdf_youll_see_a/
(setq org-latex-src-block-backend 'engraved)

(setq org-latex-compiler "xelatex")

;; Need to run multiple times to collapse any missed references
;; https://tex.stackexchange.com/a/38076
;; (setq org-latex-pdf-process '("latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f -shell-escape"
;; 			      "latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f -shell-escape"
;; 			      "latexmk -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f -shell-escape"))

;; fontspec needs xelatex or lualatex
;; https://tex.stackexchange.com/questions/264461/xelatex-minted-code-block-represents-tabs-as-i
;; -8bit because minted blocks insert a '^^I' on line break indents
;; look at "breaksymbolleft" in minted manual for more.
(setq org-latex-pdf-process '("latexmk -f -pdfxe -xelatex=\"xelatex -8bit\" -interaction=nonstopmode -output-directory=%o %f -shell-escape"
			      "latexmk -f -pdfxe -xelatex=\"xelatex -8bit\" -interaction=nonstopmode -output-directory=%o %f -shell-escape"
			      "latexmk -f -pdfxe -xelatex=\"xelatex -8bit\" -interaction=nonstopmode -output-directory=%o %f -shell-escape"))

;; for some reason, setting fonts does not work when in org-latex-classes
;; need to add manual LATEX_HEADER
;; #+LATEX_HEADER: \usepackage{fontspec}
;; #+LATEX_HEADER: \setmainfont[]{IBM Plex Sans}
;; #+LATEX_HEADER: \setmonofont[]{Iosevka Custom}

;; TODO: Remove minted after, since using engraved-faces
(setq org-latex-classes '(("article" "\\documentclass[11pt]{article}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                          ("report" "\\documentclass[11pt]{report}"
                           ("\\part{%s}" . "\\part*{%s}")
                           ("\\chapter{%s}" . "\\chapter*{%s}")
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                          ("book" "\\documentclass[11pt]{book}"
                           ("\\part{%s}" . "\\part*{%s}")
                           ("\\chapter{%s}" . "\\chapter*{%s}")
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                          ("notes"
                           "\\documentclass[8pt]{article}
  \\usepackage[letterpaper, portrait, margin=1in]{geometry}
  \\usepackage[utf8]{inputenc}
  \\usepackage[T1]{fontenc}
  \\usepackage{amsmath}
  \\usepackage{amssymb}
  \\usepackage{hyperref}
  \\hypersetup{colorlinks=true}
% autoloaded with engraved-faces:
%   \\usepackage{tcolorbox}
% http://tug.ctan.org/macros/latex/contrib/minted/minted.pdf
  \\usepackage[cache=false]{minted}
  \\setminted{breaklines=true, breakanywhere=true}
% \\usemintedstyle{paraiso-light} % pygmentize -L styles
% \\usemintedstyle{emacs} % pygmentize -L styles
% \\usemintedstyle{colorful} % pygmentize -L styles
% \\usemintedstyle{rainbow_dash} % pygmentize -L styles
  \\usemintedstyle{tango} % pygmentize -L styles
% https://tex.stackexchange.com/questions/112559/box-around-minted-environment
% https://ctan.math.utah.edu/ctan/tex-archive/macros/latex/contrib/tcolorbox/tcolorbox.pdf
  \\BeforeBeginEnvironment{minted}{\\begin{tcolorbox}[colframe=black!85!white, colback=black!5!white, boxrule=0.3mm]}
  \\AfterEndEnvironment{minted}{\\end{tcolorbox}}
  \\usepackage{enumitem}
  \\setitemize{itemsep=0.5pt}
  \\usepackage{fancyhdr}
  \\pagestyle{fancy}
  \\fancyhf{}
  \\usepackage{titling} % allows \thetitle \theauthor \thedate
  \\usepackage{lastpage}
  \\rhead{\\theauthor}
  \\lhead{\\thetitle}
% Disable pageref being a link https://tex.stackexchange.com/a/4599
  \\rfoot{\\thepage{} of \\pageref*{LastPage}}
  \\linespread{1}
  \\setlength{\\parindent}{0pt}
  \\setlength{\\parskip}{0.5em plus 0.1em minus 0.2em}
  \\hypersetup{pdfborder=0 0 0}
  \\setcounter{secnumdepth}{0}
%  \\usepackage{fontspec}
%  \\setmainfont[]{IBM Plex Sans}
%  \\setmonofont[]{Iosevka Custom}
"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
;;; Denote
(defvar prot-dired--limit-hist '()
  "Minibuffer history for `prot-dired-limit-regexp'.")

;;;###autoload
(defun prot-dired-limit-regexp (regexp omit)
  "Limit Dired to keep files matching REGEXP.

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
  (interactive
   (list
    (read-regexp
     (concat "Files "
             (when current-prefix-arg
               (propertize "NOT " 'face 'warning))
             "matching PATTERN: ")
     nil 'prot-dired--limit-hist)
    current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (unless omit (dired-toggle-marks))
  (dired-do-kill-lines))

(use-package denote
  :config
  (setq denote-file-type 'org 		; same as default
	denote-directory (substitute-in-file-name "$HOME/denote-notes")
	denote-dired-directories (list denote-directory)
	denote-prompts '(title template keywords)
	)
  (setq denote-templates '((reference . "TEXT\n\n\n* Resources\n - LINK")
			   (journal . "Today ")
			   (thoughts . "I ")))

  (setq xref-search-program 'ripgrep)	; denote-link-backlinks uses xref

  (setq denote-excluded-directories-regexp "docs\\|journal\\|scripts\\|bibliography\\|ltximg")

  :bind (("C-c n l" . #'denote-link-or-create) ; safe link insertion
	 ("C-c n f" . #'denote-open-or-create) ; safe note creation
	 ("C-c n b" . #'denote-backlinks)
	 ("C-c n s" . #'denote-find-link) ; open links in buffer
	 ("C-c n r" . #'prot-dired-limit-regexp)
	 ("C-c n c" . #'denote-rename-file-using-front-matter) ; update file name when modifying front-matter
	 ("C-c n d" . (lambda () (interactive) (progn (dired denote-directory) (dired-hide-details-mode))))
	 ("C-c n j" . #'denote-journal-extras-new-or-existing-entry))
  :hook
  (dired-mode . denote-dired-mode-in-directories))

;; (use-package denote-org-dblock
;;   :after denote
;;   :ensure nil
;;   ;; TODO 231211 figure out how to not hard code this (locate-library "denote")
;;   :load-path "~/.config/emacs-configs/emacs.d-my-vanilla/elpa/denote-2.0.0")

(use-package denote-journal-extras
  :after denote
  :ensure nil
  :config
  (setq denote-journal-extras-title-format "%y%m%d"))

(defun jf/export-denote-directory ()
  "Rudimentary export from org to html.
org-export properly converts denote links."
  (let ((default-directory denote-directory)
	(files (--filter (string= (file-name-extension it) "org")
			 (directory-files denote-directory))))
    (dolist (file files)
      (with-current-buffer
	  (find-file-noselect file)
	(org-html-export-to-html)))))
;; backlinks buffers autogenerates relative filenames of backlinks
;; need to process these filenames into correct links
;; then org-export can properly handle these links
;;
;; TODO Need a way to create an index.html, and possible backlinks.
;; For backlinks, could inspect button property of buffer, then concat to end of original note.
;; create link with proper format:
(defun jf/denote-file-to-link (file)
  (let* ((formatter (denote-link--file-type-format file nil)))
  (denote-link--format-link file formatter)))

;; (jf/denote-file-to-link "20221001T180256--denote-scripting__emacs.org")
;; do this over each line of backlinks buffer


;; ;; or can override backlink creation: modified from denote-link-backlinks
;; ;; mapping over directory, we know file name
;; (let ((file "20221001T180256--denote-scripting__emacs.org"))
;;   (when (denote-file-is-writable-and-supported-p file)
;;     (let* ((id (denote-retrieve-filename-identifier file))
;;            (file-type (denote-filetype-heuristics file))
;;            (title (denote-retrieve-title-value file file-type)))
;;       ;; denote--retrieve-process-grep pulls all backlinks
;;       (if-let ((files (denote--retrieve-process-grep id)))
;; 	;; only need to override one line
;;         ;; (denote-link--prepare-backlinks id files title)
;; 	  (mapcar 'jf/denote-file-to-link files)
;;         ;; need to insert "No Backlinks" in this case
;;         (user-error "No links to the current note")))))
;; returns list of links as strings
;; one of the return elements is self
;; because denote--retrive-process-grep deletes (buffer-file-name) from xref'd list
;; so maybe just use full path

;; (defun jf/denote-get-backlinks-as-links (file)
;;   (when (denote-file-is-writable-and-supported-p file)
;;     (let ((id (denote-retrieve-filename-identifier file)))
;;       ;; denote--retrieve-process-grep pulls all backlinks
;;       (if-let ((backlink-files
;; 		(seq-filter
;; 		 #'denote-file-is-note-p
;; 		 (delete file (denote--retrieve-files-in-xrefs
;; 			       (denote--retrieve-xrefs id))))))
;; 	  (mapcar 'jf/denote-file-to-link backlink-files)
;;         '("No backlinks to the current note")))))
;; (jf/denote-get-backlinks-as-links "/home/jonat/denote-notes/20221001T013008--test__emacs.org")
;; (jf/denote-get-backlinks-as-links "/home/jonat/denote-notes/20221001T022053--another__politics.org")
;; (jf/denote-get-backlinks-as-links "/home/jonat/denote-notes/20221001T180256--denote-scripting__emacs.org")

;; need to temp insert, then export

;; (defun jf/export-denote-directory-backlinks ()
;;   (let
;;       ((default-directory denote-directory)
;;        (files (--filter (string= (file-name-extension it) "org")
;; 			(directory-files denote-directory t))))
;;     (dolist (file files)
;;       (let ((backlinks (jf/denote-get-backlinks-as-links file)))
;; 	(with-current-buffer
;; 	    (find-file-noselect file)

;; 	  (save-excursion
;; 	    (goto-char (point-max))
;; 	    (insert "\n\n* Backlinks\n")
;; 	    (insert (mapconcat (lambda (x) (format "- %s \n" x)) backlinks))
;; 	    (org-html-export-to-html)
;; 	    (revert-buffer nil t)))))))

;; > mv ~/denote-notes/*.html ~/denote-notes/html

;; ;; could even modify denote-link--prepare-backlinks
;; ;; in the lines:
;;       ;; (mapc (lambda (f)
;;       ;;         (insert (denote-get-file-name-relative-to-denote-directory f))
;;       ;;         (make-button (line-beginning-position) (line-end-position) :type 'denote-link-backlink-button)
;;       ;;         (newline))
;;       ;;       files)
;; ; need to handle relative links, from source to denote-directory
;; ;; denote-get-file-name-relative-to-denote-directory handles from backlink to denote-directory
;; can't use denote-link--prepare-backlinks, since it takes in some id, and the id's generated backlink files
;; generation is done in denote-link-backlinks

;; ;; actually, denote-link-button-action is defined as #'find-file-other-window
;; ;; since default-directory of backlinks buffer is denote-directory

;; ;; Use something like this to temporarily insert backlinks, then export, then revert to never write file
;; (save-excursion
;;  (goto-char (point-max))
;;  (insert "\n\nhello this is a test")
;;  (org-html-export-to-html)
;;  (revert-buffer nil t))

;;; citar
(use-package citar
  :config
  (setq citar-notes-paths (list denote-directory))
  :custom
  (citar-bibliography '("~/denote-notes/bibliography/my_library.bib"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

;;; citar-denote
(use-package citar-denote
  :config (citar-denote-mode))

;;; Org-transclusion
(use-package org-transclusion
  :bind ("<f8>" . org-transclusion-add))

;;; Org-babel
(use-package org
  :ensure nil
  :config (org-babel-do-load-languages
	   'org-babel-load-languages '((R . t)
				       (shell . t)
				       (python . t)
				       (julia . t)))
  (setq org-confirm-babel-evaluate nil))

;;; pdf-tools
(defun jf/zathura-current-pdf ()
  (interactive)
  (call-process "zathura" nil 0 nil (buffer-file-name)))

(use-package pdf-tools
  :init
  (setq pdf-view-midnight-colors '("#ffffff" . "#000000"))
  (setq pdf-view-use-scaling t)
  (pdf-tools-install)
  :hook
  (pdf-view-mode . auto-revert-mode)
  :bind (:map pdf-view-mode-map
	      ("C-c o" . #'jf/zathura-current-pdf)))

;;; AucTeX
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura"))
  (setq tex-chktex-program nil)
  :hook
  (LaTeX-mode . (lambda () (flycheck-mode -1))))

;;; elfeed (w/ arxiv)
(defun concatenate-authors (authors-list)
  "Given AUTHORS-LIST, list of plists; return string of all authors concatenated.
https://cundy.me/post/elfeed/"
  (mapconcat
   (lambda (author) (car (last (split-string ; get last names
				(plist-get author :name)))))
   authors-list ", "))

(defun my-search-print-fn (entry)
  "Print ENTRY to the buffer."
  (let* ((elfeed-search-date-format '("%m-%d" 5 :left))
	 (date (elfeed-search-format-date (elfeed-entry-date entry)))
	 (title (or (elfeed-meta entry :title)
		    (elfeed-entry-title entry) ""))
	 (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
	 (feed (elfeed-entry-feed entry))
	 (feed-title
	  (when feed
	    (or (elfeed-meta feed :title) (elfeed-feed-title feed))))

	 (entry-authors (concatenate-authors
			 (elfeed-meta entry :authors)))
	 (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
	 (tags-str (mapconcat
		    (lambda (s) (propertize s 'face
					    'elfeed-search-tag-face))
		    tags ","))
	 (title-width (- (window-width) 10
			 elfeed-search-trailing-width))
	 (title-column (elfeed-format-column
			title 100
			;; (elfeed-clamp
			;;  elfeed-search-title-min-width
			;;  title-width
			;;  elfeed-search-title-max-width)
			:left))
	 (authors-width 35)
	 (authors-column (elfeed-format-column
			  entry-authors (elfeed-clamp
					 elfeed-search-title-min-width
					 authors-width
					 35)
			  :left)))

    (insert (propertize date 'face 'elfeed-search-date-face) " ")

    (insert (propertize title-column
			'face title-faces 'kbd-help title) " ")

    (insert (propertize authors-column
			'face 'elfeed-search-date-face
			'kbd-help entry-authors) " ")

    ;; (when feed-title
    ;;   (insert (propertize entry-authors
    ;; 'face 'elfeed-search-feed-face) " "))

    ;; (when entry-authors
    ;;   (insert (propertize feed-title
    ;; 			  'face 'elfeed-search-feed-face) " "))

    (when tags
      (insert "(" tags-str ")"))))



(use-package elfeed
  ;; https://www.reddit.com/r/emacs/comments/ufvg93/my_phd_research_workflowemacs_inside/
  :init
  (defun make-arxiv-url (category)
    "https://arxiv.org/category_taxonomy."
    (let ((base "http://export.arxiv.org/api/query?search_query=cat:")
	  ;; (cat "cond-mat.mtrl-sci")
	  (range "&start=0&max_results=100&sortBy=submittedDate&sortOrder=descending")
	  (sort-by "&sortBy=submittedDate")
	  (sort-order "&sortOrder=descending"))
      (concat base category range sort-by sort-order)))

  ;; TODO can use ar5iv for html render (view in browser)
  ;; Usually lags behind a few months
  ;; Can do locally with https://github.com/dginev/ar5ivist
  ;; https://ar5iv.labs.arxiv.org/html/2302.00011
  (defun jf/download-arxiv ()
    "Download pdf of current elfeed-entry."
    (interactive)
    (let* ((url (elfeed-entry-link elfeed-show-entry))
	   (pdf-url (s-replace "abs" "pdf" url))
	   (id (car (last (s-split "/" url))))
	   (pdf-path (concat arxiv-dir id ".pdf")))
      (if (file-exists-p pdf-path)
	  (find-file pdf-path)
	(progn (url-copy-file pdf-url pdf-path)
	       (find-file pdf-path)))))
  :config (setq elfeed-feeds `(
			       (,(make-arxiv-url "cond-mat.mtrl-sci") matsci)
			       (,(make-arxiv-url "cond-mat.mes-hall") matsci theory)
			       (,(make-arxiv-url "stat.TH") stats theory)
			       (,(make-arxiv-url "stat.AP") stats appl)
			       (,(make-arxiv-url "stat.ME") stats appl)))

  (setq-default elfeed-search-filter "@3-days-ago +unread ")
  ;; I have no idea how much I need to change these vars
  (setq elfeed-search-trailing-width 30)
  (setq elfeed-search-title-max-width 80)
  (setq elfeed-search-print-entry-function #'my-search-print-fn)
  :hook
  (elfeed-show-mode . mixed-pitch-mode)
  (elfeed-show-mode . olivetti-mode)
  :bind
  ("C-c e" . #'elfeed)
  (:map elfeed-search-mode-map
	("n" . #'next-logical-line)
	("p" . #'previous-logical-line))
  (:map elfeed-show-mode-map
	("o" . 'jf/download-arxiv)))

;;; Programming
(use-package helpful
  :bind (("C-h f" . #'helpful-callable)
	 ("C-h v" . #'helpful-variable)
	 ("C-h k" . #'helpful-key)
	 ("C-M-." . #'helpful-at-point)))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package comint
  :ensure nil
  :bind (:map comint-mode-map
	      ("M-S-n" . #'scroll-down-line)
	      ("M-S-p" . #'scroll-up-line)))

;;;; magit + forge
(use-package magit
  :bind
  ("C-c g" . #'magit))

(use-package forge
  :after magit
  :config
  (setq auth-sources '("~/.authinfo")))

;;;; eglot
(use-package eglot
  :ensure nil
  :config
  (setq eglot-send-changes-idle-time 0.20))

;;;; flymake
(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
	      ("M-P" . #'flymake-goto-prev-error)
	      ("M-N" . #'flymake-goto-next-error)))

;;;; eldoc
(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-display-functions '(eldoc-display-in-buffer)))

(use-package eldoc-box
  :bind (:map eglot-mode-map ("C-M-." . #'eldoc-box-help-at-point)))

;;;; compile

(use-package compile
  :ensure nil

  :config
  ;; 241023: CMAKE only respects TERM to output color (clang -fcolor-diagnostics did not work)
  ;; Using fancy-compilation package would work on `compile` and `recompile` in *compilation* buffer,
  ;; but not `recompile` in other window.
  (setq compilation-environment '("TERM=TERM=xterm-256color")))

(use-package ansi-color
  :ensure nil
  :config
  ;; https://emacs.stackexchange.com/a/55571
  (add-to-list
   'display-buffer-alist
   `((,(rx string-start "*compilation*")
      (display-buffer-reuse-window display-buffer-pop-up-frame)
      (reusable-frames . visible))))

  :hook (compilation-filter . ansi-color-compilation-filter)
  :bind ("C-c g" . #'recompile))

;;;; combobulate (treesitter)
;; `M-x combobulate' (or `C-c o o') to start using Combobulate
;; (use-package treesit
;;   :ensure nil
;;   :preface
;;   (defun mp-setup-install-grammars ()
;;     "Install Tree-sitter grammars if they are absent."
;;     (interactive)
;;     (dolist (grammar
;;              '((css "https://github.com/tree-sitter/tree-sitter-css")
;;                (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
;;                (python "https://github.com/tree-sitter/tree-sitter-python")
;;                (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
;;                (yaml "https://github.com/ikatyang/tree-sitter-yaml")
;; 	       (rust "https://github.com/tree-sitter/tree-sitter-rust")
;; 	       ;; (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.1"))
;; 	       ))
;;       (add-to-list 'treesit-language-source-alist grammar)
;;       ;; Only install `grammar' if we don't already have it
;;       ;; installed. However, if you want to *update* a grammar then
;;       ;; this obviously prevents that from happening.
;;       (unless (treesit-language-available-p (car grammar))
;;         (treesit-install-language-grammar (car grammar)))))

;;   ;; Optional, but recommended. Tree-sitter enabled major modes are
;;   ;; distinct from their ordinary counterparts.
;;   ;;
;;   ;; You can remap major modes with `major-mode-remap-alist'. Note
;;   ;; that this does *not* extend to hooks! Make sure you migrate them
;;   ;; also
;;   ;; 230206 python-ts-mode is interfering with starting normal python-mode
;;   ;;        and is not starting lsp/elpy/pyvenv
;;   ;; 231025 re-enable
;;   ;; 231108 re-disable
;;   ;; (dolist (mapping '(;; (python-mode . python-ts-mode)
;;   ;;                    (css-mode . css-ts-mode)
;;   ;;                    (typescript-mode . tsx-ts-mode)
;;   ;;                    (js-mode . js-ts-mode)
;;   ;;                    (css-mode . css-ts-mode)
;;   ;;                    (yaml-mode . yaml-ts-mode)
;;   ;; 		     (rust-mode . rust-ts-mode)
;;   ;; 		     ))
;;   ;;   (add-to-list 'major-mode-remap-alist mapping))

;;   ;; 230609 disable python-ts-mode since interferes with elpy
;;   ;; 231025 re-enable
;;   :config
;;   (mp-setup-install-grammars)
;;   ;; Do not forget to customize Combobulate to your liking:
;;   ;;
;;   ;;  M-x customize-group RET combobulate RET
;;   ;;
;;   (use-package combobulate
;;     ;; Optional, but recommended.
;;     ;;
;;     ;; You can manually enable Combobulate with `M-x
;;     ;; combobulate-mode'.
;;     :hook ((python-ts-mode . combobulate-mode)
;;            ;; (js-ts-mode . combobulate-mode)
;;            ;; (css-ts-mode . combobulate-mode)
;;            ;; (yaml-ts-mode . combobulate-mode)
;;            ;; (typescript-ts-mode . combobulate-mode)
;;            ;; (tsx-ts-mode . combobulate-mode)
;; 	   )
;;     ;; Amend this to the directory where you keep Combobulate's source
;;     ;; code.
;;     :load-path ("~/.config/emacs-configs/emacs.d-my-vanilla/custom/combobulate")
;;     :config (setq combobulate-flash-node nil)))
;;;; Lispy
(use-package lispy
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1)))
  :config (setq lispy-compat '(edebug cider))
  :bind* (:map lispy-mode-map
	       ("C-c C-n" . #'lispy-outline-next)
	       ("C-c C-p" . #'lispy-outline-prev)))

;;;; Clojure/Cider
(use-package cider)
;;;; Guile + geiser
(use-package geiser-guile)
;;;; Haskell
(use-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook #'subword-mode)
  :config
  (setq haskell-process-type 'stack-ghci))

(use-package haskell-interactive-mode
  :ensure nil
  :after (haskell-mode)
  :init   (add-hook 'haskell-interactive-mode-hook #'subword-mode)
  :bind (:map haskell-interactive-mode-map
	      ;; used to override beginning-of-visual-line
	      ("C-a" . #'haskell-interactive-mode-bol)))

;;;; VTerm
(use-package vterm)

(use-package multi-vterm
  :config
  (setq multi-vterm-buffer-name "vterm")
  :bind (("C-c t t" . multi-vterm)
	 ("C-c t n" . multi-vterm-next)
	 ("C-c t p" . multi-vterm-prev)
	 ("C-c t d" . multi-vterm-dedicated-toggle)))

;;;; Julia
(use-package julia-mode)

(use-package julia-snail
  :hook (julia-mode . julia-snail-mode)
  :config (setq-default julia-snail-multimedia-enable t))

;;;; R
(use-package ess
  :config (setq ess-use-ido nil)
  :init (add-hook 'inferior-ess-r-mode-hook (lambda () (setq-local comint-scroll-to-bottom-on-output t))))

;;;; Python
(use-package elpy
  :init
  (elpy-enable)
  :config
  (setq elpy-formatter 'black)
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt")
  :hook
  (elpy-mode . (lambda () (elpy-shell-toggle-dedicated-shell 1)))
  (python-mode . 'eglot-ensure)
)

;; using jupyter-connect-server-repl to connect to python venv jupyter servers
(use-package jupyter)

;;;; Rust
(use-package rust-mode
  :config (setq rust-format-on-save t))

;;;; Gleam
(use-package gleam-ts-mode
  :ensure nil
  :load-path "~/.config/emacs-configs/emacs.d-my-vanilla/custom/gleam-mode"
  :config
  (add-to-list 'auto-mode-alist '("\\.gleam\\'" . gleam-ts-mode))
  (add-to-list 'eglot-server-programs '(gleam-ts-mode . ("gleam" "lsp")))

  :hook
  (gleam-ts-mode . eglot-ensure))

;;;; SQL
(use-package sqlformat
  ;; https://archlinux.org/packages/community/any/pgformatter/
  :config
  (setq sqlformat-command 'pgformatter)
  (setq sqlformat-args '("-s2" "-b" "-g" "-L")))

(use-package sql
  :ensure nil
  :hook
  (sql-mode . subword-mode)
  (sql-interactive-mode . subword-mode)

  :bind
  (:map sql-mode-map
	("M-q" . sqlformat)))

;;;; csv-mode
(use-package csv-mode
  :hook
  (csv-mode . csv-align-mode)
  (csv-mode . (lambda () (visual-line-mode -1) (toggle-truncate-lines 1))))

;;;; C++
;; (use-package clang-format
;;   :ensure nil
;;   :load-path "/usr/share/clang/" ; clang-format.el
;;   :init
;;   (setq clang-format-style "Mozilla"))

;;;; gdb
(use-package emacs
  :ensure nil
  :config (setq gdb-many-windows t))

;;;; CMake
;; (use-package cmake-mode
;;   :ensure nil
;;   :load-path "/usr/share/emacs/site-lisp" ; cmake-mode.el
;;   )

;;; Navigation
;; (use-package avy
;;   :bind* (("C-j" . avy-goto-char-timer))
;;   :init (setq avy-timeout-seconds 0.2))

;;;; Hydra (custom)
(defhydra jf/hydra-volume (:hint nil)
  ""
  ("=" (shell-command (string-join '("pactl" "set-sink-volume" "@DEFAULT_SINK@" "+2%") " ")) "vol up")
  ("-" (shell-command (string-join '("pactl" "set-sink-volume" "@DEFAULT_SINK@" "-2%") " ")) "vol down")
  ("p" (shell-command (string-join '("light" "-A" "1") " ")) " bright up")
  ("n" (shell-command (string-join '("light" "-U" "1") " ")) " bright down"))

;; Maybe I can convert this to a transient,
;; to remove extra package (hydra)
(defhydra jf/hydra-resize-window (:hint nil)
  ""
  ("b" shrink-window-horizontally ">narrower<")
  ("f" enlarge-window-horizontally "<wider>")
  ("p" shrink-window "-shorter-")
  ("n" enlarge-window "=taller=")
  ("=" balance-windows "balance" :exit t)
  ("-" text-scale-decrease "zoom out")
  ("+" text-scale-increase "zoom in")
  ("q" nil)
  ("t" jf/window-split-toggle "transpose" :exit t)
  ("<f7>" nil))

(defun jf/hydra-resize-window--helper ()
  "Check case where only one window is open."
  (interactive)
  (if (and (= (count-windows) 1)
	   (not current-prefix-arg))
      (progn
	(message "Only one window present")
	(call-interactively #'text-scale-adjust t (vector 0)))
    (let ((current-prefix-arg '())) (jf/hydra-resize-window/body))))

;;;; Window
(use-package window
  :ensure nil
  :config (setq recenter-positions '(0.5 0.17 0.83))
  :bind (("M-n" . scroll-down-line)
	 ("M-p" . scroll-up-line)
	 ("<f7>" . #'jf/hydra-resize-window--helper)))

;;;; Winner
(use-package winner
  :ensure nil
  :init (winner-mode))

;;;; Navigation
(use-package emacs
  :ensure nil
  :init (setq sentence-end-double-space nil)
  :bind ("M-O" . #'mode-line-other-buffer)
  ("C-S-n" . #'forward-paragraph)
  ("C-S-p" . #'backward-paragraph))

(use-package view
  :ensure nil
  :bind ("C-v" . #'View-scroll-half-page-forward)
  ("M-v" . #'View-scroll-half-page-backward))

(use-package outshine
  :init (defvar outline-minor-mode-prefix "C-c o")
  :bind (:map outshine-mode-map ("C-M-i" . nil))
  :hook (emacs-lisp-mode . outshine-mode))

(defun jf/open-line-end ()
  (interactive)
  (end-of-line)
  (newline))

(use-package emacs
  :ensure nil
  :bind (("C-M-j" . #'jf/open-line-end)))

;;;; Burly - Window Configuration Bookmarks
(use-package burly
  :init
  (setq bookmark-set-fringe-mark nil)
  (setq bookmark-save-flag 1) ; bookmark-save on every modification
  :bind
  ("C-c b f" . burly-open-bookmark)
  ("C-c b s" . burly-bookmark-windows)
  ("C-c b d" . bookmark-delete))

;;;; activities - Manage workspace activites
(use-package activities
  :init
  (activities-mode)
  (activities-tabs-mode)
  :config
  (setq activities-name-prefix "∀ ")

  (setq frame-title-format
      '((:eval (when-let ((act (activities-tabs-current)))
		 (concat (activities-name-for act) ": ")
		   ;; (cdr (assq 'name (tab-bar--current-tab)))
		   ))
        "%b"))

  :bind
  (("C-c C-a C-n" . activities-new)
   ("<f9>" . activities-resume)
   ;; As resuming is expected to be one of the most commonly used
   ;; commands, this binding is one of the easiest to press.
   ("C-c C-a C-a" . activities-resume)
   ("C-c C-a C-s" . activities-suspend)
   ("C-c C-a C-k" . activities-kill)
   ;; This binding mirrors, e.g. "C-x t RET".
   ("C-c C-a RET" . activities-switch)
   ("C-c C-a g" . activities-revert)
   ("C-<f9>" . activities-revert)
   ("C-c C-a l" . activities-list)))

;;; Dired
(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alhv --group-directories-first")
  (setq dired-kill-when-opening-new-dired-buffer t)
  :hook
  (dired-mode . auto-revert-mode)
  )

;;; multiple-cursors
(use-package multiple-cursors
  :bind (("C->" . 'mc/mark-next-symbol-like-this)
	 ("C-<" . 'mc/mark-previous-symbol-like-this)
	 ("C-c C-," . 'mc/mark-all-symbols-like-this))
  :bind (:map mc/keymap ("<return>" . nil)))

;;; expand-region
(use-package expand-region
  :bind ("C-S-SPC" . er/expand-region))

;;; Visual QoL
(use-package hl-line
  :ensure nil
  :init (global-hl-line-mode))

(use-package pulse
  :ensure nil
  :init
  ;; https://karthinks.com/software/batteries-included-with-emacs/
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))
  (dolist (command '(scroll-up-command scroll-down-command recenter-top-bottom other-window ace-window))
    (advice-add command :after #'pulse-line)))

;;; flycheck
(use-package flycheck
  :init
  ;; (global-flycheck-mode)
  (setq flycheck-display-errors-delay 0.6))

;; (use-package consult-flycheck
;;   :bind ("M-g f" . consult-flycheck))

;;; flyspell
(use-package flyspell
  :ensure nil
  :config (setq flyspell-issue-message-flag nil)
  :hook (text-mode . flyspell-mode))
;; (add-hook 'text-mode-hook #'flyspell-mode)

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

;;; hl-todo
(use-package hl-todo :hook (prog-mode . hl-todo-mode))

;;; world-clock
(use-package time
  :ensure nil
  :config
  (setq world-clock-time-format "%a %e %l%p")
  (setq world-clock-list '(("US/Pacific" "PST")
			   ("US/Eastern" "EST")
			   ("Brazil/East" "BST")
			   ("Asia/Seoul" "KST"))))
;;; proced
(use-package proced
  :ensure nil
  :config
  (setq-default proced-auto-update-flag t)
  (setq proced-enable-color-flag t)
  (add-to-list 'proced-format-alist
	       '(tiny pid tree pcpu pmem start time comm))
  (setq-default proced-format 'tiny))

;;; Toggles
;; f5 is modus-themes-toggle
;; f7 is jf/hydra-resize-window--helper

;;;; Mixed-pitch
(use-package mixed-pitch
  :bind ("<f2>" . mixed-pitch-mode))

;;;; Olivetti
(use-package olivetti
  :bind (("<f3>" . olivetti-mode))
  :init (setq olivetti-body-width 100))

;;;; redacted-mode
(use-package redacted
  :bind (("<f4>" . #'redacted-mode))
  :init (add-hook 'redacted-mode-hook
		  (lambda () (read-only-mode (if redacted-mode 1 -1)))))

;;;; display-line-numbers
(use-package display-line-numbers
  :ensure nil
  :bind ("<f6>". display-line-numbers-mode)
  :init
  (setq display-line-numbers-minor-tick 10))

;;; Custom Utilities
;;;; Custom Spotify
;; Inspired by sp.sh: https://gist.github.com/wandernauta/6800547
;; Could use https://codeberg.org/jao/espotify, but don't need all the functionalities
;; Potential Issues: https://community.spotify.com/t5/Desktop-Linux/DBus-OpenUri-issue/td-p/1376397

;; Could just write a fn to extract the ID, and use that in jf/spotify-playlists
;; Current way with full uri allows for playlist vs artist, etc.
;; but probably don't need flexiblity for my use case
(defun jf/spotify--clean-uri (raw-uri)
  "Clean RAW-URI into a dbus-acceptable uri."
  (let* ((url-fields (split-string
		      raw-uri
		      (rx (or "/" "?"))))
	 (type (nth 3 url-fields))
	 (id (nth 4 url-fields)))
    (concat "spotify:" type ":" id)))

(defvar jf/spotify-playlists
  '(("Artist" . "RAW-URI"))
  "Alist of spotify playlists for jf/spotify-playlists to select from.
RAW-URI is from right-click on playlist > Share > Copy Link to Playlist.")

(setq jf/spotify-playlists
      '(("르세라핌" . "https://open.spotify.com/playlist/1v4UqI9mEEB4ry3a3uaorO?si=bc675402c7384080")
	("오마 우주 썸" . "https://open.spotify.com/playlist/4UIjM7jouZnTa4xoANisPb?si=c949f003223b4cf0")
	("전소미" . "https://open.spotify.com/playlist/2qB1Vg0MqGGaV2j1duunZV?si=c972bfbe573042cc")
	("선미" . "https://open.spotify.com/playlist/4U7ZXQSuir0WhdumXVkEYT?si=a8c888b73a574128")
	("프로미스나인" . "https://open.spotify.com/playlist/09FzhGfmPM4c9thkEvg1PD?si=343df5ab19534480")
	("위클리" . "https://open.spotify.com/playlist/7mEmwZg3OXrQ5RqO6UjhVk?si=c9d0d7f4a3af4a09")
	("퍼플키스" . "https://open.spotify.com/playlist/3B76Cbq0kFaORIfefTzU8L?si=756f938415f84623")
	("스테이씨" . "https://open.spotify.com/playlist/6c8KC1Ud2AHMP1c8r09tkz?si=0f6e1f8ada5d4c5d")
	("에스파" . "https://open.spotify.com/playlist/6bXa8YBSF9rQ5L30qiy7g3?si=9a2612f6e71a4181")
	("공원소녀" . "https://open.spotify.com/playlist/4ZGUaJkYMFVF5IHydwNXFm?si=c19fc8df20d24faa")
	("(여자)아이들" . "https://open.spotify.com/playlist/2aDCbDDXiXyKOa8CuiuC31?si=50a27713faea4e6b")
	("있지" . "https://open.spotify.com/playlist/0VLQup0wZFWSHEGn0DmBml?si=3874d1809c1044f7")
	("이달의 소녀" . "https://open.spotify.com/playlist/1X2j9Gsmc1VGHRNmynB5di?si=eb7820da1410467c")
	("에버글로우" . "https://open.spotify.com/playlist/4FDQB6GaQ69w1tenAhu1Cc?si=eee9b098d0ee4b3b")
	("블랙핑크" . "https://open.spotify.com/playlist/4s9t738ckXfZw66AncPfXq?si=fe3c9bfbcc4c4c93")
	("쿠ㅜㅜ" . "https://open.spotify.com/playlist/5JWSfDVtvuoi6pdPjsKtZf?si=18a6f2e170d44371")))

(defun jf/spotify--open-uri (raw-uri)
  "Open RAW-URI."
  (let ((prefix "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.OpenUri string:")
	(uri (jf/spotify--clean-uri raw-uri)))
    (shell-command (concat prefix "'" uri "'"))))

(defun jf/spotify--open-playlist ()
  "Prompt to select and play a playlist from jf/spotify-playlists."
  (let* ((key (completing-read "Playlist: " jf/spotify-playlists))
	 (raw-uri (cdr (assoc key jf/spotify-playlists))))
    (jf/spotify--open-uri raw-uri)
    (message (format "Now Playing: %s" key))))

(defun jf/spotify-open-playlist ()
  "Wrapper around `jf/spotify--open-playlist`, to check if spotify is running."
  (interactive)
  (pcase
      (shell-command "pgrep spotify")
    (1 (message "Spotify not running."))
    (0 (jf/spotify--open-playlist))))

(use-package emacs
  :ensure nil
  :bind (("C-c s" . #'jf/spotify-open-playlist)))

;;;; project
(defun jf/get-project-relative-path-line ()
  (interactive)
  (if-let ((proj (project-current)))
      (let* ((path (file-relative-name (buffer-file-name) (project-root proj)))
	     (num (number-to-string (line-number-at-pos)))
	     (pn (concat path ":" num)))
	(kill-new pn)
	(message pn))
    (message "Not in a project.")))

(defun jf/goto-project-file-line ()
  "Meant to be used after 'jf/get-project-relative-path-line'."
  (interactive)
  (if-let ((pr (project-current)))
      (let* ((kill (split-string (current-kill 0) ":"))
	     (file (car kill))
	     (line (string-to-number (cadr kill)))
       	     (root (project-root pr))
	     (abs (expand-file-name file root)))
	(if (file-exists-p abs)
	    (progn
	      (find-file abs)
	      (goto-line line)
	      (message (format "Going to %s, line %s" file line)))
	  (message "Last kill is not from jf/get-project-relative-path-line.")))
    (message "Not in a project.")))

;;;; dumb dmenu
(defun jf/dumb-dmenu ()
  "Scan $PATH (i.e., `exec-path') for names of executable files and
cache them into memory (in variable `dmenu--cache-executable-files').
https://github.com/emacsmirror/dmenu/blob/e8cc9b27c79d3ecc252267c082ab8e9c82eab264/dmenu.el#L143"
  (interactive)
  (let* ((valid-exec-path (seq-uniq
                           (cl-remove-if-not #'file-exists-p
                                             (cl-remove-if-not #'stringp exec-path))))
	 (files (cl-remove-if #'file-directory-p
			      (cl-remove-if-not #'file-executable-p
						(cl-mapcan (lambda (dir) (directory-files dir t nil nil))
							   valid-exec-path))))
	 (executable-files (mapcar #'file-name-nondirectory files))
	 (files-alist (-zip  executable-files files))
	 (executable-path (cdr (assoc (completing-read "Execute: " executable-files) files-alist))))
    (async-shell-command executable-path)))

;;; zathura file selection

;; can copy selected file path? with (kill-new)
;; then paste back into a different zathura bind

(defun jf/zathura-find-file (dir)
  "In DIR, use in zathura pdf viewer to override default open file behavior."
  (kill-new
   (prog2
       (select-frame (make-frame '((name . "floating")
				   (minibuffer . only)
				   (height . 10)
				   (width . 60))))
       (let ((vertico-count (- 10 1)))
	 (shell-quote-argument 		; escape spaces
	  (expand-file-name
	   (read-file-name "File: " dir default-directory nil nil
			   '(lambda (f) (let ((ext (url-file-extension f)))
					  (or (string= ".pdf" ext)
					      (file-directory-p ext))))))))
     (delete-frame))))

;;;; Loading in any custom elisp
(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))

;; https://github.com/tonyaldon/org-bars
;; (use-package org-bars
;;   :ensure nil
;;   :hook
;;   (org-mode . org-bars-mode))

;; https://github.com/oantolin/math-delimiters
;; (use-package math-delimiters
;;   :ensure nil)

(use-package org-modern-indent
  :ensure nil
  :hook
  (org-modern-mode . org-modern-indent-mode)
  )

;; https://github.com/trevorpogue/topspace/blob/narrow-to-region/topspace.el
;; branch for narrow-to-region
;; 231009 this branch doens't work in narrowed buffers
;; https://github.com/trevorpogue/topspace/issues/23
;; (use-package topspace
;;   :ensure nil)

;;;; job-hunt
(defun jf/mod-time (f) (let ((attrs (file-attributes f))) (nth 5 attrs)))

(defun jf/fetch-latest (path &optional extension)
  "https://stackoverflow.com/a/30886283"
  (let ((e (if extension
	       (--filter
		(string-match (concat "." extension "$") it)
		(f-entries path))
	     (f-entries path))))
    (car (sort e (lambda (a b)
                   (not (time-less-p (jf/mod-time a)
                                     (jf/mod-time b))))))))

(defun jf/job-hunt ()
  (interactive)
  (let ((async-shell-command-buffer 'new-buffer)
	(async-shell-command-display-buffer nil))
    (async-shell-command (concat "zathura " (jf/fetch-latest "~/Personal/Resumes")))
    (async-shell-command (concat "zathura " (jf/fetch-latest "~/Personal/cover_letters" "pdf")))
    (find-file (jf/fetch-latest "~/Personal/cover_letters" "tex"))))

;;;; reload dir local
;; https://emacs.stackexchange.com/a/13096
(defun jf/my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))
;;; Finish
(setq gc-cons-percentage default-gc-percentage
      gc-cons-threshold default-gc-threshold)

(provide 'init.el)
;;; init.el ends here
