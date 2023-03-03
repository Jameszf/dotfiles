(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "jmmkr")
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(set-fringe-mode 0) ;; Bars on the left and right sides of the frame.
(load-theme 'tango-dark)
(set-frame-font "Office Code Pro 12" nil t)

(setq visible-bell t)

(recentf-mode 1)
(setq recentf-max-menu-items 12)
(setq recentf-max-saved-items 12)

(save-place-mode 1)

(global-visual-line-mode 1)

(global-auto-revert-mode 1)

(add-hook 'prog-mode-hook
          (lambda() (display-line-numbers-mode 'visual)))
(add-hook 'text-mode-hook
          (lambda () (display-line-numbers-mode -1)))

(setq user-emacs-directory (expand-file-name "~/.emacs.d"))
(setq debug-on-error t)
(setq use-dialog-box nil)

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq tab-width 4)
	    (setq python-indent-offset 4)))

(add-hook 'c++-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq tab-width 4)))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (display-line-numbers-mode 1)))

(add-hook 'text-mode-hook
	  (lambda ()
	    (visual-fill-column-mode 1)))

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Hello Startup Hook Activated")))
;; (add-hook 'prog-mode-hook 'electric-pair-mode)

;; backup
(setq make-backup-files t)
(setq vc-make-backup-files nil)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 3)
(setq kept-old-versions 1)
(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/backups"))))

;; auto-save
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/.saves/" t)))

(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package '(org :type built-in))
(straight-use-package '(xrefs :type built-in))

(straight-use-package 'use-package)

(straight-use-package 'diminish)

(defvar common-files '() "List of common files for common-files command.")

(defun add-common-file (cfpath)
  (let ((name (substring cfpath (+ (string-match "/[-_.A-Za-z]+$" cfpath) 1) (string-match "\.[-_A-Za-z]+$" cfpath))))
    (add-to-list 'common-files `(,(format "[%s] %s" (upcase name) cfpath) . ,cfpath))))

(let ((common-files-to-add '("~/.emacs.d/org/roam/inbox.org"
                             "~/.emacs.d/org/roam/reflections.org"
                             "~/.emacs.d/org/roam/bibliography.org"
                             "~/.emacs.d/org/agenda/inbox.org"
                             "~/.emacs.d/org/roam/mistakes.org"
                             "~/.emacs.d/init.el"
                             "~/.emacs.d/org/roam/bookmarks.org"
                             "~/.emacs.d/org/roam/problems.org"
                             "~/.emacs.d/org/roam/work.org"
                             "~/.emacs.d/org/roam/food.org"
                             "~/.emacs.d/org/roam/code.org"
                             "~/.emacs.d/org/roam/drill.org"
                             "~/.emacs.d/org/agenda/habits.org"
                             "~/.emacs.d/org/agenda/school.org")))
  (mapcar 'add-common-file common-files-to-add))

(defun restart-emacs-debug-mode ()
  (interactive)
  (restart-emacs '("--debug-init")))

(defun restart-emacs-no-init ()
  (interactive)
  (restart-emacs '("--no-init-file")))

(defun gen-time-heading-id ()
  (format ":PROPERTIES:\n:ID: %s\n:END:" (format-time-string "%Y%m%d%k%M")))

(defun icallwp (func prefix)
  "Interactive call func with some prefix."
  (let ((current-prefix-arg prefix))
    (call-interactively 'func)))

(defun my-org-schedule ()
  (format "SCHEDULED: <%s>" (org-read-date)))

(defun add-list-to-var (dest-var some-list)
  (mapcar '(lambda (x) (add-to-list dest-var x)) some-list))

(defun open-emacs-config-file ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.org")))

(defun create-scratch-buffer ()
  ;; from https://www.emacswiki.org/emacs/RecreateScratchBuffer
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun load-config-file ()
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(defun open-common-file ()
  (interactive)
  (ivy-read "Goto: " common-files :require-match t :action (lambda (file) (find-file (cdr file)))))


(defun copy-buffer-file-name ()
  (interactive)
  (kill-new buffer-file-name))


(defun delete-buffer-file ()
  (interactive)
  (let ((current-file (buffer-file-name)))
    (if current-file
        (progn
          (save-buffer current-file)
          (delete-file current-file)
          (kill-buffer (current-buffer))))))


(defun ins-checkbox-item ()
  (interactive)
  (insert "- [ ]  "))


(message "Functions loaded in...")

(defun screenshot-p (file)
  (when (and (>= (length file) 16) (string= "Screenshot from " (substring file 0 16)))
    (progn file)))


(defun get-screenshot-files ()
  (let ((screenshot-files '()))
    (progn
      (dolist (file (directory-files "~/Pictures"))
	(when (screenshot-p file)
	  (setq screenshot-files (cons file screenshot-files))))
      screenshot-files)))


(defun insert-screenshot (filename)
  (progn 
    (org-insert-link nil filename "")
    (org-redisplay-inline-images)))


(defun move-and-insert-screenshot ()
  (interactive)
  (ivy-read "Copy Image to ~/.emacs.d/org/images/" (get-screenshot-files)
	    :action (lambda (selection)
		      (let ((new-file-name (concat "~/.emacs.d/org/images/" (read-string "New Image Name: ") ".png"))
			    (file-to-copy (concat "~/Pictures/" selection)))
			(progn
			  (copy-file file-to-copy new-file-name)
			  (insert-screenshot (concat "file:" new-file-name)))))))

(use-package general)

(use-package key-chord
  :diminish
  :config
  (key-chord-mode 1))

(use-package which-key
  :diminish
  :custom
  (which-key-side-window-location 'bottom)
  (which-key-side-window-max-height 0.30)
  (which-key-idle-delay 0.3)
  (which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode))

(general-define-key "<escape>" 'keyboard-escape-quit)
(general-define-key
 :states 'normal
 "q" nil)
(general-define-key
 :states 'normal
 "m" 'evil-record-macro)

(general-create-definer my-leader-def
  :keymaps '(normal visual emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :global-prefix "C-SPC")

(my-leader-def
  "o" '(:ignore t :which-key "Org-mode")
  "o l" '(org-add-note :which-key "Logbook entry")
  "o n" '(:ignore t :which-key "Narrow")
  "o n s" '(org-narrow-to-subtree :which-key "Subtree")
  "o n w" '(widen :which-key "Widen")
  "o r" '(org-redisplay-inline-images :which-key "Redisplay Inline Images")
  "o t" '(org-todo :which-key "Toggle Todo")
  "o s" '(org-store-link :which-key "Store Org Link")
  "o q" '(org-set-tags-command :which-key "Set Tags")
  "o x" '(org-export-dispatch :which-key "Export"))

(my-leader-def
  "o k" '(:ignore t :which-key "Clock")
  "o k i" '(org-clock-in :which-key "In")
  "o k o" '(org-clock-out :which-key "Out")
  "o k l" '(org-clock-in-last :which-key "Last")
  "o k d" '(org-clock-display :which-key "Display")
  "o k q" '(org-clock-cancel :which-key "Cancel")
  "o k g" '((lambda () (interactive) (icallwp 'org-clock-goto 4)) :which-key "Goto")
  "o k c" '(org-clock-goto :which-key "Current"))

(my-leader-def 
    "o a" '(:ignore t :which-key "Archive")
    "o a e" '(org-archive-subtree-default :which-key "Entry")
    "o a s" '(org-archive-subtree :which-key "Subtree")
    "o a S" '((lambda () (interactive) (icallwp 'org-archive-subtree 4)) :which-key "Select")
    "o a i" '(org-toggle-archive-tag :which-key "Internal"))

(my-leader-def
 "r l" 'org-roam-buffer-toggle
 "r i" 'org-roam-node-insert
 "r f" 'org-roam-node-find
 "r" '(:ignore t :which-key "Org-roam")

 "r d j" '(org-roam-dailies-capture-today :which-key "Capture today")
 "r d p" '(org-roam-dailies-goto-today :which-key "Goto today")
 "r d b" '(org-roam-dailies-goto-next-note :which-key "Next note")
 "r d f" '(org-roam-dailies-goto-previous-note :which-key "Previous note")
 "r d" '(:ignore t :which-key "Dailies"))

(my-leader-def
  "e" '(:ignore t :which-key "Emacs")
  "e c" '(open-emacs-config-file :which-key "Open config file")
  "e r" '(restart-emacs :which-key "Regular restart")
  "e d" '(restart-emacs-debug-mode :which-key "Debug mode restart")
  "e n" '(restart-emacs-no-init :which-key "No init restart")
  "e s" '(create-scratch-buffer :which-key "Open scratch buffer")
  "e l" '(load-config-file :which-key "Load config file")
  "e m" '(view-echo-area-messages :which-key "Echo messages")
  "e q" '(save-buffers-kill-terminal :which-key "Quit")
  "e e" '(eval-buffer :which-key "Eval Buffer"))

(my-leader-def
"TAB" '(ivy-switch-buffer :which-key "Switch buffer")
"SPC" '(counsel-M-x :which-key "M-x"))

(my-leader-def
"f" '(:ignore t :which-key "Files")
"f f" '(find-file :which-key "Find File")
"f c" '(open-common-file :which-key "Common Files")
"f d" '(dired :which-key "Dired")
"f r" '(recentf-open-files :which-key "Recent")
"f s" '(save-buffer :which-key "Save Buffer"))

(my-leader-def
  "h" '(:ignore t :which-key "Help")
  "h f" '(helpful-callable :which-key "Function")
  "h v" '(helpful-variable :which-key "Variable")
  "h k" '(helpful-key :which-key "Key")
  "h d" '(helpful-at-point :which-key "At point")
  "h l" '(find-library :which-key "Library")
  "h i" '(info :which-key "Info")
  "h a" '(apropos :which-key "Apropos"))

(my-leader-def
  "i" '(:ignore t :which-key "Insert")
  "i t" '(org-table-create-or-convert-from-region :which-key "Org table")
  "i d" '(org-deadline :which-key "Deadline")
  "i s" '(org-schedule :which-key "Schedule")
  "i c" '(ins-checkbox-item :which-key "Checkbox")
  "i f" '((lambda () (interactive) (icallwp 'org-insert-link 4)) :which-key "File Link")
  "i l" '(org-insert-link :which-key "Org-link"))

(general-create-definer apps-leader-def
    :keymaps '(normal visual emacs)
    :prefix "SPC a"
    :global-prefix "C-SPC a")

(my-leader-def
  "a" '(:ignore t :which-key "Apps"))

(apps-leader-def
"d" '(org-drill :which-key "Drill"))

(apps-leader-def 
  "s" '(swiper :which-key "Swiper"))

(apps-leader-def
 "a" '(org-agenda :which-key "Org Agenda"))

(apps-leader-def
 "c" '(org-capture :which-key "Capture"))

(apps-leader-def
  "b" '(counsel-bookmark :which-key "Bookmarks"))

(apps-leader-def
  "e" '(elfeed :which-key "Elfeed"))

(my-leader-def
  "p" '(:ignore t :which-key "Project")
  "p f" '(project-find-file :which-key "Find File")
  "p e" '(project-eshell :which-key "Eshell")
  "p q" '(project-query-replace-regexp :which-key "Replace w/ Regex")
  "p c" '(project-compile :which-key "Compile")
  "p k" '(project-kill-buffers :which-key "Kill Buffers")
  "p s" '(project-shell-command :which-key "Shell Command")
  "p p" '(project-switch-project :which-key "Switch Project")
  "p b" '(project-switch-to-buffer :which-key "Switch Buffer")
  "p r" '(project-find-regexp :which-key "Regex Search"))

(my-leader-def
  "m" '(:ignore t :which-key "Magit")
  "m m" '(magit-status :which-key "Status")
  "m d" '(magit-dispatch :which-key "Dispatch")
  "m f" '(magit-file-dispatch :which-key "File Dispatch"))

(my-leader-def
  "w" '(:ignore t :which-key "Window")
  "w c" '(:ignore t :which-key "Close")
  "w c o" '(delete-other-windows :which-key "Close other windows")
  "w c w" '(delete-window :which-key "Close window")
  "w s" '(:ignore t :which-key "Split")
  "w s h" '(split-window-horizontally :which-key "Split Horizontally")
  "w s v" '(split-window-vertically :which-key "Split Vertically")
  "w o" '(other-window :which-key "Other Window"))

(my-leader-def
  "c" '(:ignore t :which-key "Commands")
  "c r" '(replace-regexp :which-key "Replace")
  "c e" '(eshell :which-key "Eshell")
  "c t" '(term :which-key "Term"))

(my-leader-def
  "s" '(:ignore t :which-key "Scripts")
  "s m" '(move-and-insert-screenshot :which-key "Move+Insert Screenshoot")
  "s i" '(insert-screenshot :which-key "Insert Screenshot"))

(my-leader-def
  "y" '(:ignore t :which-key "Yasnippet")
  "y i" '(yas-insert-snippet :which-key "Insert")
  "y n" '(yas-new-snippet :which-key "New")
  "y t" '(yas-describe-tables :which-key "Describe Tables")
  "y r" '(yas-reload-all :which-key "Reload all"))

(general-define-key
 :keymaps 'org-agenda-mode-map
 "j" 'org-agenda-next-line
 "k" 'org-agenda-previous-line)

(general-define-key
 :states '(insert replace)
 (general-chord "fd") 'evil-normal-state
 (general-chord "df") 'evil-normal-state)

(general-define-key
 :states 'normal
 "j" 'evil-next-visual-line
 "k" 'evil-previous-visual-line)

(general-define-key
 :states 'normal
 :keymaps 'Info-mode-map
 "j" 'Info-scroll-up ;; <BACKSPACE>
 "k" 'Info-scroll-down ;; <SPC>
 "h" 'Info-backward-node ;; [
 "l" 'Info-forward-node ;; ]
 "e" 'Info-history-back ;; l
 "r" 'Info-history-forward ;;  r
 "m" 'Info-menu ;; m
 "n" 'Info-goto-node ;; g
 "t" 'Info-top-node ;; t
 "f" 'Info-follow-reference ;; f
 )

(general-define-key
 :states 'normal
 :keymaps 'elfeed-search-mode-map
 "r" 'elfeed-search-untag-all-unread
 "u" 'elfeed-search-tag-all-unread)

(general-define-key
 :keymaps 'ivy-switch-buffer-map
 "M-l" 'ivy-done
 "M-d" 'ivy-switch-buffer-kill)

(general-define-key
 :keymaps 'ivy-minibuffer-map
 "M-j" 'ivy-next-line
 "M-k" 'ivy-previous-line)

(require 'org)
(add-to-list 'org-modules 'org-habit)
(org-indent-mode 1)
(diminish 'org-indent-mode)
(setq org-startup-folded t)
(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)
(setq org-hide-block-startup nil)

(setq org-agenda-files `(,(expand-file-name "~/.emacs.d/org/agenda")))
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-start-day "-2d")
(setq org-agenda-span 10)
(setq org-agenda-show-future-repeats t)
(setq org-agenda-entry-text-maxlines 3)
(setq org-habit-show-habits-only-for-today t)
(setq org-agenda-custom-commands '(("d" "Dashboard"
				    ((agenda "" ((org-agenda-start-day "+0d")
						 (org-agenda-span 5)
						 (org-agenda-start-with-entry-text-mode t)))))
				   ("r" "Report"
				    ((agenda "" ((org-agenda-start-day "-21d")
						 (org-agenda-span 21)
						 (org-agenda-start-with-log-mode t)
						 (org-agenda-start-with-clockreport-mode t)
						 (org-agenda-skip-archived-trees nil)))))))

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "NEXT(n)" "|" "DONE(d)" "FAILED(f@)" "PARTIAL(p@)" "EXCUSE(e@)")))
(setq org-todo-keyword-faces '(("TODO" . org-todo) ("DONE" . org-done) ("FAILED" . "red") ("PARTIAL" . "yellow") ("EXCUSE" . "gray") ("WAITING" . "blue") ("NEXT" . "yellow")))
  (setq org-use-fast-todo-selection t)

(require 'color)
(set-face-attribute 'org-block nil :background
		    (color-darken-name
		     (face-attribute 'default :background) 3))
(set-face-attribute 'org-block-begin-line nil :foreground
		    (color-lighten-name
		     (face-attribute 'default :background) 20))
(set-face-attribute 'org-code nil :background
		    (color-darken-name
		     (face-attribute 'default :background) 3))

(setq org-return-follows-link t)
(setq org-default-notes-file (expand-file-name "~/.emacs.d/org/notes.org"))
(setq org-hide-emphasis-markers t)
(setq org-hidden-keywords '(title))
(setq org-adapt-indentation t)
(setq org-deadline-warning-days 0)
(setq org-tags-column -60)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-image-actual-width '(400))
(setq org-confirm-babel-evaluate nil)
(setq org-export-babel-evaluate nil)
(setq org-babel-default-header-args:sage '((:session . t)
                                           (:results . "output")))
(setq sage-shell:check-ipython-version-on-startup nil)
(setq sage-shell:set-ipython-version-on-startup nil)

(setq org-format-latex-options '(:foreground default
                                             :background default
                                             :scale 1.30
                                             :html-foreground "Black"
                                             :html-background "Transparent"
                                             :html-scale 1.0
                                             :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
(setq org-latex-create-formula-image-program 'dvipng)
(setq org-latex-packages-alist '(("usenames" "color")
                                 ("" "amsmath")
                                 ("mathscr" "eucal")
                                 ("utf8" "inputenc")
                                 ("T1" "fontenc")
                                 ("" "graphicx")
                                 ("normalem" "ulem")
                                 ("" "textcomp")
                                 ("" "marvosym")
                                 ("" "latexsym")
                                 ("" "amssymb")))

(defvar my-oc-templates '())
(add-list-to-var 'my-oc-templates '(("i" "Inbox" entry (file "~/.emacs.d/org/roam/inbox.org")
				     "* [%<%Y-%m-%d %k:%M>] %?\n%(gen-time-heading-id)\n** Questions\n")
				    ("m" "Mistake Entry" entry (file "~/.emacs.d/org/roam/mistakes.org") "* %? \n%(gen-time-heading-id)")
				    ("p" "CP Problem" entry (file "~/.emacs.d/org/roam/problems.org") "* [[%x][%<%Y-%m-%d>]]" :immediate-finish t)
				    ("w" "Work Session" entry (file "~/.emacs.d/org/roam/work.org") "* Work Session #%^{SESSION NUMBER}\n%(my-org-schedule)\n** TODOs\n*** TODO  %?\n** Reflection")
				    ("f" "Food" entry (file+headline "~/.emacs.d/org/roam/food.org" "Food Journal") "** [%<%d/%m/%Y>]\n + Breakfast :: %?\n + Lunch :: \n + Dinner :: \n + Misc :: ")))

(add-list-to-var 'my-oc-templates '(("a" "Agenda Items")
                                    ("at" "Todo" checkitem (file+headline "~/.emacs.d/org/agenda/inbox.org" "Box") "+ [ ] %^{TODO}." :immediate-finish t)
                                    ("al" "To Learn" item (file+headline "~/.emacs.d/org/agenda/inbox.org" "Hmmm... Interesting") "+ %^{CONCEPT} :: %^{DESCRIPTION}." :immediate-finish t)))

(add-list-to-var 'my-oc-templates '(("r" "Reflection templates")
                                   ("rg" "Reflection" entry (file+headline  "~/.emacs.d/org/roam/reflections.org" "Reflections") "**  %^{TITLE} \n%T\n %?")
                                   ("rt" "Question" checkitem (file+headline "~/.emacs.d/org/roam/reflections.org" "Questions") " + [ ] %^{Question}" :immediate-finish t)))

(add-list-to-var 'my-oc-templates '(("c" "Chinese")
                                   ("cs" "Sentence" item (file+headline "~/.emacs.d/org/roam/20220831105406-mandarin.org" "Sentences") " + [%<%Y-%m-%d>] %^{SENTENCE} :: %^{MEANING}" :immediate-finish t)
                                   ("cv" "Vocabulary" item (file+headline "~/.emacs.d/org/roam/20220831105406-mandarin.org" "Vocab") " + [%<%Y-%m-%d>] %^{CHARACTER} (%^{PINYIN}) :: %^{MEANING}" :immediate-finish t)
                                   ("ca" "Archive" item (file+headline "~/.emacs.d/org/roam/20220831105406-mandarin.org" "Vocab") " + [%<%Y-%m-%d>]  %?")))

(add-list-to-var 'my-oc-templates '(("b" "Bibliography/Bookmarks")
                                   ("bm" "Bookmarks" entry (file+headline "~/.emacs.d/org/roam/bookmarks.org" "Website Bookmarks") "** %<%Y-%m-%d> [[%x][%?]] \n%(gen-time-heading-id)")))

(setq org-capture-templates my-oc-templates)

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/init.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package org-roam
  :init
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))
  :custom
  (org-roam-directory (expand-file-name "~/.emacs.d/org/roam"))
  (org-roam-completion-everywhere t)
  (org-roam-v2-ack t)
  (org-roam-capture-templates '(("n" "Note" plain "%?"
                                 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+filetags: :note:\n#+TITLE: ${title}\n\n* Questions")
                                 :unnarrowed t)
                                ;; ("c" "Concept" plain "* Questions\n* Summary\n  %?\n* Relevance\n"
                                ;;  :target (file+head "%<%Y%m%d%H%M%S>-concept_${slug}.org" "#+filetags: :concept:\n#+TITLE: ${title}")
                                ;;  :unnarrowed t)
                                ;; ("h" "Hoard" plain "* Concepts\n* Hoard\n %?"
                                ;;  :target (file+head "%<%Y%m%d%H%M%S>-hoard_${slug}.org" "#+filetags: :hoard:\n#+TITLE: ${title}")
                                ;;  :unnarrowed t)
                                ("t" "Thought" plain "*  %?"
                                 :target (file+head "%<%Y%m%d%H%M%S>-thought_${slug}.org" "#+filetags: :thought\n#+TITLE: ${title}")
                                 :unnarrowed t)))
  (org-roam-node-display-template (concat (propertize "${tags:10}" 'face 'org-tag) " ${title:*}"))
  (org-roam-dailies-capture-templates '(("d" "default" entry "* %?"
                                         :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n")
                                         :unnarrowed t)
                                        ("m" "moment" entry "* %<%I:%M %p> %?"
                                         :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n")
                                         :unnarrowed t)))
  (org-roam-file-exclude-regexp "\\(inbox.org\\)\\|\\(work.org\\)\\|\\(daily/\\)\\|\\(mistakes.org\\)|\\(drill.org\\)")
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(defun my-org-appear-trigger-function ()
  (interactive)
  ;; (message "my org-appear-trigger function triggered!")
  (org-appear-mode)
  (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start)
  (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop))

(use-package org-appear
  :requires (org)
  :custom
  (org-appear-trigger 'manual))
  ;; :hook
  ;; (org-mode . my-org-appear-trigger-function))

(use-package org-superstar
  :custom
  (org-hide-leading-stars nil)
  (org-superstar-leading-bullet ?\s)
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-superstar-remove-leading-stars t)
  (org-cycle-level-faces nil)
  (org-n-level-faces 4)
  :config
  (set-face-attribute 'org-level-8 nil :weight 'bold :inherit 'default)
  ;; Low levels are unimportant => no scaling
  (set-face-attribute 'org-level-7 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-6 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-5 nil :inherit 'org-level-8)
  (set-face-attribute 'org-level-4 nil :inherit 'org-level-8)
  ;; Top ones get scaled the same as in LaTeX (\large, \Large, \LARGE)
  (set-face-attribute 'org-level-3 nil :inherit 'org-level-8 :height 1.2) 
  (set-face-attribute 'org-level-2 nil :inherit 'org-level-8 :height 1.4) 
  (set-face-attribute 'org-level-1 nil :inherit 'org-level-8 :height 1.6) 
  (set-face-attribute 'org-document-title nil
                      :height 2.074
                      :foreground 'unspecified
                      :inherit 'org-level-8)
  :hook (org-mode . (lambda () (interactive)(org-superstar-mode 1))))

(message "Org loaded in...")

(use-package org-drill
  :custom
  (org-drill-scope '("~/.emacs.d/org/roam/drill.org"))
  (org-drill-hide-item-headings-p t)
  (org-drill-maximum-items-per-session nil))

(use-package ox-hugo
  :requires (ox))

(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(use-package evil
  :diminish
  :custom
  (evil-want-C-i-jump nil)
  (evil-respect-visual-line-mode t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :requires (evil)
  :diminish
  :custom
  (evil-collection-calendar-want-org-bindings t)
  :config
  (evil-collection-init)
  (evil-collection-calendar-setup)
  (diminish 'evil-collection-unimpaired-mode))

(message "Evil loaded in...")

(use-package ivy
  :diminish
  :custom
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode 1))

(use-package counsel)

(use-package ivy-rich
  :requires (counsel)
  :init
  (ivy-rich-mode 1))

(use-package sage-shell-mode
  :diminish t)

(use-package ob-sagemath
  :requires (sage-shell-mode))

(use-package haskell-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package flycheck)
;;  :init (global-flycheck-mode))

(use-package company
  :diminish t
  :hook (prog-mode . company-mode))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package magit)

(use-package helpful)

(use-package restart-emacs)

(use-package elfeed
  :diminish)

(use-package elfeed-org
  :custom (rmh-elfeed-org-files (list (expand-file-name "~/.emacs.d/elfeed.org")))
  :config
  (elfeed-org))

(use-package deft
  :custom
  (deft-directory (expand-file-name "~/.emacs.d/org/"))
  (deft-recursive t ))

(use-package visual-fill-column
  :custom
  (fill-column 100)
  :config
  (setq-default visual-fill-column-center-text t))
