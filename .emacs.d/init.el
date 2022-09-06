(setq user-emacs-directory (expand-file-name "~/.emacs.d"))
(setq debug-on-error t)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

;; Disable mouse-centric menus.
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message "guerry")
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Visual alarm bell instead of audio version
(setq visible-bell t)

;; Fringes (Frame padding?)
(set-fringe-mode 10)

;; Theme
(load-theme 'tango-dark)

;; Font(s)
(set-frame-font "Office Code Pro 12" nil t)

(global-visual-line-mode 1)
(global-display-line-numbers-mode t)
(column-number-mode 1)
(setq org-startup-indented t)

(setq backup-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/backups"))))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/.saves/" t)))
(message "Base loaded in...")

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)

;; Update list of packages and allow download of them.
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package diminish)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(defvar common-files '() "List of common files for common-files command.")

(defun add-common-file (cfpath)
  (let ((name (substring cfpath (+ (string-match "/[-_.A-Za-z]+$" cfpath) 1) (string-match "\.[-_A-Za-z]+$" cfpath))))
    (add-to-list 'common-files `(,(format "[%s] %s" (upcase name) cfpath) . ,cfpath))))

(let ((common-files-to-add '("~/.emacs.d/org/roam/inbox.org"
                             "~/.emacs.d/org/roam/reflections.org"
                             "~/.emacs.d/org/roam/bibliography.org"
                             "~/.emacs.d/org/agenda/gtd.org"
                             "~/.emacs.d/org/roam/mistakes.org"
                             "~/.emacs.d/init.el"
                             "~/.emacs.d/org/roam/bookmarks.org"
                             "~/.emacs.d/org/roam/problems.org"
                             "~/.emacs.d/org/roam/work.org")))
  (mapcar 'add-common-file common-files-to-add))

(defun open-emacs-config-file ()
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/init.org")))

(defun restart-emacs-debug-mode ()
  (interactive)
  (restart-emacs '("--debug-init")))

(defun restart-emacs-no-init ()
  (interactive)
  (restart-emacs '("--no-init-file")))

(defun create-scratch-buffer ()
  ;; from https://www.emacswiki.org/emacs/RecreateScratchBuffer
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun load-config-file ()
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(defun my-org-id-get-create ()
  (format-time-string "%Y%m%d%k%M"))

(defun open-common-file ()
  (interactive)
  (ivy-read "Goto: " common-files :require-match t :action (lambda (file) (find-file (cdr file)))))

(defun gen-time-heading-id ()
  (format ":PROPERTIES:\n:ID: %s\n:END:" (my-org-id-get-create)))

(defun icallwp (func prefix)
  "Interactive call func with some prefix."
  (interactive)
  (let ((current-prefix-arg prefix))
    (call-interactively func)))

(defun copy-buffer-file-name ()
  (interactive)
  (kill-new buffer-file-name))

(message "Functions loaded in...")

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

(general-create-definer my-leader-def
  :keymaps '(normal visual emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  :global-prefix "C-SPC")

(my-leader-def
  "o" '(:ignore t :which-key "Org-mode")
  "o l" '(org-add-note :which-key "Logbook entry")
  "o a" '(:ignore t :which-key "Archive")
  "o a e" '(org-archive-subtree-default :which-key "Entry")
  "o a s" '(org-archive-subtree :which-key "Subtree")
  "o a S" '((lambda () (interactive) (icallwp 'org-archive-subtree 4)) :which-key "Select")
  "o k" '(:ignore t :which-key "Clock")
  "o k i" '(org-clock-in :which-key "In")
  "o k o" '(org-clock-out :which-key "Out")
  "o k l" '(org-clock-in-last :which-key "Last")
  "o k d" '(org-clock-display :which-key "Display")
  "o k q" '(org-clock-cancel :which-key "Cancel")
  "o k g" '((lambda () (interactive) (icallwp 'org-clock-goto 4)) :which-key "Goto")
  "o k c" '(org-clock-goto :which-key "Current"))

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
"d" '(:ignore t :which-key "Dired")
"d f" '(find-file :which-key "Find File")
"d c" '(open-common-file :which-key "Common Files")
"d d" '(dired-do-rename :which-key "Dired"))

(my-leader-def
  "h" '(:ignore t :which-key "Help")
  "h f" '(helpful-callable :which-key "Function")
  "h v" '(helpful-variable :which-key "Variable")
  "h k" '(helpful-key :which-key "Key")
  "h d" '(helpful-at-point :which-key "At point")
  "h l" '(find-library :which-key "Library"))

(my-leader-def
  "i" '(:ignore t :which-key "Insert")
  "i t" '(org-table-create-or-convert-from-region :which-key "Org table")
  "i d" '(org-deadline :which-key "Deadline")
  "i s" '(org-schedule :which-key "Schedule"))

(general-create-definer apps-leader-def
    :keymaps '(normal visual emacs)
    :prefix "SPC a")

(apps-leader-def
"d" '(deft :which-key "Deft"))

(apps-leader-def 
  "s" '(swiper :which-key "Swiper"))

(apps-leader-def
 "a" '(org-agenda :which-key "Org Agenda"))

(apps-leader-def
 "c" '(org-capture :which-key "Capture"))

(apps-leader-def
  "m" '(magit :which-key "Magit"))

(apps-leader-def
  "b" '(counsel-bookmark :which-key "Bookmarks"))

(setq org-todo-keywords '((sequence "TODO" "|" "DONE" "FAILED" "PARTIAL" "EXCUSE")))
(setq org-todo-keyword-faces '(("TODO" . org-todo) ("DONE" . org-done) ("FAILED" . "red") ("PARTIAL" . "yellow") ("EXCUSE" . "gray")))
(setq org-agenda-files `(,(expand-file-name "~/.emacs.d/org/agenda")))
(setq org-startup-folded t)
(setq org-return-follows-link t)
(setq org-default-notes-file (expand-file-name "~/.emacs.d/org/notes.org"))
(setq org-hide-emphasis-markers t)
(setq org-hidden-keywords '(title))
(setq org-adapt-indentation t)
(setq org-deadline-warning-days 7)
(setq org-tags-column -60)
(setq org-log-done 'time)
(setq org-hide-block-startup t)
(setq org-log-into-drawer t)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(general-define-key
 :keymaps 'org-agenda-mode-map
 "j" 'org-agenda-next-line
 "k" 'org-agenda-previous-line)

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

(setq org-capture-templates '(("a" "Agenda Items")
                              ("ad" "Day plan" entry (file+headline "~/.emacs.d/org/agenda/gtd.org" "Day Plans") "**  %?")
                              ("at" "Todo" checkitem (file+headline "~/.emacs.d/org/agenda/gtd.org" "Todos") "+ [ ] %^{TODO}." :immediate-finish t)
                              ("i" "Inbox Note" entry (file "~/.emacs.d/org/roam/inbox.org")
                               "* [%<%Y-%m-%d %k:%M>]  %?\n%(gen-time-heading-id)\n")
                              ("r" "Reflection templates")
                              ("rg" "Reflection" entry (file+headline  "~/.emacs.d/org/roam/reflections.org" "Reflections") "**  %^{TITLE} \n%T\n %?")
                              ("rt" "Question" checkitem (file+headline "~/.emacs.d/org/roam/reflections.org" "Questions") " + [ ] %^{Question}" :immediate-finish t)
                              ("m" "Mistake Entry" entry (file "~/.emacs.d/org/roam/mistakes.org") "* %? \n%(gen-time-heading-id)")
                              ("b" "Bibliography/Bookmarks")
                              ("bm" "Bookmarks" entry (file+headline "~/.emacs.d/org/roam/bookmarks.org" "Website Bookmarks") "** %<%Y-%m-%d> [[%x][%?]] \n%(gen-time-heading-id)")
                              ("p" "CP Problem" entry (file "~/.emacs.d/org/roam/problems.org") "* [[%x][%<%Y-%m-%d>]]" :immediate-finish t)
                              ("c" "Chinese")
                              ("cs" "Sentence" item (file+headline "~/.emacs.d/org/roam/20220831105406-mandarin.org" "Sentences") " + [%<%Y-%m-%d>] %^{SENTENCE} :: %^{MEANING}" :immediate-finish t)
                              ("cv" "Vocabulary" item (file+headline "~/.emacs.d/org/roam/20220831105406-mandarin.org" "Vocab") " + [%<%Y-%m-%d>] %^{CHARACTER} (%^{PINYIN}) :: %^{MEANING}" :immediate-finish t)
                              ("ca" "Archive" item (file+headline "~/.emacs.d/org/roam/20220831105406-mandarin.org" "Vocab") " + [%<%Y-%m-%d>]  %?")
                              ("w" "Work Sessions")
                              ("wp" "Plan" entry (file+headline "~/.emacs.d/org/roam/work.org" "Plans") "*  %?\n%(gen-time-heading-id)\n")
                              ("ws" "Session" entry (file+headline "~/.emacs.d/org/roam/work.org" "Sessions") "** %<%Y-%m-%d %k:%M>\n%(gen-time-heading-id)\n*** Objectives\n**** TODO  %?\n*** Reflection\n")))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/init.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("e" . "example"))

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
  (org-roam-capture-templates '(("d" "default" plain "%?"
                                 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
                                 :unnarrowed t)))
  (org-roam-dailies-capture-templates '(("d" "default" entry "* %?"
                                         :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n")
                                         :unnarrowed t)
                                        ("m" "moment" entry "* %<%I:%M %p> %?"
                                         :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n")
                                         :unnarrowed t)))
  :general
  (:keymaps 'org-mode-map
            "C-M-i" 'completion-at-point)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

(defun my-org-appear-trigger-function ()
  (interactive)
  ;; (message "my org-appear-trigger function triggered!")
  (org-appear-mode)
  (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start)
  (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop))

(use-package org-appear
  :after org
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

(use-package evil
  :demand t
  :diminish
  :general
  (:states 'insert
           (general-chord "fd") 'evil-normal-state
           (general-chord "df") 'evil-normal-state)
  (:states 'normal
           "j" 'evil-next-visual-line
           "k" 'evil-previous-visual-line)
  (:states 'normal
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
  (:states 'normal
   :keymaps 'elfeed-search-mode-map
   "r" 'elfeed-search-untag-all-unread
   "u" 'elfeed-search-tag-all-unread)
  :custom
  (evil-want-C-i-jump nil)
  (evil-respect-visual-line-mode t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-calendar-want-org-bindings t)
  :config
  (evil-collection-init)
  (evil-collection-calendar-setup))

(message "Evil loaded in...")

(use-package ivy
  :diminish
  :bind (:map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (:map ivy-minibuffer-map
              ("C-j" . ivy-next-line)
              ("C-k" . ivy-previous-line)))

(message "Ivy and Counsel loaded in...")

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package helpful)

(use-package flycheck)
;;  :init (global-flycheck-mode))

(use-package company)
;;:hook (prog-mode . global-company-mode))

(use-package restart-emacs)

(use-package magit)

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

(use-package ox-hugo
  :pin melpa
  :after ox)
