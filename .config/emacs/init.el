;; Higher threshold for less frequent garbage collections during startup.
(setq gc-cons-threshold (* 64 1000000)) ;;; 64MB

(defun pr/display-startup-time ()
  "Displays a message indicating the Emacs startup time and number of garbage collections."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'pr/display-startup-time)

(fset 'yes-or-no-p 'y-or-n-p)

;; Set prefered character encoding to UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; Show me what I type, immediately
(setq echo-keystrokes 0.01)

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(scroll-bar-mode -1)
(column-number-mode)

(setq inhibit-startup-screen t
      scroll-conservatively 101)

(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "")

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist `(alpha . (100 . 100)))

;; Fonts
(setq pr/fixed-pitch-font "JetBrains Mono NL")

(setq pr/variable-pitch-font "FiraGO")

(setq pr/org-heading-font "Fira Code-12")

(defun pr/set-font-faces ()
  "Sets font faces."
  (set-face-attribute 'default nil
                      :font pr/fixed-pitch-font
                      :height 102
                      :weight 'normal)

  (set-face-attribute 'fixed-pitch nil
                      :font pr/fixed-pitch-font
                      :height 1.0)

  (set-face-attribute 'variable-pitch  nil
                      :font pr/variable-pitch-font
                      :height 105
                      :weight 'normal))

(pr/set-font-faces)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
;; (setq use-package-verbose t)

(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; prevent Emacs form littering into init.el
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))

;;; required for doom-modline
(use-package all-the-icons)

(use-package doom-themes
  :config (load-theme 'doom-nord t))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-icon t)
  :custom
  (doom-modeline-height 15))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-banner-logo-title "Welcome to GNU Emacs")
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents  . 5)
                     (projects . 5)
                     (agenda . 5)
                     (registers . 5))))

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

(use-package vertico
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles . (partial-completion))))))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(defun pr/org-font-setup ()
  "Set necessary font faces in `org-mode'."

  (dolist (face '(org-level-1 org-level-2
                  org-level-3 org-level-4
                  org-level-5 org-level-6
                  org-level-7 org-level-8))
    (set-face-attribute face nil
                        :font pr/org-heading-font
                        :weight 'bold))

  ;; fixed-pitch setup
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)

  (dolist (face '(org-table org-formula
                  org-checkbox line-number
                  line-number-current-line))
    (set-face-attribute face nil :inherit 'fixed-pitch))

  (dolist (face '(org-code org-table
                  org-verbatim))
    (set-face-attribute face nil :inherit '(shadow fixed-pitch)))

  (dolist (face '(org-special-keyword
                  org-meta-line))
    (set-face-attribute face nil
                        :inherit '(font-lock-comment-face fixed-pitch))))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :bind
  ("C-c a" . org-agenda)
  :hook
  (org-mode . (lambda ()
                (pr/org-font-setup)
                (org-indent-mode)
                (visual-line-mode 1)))
  :custom
  (org-ellipsis " ▾")
  (org-directory "~/org")
  :config
  (add-to-list 'org-modules 'org-habit)
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

(setq org-default-notes-file "~/org/notes.org")
(setq org-agenda-files '("~/org/tasks.org"))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-span 'day)
(setq org-habit-show-habits-only-for-today t)

;; todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c)")
        (sequence "READ(r)" "STUDY(s)" "WRITE(w)" "|" "DONE(x)")))

(use-package org-capture
  :ensure nil
  :commands (org-capture)
  :bind ("C-c c" . org-capture)
  :init
  (setq org-capture-templates
        `(("t" "Personal TODO item" entry
           (file+headline "tasks.org" "Personal")
           ,(concat "* TODO %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CREATED: %U\n"
                    ":END:\n"))

          ("u" "University related work" entry
           (file+headline "tasks.org" "University")
           ,(concat "* %^{|TODO|READ|WRITE|STUDY} %^{Title}\n"
                    "DEADLINE: %^{DEADLINE}t\n"
                    ":PROPERTIES:\n"
                    ":CREATED: %U\n"
                    ":END:\n"
                    "Note: %?\n"))

          ("q" "Question in mind" entry
           (file+headline "tasks.org" "Figure this out")
           ,(concat "* %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CREATED: %U\n"
                    ":END:\n"
                    "_Initial Thought_\n"
                    "%?"))

          ("r" "Reading list item" entry
           (file+headline "tasks.org" "Reading List")
           ,(concat "* READ %^{Description}\n"
                    ":PROPERTIES:\n"
                    ":CREATED: %U\n"
                    ":TOPIC: %^{Topic}\n"
                    ":END:\n"
                    "URL: %(current-kill 0)\n"
                    "Note: %?\n")
           :empty-lines-after 1))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉")))

(use-package visual-fill-column
  :hook
  (org-mode . (lambda ()
                (setq visual-fill-column-width 120)
                (setq visual-fill-column-center-text t)
                (visual-fill-column-mode 1))))

(with-eval-after-load 'org
  (require 'org-tempo)
  (dolist (language '(("el" . "src emacs-lisp")
                      ("py" . "src python")
                      ("sh" . "src shell")
                      ("js" . "src js")
                      ("cpp" . "src C++ :includes <iostream>")))
    (add-to-list 'org-structure-template-alist language)))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (js . t)
     (shell . t)
     (python . t)
     (emacs-lisp . t))))

(setq org-confirm-babel-evaluate nil)

(setq org-clock-sound "~/.local/data/bell.wav")

(add-hook 'prog-mode-hook
          (lambda ()
            (hl-line-mode)
            (display-line-numbers-mode t)
            (electric-pair-local-mode)))

(use-package project
  :defer 0)

(use-package lsp-mode
  :commands  (lsp lsp-deferred)
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  (web-mode . lsp)
  (js2-mode . lsp)
  :init
  (setq lsp-headerline-breadcrumb-enable 'nil)
  (setq lsp-keymap-prefix "C-c l"))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.0))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        `( ,(concat user-emacs-directory "snippets")))
  (yas-global-mode 1)
  (yas-reload-all))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package web-mode
  :mode (("\\.html?$" . web-mode)
         ("\\.djhtml$" . web-mode)
         ("\\.tsx$" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.hbs\\'" . web-mode))
  :hook ((web-mode . company-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-html-entities-fontification t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-auto-close-style 2))

(use-package emmet-mode
  :hook
  (web-mode  . emmet-mode)
  (css-mode  . emmet-mode)
  (sgml-mode . emmet-mode))

(use-package pyvenv
  :hook (python-mode . pyvenv-mode))

(use-package js2-mode
  :mode "\\.js\\'")

(setq explicit-shell-file-name "/bin/bash")

(use-package dired
    :ensure nil
    :commands (dired dired-jump)
    :bind (("C-x C-j" . dired-jump))
    :custom ((dired-listing-switches "-lhAX --group-directories-first"))
    :hook (dired-mode . (lambda () 
                          (dired-hide-details-mode))))

  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-new-tab-choice "*dashboard*")
  (tab-bar-border 5)
  (tab-bar-separator "  ")
  (tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
  (tab-bar-tab-name-truncated-max 16)
  (tab-bar-close-button-show 'selected)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable))

;; undo-redo window configuration with C-c left and C-c right
(winner-mode)

(setq display-buffer-alist
      `((,(concat "\\*.*"
                  "\\(Backtrace"
                  "\\|Warnings"
                  "\\|Compile-Log"
                  "\\|compilation"
                  "\\|Flycheck"
                  "\\|Flymake"
                  "\\|vterm"
                  "\\|ansi-term"
                  "\\|term"
                  "\\).*\\*")
         (display-buffer-in-side-window)
         (window-height . 0.33)
         (side . bottom))))

(setq initial-buffer-choice
      (lambda () (get-buffer "*dashboard*")))

(add-hook 'server-after-make-frame-hook
                       #'pr/set-font-faces)

(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package elfeed
  :hook
  (elfeed-show-mode . (lambda ()
                        (visual-line-mode)
                        (visual-fill-column-mode)))
  :custom
  (elfeed-feeds
   '(("https://www.reddit.com/r/emacs.rss" reddit emacs)
     ("https://www.reddit.com/r/python.rss" reddit python)
     ("https://www.reddit.com/r/cpp.rss" reddit C++)
     ("https://www.reddit.com/r/git.rss" reddit git)
     ("https://www.reddit.com/r/javascript.rss" reddit javascript)
     ("https://javax0.wordpress.com/feed/" PeterVerhas Java)
     ("https://planet.gnu.org/rss20.xml" GNU)
     ("https://sachachua.com/blog/category/emacs/feed/" SachaChua emacs)
     ("https://herbsutter.com/gotw/feed" HerbSutter C++))))

(defun pr/edit-emacs-config ()
  "Edit the Emacs configuration file."
  (interactive)
  (find-file (expand-file-name "config.org" user-emacs-directory)))

(global-set-key (kbd "C-c e") 'pr/edit-emacs-config)
(global-set-key (kbd "C-c t") 'tab-bar-new-tab)

;; Lower the GC threshold, again
(setq gc-cons-threshold 16000000)
