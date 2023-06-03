(defconst my-gnu/linux-laptop-p
  (and (equal system-type 'gnu/linux)
       (eq (string-search "android" system-configuration) nil))
  "If the value is t, emacs is running on my gnu/linux laptop.")

(defconst my-android-phone-p
  (not (eq (string-search "android" system-configuration) nil))
  "If the value is t, emacs is running inside termux, on my android phone.")

(defconst my-windows-laptop-p
  (equal system-type 'windows-nt)
  "If the value is t, emacs is running on my windows laptop.")

(setq gc-cons-threshold (* 64 1000000))

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors nil)
  ;; (setq native-comp-async-report-warnings-errors 'silent)
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/"
                                 user-emacs-directory)))

(fset 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(setq echo-keystrokes 0.01)

(menu-bar-mode -1)
(column-number-mode)

(unless my-android-phone-p
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)
  (scroll-bar-mode -1))

(setq inhibit-startup-screen t
      scroll-conservatively 101)

;; prevent the annoying bell sounds
(setq visible-bell t)

(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "")

(defun my-kill-current-buffer ()
  "Bury *scratch* buffer instead of killing it. Kill other buffers immediately."
  (interactive)
  (if (equal (buffer-name) "*scratch*")
      (bury-buffer)
    (kill-buffer (current-buffer))))

(global-set-key (kbd "C-x k") 'my-kill-current-buffer)

(when my-gnu/linux-laptop-p
  (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
  (add-to-list 'default-frame-alist `(alpha . (95 . 95))))

(setq my-fixed-pitch-font "Cascadia Code")
(setq my-variable-pitch-font "Open Sans")

(defun my-set-font-faces ()
  "Set font faces."
  (set-face-attribute 'default nil
                      :font my-fixed-pitch-font
                      :weight 'normal
                      :height 110)

  (set-face-attribute 'fixed-pitch nil
                      :font my-fixed-pitch-font
                      :weight 'regular
                      :height 1.0)

  (set-face-attribute 'variable-pitch  nil
                      :font my-variable-pitch-font
                      :height 1.0))

(my-set-font-faces)

(global-set-key (kbd "M-SPC")
                (lambda ()
                  "Instruct `cycle-spacing' to delete newlines too."
                  (interactive)
                  (cycle-spacing -1)))

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(set-register ?E `(file . ,(concat
                            (file-name-as-directory
                             user-emacs-directory)
                            "config.org")))

(when my-gnu/linux-laptop-p
  (set-register ?Q '(file . "~/.config/qtile/qtile.org"))
  (set-register ?B '(file . "~/.local/data/bookmarks")))

(when my-windows-laptop-p
  (set-register ?B '(file . "~/bookmarks.txt")))

(global-unset-key (kbd "C-z"))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (package-install 'use-package)))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose t)

(use-package no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq backup-directory-alist
      `(("." . ,(no-littering-expand-var-file-name "backup/"))))

;; prevent Emacs form littering into init.el
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (setq all-the-icons-scale-factor 1.25))

(use-package nerd-icons
  :config
  (setq nerd-icons-scale-factor 1.25))

(defvar my-light-theme 'doom-gruvbox-light)
(defvar my-dark-theme 'doom-dracula)
(defvar my-current-theme-variant 'dark)

(defun my-toggle-theme ()
  "Toggle between light and dark themes, set by variables `my-light-theme'
and `my-dark-theme'"
  (interactive)
  (if (eq my-current-theme-variant 'dark)
      (progn
        (disable-theme my-dark-theme)
        (load-theme my-light-theme t)
        (setq my-current-theme-variant 'light))
    (disable-theme my-light-theme)
    (load-theme my-dark-theme t)
    (setq my-current-theme-variant 'dark))
  (message "OK: %s theme activated" (symbol-name my-current-theme-variant)))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme (if (eq my-current-theme-variant 'dark)
                  my-dark-theme
                my-light-theme) t)
  (set-face-attribute 'font-lock-comment-face  nil
                      :slant 'italic))

(use-package doom-modeline
  :config
  (setq doom-modeline-icon t)
  (setq doom-modeline-height 12))
  (doom-modeline-mode 1)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :init
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t)
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-items '((recents  . 3)
                          (projects . 5)
                          (registers . 3))))

;;(use-package dashboard)

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

(use-package vertico
  :config
  (setq vertico-cycle t)
  (setq vertico-resize t)
  (vertico-mode)
  (vertico-reverse-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles . (partial-completion))))))

(use-package marginalia
  :after vertico
  :config
  (setq marginalia-align 'right)
  (setq marginalia-annotators '(marginalia-annotators-heavy
                                marginalia-annotators-light nil))
  (marginalia-mode))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (set-face-attribute 'aw-leading-char-face nil
                      :font my-fixed-pitch-font
                      :weight 'bold
                      :slant 'normal
                      :foreground "yellow"
                      :height 200)
  :custom
  (aw-keys '(?j ?k ?l ?f ?g ?h ?a ?s ?d ?i ?e ?n ?m)))

(winner-mode)

(setq split-height-threshold nil)
(setq split-width-threshold 145)

(setq display-buffer-alist
      `((,(concat "\\*.*"
                  "\\(Backtrace"
                  "\\|Compile-Log"
                  "\\|compilation"
                  "\\|Warnings"
                  "\\|Compile-Log"
                  "\\|compilation"
                  "\\|Calendar"
                  "\\|Flycheck"
                  "\\|Flymake"
                  "\\|vterm"
                  "\\).*\\*")
         (display-buffer-in-side-window)
         (window-height . 0.25)
         (side . bottom))))

(setq-default window-divider-default-places t)
(setq-default window-divider-default-bottom-width 2)
(setq-default window-divider-default-right-width 2)
(window-divider-mode t)
(set-face-attribute 'window-divider nil
                    :foreground "#b16e75")

(defun my-org-font-face-setup ()
  "Set necessary font faces in `org-mode'."

  (dolist (face '((org-level-1 . 1.25)
                  (org-level-2 . 1.15)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil
                        :height (cdr face)
                        :weight 'bold))

  ;; fixed-pitch face setup
  (dolist (face '(org-table
                  org-formula org-block
                  org-code org-verbatim
                  org-checkbox line-number
                  org-special-keyword
                  line-number-current-line))
    (set-face-attribute face nil :inherit 'fixed-pitch))

  (dolist (face '(org-table
                  org-document-info-keyword
                  org-meta-line))
    (set-face-attribute face nil
                        :foreground nil
                        :inherit '(shadow fixed-pitch))))

(use-package org
  :pin org
  :commands
  (org-capture org-agenda)
  :hook
  (org-mode . (lambda ()
                (my-org-font-face-setup)
                (if my-gnu/linux-laptop-p (flyspell-mode))
                (org-indent-mode)
                (visual-line-mode 1)))
  :config
  (setq org-directory "~/Org")
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t)
  (setq org-startup-folded 'overview)
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  (advice-add 'org-refile
              :after 'org-save-all-org-buffers)

  ;; Add a clock sound for `org-timer-set-timer'
  (let ((sound-file "~/.local/data/bell.wav"))
    (if (file-exists-p sound-file)
        (setq org-clock-sound sound-file))))

(global-set-key (kbd "C-c c") #'org-capture)

(setq org-capture-templates
      `( ("t" "Todo item" entry
          (file+headline "inbox.org" "Tasks")
          ,(concat "* %^{|TODO|BLOG|SOMEDAY} %^{Title}\n"
                   ":PROPERTIES:\n"
                   ":CREATED: %U\n"
                   ":END:\n"
                   "Note: %?\n"))

         ("i" "Project idea" entry
          (file+headline "inbox.org" "Project Ideas")
          ,(concat "* PROJECT %^{Title}\n"
                   ":PROPERTIES:\n"
                   ":CREATED: %U\n"
                   ":END:\n"
                   "Note: %?\n"))

         ("n" "Quick note" entry
          (file+headline "inbox.org" "Quick Notes")
          ,(concat "* %^{Title}\n"
                   ":PROPERTIES:\n"
                   ":CREATED: %U\n"
                   ":END:\n"
                   "Note: %?")
          :empty-lines-after 1)

         ("r" "Reading list item" entry
          (file+headline "inbox.org" "Reading List")
          ,(concat "* READ %^{Description}\n"
                   ":PROPERTIES:\n"
                   ":CREATED: %U\n"
                   ":TOPIC: %^{Topic}\n"
                   ":END:\n"
                   "URL: %(current-kill 0)\n"
                   "Note: %?\n")
          :empty-lines-after 1)))

(global-set-key (kbd "C-c a") #'org-agenda)

(setq org-agenda-files '("~/Org/inbox.org"))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "SOMEDAY(s)" "|" "DONE(x!)")
        (sequence "READ(r)" "BLOG(b)" "PROJECT(p)" "|" "DONE(x!)")))

(setq org-enforce-todo-dependencies t)
(setq org-track-ordered-property-with-tag t)
(setq org-agenda-dim-blocked-tasks t)



(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉")))

(with-eval-after-load 'org
  (require 'org-tempo)
  (dolist (language '(("el" . "src emacs-lisp")
                      ("py" . "src python")
                      ("sh" . "src shell")
                      ("js" . "src js")))
    (add-to-list 'org-structure-template-alist language)))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t))))

(setq org-confirm-babel-evaluate nil)

(setq c-basic-offset 2) ;; defined in cc.el
(add-hook 'prog-mode-hook
          (lambda ()
            ;;(local-set-key (kbd "C-<tab>") 'yas-expand)
            ;; (set-fringe-style 8)
            (hl-line-mode)
            (electric-pair-local-mode)))

(use-package project
  :demand t)

(use-package magit
  :commands magit-status
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))

(use-package company
  :commands (company-mode global-company-mode)
  :hook (prog-mode text-mode org-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.2))

(use-package flycheck
  :commands (global-flycheck-mode flycheck-mode)
  :hook prog-mode)

(use-package yasnippet
  :requires warnings  ;; for `warning-suppress-types' below
  :config
  (setq yas-snippet-dirs
        `( ,(concat user-emacs-directory "snippets")))
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (yas-global-mode 1)
  (yas-reload-all))

(use-package eglot
  :ensure nil
  :commands eglot
  :autoload eglot-ensure
  :hook ((c-mode c++-mode python-mode) . eglot-ensure))

(use-package lsp-mode
  :disabled t
  :commands (lsp lsp-deferred)
  :hook
  (my-lsp-enabled-modes . lsp)
  :init
  (setq lsp-headerline-breadcrumb-enable 'nil)
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq-default lsp-clients-clangd-args
                '("--cross-file-rename"
                  "--enable-config"
                  "--fallback-style=WebKit"
                  "--clang-tidy"
                  "--clang-tidy-checks='*'"
                  "--suggest-missing-includes"
                  "--header-insertion=iwyu"
                  "--header-insertion-decorators=0")))

(use-package lsp-ui
  :disabled t
  :after lsp-mode)

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package web-mode
  :mode ("\\.html?$" "\\.djhtml$" "\\.mustache\\'" "\\.phtml\\'"
          "\\.as[cp]x\\'" "\\.erb\\'" "\\.hbs\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-html-entities-fontification t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-auto-close-style 2))

(use-package emmet-mode
  :hook (web-mode css-mode sgml-mode))

(use-package python
  :interpreter
  ("python" . python-mode)
  ("python3" . python-mode))

(use-package pyvenv
  :after python
  :hook python-mode)

(use-package js
  :interpreter "node"
  :init
  (setq js-jsx-syntax t)
  :config
  (setq js-indent-level 2))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local company-backends
                        '(company-elisp
                          company-files
                          company-yasnippet))
            (company-mode)))

(use-package lsp-java
  :disabled t
  :after lsp-mode
  :hook
  (java-mode . lsp))

(use-package eglot-java
  ;; :hook java-mode
  :bind
  (:map eglot-java-mode-map
        ("C-c l n" . eglot-java-file-new)
        ("C-c l x" . eglot-java-run-main)
        ("C-c l t" . eglot-java-run-test)
        ("C-c l N" . eglot-java-project-new)
        ("C-c l T" . eglot-java-project-build-task)
        ("C-c l R" . eglot-java-project-build-refresh)))

(defvar my-google-java-format-jar-file
  (concat (file-name-as-directory (concat user-emacs-directory "var"))
          "google-java-format.jar")
  "Complete path of the google java formatter jar file.")

(defun my-download-google-java-format-jar-file ()
  "Download the google java formatter jar file from github."
  (interactive)
  (url-copy-file "https://github.com/google/google-java-format/releases/download/v1.16.0/google-java-format-1.16.0-all-deps.jar"
                 my-google-java-format-jar-file 1))

(defun my-format-java-buffer ()
  "Format current java buffer to comply with google style."
  (interactive nil 'java-mode)
  (let ((temp-buffer (generate-new-buffer "*java-format*"))
        (temp-file (make-temp-file "java-format-error" nil))
        ;; Always use 'utf-8-unix' & ignore the buffer coding system.
        (default-process-coding-system '(utf-8-unix . utf-8-unix)))
    (call-process-region nil nil "java" nil
                         `(,temp-buffer ,temp-file) nil
                         "-jar" my-google-java-format-jar-file "-")
    (if (> (buffer-size temp-buffer) 0)
        ;; Replace buffer with formatted code
        (replace-buffer-contents temp-buffer)
      (message "Error: could not format current buffer!"))
    (kill-buffer temp-buffer)
    (delete-file temp-file)))

(use-package format-all
  :commands (format-all-buffer format-all-region format-all-mode)
  :autoload fomat-all-ensure-formatter
  :hook
  (prog-mode . format-all-ensure-formatter)
  :config
  (setq-default format-all-formatters
                '(("C" (clang-format "-style=file"))
                  ("C++" (clang-format "-style=file"
                                       "--fallback-style=WebKit"))
                  ("CSS" prettier)
                  ("Emacs Lisp" emacs-lisp)
                  ("Go" gofmt)
                  ("Java" (clang-format "-style=file"))
                  ("JavaScript" prettier)
                  ("Markdown" prettier)
                  ("Python" black)
                  ("TypeScript" prettier))))

(use-package restclient
  :commands (restclient-mode))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump)
         ("C-x C-d" . dired))
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-listing-switches "-lhAX --group-directories-first"))


(use-package all-the-icons-dired
  :commands all-the-icons-dired-mode
  :hook
  (dired-mode . (lambda ()
                  (if (window-system)
                      (all-the-icons-dired-mode)))))

(use-package tab-bar
  :ensure nil
  :config
  ;; set better faces for tabs
  ;; (set-face-attribute 'tab-bar nil :inherit 'mode-line)
  ;; (set-face-attribute 'tab-bar-tab nil
  ;;                     :weight 'bold
  ;;                     :slant 'italic
  ;;                     :underline t
  ;;                     :foreground "#aaee77")
  ;; (set-face-attribute 'tab-bar-tab-inactive nil
  ;;                     :slant 'italic
  ;;                     :foreground "#afafaf")
  (setq tab-bar-show nil)  ;; don't show the tab-bar
  (setq tab-bar-new-tab-choice t) ;; open the current buffer in new tab
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-button-show nil)
  (setq tab-bar-close-last-tab-choice 'tab-bar-mode-disable))

(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)

(unless my-android-phone-p
  (use-package olivetti
    :commands olivetti-mode
    :hook
    (org-mode Info-mode)
    :config
    (set-default 'olivetti-body-width 100)))

(use-package elfeed
  :hook
  (elfeed-show-mode . visual-line-mode)
  :config
  (set-face-attribute 'elfeed-search-unread-title-face nil
                      :font my-fixed-pitch-font
                      :slant 'italic
                      :weight 'bold)
  (setq elfeed-feeds
   '("http://nullprogram.com/feed/"
     "https://levelofindirection.com/main.rss"
     "https://blog.petrzemek.net/feed/"
     "https://planet.emacslife.com/atom.xml")))

(use-package erc
  :commands
  (erc-tls erc)
  :config
  (setq erc-server "irc.libera.chat")
  (setq erc-port 6697)
  (setq erc-prompt (lambda () (concat (buffer-name) ">")))
  (setq erc-nick "px86")
  (setq erc-fill-column 100))

(use-package pulse
  :ensure nil  ; built-in package
  :autoload
  (pulse-momentary-highlight-one-line pulse-momentary-highlight-region)
  :config
  (setq pulse-flag t)
  (setq pulse-delay 0.04)
  (set-face-attribute 'pulse-highlight-start-face nil
                      :background "#87ceeb"))


(dolist (command '(scroll-up-command
                   scroll-down-command
                   recenter-top-bottom
                   other-window
                   isearch-repeat-forward
                   isearch-repeat-backward
                   ace-window))
  (advice-add command :after
              (lambda (&rest args)
                "Momentarily highlight current line."
                (pulse-momentary-highlight-one-line (point)))))


(advice-add 'kill-ring-save :after
            (lambda (&rest args)
              "Momentarily highlight currently active region, otherwise the current line."
              (if mark-active
                  (pulse-momentary-highlight-region (region-beginning) (region-end))
                (pulse-momentary-highlight-one-line (point)))))

(when my-gnu/linux-laptop-p
  ;; needed for vc-git-root function
  (require 'vc-git)

  (defun my-launch-terminal ()
    "Launch a terminal in project root or in current working directory."
    (interactive)
    (let* ((term (getenv "TERMINAL"))
           (terminal (if term term "xterm"))
           (filename (buffer-file-name))
           (dir (if filename
                    (vc-git-root filename)
                  nil))
           (default-directory (or dir
                                  default-directory)))
      (start-process "Terminal" nil terminal)))

  (defun my-launch-terminal-in-cwd ()
    "Launch a terminal in the current working directory."
    (interactive)
    (let* ((term (getenv "TERMINAL"))
           (terminal (if term term "xterm")))
      (start-process "Terminal" nil terminal)))

  (global-set-key (kbd "s-t") #'my-launch-terminal))

(defun my-normalize-string-for-filename (string)
  "Downcase and remove special charactes from string."
  (downcase (string-join
             (split-string string
                           "[] ~!@#$%^&*()+={}[\\|/;:,.'\"<>?]+" t "[ _-]") "-")))

(defun my-filter-list (predicate list)
  "Filter LIST items through the PREDICATE function."
  (let ((newlist '()))
    (dolist (item list)
      (if (funcall predicate item)
          (push item newlist)))
    newlist))


(defvar my-full-name "Pushkar Raj" "My full name")
(defvar my-email "px86@protonmail.com" "My email address.")
(defvar my-blog-dir
  (expand-file-name "~/Programming/repos/px86.github.io")
  "My blog's root directory.")

(defun my-new-blog-post ()
  "Create a new blog post."
  (interactive)
  (let* ((folder (completing-read "Select blog subfolder: "
                                  (my-filter-list
                                   (lambda (f)
                                     (and (file-directory-p f)
                                          (not (member (file-name-base f)
                                                       '("." ".." "assets")))))
                                   (directory-files (expand-file-name "source" my-blog-dir) t))
                                  nil t))
         (title (read-string (format "[%s] Title: " (file-name-base folder))))
         (subtitle (read-string "Subtitle: " ))
         (filename (concat (my-normalize-string-for-filename title) ".org"))
         (filepath (expand-file-name filename folder)))
    (if (file-exists-p filepath)
        "Error: Oops! file already exists"
      (find-file filepath)
      (insert (format
               (concat
                "#+TITLE: %s\n"
                "#+SUBTITLE: %s\n"
                "#+AUTHOR: %s\n"
                "#+EMAIL: %s\n"
                "#+DATE: %s\n\n")
               title
               subtitle
               my-full-name
               my-email
               (format-time-string "[%Y-%m-%d %a]"))))))

(setq initial-buffer-choice
      (lambda () (get-buffer "*dashboard*")))

(add-hook 'server-after-make-frame-hook
                       #'my-set-font-faces)

;; Lower the GC threshold, again
(setq gc-cons-threshold 16000000)
