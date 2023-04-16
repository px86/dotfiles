;; These are the only three types of machines that I use emacs on.
(defvar pr/machine-type
  (if (equal system-type 'gnu/linux)
      (if (equal (string-search "android" system-configuration)
                 nil)
          "gnu/linux-laptop"  ; my gnu/linux laptop
        "android-phone")      ; emacs running in termux on android
    "windows-laptop")         ; work-laptop running windows
  "The value is string indicating the system type on which emacs is currently running.")

;; Higher threshold for less frequent garbage collections during startup.
(setq gc-cons-threshold (* 64 1000000)) ;;; 64MB

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors nil)
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/" user-emacs-directory)))

(fset 'yes-or-no-p 'y-or-n-p)

;; Set prefered character encoding to UTF-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; Show me what I type, immediately
(setq echo-keystrokes 0.01)

(menu-bar-mode -1)
(column-number-mode)

(unless (equal pr/machine-type "android-phone")
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)
  (scroll-bar-mode -1))

(setq inhibit-startup-screen t
      scroll-conservatively 101)

(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message "")

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist `(alpha . (95 . 95)))

;; Fonts
(setq pr/fixed-pitch-font "Cascadia Code")
(setq pr/variable-pitch-font "Open Sans")

(defun pr/set-font-faces ()
  "Sets font faces."
  (set-face-attribute 'default nil
                      :font pr/fixed-pitch-font
                      :weight 'normal
                      :height 110)

  (set-face-attribute 'fixed-pitch nil
                      :font pr/fixed-pitch-font
                      :weight 'regular
                      :height 1.0)

  (set-face-attribute 'variable-pitch  nil
                      :font pr/variable-pitch-font
                      :height 1.0))

(pr/set-font-faces)

;; Repeated invocation of M-<space> will cycle through
;; 1. single space, 2. no space, 3. original
(substitute-key-definition 'just-one-space
                           (lambda ()
                             (interactive)
                             (cycle-spacing -1 t))
                           (current-global-map))

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(set-register ?E `(file . ,(concat
                           (file-name-as-directory
                            user-emacs-directory)
                           "config.org")))

(set-register ?Q '(file . "~/.config/qtile/config.py"))
(set-register ?B '(file . "~/.local/data/bookmarks"))

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

(setq pr/light-theme 'doom-gruvbox-light)
(setq pr/dark-theme 'doom-dracula)
(setq pr/current-theme-variant "dark")

(defun pr/toggle-theme ()
  "Toggle between light and dark themes, set by variables `pr/light-theme'
and `pr/dark-theme'"
  (interactive)
  (if (string= pr/current-theme-variant "dark")
      (progn
        (disable-theme pr/dark-theme)
        (load-theme pr/light-theme t)
        (setq pr/current-theme-variant "light"))
    (disable-theme pr/light-theme)
    (load-theme pr/dark-theme t)
    (setq pr/current-theme-variant "dark"))
  (message "%s theme activated" pr/current-theme-variant))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme pr/dark-theme t)
  (set-face-attribute 'font-lock-comment-face  nil
                      :slant 'italic))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-icon t)
  :custom
  (doom-modeline-height 12))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents  . 3)
                     (projects . 5)
                     (registers . 3))))

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

(use-package vertico
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  :init
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
  :custom
  (marginalia-align 'right)
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(defun pr/org-font-face-setup ()
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
                (pr/org-font-face-setup)
                (flyspell-mode)
                (org-indent-mode)
                (visual-line-mode 1)))
  :custom
  (org-directory "~/Org")
  (org-ellipsis " ▾")
  (org-hide-emphasis-markers t)
  (org-startup-folded 'overview)
  :config
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
          (file+headline "tasks.org" "Tasks")
          ,(concat "* %^{|TODO|READ|WRITE} %^{Title}\n"
                   "DEADLINE: %^{DEADLINE}t\n"
                   ":PROPERTIES:\n"
                   ":CREATED: %U\n"
                   ":END:\n"
                   "Note: %?\n"))

         ("n" "Quick note" entry
          (file+headline "notebook.org" "Quick Notes")
          ,(concat "* %^{Title}\n"
                   ":PROPERTIES:\n"
                   ":CREATED: %U\n"
                   ":END:\n"
                   "Note: %?")
          :empty-lines-after 1)

         ("r" "Reading list item" entry
          (file+headline "notebook.org" "Reading List")
          ,(concat "* READ %^{Description}\n"
                   ":PROPERTIES:\n"
                   ":CREATED: %U\n"
                   ":TOPIC: %^{Topic}\n"
                   ":END:\n"
                   "URL: %(current-kill 0)\n"
                   "Note: %?\n")
          :empty-lines-after 1)))

(global-set-key (kbd "C-c a") #'org-agenda)

(setq org-agenda-files '("~/Org/tasks.org"))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "READ(r)" "WRITE(w)" "|" "COMPLETED(c@)")
        (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)"
                  "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

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

(add-hook 'prog-mode-hook
          (lambda ()
            ;; <tab> is already taken by company expand
            (local-set-key (kbd "C-<tab>") 'yas-expand)
            (set-fringe-style 8)
            (hl-line-mode)
            (electric-pair-local-mode)))

(use-package project
  :defer 0)

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  (web-mode . lsp)
  (css-mode . lsp)
  (js-mode . lsp)
  (typescript-mode . lsp)
  (python-mode . lsp)
  ;; (clojure-mode . lsp)
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
                  "--header-insertion-decorators=0"))
  (setq-default c-basic-offset 4))

;; (use-package lsp-ui)

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
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (yas-global-mode 1)
  (yas-reload-all))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package web-mode
  :mode (("\\.html?$" . web-mode)
         ("\\.djhtml$" . web-mode)
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

(use-package js
  :ensure nil
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

(setq inferior-lisp-program "sbcl")

(use-package lsp-java
  :hook
  (java-mode . lsp))

;; TODO: discover the latest version of the jar file while downloading
(defun pr/download-google-java-formatter ()
  "Download google java format jar file from github."
  (interactive)
  (let ((filepath (expand-file-name
                   "~/.local/lib/google-java-format.jar")))
    (if (file-exists-p filepath)
        (message "Error: file '%s' already exists!" filepath)
      (start-process "wget" nil
                     "wget" "-q"
                     "https://github.com/google/google-java-format/releases/download/v1.15.0/google-java-format-1.15.0-all-deps.jar"
                     "-O" filepath)
      (message "done!"))))

(defun pr/format-java-buffer ()
  "Format current java buffer."
  (interactive)
  (let ((jarfile (expand-file-name
                   "~/.local/lib/google-java-format.jar"))
        (temp-buffer (generate-new-buffer " *java-format*"))
        (temp-file (make-temp-file "java-format-error" nil))
        ;; Always use 'utf-8-unix' & ignore the buffer coding system.
        (default-process-coding-system '(utf-8-unix . utf-8-unix)))

    (call-process-region nil nil "java" nil
                         `(,temp-buffer ,temp-file) nil
                         "-jar" jarfile "-")
    (if (> (buffer-size temp-buffer)
           0)
        ;; Replace buffer with formatted code
        (replace-buffer-contents temp-buffer)
      (message "Error: could not format current buffer!"))
    ;; Clean up
    (kill-buffer temp-buffer)
    (delete-file temp-file)))

(use-package format-all
  :hook
  (prog-mode . format-all-ensure-formatter)
  (c-mode . format-all-mode)
  (c++-mode . format-all-mode)
  (js-mode . format-all-mode)
  (python-mode . format-all-mode)
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
    :custom ((dired-listing-switches "-lhAX --group-directories-first"))
    :hook (dired-mode . (lambda ()
                          (dired-hide-details-mode))))

  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))

(use-package tab-bar
  :ensure nil
  :config
  ;; set better faces for tabs
  (set-face-attribute 'tab-bar nil :inherit 'mode-line)
  (set-face-attribute 'tab-bar-tab nil
                      :weight 'bold
                      :slant 'italic
                      :underline t
                      :foreground "#aaee77")
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :slant 'italic
                      :foreground "#afafaf")
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  ;; don't show close and new buttons
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable))

;; undo-redo window configuration with C-c left and C-c right
(winner-mode)

(setq split-height-threshold nil)
;; Split vertically if width >= 145 characters
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

(setq initial-buffer-choice
      (lambda () (get-buffer "*dashboard*")))

(add-hook 'server-after-make-frame-hook
                       #'pr/set-font-faces)

(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package olivetti
  :commands (olivetti-mode)
  :hook
  (org-mode . (lambda ()
                (olivetti-mode)))
  (Info-mode . (lambda ()
                 (olivetti-mode)))
  :config
  (set-default 'olivetti-body-width 100))

(use-package elfeed
  :hook
  (elfeed-show-mode . (lambda ()
                        (visual-line-mode)
                        (olivetti-mode)))
  :config
  (set-face-attribute 'elfeed-search-unread-title-face nil
                      :font pr/fixed-pitch-font
                      :slant 'italic
                      :weight 'bold)
  :custom
  (elfeed-feeds
   '(("http://nullprogram.com/feed/" nullprogram)
     ("https://jenkov.com/rss.xml" jenkov-java)
     ("https://javax0.wordpress.com/feed/" peter-verhas)
     ("https://levelofindirection.com/main.rss" level-of-indirection)
     ("https://blog.petrzemek.net/feed/" peter-zemek))))

(use-package erc
  :ensure nil
  :commands
  (erc-tls erc)
  :custom
  (erc-server "irc.libera.chat")
  (erc-port 6697)
  (erc-prompt (lambda () (concat (buffer-name) ">")))
  (erc-nick "px86")
  (erc-fill-column 100))

(use-package denote
  :commands
  (denote)
  :custom
  (denote-directory "~/Notes")
  (denote-known-keywords '("js" "nodejs" "cpp" "linux" "react" "emacs" "java")))

(defun pr/kill-current-buffer ()
  "Kill current buffer immediately."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'pr/kill-current-buffer)

(use-package pulse
  :defer t
  :ensure nil
  :custom
  (pulse-flag t)
  (pulse-delay 0.03)
  :config
  (set-face-attribute 'pulse-highlight-start-face nil
                      :background "#87ceeb"))

(defun pr/pulse-momentary-highlight-one-line (&rest args)
  "Momentarily highlight current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command
                   scroll-down-command
                   recenter-top-bottom
                   other-window
                   isearch-repeat-forward
                   isearch-repeat-backward))
  (advice-add command :after #'pr/pulse-momentary-highlight-one-line))

(when (equal pr/machine-type "gnu/linux-laptop")

  ;; needed for vc-git-root function
  (require 'vc-git)

  (defun pr/launch-terminal ()
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

  (defun pr/launch-terminal-in-cwd ()
    "Launch a terminal in the current working directory."
    (interactive)
    (let* ((term (getenv "TERMINAL"))
           (terminal (if term term "xterm")))
      (start-process "Terminal" nil terminal)))

  (global-set-key (kbd "s-t") #'pr/launch-terminal))

;; Lower the GC threshold, again
(setq gc-cons-threshold 16000000)
