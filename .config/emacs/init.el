;; Higher threshold for less frequent garbage collections during startup.
(setq gc-cons-threshold (* 64 1000000)) ;;; 64MB

(setq native-comp-async-report-warnings-errors nil)
(add-to-list 'native-comp-eln-load-path
             (expand-file-name "eln-cache/" user-emacs-directory))

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
(setq pr/fixed-pitch-font "Victor Mono")
(setq pr/variable-pitch-font "Open Sans")

(defun pr/set-font-faces ()
  "Sets font faces."
  (set-face-attribute 'default nil
                      :font pr/fixed-pitch-font
                      :weight 'bold
                      :height 110)

  (set-face-attribute 'fixed-pitch nil
                      :font pr/fixed-pitch-font
                      :weight 'normal
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

(set-register ?E '(file . "~/.config/emacs/config.org"))
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
(setq pr/dark-theme 'doom-rouge)
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
                     (registers . 5))))

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

(defun pr/org-font-setup ()
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

  ;; fixed-pitch setup
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
  :defer t
  :hook
  (org-mode . (lambda ()
                (pr/org-font-setup)
                (flyspell-mode)
                (org-indent-mode)
                (visual-line-mode 1)))
  :custom
  (org-ellipsis " ▾")
  (org-directory "~/Org")
  (org-hide-emphasis-markers t)
  (org-clock-sound "~/.local/data/bell.wav")
  :config
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

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
            (electric-pair-local-mode)))

(use-package project
  :defer 0)

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge
  :after magit)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  (web-mode . lsp)
  (js-mode . lsp)
  (typescript-mode . lsp)
  (python-mode . lsp)
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
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (yas-global-mode 1)
  (yas-reload-all))

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

(use-package js2-mode
  :commands (js2-minor-mode))

(use-package js
  :ensure nil
  :config
  (setq js-indent-level 2)
  (js2-minor-mode))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package restclient
  :commands (restclient-mode))

(use-package multiple-cursors
  :bind
  ("C-S-c C-S-c" . mc/edit-lines)
  ("C->" . mc/mark-next-like-this)
  ("C-<" . mc/mark-previous-like-this)
  ("C-c C-<" . mc/mark-all-like-this))

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

(setq display-buffer-alist
      `((,(concat "\\*.*"
                  "\\(Backtrace"
                  "\\|Compile-Log"
                  "\\|compilation"
                  "\\|Warnings"
                  "\\|Compile-Log"
                  "\\|compilation"
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
   '(("http://nullprogram.com/feed/" nullprogram))))

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

(defun pr/launch-xterm-in-cwd ()
  "Launch XTerm in the current working directory."
  (interactive)
  (start-process "XTerm" nil "xterm"))

(global-set-key (kbd "s-t") #'pr/launch-xterm-in-cwd)

;; Lower the GC threshold, again
(setq gc-cons-threshold 16000000)
