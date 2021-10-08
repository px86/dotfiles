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

(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(scroll-bar-mode -1)
(column-number-mode)

(setq inhibit-startup-screen t
      scroll-conservatively 101)

;; scratch buffer starts in org-mode
;; (setq initial-major-mode 'org-mode)
(setq initial-scratch-message "")

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist `(alpha . (100 . 100)))

(defun pr/set-font-faces ()
  "Sets font faces."
  (set-face-attribute 'default nil
                      :font "JetBrains Mono-10")

  (set-face-attribute 'fixed-pitch nil
                      :font "JetBrains Mono-10")

  (set-face-attribute 'variable-pitch  nil
                      :font "Noto Sans-12"))

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

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 2))

;;; required for doom-modline
(use-package all-the-icons)

(use-package doom-themes
  :config (load-theme 'doom-dracula t))

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
  (dashboard-items '((recents  . 5)
                     (projects . 5)
                     (agenda . 5)
                     (registers . 5))))

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

(use-package vertico
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit))
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

(use-package consult
  :demand t
  :bind (("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         :map minibuffer-local-map
         ("C-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(defun pr/org-font-setup ()
  "Set necessary font faces in `org-mode'."

  (dolist (face '(org-level-1 org-level-2
                  org-level-3 org-level-4
                  org-level-5 org-level-6
                  org-level-7 org-level-8))
    (set-face-attribute face nil :font "Fira Code-11" :weight 'semi-bold))

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

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path '("~/code"))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package magit
  :commands magit-status
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

(use-package lsp-mode
  :commands
  (lsp lsp-deferred)
  :hook
  (web-mode . lsp)
  (js2-mode . lsp)
  (c++-mode . lsp)
  (python-mode . lsp)
  (lsp-mode . (lambda ()
                (setq
                 lsp-headerline-breadcrumb-segments '(project file symbols)
                 lsp-headerline-breadcrumb-enable-diagnostics nil)
                (lsp-headerline-breadcrumb-mode)))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))


(use-package lsp-ui)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
        `( ,(concat user-emacs-directory "snippets")))
  (yas-global-mode 1)
  (yas-reload-all))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(defun pr-prettify-setup ()
  "Set default prettify symbols."
  (setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                         ("->" . ?→)
                                         ("=>" . ?⇒)
                                         ("!=" . ?≠)
                                         ("==" . ?≡)
                                         ("<=" . ?≤)
                                         (">=" . ?≥))))
(pr-prettify-setup)

(add-hook 'prog-mode-hook
          (lambda ()
            (hl-line-mode)
            (display-line-numbers-mode t)
            (prettify-symbols-mode)
            (electric-pair-local-mode)))

(use-package web-mode
  :mode (("\\.html$" . web-mode)
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
  (setq web-mode-auto-close-style 2))

(use-package emmet-mode
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

(use-package pyvenv
  :demand t
  :config
  (pyvenv-activate (expand-file-name "~/.local/share/virtualenvs/emacs")))

(use-package pipenv
  :defer 0
  :hook (python-mode . pipenv-mode)
  :init
  (setq pipenv-projectile-after-switch-function
        #'pipenv-projectile-after-switch-default))

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :config
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
  (setq js2-mode-show-strict-warnings nil))

(use-package vterm
  :ensure nil)

(use-package dired
    :ensure nil
    :commands (dired dired-jump)
    :bind (("C-x C-j" . dired-jump))
    :custom ((dired-listing-switches "-lhAX --group-directories-first"))
    :hook (dired-mode . (lambda () (dired-hide-details-mode)))
    :config
    (evil-collection-define-key 'normal 'dired-mode-map
      "h" 'dired-single-up-directory
      "l" 'dired-single-buffer))

  (use-package dired-single
    :commands (dired dired-jump))

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
                  "\\).*\\*")
         (display-buffer-in-side-window)
         (window-height . 0.33)
         (side . bottom))))

(setq initial-buffer-choice
      (lambda () (get-buffer "*dashboard*")))

(add-hook 'server-after-make-frame-hook
                       #'pr/set-font-faces)

(defun pr/edit-emacs-config ()
  "Edit the Emacs configuration file."
  (interactive)
  (find-file (expand-file-name "config.org" user-emacs-directory)))

(global-set-key (kbd "C-c e") 'pr/edit-emacs-config)
(global-set-key (kbd "C-c t") 'tab-bar-new-tab)

(defun pr/toggle-vterm ()
  "Toggle vterm window."
  (interactive)
  (if (get-buffer-window "*vterm*" t)
      (delete-window (get-buffer-window "*vterm*" t))
    (vterm)))

(global-set-key (kbd "s-<tab>") 'pr/toggle-vterm)

(global-unset-key (kbd "C-x C-b"))
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Lower the GC threshold, again
(setq gc-cons-threshold 16000000)
