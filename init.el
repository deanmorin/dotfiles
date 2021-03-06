;;; ---------------------------------------------------------------------------
;;; Appearance
;;; ---------------------------------------------------------------------------
(load-theme 'wombat)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(toggle-frame-maximized)

;;; ---------------------------------------------------------------------------
;;; Bootstrap use-package
;;; ---------------------------------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(with-eval-after-load 'use-package
  (setq use-package-always-ensure t))

;;; ---------------------------------------------------------------------------
;;; Packages
;;; ---------------------------------------------------------------------------
(use-package evil
  :init (evil-mode 1))

(use-package auto-complete
  :config (ac-config-default))

(use-package cider
  :config
  (setq cider-prompt-for-symbol nil)
  (define-key evil-normal-state-map (kbd "C-=") 'cider-find-var))

(use-package clojure-mode)

(use-package evil-commentary
  :init (evil-commentary-mode))

(use-package evil-leader
  :init (global-evil-leader-mode 1)
  :config
  (defun open-init-file ()
    (interactive)
    (find-file user-init-file))

  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "b"  'ido-switch-buffer
    "c"  'mode-specific-command-prefix
    "hf" 'describe-function
    "hv" 'describe-variable
    "hk" 'describe-key
    "init" 'open-init-file
    "k"  'kill-this-buffer
    "t"  'projectile-find-file
    "wd" 'delete-trailing-whitespace)

  (add-to-list 'evil-emacs-state-modes 'repl-mode))


(use-package evil-surround
  :pin melpa
  :init (global-evil-surround-mode 1))

(use-package evil-numbers
  :pin melpa-stable
  :config
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt))

(use-package exec-path-from-shell
  :pin melpa-stable
  :init (when (memq window-system '(mac ns))
          (exec-path-from-shell-initialize))
  :config
  (defun source-file-and-get-envs (filename)
    (let* ((cmd (concat ". " filename "; env"))
           (env-str (shell-command-to-string cmd))
           (env-lines (split-string env-str "\n"))
           (envs (mapcar (lambda (s) (replace-regexp-in-string "=.*$" "" s)) env-lines)))
      (delete "" envs)))

  (exec-path-from-shell-copy-envs (source-file-and-get-envs "~/.profile")))

(use-package flx-ido
  :pin melpa-stable
  :init
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  :config
  ;; Disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t
        ido-use-faces nil))
  ;; Tell emacs where is your personal elisp lib dir
  (add-to-list 'load-path "~/.emacs.d/lisp/")

(use-package guide-key
  :pin melpa-stable
  :diminish guide-key-mode
  :init (guide-key-mode 1)
  :config
  (setq guide-key/guide-key-sequence t
        guide-key/popup-window-position :bottom))

;; (use-package helm
;;   :pin melpa-stable
;;   :diminish helm-mode
;;   :bind ("M-x" . helm-M-x)
;;   :init (helm-mode 1))

(use-package nlinum
  :pin melpa
  :init (global-nlinum-mode 1))

(use-package paredit
  :pin melpa-stable
  :diminish paredit-mode)

(use-package projectile
  :pin melpa-stable
  :init (projectile-global-mode))

(use-package smooth-scrolling
  :pin melpa-stable)

(use-package undo-tree
  :pin melpa-stable
  :diminish undo-tree-mode
  :bind ("<f5>" . undo-tree-visualize)
  :init (global-undo-tree-mode 1)
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz"))))

(use-package yaml-mode
  :pin melpa-stable)

;;; ---------------------------------------------------------------------------
;;; Package Management
;;; ---------------------------------------------------------------------------
        ;;(company              . "melpa-stable")

;(ensure-package-installed
                          ;;'company

(setq-default indent-tabs-mode nil)

(setq column-number-mode t)
(show-paren-mode 1)


(setq ring-bell-function 'ignore)
(savehist-mode 1)

(add-to-list 'auto-mode-alist
             '("\\.psql$" . (lambda ()
                              (sql-mode)
                              (sql-highlight-postgres-keywords))))

(defun turn-on-lisp-modes-and-options ()
  (paredit-mode 1)
  (setq evil-symbol-word-search t))

(add-hook 'clojure-mode-hook 'turn-on-lisp-modes-and-options)
(add-hook 'emacs-lisp-mode-hook 'turn-on-lisp-modes-and-options)

;;; ---------------------------------------------------------------------------
;;; OCD
;;; ---------------------------------------------------------------------------
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; How many of the newest versions to keep
      kept-old-versions 5)   ; And how many of the old

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave" t)))

(setq vc-follow-symlinks t
      require-final-newline t)

;; Easier to reach than C-x in Dvorak, and C-x is decrement in Vim
(global-set-key (kbd "C-u") 'Control-X-prefix)
;; Easier to reach than M-x in Dvorak
(global-set-key (kbd "M-u") 'execute-extended-command)

(global-set-key (kbd "<M-s-left>") 'windmove-left)
(global-set-key (kbd "<M-s-right>") 'windmove-right)
(global-set-key (kbd "<M-s-up>") 'windmove-up)
(global-set-key (kbd "<M-s-down>") 'windmove-down)
