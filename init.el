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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(with-eval-after-load 'use-package
  (setq use-package-always-ensure t))
        ;use-package-always-pin 'melpa-stable))
;; (setq use-package-always-ensure t
;;(setq use-package-always-pin "melpa-stable")

;;; ---------------------------------------------------------------------------
;;; Packages
;;; ---------------------------------------------------------------------------
(use-package auto-complete
  :pin melpa-stable
  :config (ac-config-default))

(use-package cider
  :pin melpa-stable)

(use-package clojure-mode
  :pin melpa-stable
  :config
  (defun turn-on-paredit ()
    (paredit-mode 1))
  (add-hook 'clojure-mode-hook 'turn-on-paredit))

(use-package evil
  :pin melpa-stable
  :init (evil-mode 1)
  :config
  (setq-default evil-symbol-word-search t)
  ;(global-unset-key "H")
  ;(define-key evil-normal-state-map "H" 'evil-window-left)
  ;(define-key evil-normal-state-map "L" 'evil-window-right)
  ;(define-key evil-normal-state-map "K" 'evil-window-up)
  ;(define-key evil-normal-state-map "J" 'evil-window-down)
  )

(use-package evil-leader
  :pin melpa-stable
  :init (global-evil-leader-mode 1)
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "h"  'kill-this-buffer
    "t"  'projectile-find-file
    "wd" 'delete-trailing-whitespace
    "cc" 'evilnc-comment-or-uncomment-lines
    "cu" 'evilnc-comment-or-uncomment-lines
    ))

(use-package evil-surround
  :pin melpa
  :init (global-evil-surround-mode 1))

(use-package evil-nerd-commenter
  :pin melpa-stable)

(use-package evil-numbers
  :pin melpa-stable
  :config
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt))

(use-package exec-path-from-shell
  :pin melpa-stable
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  :config
  (defun env-names ()
    (let* ((env-string (shell-command-to-string "env"))
           (env-list (split-string env-string "\n")))
      (mapcar (lambda (s) (replace-regexp-in-string "=.*$" "" s)) env-list)))

  (defun read-lines-without-blank-lines-or-comments (filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (delete-blank-lines)
      (flush-lines "^\s*#")
      (split-string (buffer-string) "\n" t)))

  (defun extract-env (s)
    (replace-regexp-in-string "=.*$" ""
                              (replace-regexp-in-string "^export " "" s)))

  (defun read-envs-from-file (filename)
    (mapcar 'extract-env (read-lines-without-blank-lines-or-comments filename)))

  (exec-path-from-shell-copy-envs (read-envs-from-file "~/.profilelocal")))

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

(use-package nlinum
  :pin melpa
  :init (global-nlinum-mode 1))

(use-package paredit
  :pin melpa-stable)

(use-package projectile
  :pin melpa-stable
  :init (projectile-global-mode))

(use-package smooth-scrolling
  :pin melpa-stable)

(use-package undo-tree
  :pin melpa-stable
  :diminish undo-tree-mode
  :bind ("<f5>" . undo-tree-visualize)
  :init
  (global-undo-tree-mode 1)
  :config
  (setq undo-tree-auto-save-history t
        backup-directory-alist '(("." . "~/.emacs.d/undo"))))

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

(setq vc-follow-symlinks t)

;; Easier to reach than C-x in Dvorak, and C-x is decrement in Vim
(global-set-key (kbd "C-u") 'Control-X-prefix)
;; Easier to reach than M-x in Dvorak
(global-set-key (kbd "M-u") 'execute-extended-command)
