(setq inhibit-splash-screen t)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error)

(setq enable-recursive-minibuffers t) ; Allows Cx C-f M-: (insert user-init-file)


;;; Backups
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file "saves"))))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)


;;; Package manager

(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(setq use-package-always-ensure t)


;;; Better ELisp packages.

(use-package ht)
(use-package dash)
(use-package s)
(use-package elmacro)
(elmacro-mode)

;;; allow right ALT when focused.

(add-hook'focus-out-hook (lambda () (call-process "setxkbmap" nil "*trash*" nil "-option")))
(add-hook 'kill-emacs-hook (lambda () (call-process "setxkbmap" nil "*trash*" nil "-option")))
(add-hook 'focus-in-hook (lambda () (call-process "setxkbmap" nil "*trash*" nil "-option" "lv3:ralt_alt,ctrl:nocaps")))

;;; Keep help window selected

(setq help-window-select t)
(setq help-window-keep-selected t)
(keymap-global-set "C-h f" #'helpful-callable)
(keymap-global-set "C-h v" #'helpful-variable)
(keymap-global-set "C-h k" #'helpful-key)
(keymap-global-set "C-h x" #'helpful-command)

(keymap-global-set "C-c C-d" #'helpful-at-point)
(keymap-global-set "C-h F" #'helpful-function) ; replace Info-goto-emacs-command-node

;;; Text expansion

;; Expands words, filenames

(keymap-global-set "<remap> <dabbrev-expand>" 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill ;; Not word.
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs ;; Not word.
        try-expand-list	       ;; Not word.
        try-expand-line	       ;; Not word.
        ))

;; Abbrev mode is used for automatic word replacement (like resume -> résumé).

;; Template system
;; ah TAB expands to (add-hook 'name-hook 'function)
;; M-x yas-describe-tables shows all snippets available in the current mode.

(use-package yasnippet
  :hook ((text-mode
	  prog-mode
	  conf-mode
	  snippet-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dir (locate-user-emacs-file "snippets")))
(use-package yasnippet-snippets)

(keymap-global-set "M-Z" 'zap-up-to-char)

;;; Don't display some buffers in window.

(add-to-list 'display-buffer-alist
	     '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;; Basics

(windmove-default-keybindings) ; SHIFT+arrows

;; FIDO + orderless
(use-package orderless
  :ensure t
  :config
  (fido-vertical-mode)
  :custom
  (completion-styles '(orderless)))

(defun my-icomplete-styles ()
  (setq-local completion-styles '(orderless)))
(add-hook 'icomplete-minibuffer-setup-hook 'my-icomplete-styles)



(global-completion-preview-mode 1)
(setq isearch-allow-motion t) ; Allow C-v, M-v

                                        ;(which-key-mode 1)
                                        ;(which-key-setup-side-window-right-bottom)

(tab-bar-mode 1) ; Nice for different window configurations.
(tab-bar-history-mode 1)
(keymap-global-set "M-[" 'tab-bar-history-back)
(keymap-global-set "M-]" 'tab-bar-history-forward)


;;; global-set-key
(global-set-key [f8] 'view-mode) ; SPACE, DEL movement
(keymap-global-set "C-x j" 'bury-buffer) ; move buffer to the end of buffer list making it unlikely to be selected
(global-set-key [remap list-buffers] 'ibuffer)
(keymap-global-set "M-o" 'other-window) ; Faster than C-x o
(keymap-global-set "M-i" 'imenu)

;; Trick: binding new key:
;; M-x global-set-key RET key cmd RET
;; C-x ESC ESC C-a C-k C-g

(defun my/keyboard-quit ()
  "Smarter keyboard-quit, closes minibufer even when it's unfocused"
  (interactive)
  (if (> (minibuffer-depth) 0)
      (abort-recursive-edit)
    (keyboard-quit)))
(global-set-key "" 'my/keyboard-quit)

(defun my/insert-line-before (times)
  "Insert newline above line containing the cursor."
  (interactive "p")
  (move-beginning-of-line 1)
  (newline-and-indent times))
(keymap-global-set "C-S-o" 'my/insert-line-before)

(defun my/insert-line-after (times)
  "Insert newline below line containing the cursor."
  (interactive "p")  
  (end-of-line)
  (newline-and-indent times))
(global-set-key [C-return] 'my/insert-line-after)

(defun my/print-command ()
  "Print function name of pressed key"
  (interactive)
  (let* ((key (read-key-sequence "Key: "))
       	 (str (key-description key))
	 (desc (key-binding key)))
    (princ (format "%s\t\t\t(%s)\n" str desc) (current-buffer))))

(defun my/show-kill-ring ()
  "Show kill ring contents in help buffer"
  (interactive)
  (save-excursion
    (with-help-window "*kill ring*"
      (dolist (v kill-ring)
	(insert v)
	(newline)))))

(put 'erase-buffer 'disabled nil)

;;; Command stats

(setq command-stats-file (locate-user-emacs-file "command-stats.el"))
(load command-stats-file :no-error)
(unless (boundp 'command-stats)
  (setq command-stats (ht)))

(defun my/command-stats-setup ()
  (with-current-buffer (find-file-noselect command-stats-file)
    (setq-local buffer-save-without-query t)
    (electric-indent-local-mode -1)))

(add-hook 'after-init-hook 'my/command-stats-setup)

(defun my/command-stats-pre-command-hook ()
  "Record command execution."
  (let ((command real-last-command))
    (if (symbolp command) (ht-update-with! command-stats command (lambda (x) (+ x 1)) 1))))

(add-hook 'pre-command-hook 'my/command-stats-pre-command-hook)

(defun my/command-stats-kill-emacs-hook ()
  "Dump command-stats to file."
  (with-current-buffer (find-file-noselect command-stats-file)
    (erase-buffer)
    (insert "(setq command-stats\n")
    (prin1 command-stats (current-buffer))
    (insert ")")
    
    (beginning-of-buffer)
    (search-forward "data (")
    (newline 2)

    (ignore-error scan-error
        (while t
          (forward-sexp 2)
          (delete-char 1) (newline)))
    (insert ")")
    
    (save-buffer)))

(defadvice save-buffers-kill-emacs (before update-mod-flag activate)
  (my/command-stats-kill-emacs-hook))


;;; Macros
;; Macros for frequent text editing operations.
;; Defer complex decision to a human with C-x Ck-k q and enter recursive editing with C-r

(defun my/prepare-windows ()
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (find-file "~/.emacs.d/init.el" t)
  (other-window 1)
  (find-file "~/Repos/emacs/emacs-hints.txt" t))
(keymap-global-set "C-c p" 'my/prepare-windows)
