;;; -*- lexical-binding: t -*-

;; Basic Emacs configuration

(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)

(setq source-directory (locate-user-emacs-file "source"))

(setq warning-minimum-level :error)

(setq
 scroll-margin 15
 scroll-conservatively 101
 scroll-preserve-screen-position 1)
(pixel-scroll-precision-mode 1)

(setq
 enable-recursive-minibuffers t ; Allows Cx C-f M-: (insert user-init-file)
 history-delete-duplicates t
 use-short-answers t
 confirm-nonexistent-file-or-buffer nil
 kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                   kill-buffer-query-functions)
 )

(tooltip-mode 1)

(savehist-mode 1) ; Remember command history.
(save-place-mode 1) ; Remeber cursor position.

;; Remember opened files.
(desktop-save-mode 1)
(setq
 desktop-save t
 desktop-load-locked-desktop t
 desktop-restore-frames t
 desktop-auto-save-timeout 300
 desktop-globals-to-save nil)

;;; Appearance
; (enable-theme 'modus-vivendi-deuteranopia)

;;; Backups
(setq
 backup-directory-alist `(("." . ,(locate-user-emacs-file "saves")))
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 create-lockfiles nil
 version-control t)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error)


;;; Package manager

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default 1)

(setq straight-cache-autoloads nil) ; Prevent Magit problem

;; Compile-angel
(use-package compile-angel
  :ensure t
  :demand t
  :config
  ;; Set `compile-angel-verbose' to nil to disable compile-angel messages.
  ;; (When set to nil, compile-angel won't show which file is being compiled.)
  (setq compile-angel-verbose t)

  ;; Uncomment the line below to compile automatically when an Elisp file is saved
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-files'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; the `use-package' macro, you'll need to explicitly add:
  ;; (eval-when-compile (require 'use-package))
  ;; at the top of your init file.
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)

  ;; A global mode that compiles .el files before they are loaded
  ;; using `load' or `require'.
  (compile-angel-on-load-mode 1))

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
(use-package helpful)
(keymap-global-set "C-h f" #'helpful-callable)
(keymap-global-set "C-h v" #'helpful-variable)
(keymap-global-set "C-h k" #'helpful-key)
(keymap-global-set "C-h x" #'helpful-command)

(keymap-global-set "C-c C-d" #'helpful-at-point)
(keymap-global-set "C-h F" #'helpful-function) ; replace Info-goto-emacs-command-node

;; Xref using dumb-jump

(use-package dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)


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
;; M+/


;; iedit replaces replace and ocurr
;; https://www.masteringemacs.org/article/iedit-interactive-multi-occurrence-editing-in-your-buffer
;; C-; C+' works like occur
(use-package iedit)

(defun iedit-dwim (arg)
  "Starts iedit but uses \\[narrow-to-defun]."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other
          ;; functions.
          (narrow-to-defun)
          (iedit-start (current-word) (point-min) (point-max)))))))
(keymap-global-set "C-;" 'iedit-dwim)

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
;; TODO: Check why segfaults.
;; (use-package yasnippet-snippets)

(keymap-global-set "M-Z" 'zap-up-to-char)

;;; Don't display some buffers in window.

(add-to-list 'display-buffer-alist
	     '("\\`\\*\\(Warnings\\|Compile-Log\\|Org-Babel Error Output\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

;;; Basics

(defun my/computer ()
      (let ((name (system-name)))
        (cond
         ((s-contains? "sszczyrb-B" name) 'desktop)
         (t 'unknown))))


;; Fonts

(setq my/fixed-font
      (cond
       ((member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono")
       ((member "Inconsolata" (font-family-list)) "Inconsolata")
       ;;((member "Unifont" (font-family-list)) "Unifont")
       (t nil)))
(setq my/variable-font
      (cond
       ((member "Noto Serif" (font-family-list)) "Noto Serif")
       ((member "DejaVu Serif" (font-family-list)) "DejaVu Serif")
       (t nil)))

(set-frame-font my/fixed-font t t)
(set-fontset-font t nil my/variable-font nil 'append)
(set-face-attribute 'default nil :height 150)

; Thanks Ryan from StackOverflow.
(defun my/buffer-face-mode-variable ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face `(:family ,my/variable-font :height 180 :width semi-condensed))
  (buffer-face-mode))
(add-hook 'Info-mode-hook 'my/buffer-face-mode-variable)

(defun my/buffer-face-mode-fixed ()
  "Set font to a fixed width (monospace) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face `(:family ,my/fixed-font :height 150 :width semi-condensed))
  (buffer-face-mode))

(defun my/toggle-buffer-face-mode ()
  "Toggle between fixed and variable buffer face mode"
  (interactive)
  (-when-let ((&plist :family family) buffer-face-mode-face)
    (if (s-contains? "Serif" family)
        (my/buffer-face-mode-fixed)
      (my/buffer-face-mode-variable))))


;; TODO: Golden ratio for windows

;; Avy
(use-package avy)
(keymap-global-set "C-z" 'avy-goto-word-or-subword-1)

;; Flycheck
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; FIDO + orderless
(use-package orderless
  :ensure t
  :config
  (fido-vertical-mode)
  :custom
  (completion-styles '(substring orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(defun my-icomplete-styles ()
  (setq-local completion-styles '(substring orderless basic)))
(add-hook 'icomplete-minibuffer-setup-hook 'my-icomplete-styles)



(global-completion-preview-mode 1)
(setq isearch-allow-motion t) ; Allow C-v, M-v

                                        ;(which-key-mode 1)
                                        ;(which-key-setup-side-window-right-bottom)

;; LSP

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))


(use-package rust-mode)
(use-package geiser-guile)

(use-package lsp-mode
  :commands lsp
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((rust-mode . lsp))
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package dap-mode)
;; TODO: (use-package dap-rust)

(defun org-babel-edit-prep:rust (babel-info)
  (setq-local buffer-file-name (->> babel-info caddr (alist-get :tangle)))
  (lsp))

;; Marking

(setq set-mark-command-repeat-pop t)

(defun my/Info-push-history (&optional fork)
  (and Info-current-file
       (push (list Info-current-file Info-current-node (point))
             Info-history)))
(advice-add 'Info-follow-nearest-node :before #'my/Info-push-history)
;;(advice-remove 'Info-follow-nearest-node #'my/Info-push-history)


;; Tabs

(tab-bar-mode 1) ; Nice for different window configurations.
(tab-bar-history-mode 1)
(keymap-global-set "M-[" 'tab-bar-history-back)
(keymap-global-set "M-]" 'tab-bar-history-forward)


;;; global-set-key
(keymap-global-set "<f8>" 'view-mode) ; SPACE, DEL movement
(keymap-global-set "C-x j" 'bury-buffer) ; move buffer to the end of buffer list making it unlikely to be selected
(global-set-key [remap list-buffers] 'ibuffer)
(keymap-global-set "M-i" 'imenu)

(defun switch-to-buffer-quick ()
  "Display other-buffer in the selected  window"
  (interactive)
  (switch-to-buffer nil nil 'force-same-window))
(keymap-global-set "s-b" 'switch-to-buffer-quick)

(use-package ace-window)
(keymap-global-set "M-o" 'ace-window) ; Faster than C-x o

;; TODO (insert (file-newest-backup (buffer-file-name current-buffer)))

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


(defun my/kill-thing-at-point (thing)
  "Kill the `thing-at-point' for the specified kind of THING."
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds))
      (error "No %s at point" thing))))

;; HIRO structural editing

(defun my/backward-kill-word ()
  "Kill word backward until newline."
  (interactive)
  (cond 
   ((looking-back (rx (char word)) 1)
    (backward-kill-word 1))
   ((looking-back (rx (char blank)) 1)
    (delete-horizontal-space t))
   (t
    (backward-delete-char 1))))
(keymap-global-set "C-<backspace>" 'my/backward-kill-word)


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
    (insert "(setq command-stats (ht<-alist '\n")
    (prin1 (ht->alist command-stats) (current-buffer))
    (insert "))")
    
    (beginning-of-buffer)
    ;(down-list)
    (search-forward "((")
    (backward-char)
    ;(newline)

    (ignore-error scan-error
        (while t
          (forward-sexp)
          (newline)))
    
    (save-buffer)))

(defadvice save-buffers-kill-emacs (before update-mod-flag activate)
  (my/command-stats-kill-emacs-hook))


;; Magit
(use-package magit)


;; Org-mode
(use-package org)
(use-package org-web-tools)
(customize-set-value 'org-src-tab-acts-natively t)
(customize-set-value 'org-edit-src-content-indentation 0)

(defun org-web-tools-insert-link-for-clipboard-url ()
  "Extend =org-web-tools-insert-link-for-url= to take URL from clipboard or 'kill-ring'."
  (interactive)
  (org-web-tools--org-link-for-url (org-web-tools--get-first-url)))

(setq org-capture-templates
      '(("t" "Task" entry (file+headline "~/org/journal.org" "Tasks")
         "* TODO %?\n"
         :empty-lines 1
         :before-finalize (lambda () (my/org-capture-prompt-date "SCHEDULED")))
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %U %^{Title}\n%?")
        ("b" "Bookmark (Clipboard)" entry (file+headline "~/org/journal.org" "Bookmarks")
         "** %(org-web-tools-insert-link-for-clipboard-url)\n%?" :empty-lines 1 :prepend t)))


(use-package org-contrib)
(require 'org-expiry)
(customize-set-value 'org-expiry-inactive-timestamps t)
(org-expiry-insinuate)
(setq org-expiry-created-property-name "CREATED")
(defun my/org-capture-insert-created ()
  (save-excursion
    (org-back-to-heading)
    (org-expiry-insert-created)))
(add-hook 'org-capture-prepare-finalize-hook #'my/org-capture-insert-created)

(defun my/org-capture-prompt-date (prop)
    (let ((ts (org-read-date (current-time))))
      (org-entry-put nil prop ts)))

(keymap-global-set "C-c c" 'org-capture)
(keymap-global-set "C-c l" #'org-store-link)
(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "<f12>" #'org-agenda)

(setq calendar-week-start-day 1)
(setq org-log-into-drawer "LOGBOOK")
(setq org-todo-keywords
      '((sequence "TODO(t!)" "NEXT(n!)" "WAIT(w@/@)" "|" "DONE(d!)" "CNCL(c!)")))
(setq org-log-reschedule 'time)
(setq org-log-redeadline 'time)

(add-to-list 'org-modules 'org-habit t)

;;; Automate CUSTOM_ID
;;; https://writequit.org/articles/emacs-org-mode-generate-ids.html

(require 'org-id)
(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(defun org-id-new (&optional prefix)
  "Create a new globally unique ID.

An ID consists of two parts separated by a colon:
- a prefix
- a unique part that will be created according to `org-id-method'.

PREFIX can specify the prefix, the default is given by the variable
`org-id-prefix'.  However, if PREFIX is the symbol `none', don't use any
prefix even if `org-id-prefix' specifies one.

So a typical ID could look like \"Org:4nd91V40HI\"."
  (let* ((prefix (if (eq prefix 'none)
		     ""
		   (concat (or prefix org-id-prefix) "-")))
	 unique)
    (if (equal prefix "-") (setq prefix ""))
    (cond
     ((memq org-id-method '(uuidgen uuid))
      (setq unique (org-trim (shell-command-to-string org-id-uuid-program)))
      (unless (org-uuidgen-p unique)
	(setq unique (org-id-uuid))))
     ((eq org-id-method 'org)
      (let* ((etime (org-reverse-string (org-id-time-to-b36)))
	     (postfix (when org-id-include-domain
			(require 'message)
			(concat "@" (message-make-fqdn)))))
	(setq unique (concat etime postfix))))
     ((eq org-id-method 'ts)
      (let ((ts (format-time-string org-id-ts-format))
	    (postfix (when org-id-include-domain
		       (require 'message)
		       (concat "@" (message-make-fqdn)))))
	(setq unique (concat ts postfix))))
     (t (error "Invalid `org-id-method'")))
    (concat prefix unique)))

(defun my/org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
If POM is nil, refer to the entry at point.  If the entry does
not have an CUSTOM_ID, the function returns nil.  However, when
CREATE is non nil, create a CUSTOM_ID if none is present
already.  PREFIX will be passed through to `org-id-new'.  In any
case, the CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new (concat prefix "h")))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

(defun my/org-add-ids-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the current
   file which do not already have one. Only adds ids if the
   `auto-id' option is set to `t' in the file somewhere. ie,
   #+OPTIONS: auto-id:t"
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t" (point-max) t)
      (org-map-entries (lambda () (my/org-custom-id-get (point) 'create))))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (and (eq major-mode 'org-mode)
                                   (eq buffer-read-only nil))
                          (my/org-add-ids-to-headlines-in-file))))))


(defun org-babel-detangle-no-buffer-pop-up (orig-fun &rest args)
  (save-excursion
    (let ((display-buffer-alist
           '((".*" (display-buffer-no-window) (allow-no-window . t)))))
      (apply orig-fun args))))

(advice-add 'org-babel-detangle :around #'org-babel-detangle-no-buffer-pop-up)


;;; Reverso Context dictionary
(load (locate-user-emacs-file "reverso-dict.el"))
(reverso-dict-restart)
;; TODO: defadvice for killing *Dictionary* before dictionary-search
;; TODO: Kill processes on exit.

(defadvice save-buffers-kill-emacs (before update-mod-flag activate)
  (reverso-dict-stop))


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
  (find-file "~/.emacs.d/reverso-dict.el" t))
(keymap-global-set "C-c p" 'my/prepare-windows)


(defun sudo ()
  "Use TRAMP to 'sudo' the current buffer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:" buffer-file-name))))
