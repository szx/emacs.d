;;; early-init.el -*- no-byte-compile: t; lexical-binding: t; -*-

(setq package-enable-at-startup nil)

(setq load-prefer-newer t)

(setq native-comp-jit-compilation t)
(setq native-comp-deferred-compilation native-comp-jit-compilation)  ; Deprecated


(when (string-equal system-type "android")
  ;; Add Termux binaries to PATH environment
  (let ((termuxpath "/data/data/com.termux/files/usr/bin"))
    (setenv "PATH" (concat (getenv "PATH") ":" termuxpath))
    (setq exec-path (append exec-path (list termuxpath)))))
