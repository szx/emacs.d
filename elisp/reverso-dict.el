;;; -*- lexical-binding: t -*-

;; https://github.com/skeeto/emacs-web-server/blob/master/simple-httpd.el#L90
;; https://stackoverflow.com/questions/78658384/elisp-process-send-string-giving-unexpected-error-process-not-running-listen
;; https://dict.org/rfc2229.txt

(setq reverso-dict--delimiter "\15\n")

(defun reverso-dict--msg (msg &rest args)
  (concat (apply #'format (cons msg args)) reverso-dict--delimiter))

(defun reverso-dict--buffer ()
  (get-buffer-create "*reverso-dict*"))

(setq reverso-dict--mutex (make-mutex))
(setq reverso-dict--cond-var (make-condition-variable reverso-dict--mutex))
(defun reverso-dict--buffer-temp ()
  (unless (buffer-live-p "*reverso-dict-temp*")
    (with-current-buffer (get-buffer-create "*reverso-dict-temp*")
      (eww-mode)
      (current-buffer))))

(setq reverso-dict--web-done nil)
(defun reverso-dict--hook ()
  (setq reverso-dict--web-done t))

(defun reverso-dict--t1 () 
  (with-current-buffer (reverso-dict--buffer-temp)
    (setq reverso-dict--web-done nil)
    (add-hook 'eww-after-render-hook #'reverso-dict--hook)
    
    (eww (format "context.reverso.net/translation/english-polish/%s" (s-replace " " "+" reverso-dict--word)))
    (let ((old-data eww-data)) ; For some reason eww-data gets overwritten.
      (while (not reverso-dict--web-done) (sit-for 0.01))
      ;; TODO: Is there a better way than spinlock?
      (remove-hook 'eww-after-render-hook #'reverso-dict--hook)
      (setq reverso-dict--web-done nil)
      (setq eww-data (copy-sequence old-data))
      ;;(eww-readable) ;; TODO: Better readable from firefox?
      (let (beg end str (inhibit-read-only t))
        (while (re-search-forward (rx bol (1+ "Submit" (0+ blank))) nil t)
	  (replace-match "\n\n"))
        (goto-char (point-min))
        (while (search-forward reverso-dict--word nil t)
          (replace-match (concat "{+" reverso-dict--word "+}")))
        (goto-char (point-min))
        (while (re-search-forward (rx bol (or "Potentially sensitive or inappropriate" "Definition NEW" "More expressions") (+? anychar) (or "words based on your search" "Display more examples" "Search in Web" (and bol "games" eol))) nil t)
	  (replace-match "\n\n"))
        ;;(delete-duplicate-lines (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward (rx (>= 3 "\n")) nil t)
	  (replace-match "\n\n"))
        (goto-char (point-min))
        (search-forward "Add to list")
        (forward-line)
        (setq beg (point))
        (if (not (re-search-forward (rx "Results") nil t))
            (goto-char (point-max))
          (beginning-of-line) (forward-line -1))
        (setq end (point))
        (setq str (buffer-substring beg end))
        (kill-buffer)
        str))))


(defun reverso-dict--desc (word)
  (setq reverso-dict--word word)
  (reverso-dict--t1))

(defun reverso-dict--filter (proc msg)
  "Runs each time client makes a request."
  (with-current-buffer "*reverso-dict*"
    (message "[reverso-dict--filter] %S received %S" proc msg)
    ;; For logging purposes
    (goto-char (point-max))
    (insert msg)
    (cond ((string-match (rx "define" blank "\*" blank  (syntax string-quote) (group (* ascii)) (syntax string-quote) "\n") msg)
           (let* ((word (match-string 1 msg))
                  (desc (reverso-dict--desc word)))
             (message "[reverso-dict--filter] describe %S" word)
             (process-send-string proc (reverso-dict--msg "150 1 definitions found: list follows"))
             (process-send-string proc (reverso-dict--msg "151 \"%s\" db_name \"db_desc\" - text follows\n%s\n%s" word word desc))
             (process-send-string proc (reverso-dict--msg "\n."))
             (process-send-string proc (reverso-dict--msg "250 Command complete"))))
          ((string-match-p "^client .*\n$" msg)
           (process-send-string proc (reverso-dict--msg "250 ok")))
          (t (process-send-string proc "502 Command not implemented")))))

(defun reverso-dict--sentinel (proc msg)
  "Runs each time process state changes."
  (message "[reverso-dict--sentinel] %S received %S" proc msg)
  (cond ((string-prefix-p "open" msg)
         (message "%S connection" proc)
         (process-send-string proc (reverso-dict--msg "220 reverso-dict-server")))
        ((string-prefix-p "deleted" msg)
         (message "%S connection deleted proc" proc)
         (let ((buffer (reverso-dict--buffer)))
           (when buffer
             (kill-buffer buffer))))
        (t (message "something went wrong"))))

                                        ;(fmakunbound 'reverso-dict--msg)

(defun reverso-dict-start ()
  "Start DICT server for Reverso Context."
  (interactive)
  (reverso-dict-stop)
  (make-network-process
   :name "reverso-dict"
   :buffer (buffer-name (reverso-dict--buffer))
   :service 2628
   :host 'local
   ;;:family 'ipv4
   ;;:coding 'binary
   :server t
   ;;:log 'reverso-dict--log
   :filter 'reverso-dict--filter
   :sentinel 'reverso-dict--sentinel
   ))

(defun reverso-dict-stop ()
  "Stop DICT server."
  (interactive)
  (while (process-status "reverso-dict")
    (delete-process "reverso-dict")
    (ignore-errors (kill-buffer "*reverso-dict*"))))

(defun reverso-dict-restart ()
  "Restart DICT server."
  (interactive)
  (reverso-dict-stop)
  (reverso-dict-start))

(provide 'reverso-dict)
