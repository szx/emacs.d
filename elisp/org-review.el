;;; org-review.el --- Supermemo-like priority queue with outstanding items. -*- lexical-binding: t -*-

;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'org)
(require 'org-ql-view)
(require 'fsrs)
(require 'thunk)
(require 'eieio)
(require 'eieio-base)

;; Open default agenda

(defun org-review-agenda ()
  "Display agenda."
  (interactive)
  ;; TODO: Use priority queue.
  ;; TODO: Don't show only today.
  (ignore-errors
    (kill-buffer "*Priority Queue*"))
  (ignore-errors  (org-ql-search org-agenda-files
                    '(and
                      (tags "item")
                      (or
                       (ts-a :to today)
                       (not (scheduled))
                       ))
                    ;; TODO: Sort
                    :buffer "*Priority Queue*"
                    ))
  (pop-to-buffer "*Priority Queue*"))


(defun org-review-goto (id)
  "Go to element with ID.
Respect indirect buffers."
  (interactive "sID: ")
  (let ((m (org-id-find id 'marker)))
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))
    (pop-to-buffer-same-window (marker-buffer m))
    (when (buffer-narrowed-p) (widen))
    (goto-char m)
    ;;(move-marker m nil)
    ))

(defmacro with-indirect-buffer (&rest body)
  "Create temporary indirect buffer, and execute BODY."
  `(let ((buf (clone-indirect-buffer nil t)))
     (with-current-buffer buf
       (progn ,@body))
     (kill-buffer buf)))

(defmacro with-org-review-element (id &rest body)
  "Go to element with ID, narrow Org subtree, and execute BODY."
  (declare (indent defun))
  `(with-scratch-tab
     (let* ((fun (lambda ()
                   (org-review-goto ,id)
                   (with-indirect-buffer
                    (org-narrow-to-subtree)
                    (org-back-to-heading)
                    ,@body)))
            (m (org-id-find id 'marker))
            (buf (marker-buffer m)))
       (pop-to-buffer-same-window buf)
       (save-excursion
         (if (buffer-narrowed-p)
             (save-restriction
               (widen)
               (funcall fun))
           (funcall fun))
         ))))

(defvar with-scratch-tab 1)
(defmacro with-scratch-tab (&rest body)
  "Open new scratch tab, run BODY, close scratch tab."
  (declare (indent defun))
  `(progn
     (let ((scratch-tab-name (format "scratch-%s" with-scratch-tab))
           (ret-val nil))
       (tab-bar-new-tab-to -1)
       (tab-bar-rename-tab scratch-tab-name)
       (setq ret-val
             (progn ,@body))
       (while (tab-bar--tab-index-by-name scratch-tab-name)
         (tab-bar-close-tab-by-name scratch-tab-name))
       ret-val)
     ))

(defun org-review-re-drawer (name)
  "Regexp for custom Org drawer NAME."
  (rx line-start (0+ (any ?\s ?\t))
      ":" (literal name) ":"
      (0+ (any ?\s ?\t)) line-end))

(defun org-review-narrow-to-drawer (drawer)
  "Narrow to DRAWER of note.
Return nil if drawer didn't exist."
  (interactive
   (list (org-element-property ':drawer-name (org-element-context))))
  (when (not drawer) (setq drawer "MY_ITEM"))
  (save-excursion
    (org-back-to-heading)
    (org-narrow-to-subtree)
    (let ((drawer-pos (re-search-forward (org-review-re-drawer drawer) nil t)))
      (if drawer-pos
          (let* ((element (org-element-context))
                 (beg (org-element-property :begin element))
                 (end (org-element-property :end element)))
            (goto-char beg)
            (forward-line)
            (setq beg (point))
            (goto-char end)
            (re-search-backward (org-review-re-drawer "END") nil t)
            (backward-char)
            (setq end (point))
            (narrow-to-region beg end)
            t)
        (org-review--add-drawer drawer)
        (org-review-narrow-to-drawer drawer)
        nil))))

(defun org-review--item-narrow-to-drawer ()
  "Narrow to item's drawer."
  (org-toggle-tag "item" 'on)
  (org-review-narrow-to-drawer "MY_ITEM"))

(defun org-review--topic-narrow-to-drawer ()
  "Narrow to topics' drawer."
  (org-toggle-tag "topic" 'on)
  (org-review-narrow-to-drawer "MY_TOPIC"))

(defun org-review--add-drawer (drawer)
  "Add DRAWER and properties."
  (interactive)
  (save-excursion
    (save-restriction
      (message "Adding %s drawer" drawer)
      (org-end-of-meta-data)
      (org-insert-drawer nil drawer)
      (org-review-narrow-to-drawer drawer)
      (insert "nil"))))

(defun org-review--read-from-drawer (&rest default)
  "Read Lisp object from narrowed drawer or return DEFAULT."
  (save-excursion
    (goto-char (point-min))
    (or (read (current-buffer)) default)))

(defun org-review--write-to-drawer (obj)
  "Write OBJ as Lisp object to narrowed drawer." 
  (save-excursion
    (delete-region (point-min) (point-max))
    (pp obj (current-buffer))))

(defun org-review-create-item ()
  "Add item's drawer and properties."
  (interactive)
  (save-excursion
    (save-restriction
      (when (not (org-review--item-narrow-to-drawer))
        (message "Creating new item")
        (let ((card (fsrs-make-card)))
          (org-review--write-fsrs-card card)
          (org-review--schedule-fsrs-card card)
          )))))

(defun org-review-create-topic ()
  "Add topics' drawer and properties."
  (interactive)
  (save-excursion
    (save-restriction
      (when (not (org-review--topic-narrow-to-drawer))
        (message "Creating new topic")
        (org-review--write-topic-data (org-review-topic-default))
        (org-review--schedule (current-time))))))


(cl-defgeneric object-read (string)
  "Return object from STRING."
  (cl-check-type string string)
  (let ((ret (read string)))
    (eieio-persistent-make-instance (car ret) (cdr ret))))

;; HIRO element with 
;; HIRO element priority based on retrievability (calculated from FSRS state) or days overdue (calculated from FRSR's scheduled date)

`( ;; HIRO E2E test
  (ert-deftest entangle-test-end-to-end ()
  "Full round-trip: scan -> encode -> write produces valid org."
  (let ((tmp (make-temp-file "entangle-e2e-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "main.py" tmp)
            (insert "print('hello')\n"))
          (with-temp-file (expand-file-name "config.yaml" tmp)
            (insert "key: value\n"))
          (let* ((entries (org-babel-entangle--scan tmp))
                 (encoded-entries nil)
                 (all-fixups nil))
            (dolist (entry entries)
              (let ((result (org-babel-entangle--encode entry)))
                (push (cons entry (car result)) encoded-entries)
                (setq all-fixups (append (cdr result) all-fixups))))
            (setq encoded-entries (nreverse encoded-entries))
            (let ((org-content (org-babel-entangle--write "test-project" encoded-entries all-fixups)))
              ;; Should contain expected structure
              (should (string-match-p "#\\+TITLE: test-project" org-content))
              (should (string-match-p ":PROPERTIES:" org-content))
              (should (string-match-p ":header-args:python:" org-content))
              (should (string-match-p ":comments link" org-content))
              (should (string-match-p "begin_src python" org-content))
              (should (string-match-p "begin_src yaml" org-content))
              (should (string-match-p ":tangle main\\.py" org-content))
              (should (string-match-p ":tangle config\\.yaml" org-content)))))
      (delete-directory tmp t))))
  )

(defclass org-review-topic ()
  ((interval :initarg :interval :type integer)
   (a-factor :initarg :a-factor :type float)
   (last-review-time :initarg :last-review-time :type cons)))

(defun org-review-topic-default ()
  "Return default topic."
  (org-review-topic :interval 1 :a-factor 2.0 :last-review-time (current-time)))

(cl-defmethod org-review-topic-next-interval ((topic org-review-topic))
  "Return TOPIC's next interval."
  (with-slots (interval a-factor) topic
    (round (* interval a-factor))))

(cl-defmethod org-review-topic-next-review-time ((topic org-review-topic))
  "Return TOPIC's next review."
  (with-slots (interval last-review-time) topic
    (time-add last-review-time (days-to-time interval))))

(cl-defmethod org-review-topic-reschedule ((topic org-review-topic) current-time)
  "Set new interval and last review time for TOPIC reviewed at CURRENT-TIME."
  (with-slots (interval last-review-time) topic
    (setf last-review-time current-time)
    ;; TODO: max interval
    ;; TODO: Outstanding queue of elements awaiting optimal repetition (determined by FSRS / A-factor) - sort PQ by next-repetition time and randomize
    ;; TODO: Interval dispersion for topics.
    ;; Adjust A-Factor:
    ;;   long text receive 1.1 - 1.3
    ;;   short text receive 1.8 - 6.9
    ;;   manually typed has lower A-factor and higher priority
    ;; but really increasing/diluting priority should be preferred
    ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Database.html
    (setf interval (org-review-topic-next-interval topic))))


(ert-deftest org-review-topic-serialization ()
  (let (foo foo2 current-time)
    (setq foo (org-review-topic-default))
    (setq foo2 (object-read (with-output-to-string (object-write foo))))
    (should (equal foo foo2))
    (setq current-time (current-time))
    (org-review-topic-reschedule foo current-time)
    (should (equal (slot-value foo 'a-factor) 2.0))
    (should (equal (slot-value foo 'last-review-time) current-time))
    (should (equal (org-review-topic-next-review-time foo) (time-add current-time (days-to-time 2))))))


(defun org-review--read-topic-data ()
  "Read Lisp object from narrowed drawer."
  (object-read (org-review--read-from-drawer
                (with-output-to-string (object-write (org-review-topic-default))))))

(defun org-review--write-topic-data (topic)
  "Write TOPIC as Lisp object to narrowed drawer."
  (org-review--write-to-drawer (with-output-to-string (object-write topic))))


(defun org-review-topic-rate (id)
  "Go to ID, reschedule element."
  (interactive (list (org-id-get-create)))
  (with-org-review-element id
    (org-review--topic-narrow-to-drawer)
    (let ((topic (org-review--read-topic-data))
          (time (current-time)))
      (org-review-topic-reschedule topic time)
      (org-review--write-topic-data topic)
      (org-review--schedule (org-review-topic-next-review-time topic)))))


(setq org-review-fsrs-parameters [0.2172 1.1771 3.2602 16.1507 7.0114 0.57 2.0966 0.0069 1.5261 0.112 1.0178 1.849 0.1133 0.3127 2.2934 0.2191 3.0004 0.7536 0.3332 0.1437 0.2])

(setq org-review-fsrs-scheduler (fsrs-make-scheduler))

;; TODO: FSRS optimize reviewed cards once a months.
;; TODO: Separate parameters for different decks?
;; TODO: New capture types: https://expertium.github.io/Avoid_Pattern_Matching.html

(defun org-review-read-rating ()
  (read (completing-read "Rating: " '(:good :again) nil t nil nil ":good")))

(defun org-review-item-rate (rating)
  "Rate note with RATING."
  (interactive
   (list (org-review-read-rating)))
  (save-excursion
    (save-restriction
      (org-review-create-item)
      (org-review--item-narrow-to-drawer)
      (let* ((old-card (car (org-review--read-fsrs-card-history)))
             (new-card (cl-nth-value 0 (fsrs-scheduler-review-card org-review-fsrs-scheduler old-card rating)))
             (due (date-to-time (fsrs-card-due new-card))))
        (org-review--write-fsrs-card new-card)
        (org-review--schedule-fsrs-card new-card))
      )))

(defun org-review--read-fsrs-card-history ()
  "Read Lisp object from narrowed drawer."
  (org-review--read-from-drawer (list (fsrs-make-card))))

(defun org-review--write-fsrs-card (card)
  "Write CARD as Lisp object to narrowed drawer."
  (let* ((old (org-review--read-fsrs-card-history))
         (new (cons card old)))
    (org-review--write-to-drawer new)))

(defun org-review--precise-timestamp-advice (orig-fun time &optional with-hm inactive pre post extra)
  "Advice for ORIG-FUN (org-insert-timestamp) for inserting TIME with HH:MM.
Same optional arguments - WITH-HM, INACTIVE, PRE, POST, EXTRA."
  (apply orig-fun time t inactive pre post extra))

(defun org-review--schedule (time)
  "Add SCHEDULED: with TIME."
  (advice-add 'org-insert-timestamp :around #'org-review--precise-timestamp-advice)
  (org-schedule nil time)
  (advice-remove 'org-insert-timestamp #'org-review--precise-timestamp-advice))

(defun org-review--schedule-fsrs-card (card)
  "Add SCHEDULED: with CARD due."
  (let ((due (date-to-time (fsrs-card-due card))))
    (org-review--schedule due)))

(defun org-review--priority-queue-ids ()
  "Return the priority queue - a list of IDs."
  (org-ql-select org-agenda-files
    '(and
      (tags "item")
      (or
       (ts-a :to today)
       (not (scheduled))
       ))
    :action (lambda () (org-id-get))
    :sort 'priority))

(defun org-review-outstanding-id ()
  "Return top element ID of priority queue."
  (car (org-review--priority-queue-ids))) 

(cl-defun org-review--element-type (&optional (id (org-id-get-create)))
  "Go to ID, check if any clozes exist."
  (with-org-review-element id
    (goto-char (point-max))
    (if (org-review-bounds-of-cloze-at-point)
        :item
      :topic)))

(cl-defun org-review--element-review-stage (&optional (id (org-id-get-create)))
  "Return element ID review stage."
  (with-org-review-element id
    (goto-char (point-max))
    ;; HIRO check
    (if (org-review-bounds-of-cloze-at-point)
        :item
      :topic)))

(defun org-review--current-review-buffer ()
  "Return current review buffer or nil."
  (get-buffer "*Current Review*"))

(cl-defun org-review--item-review-stage (&optional (id (org-id-get-create)))
  "Return item ID review stage (:unanswered, :answered, :done)."
  (with-org-review-element id
    (thunk-let ((due (org-review--element-due?))
                (clozes-hidden
                 (and (org-review--current-review-buffer)
                      (with-current-buffer (org-review--current-review-buffer)
                        (and (s-equals? id (org-id-get)) (length> (org-review-cloze-overlays) 0))))))
      (cond
       ((not due) :done)
       (clozes-hidden :unanswered)
       (t :answered)))))

(cl-defun org-review--element-due? (&optional (time (org-today)))
  "True if element scheduled before TIME."
  (<= (time-to-days (org-get-scheduled-time (point))) time))

(define-minor-mode org-review-mode
  "Major mode for element review."
  :init-value nil
  :lighter " org-review"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [f7] #'kill-buffer-and-window)
            (define-key map [f5] #'org-review-next)
            map))

(defun org-review-next ()
  "Check if there is outstanding item in priority queue, go to it, narrow.
Do nothing if already on outstanding item."
  (interactive)
  (thunk-let* ((id (org-review-outstanding-id))
               (type (org-review--element-type id))
               (current-review (eq (current-buffer) (org-review--current-review-buffer))))
    (cond
     ((not id)
      (message "Empty priority queue 😎")
      (and (buffer-live-p (org-review--current-review-buffer)) (kill-buffer (org-review--current-review-buffer))))
     ((and current-review (length> (org-review-cloze-overlays) 0))
      ;; TODO: Move to org-review-item-show
      (org-review-cloze-show)
      (condition-case err
          (save-excursion
            (org-back-to-heading)
            (org-babel-next-src-block)
            (org-babel-execute-src-block))
        (user-error nil))
      )
     ((and current-review (<= (time-to-days (org-get-scheduled-time (point))) (org-today)))
      (message (call-interactively #'org-review-item-rate))
      ;; TODO: Check if again, hi
      )
     (t
      (org-review-item-start (org-review-outstanding-id))
      
      ;; TODO: Check if current element is org-review-mode and is item
      ;; TODO:     (org-review-item-rate)
      ;; TODO: Check if current element is topic
      ;; TODO:     go to next element
      
      ;;  (org-review-cloze-show)
      ))))


(defun org-review-item-start (id)
  "Go to ID, narrow, start review mode."
  (interactive (list (org-id-get-create)))
  (save-window-excursion
    (org-review-goto id)
    (and (buffer-live-p (org-review--current-review-buffer)) (kill-buffer (org-review--current-review-buffer)))
    (with-current-buffer (or (org-review--current-review-buffer) (clone-indirect-buffer "*Current Review*" t))
      (org-narrow-to-subtree) ;; Why it is needed?
      (org-fold-hide-drawer-all)
      (org-review-cloze-hide)
      (org-review-mode))
    )
  (pop-to-buffer "*Current Review*"))


;; TODO: Reschedule outstanding element
;; TODO: Postpone element
;; https://emacs.stackexchange.com/questions/71890/how-can-i-automate-rescheduling-overdue-daily-tasks

(require 'hi-lock)

(defun org-review-cloze-hide ()
  "Hide all clozes."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (while (org-review-cloze-hide-single))))

(defun org-review-cloze-hide-single (&optional start end hint)
  "Hide the region from START to END with HINT."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end) "…")
                 (list (and (boundp 'start) start)
                       (and (boundp 'end) end)
                       (and (boundp 'hint) hint))))
  (when (not (and start end hint))
    (when-let* ((bounds (org-review-bounds-of-cloze-at-point)))
      (when (not start) (setq start (car bounds)))
      (when (not end) (setq end (-last-item bounds)))
      (when (not hint)
        (setq hint (let ((hint (buffer-substring (caddr bounds) (- (cadddr bounds) 2))))
                     (if (s-present? hint) hint "…"))))))
  (when (and start end hint)
    (let ((o (make-overlay start end)))
      (overlay-put o 'org-review-cloze t)
      (overlay-put o 'invisible t)
      (overlay-put o 'face '(hi-yellow))
      (overlay-put o 'display (s-concat "[" hint "]"))
      t)))

(defmacro with-ignore-invisible-overlays (&rest form)
  "FORM should be a callable that return a point based on the current point."
  (let ((search-fun (car form))
        (args (cdr-safe form)))
    `(let ((overlay nil)
           (point nil))
       (setq point ,@form)
       (setq overlay (car (overlays-at (point))))
       (while (and point overlay (member 'invisible (overlay-properties overlay)))
         (goto-char (overlay-end overlay))
         (setq point ,@form)
         (setq overlay (car (overlays-at (point))))
         (when (and point overlay (member 'invisible (overlay-properties overlay)))
           (goto-char (overlay-start overlay))
           (setq point ,@form)
           (setq overlay (car (overlays-at (point))))
           ))
       point)))

(defun org-review-bounds-of-cloze-at-point ()
  "Return bounds (START MID1 MID2 END) of cloze at point."
  (save-excursion
    (when-let* ((start (with-ignore-invisible-overlays (re-search-backward "{{" nil t)))
                (end (re-search-forward "}}" nil t))
                (mid (re-search-backward "::" nil t)))
      (list start mid (+ mid 2) end))))

(defun org-review-cloze-overlays ()
  "Return all overlays in the current buffer."
  (-sort
   (lambda (a b) (< (overlay-start a) (overlay-start b)))
   (seq-filter (lambda (o) (overlay-get o 'org-review-cloze))
               (overlays-in (point-min) (point-max)))))

(defun org-review-cloze-show ()
  "Remove all cloze overlays in the buffer."
  (interactive)
  (dolist (o (org-review-cloze-overlays))
    (delete-overlay o)))


(defun org-review-init ()
  "Add entry to `display-buffer-alist`."
  (add-to-list 'display-buffer-alist
	       '("\\*\\(Current Review\\)\\*\\'"
                 (display-buffer-reuse-mode-window display-buffer-pop-up-window)
                 (dedicated . t)
                 )))

(provide 'org-review)
;;; org-review.el ends here

