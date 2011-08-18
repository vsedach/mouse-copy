;; Adapted from http://www.mail-archive.com/gnu-emacs-sources@gnu.org/msg00393.html
;; and http://www.foldr.org/~michaelw/projects/redshank/redshank.el

(global-set-key [C-mouse-1] 'ignore)
(global-set-key [C-drag-mouse-1] 'ignore)
(global-set-key [C-down-mouse-1] 'mouse-insert-sexp-at-point)
(global-set-key [C-M-mouse-1] 'ignore)
(global-set-key [C-M-drag-mouse-1] 'ignore)
(global-set-key [C-M-down-mouse-1] 'mouse-yank-sexp-to-point)

(defun mouse-copy-do-at-point (start-event thunk)
  (let ((posn (event-start start-event)))
    (let ((sexp-at-mouse-pos
           (with-selected-window (posn-window posn)
             (save-excursion
               (goto-char (posn-point posn))
               (let ((sexp (thing-at-point 'sexp)))
                 (funcall thunk sexp))))))
      (if sexp-at-mouse-pos
          (progn
            (unless (or (bolp)
                         (and (minibufferp)
                              (= (point) (minibuffer-prompt-end)))
                         (save-excursion
                           (backward-char)
                           (looking-at "\\s-\\|\\s\(")))
              (insert " "))
            (insert sexp-at-mouse-pos)
            (unless (or (eolp)
                        (and (minibufferp)
                             (= (point) (minibuffer-prompt-end)))
                        (looking-at "\\s-\\|\\s\)"))
              (insert " ")))
        (error "Mouse not at a sexp")))))

(defun mouse-insert-sexp-at-point (start-event)
  "Insert the sexp under the mouse cursor at point.
This command must be bound to a mouse event."
  (interactive "*e")
  (mouse-copy-do-at-point start-event (lambda (sexp) sexp)))

(defun delete-sexp ()
  (let ((point (point)))
    (forward-sexp)
    (delete-region point (point))))

(defun mouse-yank-sexp-to-point (start-event)
  "Yank the sexp under the mouse cursor to point.
This command must be bound to a mouse event."
  (interactive "*e")
  (mouse-copy-do-at-point start-event
                          (lambda (sexp)
                            (beginning-of-thing 'sexp)
                            (delete-sexp)
                            sexp)))

;;; need functions to replace highlighted region w/sexp, replace and yank

(provide 'mouse-copy)
