;;; mouse-copy.el --- easily copy/yank sexps under the mouse cursor

;; Copyright (c) 2011, Vladimir Sedach <vsedach@gmail.com>, Stas Boukarev <stassats@gmail.com>
;; Keywords: lisp

;; Permission to use, copy, modify, and/or distribute this software for
;; any purpose with or without fee is hereby granted, provided that the
;; above copyright notice and this permission notice appear in all
;; copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; Adapted from http://www.mail-archive.com/gnu-emacs-sources@gnu.org/msg00393.html
;; and http://www.foldr.org/~michaelw/projects/redshank/redshank.el

;;; Code:

(require 'thingatpt)

(defvar mouse-mode-map (make-sparse-keymap))

(define-minor-mode mouse-copy-mode
    "Minor mode for copying and moving text using a mouse
\\{mouse-mode-map}"
  :global t
  :keymap mouse-mode-map)

(define-key mouse-mode-map [C-mouse-1] 'ignore)
(define-key mouse-mode-map [C-drag-mouse-1] 'ignore)
(define-key mouse-mode-map [C-down-mouse-1] 'mouse-insert-sexp-at-point)
(define-key mouse-mode-map [C-M-mouse-1] 'ignore)
(define-key mouse-mode-map [C-M-drag-mouse-1] 'ignore)
(define-key mouse-mode-map [C-M-down-mouse-1] 'mouse-yank-sexp-to-point)

(defun mouse-copy-sexp-at-mouse (event thunk)
  (let ((position (event-start event)))
    (with-selected-window (posn-window position)
      (save-excursion
       (goto-char (posn-point position))
       (let ((sexp (thing-at-point 'sexp)))
         (funcall thunk sexp))))))

(defun mouse-copy-do-at-point (event thunk)
  (let ((sexp (mouse-copy-sexp-at-mouse event thunk)))
    (unless sexp
      (error "Mouse not at a sexp"))
    (when (and delete-selection-mode
               (use-region-p))
      (delete-region (region-beginning) (region-end)))
    (unless (or (bolp)
                (and (minibufferp)
                     (= (point) (minibuffer-prompt-end)))
                (save-excursion
                 (backward-char)
                 (looking-at "\\s-\\|\\s\(")))
      (insert " "))
    (insert sexp)
    (unless (or (eolp)
                (and (minibufferp)
                     (= (point) (minibuffer-prompt-end)))
                (looking-at "\\s-\\|\\s\)"))
      (insert " "))))

(defun mouse-insert-sexp-at-point (start-event)
  "Insert the sexp under the mouse cursor at point.
This command must be bound to a mouse event."
  (interactive "*e")
  (mouse-copy-do-at-point start-event (lambda (sexp) sexp)))

(defun mouse-copy-delete-sexp ()
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
                            (mouse-copy-delete-sexp)
                            sexp)))

(provide 'mouse-copy)
;;; mouse-copy.el ends here
