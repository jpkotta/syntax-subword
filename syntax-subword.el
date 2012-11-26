;;; syntax-subword.el --- make operations on words more fine-grained

;; Copyright (C) 2012 Jonathan Kotta

;; Author: Jonathan Kotta <jpkotta@gmail.com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides `syntax-subword' minor mode, which extends
;; `subword-mode' to make word editing and motion more fine-grained.
;; Basically, it makes syntax changes, CamelCaseWords, and the normal
;; word boundaries the boundaries for word operations.  Here's an
;; example of where the cursor stops using `forward-word' in
;; `emacs-lisp-mode':
;;
;; (defun FooBar (arg) "doc string"
;; |     |      |    |     |      |  standard
;; |     |   |  |    |     |      |  subword-mode
;; ||    ||  |  |||  ||||  ||     || syntax-subword-mode
;; ||     |      ||  | ||   |     |  vim
;;
;; As you can see, syntax boundaries are places where the syntax
;; changes, i.e. we change from a bracket to a keyword, to a space, to
;; an argument, to a space, etc.  This makes word movement much more
;; fine-grained, to the point that you almost never need to operate by
;; single characters anymore.  Vim's word operations are similar to
;; this mode's.
      
;;; Code:


(require 'subword)
(require 'delete-things)

(defvar syntax-subword-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (old-and-new
             '((right-word           right-syntax-or-subword)
               (left-word            left-syntax-or-subword)
               (forward-word         forward-syntax-or-subword)
               (backward-word        backward-syntax-or-subword)
               (mark-word            syntax-subword-mark)
               (kill-word            syntax-subword-kill)
               (backward-kill-word   syntax-subword-backward-kill)
               (delete-word          syntax-subword-delete)
               (backward-delete-word syntax-subword-backward-delete)
               (transpose-words      syntax-subword-transpose)
               (capitalize-word      syntax-subword-capitalize)
               (upcase-word          syntax-subword-upcase)
               (downcase-word        syntax-subword-downcase)))
      (let ((oldcmd (car old-and-new))
            (newcmd (cadr old-and-new)))
        (define-key map (vector 'remap oldcmd) newcmd)))
    map)
  "Keymap used in `syntax-subword-mode' minor mode.")

;; make these work with CUA shift-selection
(dolist (c '(forward-syntax-or-subword
             backward-syntax-or-subword
             right-syntax-or-subword
             left-syntax-or-subword
             forward-syntax
             backward-syntax))
  (put c 'CUA 'move))

;;;###autoload
(define-minor-mode syntax-subword-mode
  "This mode is like `subword-mode', but also treats syntax
  changes as word boundaries.  Syntax changes are generally the
  same as face changes when font lock is
  enabled. \\{syntax-subword-mode-map}"
    nil
    nil
    syntax-subword-mode-map
    (when (and syntax-subword-mode subword-mode)
      (subword-mode -1)
      (message "Disabling subword-mode"))
    )

;;;###autoload
(define-global-minor-mode global-syntax-subword-mode syntax-subword-mode
  (lambda () (syntax-subword-mode 1)))

(defun forward-syntax-or-subword (&optional n)
  "Go forward by either the next change in syntax or a
  subword (see `subword-mode' for a description of
  subwords)."
  (interactive "^p")
  (cond
   ((< 0 n)
    (dotimes (i n)
      (goto-char (forward-syntax-or-subword-pos))))
   ((> 0 n)
    (dotimes (i (- n))
      (goto-char (backward-syntax-or-subword-pos))))))

(defun backward-syntax-or-subword (&optional n)
  "Go backward to the previous change in syntax or subword (see
  `subword-mode' for a description of subwords)."
  (interactive "^p")
 (forward-syntax-or-subword (- n)))

(defun right-syntax-or-subword (&optional n)
  (interactive "^p")
  (if (eq (current-bidi-paragraph-direction) 'left-to-right)
      (forward-syntax-or-subword n)
    (backward-syntax-or-subword n)))

(defun left-syntax-or-subword (&optional n)
  (interactive "^p")
  (if (eq (current-bidi-paragraph-direction) 'left-to-right)
      (backward-syntax-or-subword n)
    (forward-syntax-or-subword n)))

(defun syntax-subword-kill (&optional n)
  (interactive "^p")
  (let ((beg (point))
        (end (save-excursion (forward-syntax-or-subword n) (point))))
    (kill-region beg end)))

(defun syntax-subword-delete (&optional n)
  (interactive "^p")
  (delete-instead-of-kill (syntax-subword-kill n)))

(defun syntax-subword-backward-kill (&optional n)
  (interactive "^p")
  (syntax-subword-kill (- n)))

(defun syntax-subword-backward-delete (&optional n)
  (interactive "^p")
  (delete-instead-of-kill (syntax-subword-backward-kill n)))

(defalias 'syntax-subword-mark 'subword-mark)
(defalias 'syntax-subword-transpose 'subword-transpose)
(defalias 'syntax-subword-capitalize 'subword-capitalize)
(defalias 'syntax-subword-downcase 'subword-downcase)
(defalias 'syntax-subword-upcase 'subword-upcase)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; internal functions

(defun forward-syntax (&optional arg)
  "Like `forward-word', but jump to the next change in syntax.
  This is closer to Vim's behavior when moving by words."
  (interactive "p")
  (let ((arg (or arg 1))
        (inc (if (and arg (< arg 0)) 1 -1)))
    (while (not (= arg 0))
      (if (> arg 0)
          (skip-syntax-forward (string (char-syntax (char-after))))
        (skip-syntax-backward (string (char-syntax (char-before)))))
      (setq arg (+ arg inc)))))

(defun backward-syntax (&optional arg)
  "Like `backward-word', but jump to the next change in syntax.
  This is closer to Vim's behavior when moving by words."
  (interactive "p")
  (forward-syntax (- 0 (or arg 1))))

(defun forward-syntax-or-subword-pos ()
  (let (subword-pos syntax-pos)
    (save-excursion
      (forward-syntax)
      (setq syntax-pos (point)))
    (save-excursion
      (subword-forward)
      (setq subword-pos (point)))
    ;; always move at least one char forward
    (max (1+ (point)) (min subword-pos syntax-pos))))

(defun backward-syntax-or-subword-pos ()
  (let (subword-pos syntax-pos)
    (save-excursion
      (backward-syntax)
      (setq syntax-pos (point)))
    (save-excursion
      (subword-backward)
      (setq subword-pos (point)))
    ;; always move at least one char backward
    (min (1- (point)) (max syntax-pos subword-pos))))


(provide 'syntax-subword)
;;; syntax-subword.el ends here
