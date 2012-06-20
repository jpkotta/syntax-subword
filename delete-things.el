;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; miscellaneous utilities to delete and kill text

;; recommended keybindings:

;; (global-set-key (kbd "C-k") 'delete-line)
;; (global-set-key (kbd "C-S-k") 'kill-line)

;; (global-set-key (kbd "M-SPC") 'delete-horizontal-space)
;; (global-set-key (kbd "M-S-SPC") 'delete-blank-lines)

;; (global-set-key (kbd "<C-S-delete>") 'delete-syntax)
;; (global-set-key (kbd "<C-S-backspace>") 'backward-delete-syntax)
;; (global-set-key (kbd "C-<delete>") 'delete-word)
;; (global-set-key (kbd "M-d") 'delete-word)
;; (global-set-key (kbd "C-<backspace>") 'backward-delete-word)


;; from Jonathan Arkell (http://stackoverflow.com/questions/154097/whats-in-your-emacs/154980#154980)
(defun kill-syntax (&optional arg)
  "Kill ARG sets of syntax characters after point."
  (interactive "p")
  (let ((arg (or arg 1))
        (inc (if (and arg (< arg 0)) 1 -1))
        (opoint (point)))
    (while (not (= arg 0))
      (if (> arg 0)
          (skip-syntax-forward (string (char-syntax (char-after))))
        (skip-syntax-backward (string (char-syntax (char-before)))))
      (setq arg (+ arg inc)))
    (kill-region opoint (point))))

(defun kill-syntax-backward (&optional arg)
  "Kill ARG sets of syntax characters preceding point."
  (interactive "p")
  (kill-syntax (- 0 (or arg 1))))


(defmacro delete-instead-of-kill (&rest body)
  "Replaces kill-region with delete-region in `body'."
  `(flet ((kill-region (beg end &optional yank-handler) (delete-region beg end)))
     ,@body))

(defun delete-syntax (arg)
  (interactive "*p")
  (delete-instead-of-kill (kill-syntax arg)))
(put 'delete-syntax 'CUA 'move)

(defun backward-delete-syntax (arg)
  (interactive "*p")
  (delete-syntax (- arg)))
(put 'backward-delete-syntax 'CUA 'move)

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "*p")
  (delete-instead-of-kill (kill-word arg)))
(put 'delete-word 'CUA 'move)

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this ARG many times."
  (interactive "*p")
  (delete-word (- arg)))
(put 'backward-delete-word 'CUA 'move)

(defun delete-line (&optional arg)
  "Delete the rest of the current line; if no nonblanks there, delete thru newline.
With prefix argument ARG, delete that many lines from point.
Negative arguments delete lines backward.
With zero argument, deletes the text before point on the current line.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg.

To delete a whole line, when point is not at the beginning, type \
\\[move-beginning-of-line] \\[delete-line] \\[delete-line].

If `kill-whole-line' is non-nil, then this command deletes the whole line
including its terminating newline, when used at the beginning of a line
with no argument.  As a consequence, you can always delete a whole line
by typing \\[move-beginning-of-line] \\[delete-line]."
  (interactive "*P")
  (delete-instead-of-kill (kill-line arg)))


(provide 'delete-things)
