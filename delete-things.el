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
  "Replaces `kill-region' with `delete-region' in BODY."
  `(flet ((kill-region (beg end &optional yank-handler) (delete-region beg end)))
     ,@body))

(defun delete-syntax (arg)
  "Like `kill-syntax', but does not save to the `kill-ring'."
  (interactive "*p")
  (delete-instead-of-kill (kill-syntax arg)))
(put 'delete-syntax 'CUA 'move)

(defun backward-delete-syntax (arg)
  "Like `backward-kill-syntax', but does not save to the `kill-ring'."
  (interactive "*p")
  (delete-syntax (- arg)))
(put 'backward-delete-syntax 'CUA 'move)

(defun delete-word (arg)
  "Like `kill-word', but does not save to the `kill-ring'."
  (interactive "*p")
  (delete-instead-of-kill (kill-word arg)))
(put 'delete-word 'CUA 'move)

(defun backward-delete-word (arg)
  "Like `backward-kill-word', but does not save to the `kill-ring'."
  (interactive "*p")
  (delete-word (- arg)))
(put 'backward-delete-word 'CUA 'move)

(defun delete-line (&optional arg)
  "Like `kill-line', but does not save to the `kill-ring'."
  (interactive "*P")
  (delete-instead-of-kill (kill-line arg)))


(provide 'delete-things)
