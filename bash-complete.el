
(require 'comint)

(defun bash-complete-dynamic-complete ()
  "Bash completion function for `comint-complete-dynamic-functions'.

Call bash to do the completion."
  (when (comint-match-partial-filename)
    (unless (window-minibuffer-p (selected-window))
      (message "Bash completion..."))
    (bash-complete-dynamic-complete-0)))

(defun bash-complete-dynamic-complete-0 ()
  (save-excursion
    (let* ( (pos (point))
	    (start (comint-line-beginning-position))
	    (end (line-end-position))
	    (line (buffer-substring-no-properties start end))
	    (wordsplit (bash-complete-split start end pos))
	    (words (car wordsplit))
	    (cword (cdr wordsplit))
	    (stub (nth cword words)) )
      (comint-simple-complete stub
			      (bash-complete-comm
			       line pos words cword)))))

;; "hell o" wor\ ld 'baa baaaa'a"hell o"world a

;; (progn
;;   (load-library "~/.emacs.d/bash-complete.el")
;;   (let ((start 64) (end 108))
;;     (bash-complete-split start end 80)))

(defun bash-complete-split (start end pos)
  "Split LINE like bash would do, keep track of current word at POS.

Return a list containing the words and the number of the word
at POS, the current word: ( (word1 word2 ...) . wordnum )"
  (save-excursion
    (goto-char start)
    (let ((accum (cons nil nil)))
      (setq accum (bash-complete-split-0 start end pos accum ""))
      (cons (car accum) (nreverse (cdr accum))))))

(defun bash-complete-split-0 (start end pos accum straccum)
  (let ( (char-start (char-after))
	 (quote nil) )
    (when (or (= char-start ?') (= char-start ?\"))
      (forward-char)
      (setq quote char-start))
    (bash-complete-split-1 start end pos quote accum straccum)))

(defun bash-complete-split-1 (start end pos quote accum straccum)
  (let ((local-start (point)))
    (skip-chars-forward (bash-complete-nonsep quote) end)
    (setq straccum (concat straccum (buffer-substring-no-properties local-start (point)))))
  (cond
   ;; an escaped char, skip, whatever it is
   ((and (char-before) (= ?\\ (char-before)))
    (forward-char)
    (bash-complete-split-1
     start end pos quote
     accum
     (concat (substring straccum 0 (- (length straccum) 1))  (char-to-string (char-before)))))
   ;; opening quote
   ((and (not quote) (char-after) (or (= ?' (char-after)) (= ?\" (char-after))))
    (bash-complete-split-0 start end pos accum straccum))
   ;; closing quote
   ((and quote (char-after) (= quote (char-after)))
    (forward-char)
    (bash-complete-split-0 start end pos accum straccum))
   ;; space inside a quote
   ((and quote (char-after) (not (= quote (char-after))))
    (forward-char)
    (bash-complete-split-1
     start end pos quote accum
     (concat straccum (char-to-string (char-before)))))
   ;; word end
   (t
    (skip-chars-forward " \t\n\r" end)
    (when (> (length straccum) 0)
      (setcdr accum (cons straccum (cdr accum)))
      (when (and (not (car accum)) (> pos 0) (<= pos (point)))
	(setcar accum (- (length (cdr accum)) 1))))
    (if (< (point) end)
	(bash-complete-split-0 (point) end pos accum "")
      accum))))

(defun bash-complete-nonsep (quote)
  (if quote
      (concat "^ \t\n\r" (char-to-string quote))
    "^ \t\n\r'\""))

(defun bash-complete-comm (line pos words cword)
  "Set LINE, POS, WORDS and CWORD, call bash completion, return the result.

This function starts a separate bash process if necessary, sets up the
completion environment (COMP_LINE, COMP_POINT, COMP_WORDS, COMP_CWORD) and
calls compgen.

The result is a list of candidates, which might be empty."

  )

(provide 'bash-complete)