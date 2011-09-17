; like universal argument, don't use it much, need C-u
(global-set-key (kbd "C-S-u") 'universal-argument)
(global-set-key (kbd "C-u") 'forward-sexp)
(global-set-key (kbd "C-t") 'transpose-sexps)
(global-set-key (kbd "C-M-t") 'transpose-chars)
(global-set-key (kbd "C-o") 'backward-sexp)
(global-set-key (kbd "C-M-u") 'backward-char)
(global-set-key (kbd "C-n") 'next-line)
(global-set-key (kbd "C-M-n") 'forward-char)
(global-set-key (kbd "C-TAB")  'lisp-indent-line)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key "\C-x\C-m" 'smex)

(global-set-key "\C-x\C-g" 'magit-status)
(global-set-key "\C-x\g" 'magit-status)
(global-set-key "\M-T" 'textmate-goto-symbol)
(global-set-key [(control x) (control b)] 'electric-buffer-list)

(global-set-key "\C-\M-h" 'backward-kill-word)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-x\C-r" 'jump-to-register)

(global-set-key "\M-w" 'kill-buffer-and-close-frame)
(global-set-key "\M-W" 'kill-this-buffer)
(global-set-key (kbd "A-w") 'kill-this-buffer)
(global-set-key "\M-t" 'textmate-goto-file)
(global-set-key "\M-#" 'comment-dwim) ; TODO: should this be paredit-comment-dwim?
(global-set-key (kbd "M-DEL") 'backward-kill-sexp)
(global-set-key "\C-xh" (lambda (url) (interactive "MUrl: ")
			  (switch-to-buffer (url-retrieve-synchronously url))
			  (rename-buffer url t)
			  (html-mode)))

(global-set-key [C-tab] 'other-window)
(global-set-key "\C-c\C-g" 'gist-buffer-confirm)
(global-set-key (kbd "C-S-N") 'word-count)
(global-set-key (kbd "A-F") 'ack)
(global-set-key (kbd "<f1>") 'maximize-frame)
(global-set-key (kbd "<S-backspace>") 'kill-region)
(global-set-key (kbd "M-s") 'save-some-buffers)
(global-set-key (kbd "M-r") 'ido-find-alternate-file)
(global-set-key (kbd "C-=") 'universal-argument)

;; mode-specific keybindings: maybe move these to their own file(s)?
(eval-after-load "paredit" '(define-key paredit-mode-map (kbd "TAB") 'slime-complete-symbol))
(eval-after-load "paredit"
  '(define-key paredit-mode-map (kbd ")")
     'paredit-close-parenthesis))
(eval-after-load "paredit"
  '(define-key paredit-mode-map (kbd "M-)")
     'paredit-close-parenthesis-and-newline))

;(define-key term-mode-map (kbd "A-h") 'term-char-mode)


