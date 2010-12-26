(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/backups/" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

(require 'maxframe)
(load-file (concat user-specific-dir "/uptime.el"))
(uptime-init)

(autoload 'color-theme-knoxboard "knoxboard" "\
Color theme by Matt Knox, based off twilight.el and blackboard.el, created 2010-04
\(fn)" t nil)

(color-theme-knoxboard)

(add-to-list 'load-path (concat user-specific-dir "/arc" ))
(add-to-list 'load-path (concat user-specific-dir "/ensime/src/main/elisp"))
(add-to-list 'load-path (concat user-specific-dir "/fuel" ))
(add-to-list 'load-path (concat user-specific-dir "/rhtml" ))
(add-to-list 'load-path (concat user-specific-dir "/scala"))
(add-to-list 'load-path (concat user-specific-dir "/w3m"))
(add-to-list 'load-path (concat user-specific-dir "/zencoding"))


(setq fuel-listener-factor-binary "~/bin/factor/factor")
(setq fuel-listener-factor-image "~/bin/factor/factor.image")
(load-file "~/.emacs.d/elpa/yaml-mode-0.0.5/yaml-mode.el")
(load-file (concat user-specific-dir "/fuel/fu.el"))
(load-file (concat user-specific-dir "/ensime/src/main/elisp/ensime.el"))
(load-file (concat user-specific-dir "/scala/scala-mode.el"))
(load-file (concat user-specific-dir "/arc/inferior-arc.el"))
(load-file (concat user-specific-dir "/arc/arc.el"))
(load-file (concat user-specific-dir "/tuareg-mode-1.45.6/tuareg.el"))
(load-file (concat user-specific-dir "/haskell-mode-2.7.0/haskell-mode.el"))
(load-file (concat user-specific-dir "/coffee-mode/coffee-mode.el"))
(load-file (concat user-specific-dir "/twittering-mode/twittering-mode.el"))
(load-file (concat user-specific-dir "/zencoding/zencoding-mode.el"))
(load-file (concat user-specific-dir "/snippet.el"))
(require 'w3m-load)
(require 'w3m-e21)
(provide 'w3m-e23)
(require 'textmate)

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-use-cookies t)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.arc$" . arc-mode))
(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.topml$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.html.erb$" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'rdebug)

(require 'edit-server)
; (setq debug-on-error 't)
; (setq edebug-all-defs 't)
(setq edit-server-new-frame nil)
(edit-server-start)

(add-hook 'js2-mode-hook 'js2-custom-setup)
(defun js2-custom-setup ()
  (moz-minor-mode 1))

(setq js2-basic-offset 2)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'nxml-mode-hook 'zencoding-mode)

;; (add-hook 'js2-mode-hook 'esk-paredit-nonlisp)
;; (add-hook 'js2-mode-hook 'run-coding-hook)
;; (add-hook 'js2-mode-hook 'idle-highlight)

;; need to find something do disable ruby inserting encoding strings

(add-hook 'ruby-mode-hook
          (lambda()
	    (ruby-electric-mode t)))
(add-hook 'scheme-mode-hook
          (lambda()
	    (paredit-mode t)))
(add-hook 'emacs-lisp-mode-hook
          (lambda()
	    (paredit-mode t)))
(add-hook 'arc-mode-hook
          (lambda()
	    (paredit-mode t)))
(add-hook 'clojure-mode-hook
          (lambda()
	    (paredit-mode t)))

(setq extra-path '("/opt/ruby-enterprise-1.8.7-2009.10/bin" "~/bin" "/opt/local/bin" "/opt/local/sbin" "/usr/local/mysql/bin" "/usr/local/bin"))
(setenv "PATH" (concat (mapconcat 'identity extra-path ":") ":" (getenv "PATH")))
(setq exec-path (append extra-path exec-path))

(setq kill-whole-line t)
(setq ido-case-fold nil)
(column-number-mode)

(setq ns-pop-up-frames nil)

(defun ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert "#")
  (let ((properties (text-properties-at (point))))
    (when (and
           (memq 'font-lock-string-face properties)
           (save-excursion
             (ruby-forward-string "\"" (line-end-position) t)))
      (insert "{}")
      (backward-char 1))))

(global-set-key (kbd "C-u") 'forward-sexp)
(global-set-key (kbd "C-t") 'transpose-sexps)
(global-set-key (kbd "C-M-t") 'transpose-chars)
(global-set-key (kbd "C-o") 'backward-sexp)
(global-set-key (kbd "C-M-u") 'backward-char)
(global-set-key (kbd "C-n") 'next-line)
(global-set-key (kbd "C-M-n") 'forward-char)
(global-set-key (kbd "TAB")  'slime-complete-symbol)
(global-set-key (kbd "C-TAB")  'lisp-indent-line)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-g" 'magit-status)
(global-set-key "\C-x\g" 'magit-status)
(global-set-key "\M-t" 'textmate-goto-symbol)
(global-set-key [(control x) (control b)] 'electric-buffer-list)

(global-set-key "\C-\M-h" 'backward-kill-word)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-x\C-r" 'jump-to-register)

(global-set-key "\M-t" 'textmate-goto-file)
(global-set-key "\M-#" 'comment-or-uncomment-region-or-line)

(global-set-key "\C-xh" (lambda (url) (interactive "MUrl: ")
			  (switch-to-buffer (url-retrieve-synchronously url))
			  (rename-buffer url t)
			  (html-mode)))

;(define-key ruby-mode-map (kbd "#") 'ruby-interpolate)

(require 'js-comint)
(setq inferior-js-program-command "node-repl")
(add-hook 'js2-mode-hook '(lambda ()
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)k
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))

;; this is necessary for emacs 23, because it uses the alt/option key for meta.
(setq mac-command-modifier 'meta)
(set-default-font "-apple-DejaVu_Sans_Mono-medium-normal-normal-*-12-*-*-*-m-0-iso10646-")
; use default Mac browser
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

; delete files by moving them to the OS X trash
(setq delete-by-moving-to-trash t)
(setq quack-remap-find-file-bindings-p nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; emacsclient config
(server-start)
;; (add-hook 'after-init-hook 'server-start)
;; (add-hook 'server-done-hook
;; (lambda ()
;; (shell-command
;; "screen -r -X select `cat ~/tmp/emacsclient-caller`")))

(eval-after-load "paredit"
  '(define-key paredit-mode-map (kbd ")")
     'paredit-close-parenthesis))
(eval-after-load "paredit"
  '(define-key paredit-mode-map (kbd "M-)")
     'paredit-close-parenthesis-and-newline))

(set-frame-parameter (selected-frame) 'alpha '(98 96))
(add-to-list 'default-frame-alist '(alpha 98 96))
(setq save-abbrevs nil) ; don't bug me about saving ~/.abbrev_defs

(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
     	  (lambda () (rinari-launch)))

(defun toggle-fullscreen (&optional f)
      (interactive)
      (let ((current-value (frame-parameter nil 'fullscreen)))
           (set-frame-parameter nil 'fullscreen
                                (if (equal 'fullboth current-value)
                                    (if (boundp 'old-fullscreen) old-fullscreen nil)
                                    (progn (setq old-fullscreen current-value)
                                           'fullboth)))))
(eval-when-compile (require 'cl))
 (defun toggle-transparency ()
   (interactive)
   (if (/=
        (cadr (frame-parameter nil 'alpha))
        100)
       (set-frame-parameter nil 'alpha '(100 100))
     (set-frame-parameter nil 'alpha '(85 50))))
(global-set-key (kbd "C-c t") 'toggle-transparency)


(maximize-frame)
(split-window-horizontally)
