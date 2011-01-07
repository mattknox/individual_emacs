(defvar paredit-space-for-delimiter-predicates nil)
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/backups/" t)))
  '(backup-directory-alist '(("." . "~/.emacs.d/backups/"))))

(require 'maxframe)
(autoload 'color-theme-knoxboard "knoxboard" "Color theme by Matt Knox, based off twilight.el and blackboard.el, created 2010-04
\(fn)" t nil)

(setq user-specific-loadpath-dirs '("/arc" "/ensime/src/main/elisp" "/rhtml" "/scala" "/w3m" "/zencoding" "/fuel_new"))
(setq user-specific-load-files '( "keybindings"
                                  "defuns.el"
                                  "/fuel_new/fu.el"
                                  "/ensime/src/main/elisp/ensime.el"
                                  "/scala/scala-mode.el"
                                  "/arc/inferior-arc.el"
                                  "/arc/arc.el"
                                  "/tuareg-mode-1.45.6/tuareg.el"
                                  "/haskell-mode-2.7.0/haskell-mode.el"
                                  "/coffee-mode/coffee-mode.el"
                                  "/twittering-mode/twittering-mode.el"
                                  "/zencoding/zencoding-mode.el"
                                  "/snippet.el"
                                  "/uptime.el"))

(setq mode-list-map '(("\\.hs$" . haskell-mode)
                      ("\\.ml[ily]?$" . tuareg-mode)
                      ("\\.topml$" . tuareg-mode)
                      ("\.coffee$" . coffee-mode)
                      ("\\.js$" . js2-mode)
                      ("\\.scala$" . scala-mode)
                      ("\\.html.erb$" . rhtml-mode)
                      ("\\.yml$" . yaml-mode)))

(mapc (lambda (x) (add-to-list 'auto-mode-alist x))
      mode-list-map)

(load-file (concat dotfiles-dir "/elpa/yaml-mode-0.0.5/yaml-mode.el"))
(mapc (lambda (x) (add-to-list 'load-path (concat user-specific-dir x)))
      user-specific-loadpath-dirs)
(mapc (lambda (x) (load-file (concat user-specific-dir x)))
      user-specific-load-files)

(setq fuel-listener-factor-binary "~/bin/factor/factor")
(setq fuel-listener-factor-image "~/bin/factor/factor.image")

(uptime-init)
(color-theme-knoxboard)

(require 'w3m-load)
(require 'w3m-e21)
(provide 'w3m-e23)
(require 'textmate)
(require 'rdebug)
(require 'edit-server)
(setq edit-server-new-frame nil)

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)
(setq w3m-use-cookies t)

; FIXME: shouldn't need this here, put it in because of load order strangeness
(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers"
  (run-hooks ‘coding-hook)
  (add-hook 'js2-mode-hook 'js2-custom-setup))

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

(defun run-servers ()
  (edit-server-start)
  (server-start))

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
;(server-start)
;(add-hook 'after-init-hook 'server-start)
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

