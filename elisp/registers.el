;;; starter-kit-registers.el --- Set up registers
;;
;; Part of the Emacs Starter Kit

;; Registers allow you to jump to a file or other location
;; quickly. Use C-x r j followed by the letter of the register (i for
;; init.el, r for this file) to jump to it.

;; You should add registers here for the files you edit most often.

(dolist (r `((?i (file . ,(concat user-specific-dir "/init.el")))
             (?b (file . "~/.bashrc"))
             (?g (file . "~/h/goaloc/README"))))
  (set-register (car r) (cadr r)))

(provide 'starter-kit-registers)
;;; starter-kit-registers.el ends here
