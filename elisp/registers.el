;; Registers allow you to jump to a file or other location
;; quickly. Use C-x r j followed by the letter of the register (i for
;; init.el, r for this file) to jump to it.

;; You should add registers here for the files you edit most often.

(dolist (r `((?i (file . ,(concat user-specific-dir "/init.el")))
             (?r (file . ,(concat user-specific-dir "/elisp/registers.el")))
             (?b (file . "~/.bashrc"))
             (?d (file . "~/h/editor/README"))
             (?n (file . "~/h/nyclisp/desiderata"))
             (?e (file . "~/h/prose/essays/README"))
             (?j (file . "~/h/ruby/script_jank.rb"))
             (?p (file . "~/promptbird/src/main/scala/com/twitter/promptbird/Main.scala"))
             (?g (file . "~/h/goaloc/README"))))
  (set-register (car r) (cadr r)))
