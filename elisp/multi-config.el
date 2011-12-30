(defvar emacs-sync-path (if (or (eq system-type 'cygwin)
                                (eq system-type 'gnu/linux)
                                (eq system-type 'linux)
                                (eq system-type 'darwin))
                            (concat (getenv "HOME") "/.emacs.git/")
                          (concat (getenv "USERPROFILE") "/.emacs.git/"))
  "emacs sync directory")
(add-to-list 'load-path emacs-sync-path)
(load "common-init.el")

;; example of how I might choose to use submodules.
;; definitely need something like bundler if I am going to go this sort of route.
;; might bundler itself work?  If it can take a git sha, that'd be awesome.
;; git submodule add git://jblevins.org/git/markdown-mode.git submodules/markdown-mode
