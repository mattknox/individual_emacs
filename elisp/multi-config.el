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
;; anyway, this is how this dude does it.
;; git submodule add git://jblevins.org/git/markdown-mode.git submodules/markdown-mode
;; git submodule foreach git checkout master
;; git submodule foreach git pull

;; this is the only benefit, afaict, to doing this crazy submodule thing:
;; grabs the super-repo and then the submodules.
;; git clone <source_repository> <target_directory>
;; git submodule update --init
