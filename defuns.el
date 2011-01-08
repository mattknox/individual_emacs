
;;; Part of my .emacs file

;; by Phil Hagelberg
;; Much thanks to emacswiki.org and RMS.

;; Note: this relies on files found in my dotfiles repository:
;; http://dev.technomancy.us/phil/browser/dotfiles/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defuns


;; I should try to gank/adapt everything here as I find time/interest
(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad "
          "minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

(defun display-image ()
  "display images using imagemagick"
  (interactive)
  (shell-command (concat "display "
			 (thing-at-point 'filename))))

(defun flickr-grab ()
  "Display only the photo from a flickr url"
  (interactive)
  (w3m-browse-url
   (with-current-buffer (url-retrieve-synchronously (thing-at-point 'filename))
     (save-excursion
       (re-search-backward "src=\"\\(http://static\\.flickr\\.com/[[:digit:]]*/[[:digit:]]*\_[[:alnum:]]*\\.jpg\\)")
       (match-string 1)))))

(defun map-coords (lat lon)
  (interactive "BLatitude: \nBLongitude")
  (w3m-browse-url (concat "http://maps.yahoo.com/maps_result?mag=12&lat="
			  lat "&lon=" lng)))

(defun mark-string ()
  (interactive)
  (setq deactivate-mark nil)
  (push-mark (search-forward "\"") t t)
  (search-backward "\""))

(defun blog-edit ()
  (interactive)
  (require 'tramp)
  (find-file "/philisha.net:blog")
  (html-mode)
  (auto-fill-mode))

(defun blog-post-region (begin end)
  "Create a new post on technomancy.us from the contents of the region"
  (interactive "r")

  (let ((content (buffer-substring begin end))
	(title (read-string "Title: "))
	(tags (split-string (read-string "Tags: ") ", ")))

    (shell-command
     (concat "ssh philisha.net apps/technomancy/script/runner \"p = Post.new(:title => '" title
	     "', :content => '" content "'); \""))))

(random t)
(defun random-music ()
  (interactive)
  (unless (boundp 'music-dirs)
    (setq music-dirs (split-string (shell-command-to-string "find /home/phil/music -type d | cut -c 18-") "\n")))
  (let ((dir (nth (random (length music-dirs)) music-dirs)))
    (shell-command (concat
		    "ssh philisha.net mpc clear; "
		    "ssh philisha.net mpc add " dir " > /dev/null"))
    (message dir)))

(defun make-frame-on-memex ()
  (interactive)
  (make-frame-on-display "192.168.1.47:0.0"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     cosmetics

(defun smallish (&optional font-size)
  (interactive)
  (set-default-font (concat "terminus-" (or font-size "12")))
  (tabbar-mode -1)
  (scroll-bar-mode -1))

(defun pretty-lambdas ()
    (font-lock-add-keywords
     nil `(("(\\(lambda\\>\\)"
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      ,(make-char 'greek-iso8859-7 107))
                      nil))))))

(defun ff/move-region-to-fridge ()
  (interactive)
  "Cut the current region, paste it in a file called ./fridge with a time tag, and save this file"
  (unless (use-region-p) (error "No region selected"))
  (let ((bn (file-name-nondirectory (buffer-file-name))))
    (kill-region (region-beginning) (region-end))
    (with-current-buffer (find-file-noselect "fridge")
      (goto-char (point-max))
      (insert "\n")
      (insert "######################################################################\n")
      (insert "\n" (format-time-string "%Y %b %d %H:%M:%S" (current-time)) " (from " bn ")\n\n")
      (yank)
      (save-buffer)
      (message "Region moved to fridge"))))

; stolen from defunkt
(defun word-count ()
  "Count words in buffer"
  (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

; for loading libraries in from the vendor directory
(defun vendor (library)
  (let* ((file (symbol-name library))
         (normal (concat "~/.emacs.d/vendor/" file))
         (suffix (concat normal ".el"))
         (personal (concat user-specific-dir file))
         (found nil))
    (cond
     ((file-directory-p normal) (add-to-list 'load-path normal) (require library))
     ((file-directory-p suffix) (add-to-list 'load-path suffix) (require library))
     ((file-exists-p suffix) (set 'found t)))
    (when found
      (if autoload-functions
          (dolist (autoload-function autoload-functions)
            (autoload autoload-function (symbol-name library) nil t))
        (require library)))
    (when (file-exists-p (concat personal ".el"))
      (load personal))))

(defun url-fetch-into-buffer (url)
  (interactive "sURL:")
  (insert (concat "\n\n" ";; " url "\n"))
  (insert (url-fetch-to-string url)))

(defun url-fetch-to-string (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (beginning-of-buffer)
    (search-forward-regexp "\n\n")
    (delete-region (point-min) (point))
    (buffer-string)))

(defun gist-buffer-confirm (&optional private)
  (interactive "P")
  (when (yes-or-no-p "Are you sure you want to Gist this buffer? ")
    (gist-region-or-buffer private)))


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

(defun kill-buffer-and-close-frame ()
  (interactive)
  (kill-this-buffer)
  (delete-window))

;; this is from rails-lib.el in the emacs-rails package
(defun string-join (separator strings)
  "Join all STRINGS using SEPARATOR."
  (mapconcat 'identity strings separator))

(provide 'defuns)
