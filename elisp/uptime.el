;;; uptime.el by David N. Welton <davidw@dedasys.com>

;;; uptime defines 2 user visible functions, `uptime' and
;;; `linux-uptime'. `uptime' works on all systems, but must be
;;; initiliazed with `uptime-init' in your .emacs file.
;;; `linux-uptime' is, as the name suggests, specific to GNU/Linux
;;; systems because of its dependence on the /proc filesystem.

;;; Modified by Trond Endrestøl <Trond.Endrestol@fagskolen.gjovik.no>
;;; on Monday 2 June 2008.  I made the code run on FreeBSD 6.2, using
;;; FreeBSD's /proc filesystem and the date command.  The code is
;;; untested with respect to the other *BSDs, such as NetBSD, OpenBSD,
;;; and even BSD/OS.

;;; With my (Trond's) modifications, uptime defines 3 user visible
;;; functions `uptime', `linux-uptime' and `bsd-uptime'.  `bsd-uptime'
;;; is dependant on the BSD /proc filesystem and the date command.

;;; The original variable uptime-stat-file-starttime-position, has
;;; been given its companion, bsd-uptime-stat-file-starttime-position.

(require 'cl)

(defvar uptime-stat-file-starttime-position 21
  "Position in the file /proc/(emacs-pid)/stat where the `starttime'
value may be found.  Currently defaults to 21 on GNU/Linux systems")

(defvar bsd-uptime-stat-file-starttime-position 7
  "Position in the file /proc/(emacs-pid)/status where the `starttime'
value may be found.  Currently defaults to 7 on 'berkeley-unix systems")

(defun uptime-init ()
  "Initialize emacs uptime"
  (setq uptime-time-init (current-time)))

(defun uptime ()
  "Emacs uptime:-)"
  (interactive)
  (unless (boundp 'uptime-time-init)
    (setq uptime-time-init (current-time))
    (message "better `uptime-init' in your .emacs!"))

  ;;(if (eq system-type 'gnu/linux)
  ;;    (linux-uptime)
  ;;  (non-linux-uptime))

  ;; Replaced (if ...) with (case ...), could have used (cond ...).
  (case system-type
    ('gnu/linux (linux-uptime))
    ('berkeley-unix (bsd-uptime))
    (otherwise (non-linux-uptime))))

(defun non-linux-uptime ()
  (let* ((tm (current-time))
	 (diff (list (- (car tm) (car uptime-time-init))
		     (- (cadr tm) (cadr uptime-time-init))))
	 (seconds (+ (* (float (car diff)) 65536) (float (cadr diff))))
	 (days  (floor (/ seconds 86400)))
         (hours (progn (decf seconds (* days  86400)) (floor (/ seconds 3600))))
         (mins  (progn (decf seconds (* hours 3600))  (floor (/ seconds 60)))))
    (message (format "up %d days,  %02d:%02d" days hours mins))))

(defun bsd-uptime ()
  "Emacs uptime for BSD (relies on (FreeBSD) /proc)"
  (interactive)
  (if (eq system-type 'berkeley-unix)
      (message "Berkeley UNIX system")
    (error "`bsd-uptime' only works on berkeley-unix systems with the (FreeBSD) proc filesystem installed!"))
  (let* ((seconds (bsd-getprocstat))
         (days (floor (/ seconds 86400)))
         (hours (progn (decf seconds (* days 86400)) (floor (/ seconds 3600))))
         (mins (progn (decf seconds (* hours 3600)) (floor (/ seconds 60)))))
    (message (format "up %d days,  %02d:%02d" days hours mins))))

(defun linux-uptime ()
  "Emacs uptime for Linux (relies on /proc)"
  (interactive)
  (if (eq system-type 'gnu/linux)
      (message "GNU/Linux system")
    (error "`linux-uptime' only works on gnu/linux systems with the proc filesystem installed!"))
  (let* ((seconds (getprocstat))
	 (days  (floor (/ seconds 86400)))
         (hours (progn (decf seconds (* days  86400)) (floor (/ seconds 3600))))
         (mins  (progn (decf seconds (* hours 3600))  (floor (/ seconds 60)))))
    (message (format "up %d days,  %02d:%02d" days hours mins))))

(defun getprocstat ()
  "do the dirty work of processing the proc files"
  (let* ((foo (call-process "cat" nil "statout" nil  (format "/proc/%d/stat" (emacs-pid))))
	 (foo (switch-to-buffer (get-buffer "statout")))
	 (statstring (buffer-string))
	 (starttime (aref (vconcat (split-string statstring)) uptime-stat-file-starttime-position))
	 (foo (kill-buffer (current-buffer)))
	 (foo (call-process "cat" nil "uptimeout" nil "/proc/uptime"))
	 (foo (switch-to-buffer (get-buffer "uptimeout")))
	 (uptimestring (buffer-string))
	 (systemuptimestring (car (split-string uptimestring)))
	 (foo (kill-buffer (current-buffer))))
    (round (- (string-to-number systemuptimestring)
	      (/ (string-to-number starttime) 100)))))

(defun bsd-getprocstat ()
  "do the dirty work of processing the (Free)BSD proc files"
  (let*
      ((foo (call-process "cat" nil "statout" nil (format "/proc/%d/status" (emacs-pid))))
       (foo (switch-to-buffer (get-buffer "statout")))
       (statstring (buffer-string))
       (starttime (aref (vconcat (split-string (aref (vconcat (split-string statstring)) bsd-uptime-stat-file-starttime-position) ",")) 0))
       (foo (kill-buffer (current-buffer)))
       (foo (call-process "date" nil "uptimeout" nil "+%s"))
       (foo (switch-to-buffer (get-buffer "uptimeout")))
       (systemuptimestring (buffer-string))
       (foo (kill-buffer (current-buffer))))
    (- (string-to-number systemuptimestring) (string-to-number starttime))))

;; EOF
