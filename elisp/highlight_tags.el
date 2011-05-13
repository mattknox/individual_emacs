;; from http://www.reddit.com/r/emacs/comments/ha7l9/html_matching_tag_highlighting/
(defun hilight-tags-contexts ()
  (save-excursion
    (let ((ctx-a (sgml-get-context)))
      (if (eq (sgml-tag-type (car ctx-a)) 'close)
          (cons (sgml-get-context) ctx-a)
        (cons ctx-a
              (progn
                (sgml-skip-tag-forward 1)
                (backward-char 1)
                (sgml-get-context)))))))

(defun highlight-tags-hook ()
  (let ((ctx (hilight-tags-contexts)))
    (move-overlay sgml-match-end-overlay
                  (sgml-tag-start (caar ctx))
                  (sgml-tag-end (caar ctx)))
    (move-overlay sgml-match-start-overlay
                  (sgml-tag-start (cadr ctx))
                  (sgml-tag-end (cadr ctx)))

    (overlay-put sgml-match-start-overlay 'face 'show-paren-match-face)
    (overlay-put sgml-match-end-overlay 'face 'show-paren-match-face)))

(define-minor-mode highlight-tags-mode
  "Toggle highlight-tags mode."
  nil "" nil
  (if highlight-tags-mode
      (progn
        (set (make-local-variable 'sgml-match-start-overlay)
             (make-overlay 0 0))
        (set (make-local-variable 'sgml-match-end-overlay)
             (make-overlay 0 0))
        (add-hook 'post-command-hook 'highlight-tags-hook nil t))
    (progn
      (remove-hook 'post-command-hook 'highlight-tags-hook t)
      (delete-overlay sgml-match-start-overlay)
      (delete-overlay sgml-match-end-overlay))))
