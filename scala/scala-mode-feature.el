;;; -*-Emacs-Lisp-*-
;;; scala-mode-feature.el - 

;; Copyright (C) 2009 Scala Dev Team at EPFL
;; Authors: See AUTHORS file
;; Keywords: scala languages oop
;; $Id$

;;; License

;; SCALA LICENSE
;;  
;; Copyright (c) 2002-2010 EPFL, Lausanne, unless otherwise specified.
;; All rights reserved.
;;  
;; This software was developed by the Programming Methods Laboratory of the
;; Swiss Federal Institute of Technology (EPFL), Lausanne, Switzerland.
;;  
;; Permission to use, copy, modify, and distribute this software in source
;; or binary form for any purpose with or without fee is hereby granted,
;; provided that the following conditions are met:
;;  
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;  
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;  
;;    3. Neither the name of the EPFL nor the names of its contributors
;;       may be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;  
;;  
;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'scala-mode-feature)

;; Feature loading

(defvar scala-mode-feature-list
  '(scala-mode-feature-tags
    scala-mode-feature-speedbar
    scala-mode-feature-electric
    )
  "List of features")

(defvar scala-mode-feature-installed-p nil)

(defun scala-mode-feature-install ()
  (unless scala-mode-feature-installed-p
    (dolist (feature scala-mode-feature-list)
      (when (require feature nil t)
        (apply
         (intern (concat (symbol-name feature) "-install"))
         (list))))
    (setq scala-mode-feature-installed-p t)))
