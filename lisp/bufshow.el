;;; bufshow.el -- A simple presentation tool for Emacs.
;;
;; Copyright (C) 2012 Peter Jones <pjones@pmade.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;; Commentary:

;;; Code
(defgroup bufshow nil
  "A simple presentation tool for Emacs."
  :version "0.1.0"
  :prefix "bufshow-"
  :group 'applications)

(defcustom bufshow-mode-functions
  '((org-mode . bufshow-narrow-to-org-id)
    (default  . bufshow-narrow-to-token))
  "An alist of major modes and the corresponding functions used
to narrow a buffer.  When showing a buffer as a presentation
slide the function listed in this alist for the major mode will
be invoked to narrow the buffer to the slide.  The function will
be called with narrowing token given to `bufshow-start'."
  :type 'alist
  :group 'bufshow)

;;; Internal Variables
(defvar bufshow--slide-id 0)
(defvar bufshow--slide-vector [])
(defvar bufshow--dir nil)

;;; Interactive Functions
(defun bufshow-load ()
  "FIXME:"
  (interactive))

(defun bufshow-next ()
  "Advance to the next slide."
  (interactive)
  (let ((next (1+ bufshow--slide-id))
        (size (length bufshow--slide-vector)))
    (bufshow-activate-slide
     (setq bufshow--slide-id (if (>= next size) 0 next)))))
  
(defun bufshow-prev ()
  "Return to the previous slide."
  (interactive)
  (let ((prev (1- bufshow--slide-id))
        (size (length bufshow--slide-vector)))
    (bufshow-activate-slide
     (setq bufshow--slide-id (if (< prev 0) (1- size) prev)))))

;;; External Functions
(defun bufshow-start (slides)
  "Enable the bufshow global minor mode and start a presentation.
SLIDES must be a vector of lists.  For example:

  (bufshow-start
    [(\"file1\" \"token1\")
     (\"file2\" \"token2\")])

This defines the order of slides.  File names will be relative to
the current directory.
"
  (unless (vectorp slides) (error "slides should be a vector."))
  (if (= (length slides) 0) (error "slides can't be empty."))
  (setq bufshow--slide-id 0
        bufshow--slide-vector slides
        bufshow--dir default-directory)
  (bufshow-activate-slide 0))

;;; Internal Functions
(defun bufshow-activate-slide (n)
  "Active slide number N."
  (let* ((slide (aref bufshow--slide-vector bufshow--slide-id))
         (file (concat bufshow--dir (car slide)))
         (token (cadr slide)))
    (find-file file)
    (widen)
    (goto-char (point-min))
    (if token
        (cond
         ((assoc major-mode bufshow-mode-functions)
          (funcall (cdr (assoc major-mode bufshow-mode-functions)) token))
         ((assoc 'default bufshow-mode-functions)
          (funcall (cdr (assoc 'default bufshow-mode-functions)) token))
         (t (error "no bufshow mode function for this buffer."))))))

(defun bufshow-narrow-to-org-id (token)
  "Narrow the buffer to the org subtree whose ID is TOKEN."
  (org-id-goto token)
  (org-narrow-to-subtree)
  (org-show-subtree)
  (run-hook-with-args 'org-cycle-hook 'subtree))

(defun bufshow-narrow-to-token (token)
  "Narrow the buffer using begin/end tokens."
  (let* ((start (save-excursion
                  (search-forward (concat "{BEGIN: " token "}"))
                  (forward-line)
                  (point)))
         (end (save-excursion
                (goto-char start)
                (search-forward "{END}")
                (forward-line -1)
                (point))))
    (narrow-to-region start end)))
