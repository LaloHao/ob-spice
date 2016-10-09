;;; ob-spice.el --- org-babel functions for spice evaluation

;;; Commentary:
;;; Eduardo Vazquez (lalohao@gmail.com)
;;;
;;; Original version by: Tiago Oliveira Weber
;;;
;;; Code:
(require 'ob)

(add-to-list 'org-babel-tangle-lang-exts '("spice" . "cir"))

(defun org-babel-expand-body:spice (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((vars (mapcar #'cdr (org-babel-get-header params :var)))
      (newbody "")
      (bodylinelist (split-string body "\n"))
      wordlist
      varname
      varindex
      newword
      firstword)
    (dolist (line bodylinelist newbody)
      (progn
        (setq wordlist (split-string line " "))
        (setq firstword 1)
        (dolist (word wordlist)
          (progn
            (if (string-match "\\$\\(.*\\)\\[\\(.*\\)\\]" word)
                (progn
                  (setq varname (match-string 1 word))
                  (setq varindex (match-string 2 word))
                  (setq newword
                        (nth (string-to-number varindex)
                             (car (assoc-default varname vars
                                               (lambda (key candidate)
                                                 (string= key candidate))))))
                  (if (not (eq newword nil))
                      (if (not (stringp newword))
                          (setq word (number-to-string newword))
                        (setq word newword)))))
            (if (string-match "\\$\\(.*\\)\\." word)
                (progn
                  (setq varname (match-string 1 word))
                  (setq newword
                        (assoc-default varname vars
                                       (lambda (key candidate)
                                         (string= key candidate))))
                  (if (not (eq newword nil))
                      (progn
                        (if (not (stringp newword))
                            (setq newword (number-to-string newword)))
                        (setq word (replace-match (concat newword ".")  nil nil word))))))
            (if (string-match "\\$\\(.*\\)" word)
                (progn
                  (setq varname (match-string 1 word))
                  (setq newword
                        (assoc-default varname vars
                                       (lambda (key candidate)
                                         (string= key candidate))))
                  (if (not (eq newword nil))
                      (if (not (stringp newword))
                          (setq word (number-to-string newword))
                        (setq word newword)))))
            (setq newbody (concat newbody
                                  (if (not (eq firstword 1)) " ")
                                  word))
            (setq firstword 0)))
        (setq newbody (concat newbody "\n"))))))

;;;###autoload
(defun org-babel-execute:spice (body params)
  "Execute BODY with specified PARAMS."
  (let ((body (org-babel-expand-body:spice body params))
      (vars (mapcar #'cdr (org-babel-get-header params :var)))
      textfile
      imagefile
      raw
      result)
    (org-babel-eval "ngspice " body)
    (mapc (lambda (pair)
         (when (string= (car pair) "file")
           (setq textfile (concat (cdr pair) ""))
           (setq imagefile (concat (cdr pair) ".png"))))
       vars)
    (when (file-readable-p textfile)
      (setq raw (with-temp-buffer (insert-file-contents textfile) (buffer-string)))
      (setq raw (replace-regexp-in-string "\n" "" raw))
      (setq result (split-string raw ",")))
    (when (file-readable-p imagefile)
      (add-to-list 'result (concat '"[[file:./" imagefile "]]") t))
    result))

(provide 'ob-spice)
;;; ob-spice.el ends here
