;;; ob-spice.el --- org-babel functions for spice evaluation
;;; -*- coding: utf-8 -*-

;; Author: Tiago Oliveira Weber
;; Version: 0.4
;; Package-Requires: ((spice-mode "0.0.1") (org "8"))
;; Homepage: http://tiagoweber.github.io

;;; Commentary:

;; Org-Babel support for evaluating spice script.
;; Inspired by Ian Yang's org-export-blocks-format-plantuml (http://www.emacswiki.org/emacs/org-export-blocks-format-plantuml.el)

;;; Requirements:
;;
;; - ngspice

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
      (progn  ;loop through list of lines
        (setq wordlist (split-string line " "))
        (setq firstword 1)
        (dolist (word wordlist)
          (progn  ;loop through the words
            (if (string-match "\\$\\(.*\\)\\[\\(.*\\)\\]" word)
                (progn
                  ;; if matchs a vector variable format
                  (setq varname (match-string 1 word))
                  (setq varindex (match-string 2 word))
                  ;; search varname in vars and use the value of varindex to word
                  (setq newword
                        (nth (string-to-number varindex)
                             (car (assoc-default varname vars
                                               (lambda (key candidate)
                                                 (string= key candidate))))))
                  (if (not (eq newword nil))
                      (if (not (stringp newword))
                          (setq word (number-to-string newword))
                        (setq word newword)))
                  )
              ) ; end of (if (string-match "\\$\\(.*\\)\\[\\(.*\\)\\]" word))
            (if (string-match "\\$\\(.*\\)\\." word) ; if variable has a dot in the end
                (progn
                  ;; if matchs a non-vector variable format
                  (setq varname (match-string 1 word))
                  (setq newword
                        (assoc-default varname vars
                                       (lambda (key candidate)
                                         (string= key candidate))))
                  (if (not (eq newword nil))
                      (progn
                        (if (not (stringp newword))
                            (setq newword (number-to-string newword)))
                        (setq word (replace-match (concat newword ".")  nil nil word))
                                        ;(setq word word)
                        )
                    ))
              );; end of (if (string-match "\\$\\(.*\\)\\." word)
            (if (string-match "\\$\\(.*\\)" word)
                (progn
                  ;; if matchs a non-vector variable format
                  (setq varname (match-string 1 word))
                  (setq newword
                        (assoc-default varname vars
                                       (lambda (key candidate)
                                         (string= key candidate))))
                  (if (not (eq newword nil))
                      (if (not (stringp newword))
                          (setq word (number-to-string newword))
                        (setq word newword)
                        ))
                  )
              ) ; end of (if (string-match "\\$\\(.*\\)" word)


            (setq newbody (concat newbody
                                  (if (not (eq firstword 1)) " ")
                                  word))
            (setq firstword 0)
            ) ; end of (progn
          ) ; end of (dolist (word wordlist))

        (setq newbody (concat newbody "\n"))
        ) ; end of (progn ;; loop through list of lines ... )
      ) ; end of (dolist (line bodylinelist)  ...function ...)
    ))

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
