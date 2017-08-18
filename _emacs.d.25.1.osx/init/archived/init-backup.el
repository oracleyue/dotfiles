(eval-after-load "tex"
   '(add-to-list 'TeX-command-list
                 '("convert to png" "convert -density 300 %s.pdf -quality 90 %s.png" TeX-run-command nil t) t))

(eval-after-load "tex"
   '(add-to-list 'TeX-command-list
                 '("update bib library" "/Users/oracleyue/Public/Dropbox/Academia/Manuscripts/archive/bibupdate.sh" TeX-run-command nil t) t))

(eval-after-load "tex"
   '(add-to-list 'TeX-command-list
                 '("backup doc files" "/Users/oracleyue/Public/Dropbox/Academia/Manuscripts/archive/srcbackup.sh" TeX-run-command nil t) t))

(eval-after-load "tex"
   '(add-to-list 'TeX-command-list
                 '("backup tex files" "/Users/oracleyue/Public/Dropbox/Academia/Manuscripts/archive/texbackup.sh" TeX-run-command nil t) t))

(eval-after-load "tex"
   '(add-to-list 'TeX-command-list
                 '("update userdef-mathsymb" "/Users/oracleyue/Public/Dropbox/Academia/Manuscripts/archive/mathsym_update.sh" TeX-run-command nil t) t))

;; (eval-after-load "tex"
;;    '(add-to-list 'TeX-command-list
;;                  '("Git regular push" "git add --all && git ci -m \"regular update\" && git push" TeX-run-command nil t) t))
