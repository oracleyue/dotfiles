;; ----------------------------------------
;; For /jmax/ : copy it to jmax://user/.
;; ----------------------------------------
;; Manually install the following packages:
;;      - evil
;;      - evil-leader
;; Dangerous modification of jmax original files:
;;      - ../themes/my-theme.el: LINE 8 "t" --> "nil"
;;      - ../jmax-org.el: LINE 313 comment

 ; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 ; Settings of Appearance
(custom-set-variables
 '(org-file-apps (quote ((auto-mode . emacs) ("\\.mm\\'" . default) ("\\.x?html?\\'" . "firefox %s") ("\\.pdf\\'" . "evince %s"))))
)
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                     charset
                    (font-spec :family "WenQuanYi Micro Hei" :size 12)))
;; choose color-theme
   ; (load-theme 'adwaita t)        ;grey
   (load-theme 'oracleyue t)
;; hide scroll-bar
   (scroll-bar-mode -1)
;; set size of frames
   (when window-system (set-frame-size (selected-frame) 80 27))
;; Setting the url brower for emacs
(setq browse-url-browser-function 'browse-url-firefox
         browse-url-new-window-flag  t
         browse-url-firefox-new-window-is-tab t)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; /Yasnippet/ A template system
(require 'popup)
(require 'yasnippet) ;; not yasnippet-bundle
(yas-global-mode 1)

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; For /org-mode/
(setq org-src-fontify-natively t)

(eval-after-load "org"
  '(progn
     ;; ;; .txt files aren't in the list initially, but in case that changes
     ;; ;; in a future version of org, use if to avoid errors
     ;; (if (assoc "\\.txt\\'" org-file-apps)
     ;;     (setcdr (assoc "\\.txt\\'" org-file-apps) "kate %s")
     ;;   (add-to-list 'org-file-apps '("\\.txt\\'" . "kate %s") t))

     ;; Change .pdf association directly within the alist
     (setcdr (assoc "\\.pdf\\'" org-file-apps) "evince %s")))

;; Easy-Templates for LaTeX macros
   (add-to-list 'org-structure-template-alist '("eq" "\\begin{equation}\n?\n\\end{equation}")) 
   (add-to-list 'org-structure-template-alist '("eqa" "\\begin{equation}\n \\begin{array}{}\n?\n \\end{array}\n\\end{equation}")) 
   (add-to-list 'org-structure-template-alist '("bm" "\\begin{bmatrix}\n?\n\\end{bmatrix}")) 
   (add-to-list 'org-structure-template-alist '("p" ":PROPERTIES:\n:CUSTOM_ID:?\n:END:")) 
   (add-to-list 'org-structure-template-alist '("fig" "#+CAPTION:?\n#+LABEL:\n#+ATTR_LaTeX: :width 2in :placement [H]")) 
   (add-to-list 'org-structure-template-alist '("tbl" "#+CAPTION:?\n#+LABEL:\n#+ATTR_LaTeX: placement [H] :align |c|")) 
   (add-to-list 'org-structure-template-alist '("suml" "#+BEGIN_SRC plantuml :file files/dia#.png :exports results\n?\n#+END_SRC")) 

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Enable /evil-mode/
(require 'evil-leader)
    (global-evil-leader-mode)
(require 'evil)
    (evil-mode 1)
   ;; Enable default emacs keybindings
   (define-key evil-insert-state-map "\C-e" 'org-end-of-line)
   (define-key evil-normal-state-map "\C-e" 'org-end-of-line)
   (define-key evil-visual-state-map "\C-e" 'org-end-of-line)
   (define-key evil-insert-state-map "\C-y" 'yank)
   (define-key evil-normal-state-map "\C-y" 'yank)
   (define-key evil-insert-state-map "\C-n" 'next-line)
   (define-key evil-normal-state-map "\C-n" 'next-line)
   (define-key evil-visual-state-map "\C-n" 'next-line)
   (define-key evil-insert-state-map "\C-p" 'previous-line)
   (define-key evil-normal-state-map "\C-p" 'previous-line)
   (define-key evil-visual-state-map "\C-p" 'previous-line)
   (define-key evil-normal-state-map [up] 'previous-line)
   (define-key evil-normal-state-map [down] 'next-line)
   (define-key evil-normal-state-map [left] 'left-char)
   (define-key evil-normal-state-map [right] 'right-char)
   (define-key evil-insert-state-map (kbd "C-S-d") 'evil-shift-left-line)
   (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
   (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
   ;; (define-key evil-insert-state-map "\C-i" (lambda () (interactive) (setq tab-width 4) (self-insert-command 1))) 
   (define-key evil-normal-state-map "\M-." nil)
   (define-key evil-normal-state-map "\M-?" nil)
   ;; For keybindings defined by /evil-leader/			
   (evil-leader/set-key 
     "cc" 'comment-region
     "cu" 'uncomment-region
   )

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; oracleyue's inital path setting
(cd "~/Public/Dropbox/oracleyue/OrgNote")
;(cd "~/Public/Dropbox/Academia/prjReports")
;(cd "~/Workspace/github.com/pycse")
    ;;default path for Ubuntu@LCSB 
    ;(setq default-directory "~/Workspace/Fang_prj")
