;; =======================================
;; C/C++ Development Environment Setup
;; ------

(require 'cc-mode)
(add-to-list 'auto-mode-alist '("/usr/local/include/eigen3/Eigen" . c++-mode) t)

;;; Source code completion by /CEDET/

;; /Semantic/ minior mode
;; setup semantic
(require 'semantic)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(semantic-mode 1)
;; add system include paths
(semantic-add-system-include "/usr/local/include/boost" 'c++-mode)
;(semantic-add-system-include "/usr/local/include/eigen3" 'c++-mode)
(semantic-add-system-include "/usr/local/include/c++" 'c++-mode)
(semantic-add-system-include "/usr/local/include/c")
(semantic-add-system-include "/usr/local/include")
;; completion toolkits
;; use /auto-complete/
(defun y:add-to-ac-sources ()
  (add-to-list 'ac-sources 'ac-source-semantic)
  ;(add-to-list 'ac-sources 'ac-source-semantic-raw))
(add-hook 'c-mode-common-hook 'y:add-to-ac-sources)
;(add-hook 'c++-mode-hook 'y:add-to-ac-sources)
;; use /function-args/
(require 'function-args)
(fa-config-default)
;(define-key c-mode-map   [(control tab)] 'moo-complete)   ;default "M-i"
;(define-key c++-mode-map [(control tab)] 'moo-complete)
;(define-key c-mode-map   (kbd "M-O") 'fa-show)            ;default "M-o"
;(define-key c++-mode-map (kbd "M-O") 'fa-show)

;; /EDE/ project management
(require 'ede)
(global-ede-mode)
(ede-cpp-root-project "project_root"
                      :file "/Users/oracleyue/tmp/eigen/src/third.cc"
                      :include-path '("../include"
                                      "/usr/local/include/eigen3"
                                      "/usr/local/include/eigen3/Eigen"
                                      )
                      :system-include-path '("/usr/local/include")
                      )
