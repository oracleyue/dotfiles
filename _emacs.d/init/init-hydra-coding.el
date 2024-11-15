;; ===============================================================
;; Hydra support for programming facilities (lsp, jump, rename, etc)
;; ===============================================================
;; Last modified on 12 Jan 2024

;; trigger keymap
;; (global-set-key (kbd "M-s SPC") 'hydra-coding/body)
(require 'lsp-bridge)
(require 'citre)
(define-key prog-mode-map (kbd "M-s SPC") 'hydra-coding/body)

;; hydra definition
(defhydra hydra-coding (:hint nil)
  "
Jump^^         ^Action^         ^Help^             |    ^Citre^
---------------------------------------------------------------
_._def         _;_rename        _p_eek             |    _P_eek
_,_return      _s_ymbol query   _l_ist diagnosis   |    _A_ce peek
_i_mpl         _a_ction         _h_elp doc         |    _U_pdate tags
_r_eferences   _F_ormatting     _c_all tips        |    _C_reate tags
                            _d_ash at point    |    _E_dit recipe
_SPC_ CANCEL   _R_un/Compile script
"
  ("." lsp-bridge-find-def)
  ("," lsp-bridge-find-def-return)
  ;; ("D" lsp-bridge-find-def-other-window)
  ("i" lsp-bridge-find-impl)
  ;; ("I" lsp-bridge-find-impl-other-window)
  ("r" lsp-bridge-find-references)

  (";" lsp-bridge-rename)
  ("s" lsp-bridge-workspace-list-symbols)
  ("a" lsp-bridge-code-action)
  ("F" lsp-bridge-code-format)

  ("p" lsp-bridge-peek)
  ("l" lsp-bridge-diagnostic-list)
  ("h" lsp-bridge-show-documentation)
  ("c" lsp-bridge-signature-help-fetch)
  ("d" dash-at-point)

  ("P" citre-peek)
  ("A" citre-ace-peek)
  ("U" citre-update-this-tags-file)
  ("C" citre-create-tags-file)
  ("E" citre-edit-tags-file-recipe)

  ("R" compile)
  ("SPC" nil))


(provide 'init-hydra-coding)
;; ================================================
;; init-hydra-coding.el ends here
