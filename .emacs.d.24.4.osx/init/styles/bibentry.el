;;; apalike.el --- AUCTeX style file for apalike bibliography style in beamer
(TeX-add-style-hook "bibentry"
                    (function
                     (lambda ()
                       (setq reftex-cite-format
                                  '((?\r . "\\cite{%l}")
                                    (?f . "\\footcite{%l}")
                                    (?t . "\\textcite{%l}"))))))                      