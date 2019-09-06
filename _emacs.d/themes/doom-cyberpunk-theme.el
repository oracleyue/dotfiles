;;; doom-cyberpunk-theme.el --- inspired by Atom City Lights
(require 'doom-themes)

;;
(defgroup doom-cyberpunk-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-cyberpunk-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-cyberpunk-theme
  :type 'boolean)

(defcustom doom-cyberpunk-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-cyberpunk-theme
  :type 'boolean)

(defcustom doom-cyberpunk-comment-bg doom-cyberpunk-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-cyberpunk-theme
  :type 'boolean)

(defcustom doom-cyberpunk-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-cyberpunk-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-cyberpunk
  "A dark theme inspired by Atom One Dark"

  ;; name        default   256       16
  ((bg         '("#01283b" nil       nil            ))
   (bg-alt     '("#011b28" nil       nil            ))
   (base0      '("#011b28" "black"   "black"        ))
   (base1      '("#01283b" "#1e1e1e" "brightblack"  ))
   (base2      '("#06618d" "#2e2e2e" "brightblack"  ))
   (base3      '("#14709c" "#262626" "brightblack"  ))
   (base4      '("#043f5c" "#3f3f3f" "brightblack"  ))
   (base5      '("#034869" "#525252" "brightblack"  ))
   (base6      '("#1592ce" "#6b6b6b" "brightblack"  ))
   (base7      '("#3898c6" "#979797" "brightblack"  ))
   (base8      '("#bbe7fd" "#dfdfdf" "white"        ))
   (fg-alt     '("#60beeb" "#bfbfbf" "brightwhite"  ))
   (fg         '("#84cbed" "#2d2d2d" "white"        ))

   (grey       base4)
   (red        '("#f5355e" "#ff6655" "red"          ))
   (orange     '("#ff5a19" "#dd8844" "brightred"    ))
   (green      '("#14d3cc" "#99bb66" "green"        ))
   (teal       '("#17c696" "#44b9b1" "brightgreen"  ))
   (yellow     '("#ffc405" "#ECBE7B" "yellow"       ))
   (blue       '("#389fee" "#51afef" "brightblue"   ))
   (dark-blue  '("#1e679f" "#2257A0" "blue"         ))
   (magenta    '("#de379c" "#c678dd" "magenta"      ))
   (violet     '("#e54097" "#a9a1e1" "brightmagenta"))
   (cyan       '("#3ea4cb" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#078abc" "#5699AF" "cyan"   ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.5))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-cyberpunk-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-cyberpunk-brighter-comments dark-cyan base5) 0.25))
   (constants      red)
   (functions      green)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        base7)
   (variables      base8)
   (numbers        magenta)
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-cyberpunk-brighter-modeline)
   (-modeline-pad
    (when doom-cyberpunk-padded-modeline
      (if (integerp doom-cyberpunk-padded-modeline) doom-cyberpunk-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
      `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-builtin-face :foreground builtin :background (doom-darken (doom-blend 'builtin 'bg 0.25) 0.55) :slant 'italic :weight 'light)
   (font-lock-variable-name-face :foreground variables :background (doom-darken (doom-blend 'variables 'bg 0.25) 0.55) :weight 'semi-bold)
   (font-lock-function-name-face :foreground functions :background (doom-darken (doom-blend 'functions 'bg 0.25) 0.55) :weight 'semi-bold)
   (font-lock-keyword-face :foreground keywords :background (doom-darken (doom-blend 'keywords 'bg 0.25) 0.55) :weight 'semi-bold)
   (font-lock-string-face :foreground strings :background (doom-darken (doom-blend 'strings 'bg 0.25) 0.55) :weight 'semi-bold)
   (font-lock-type-face :foreground type :background (doom-darken (doom-blend 'type 'bg 0.25) 0.55) :slant 'italic)
   (font-lock-constant-face :foreground constants :background (doom-darken (doom-blend 'constants 'bg 0.25) 0.55) :slant 'italic)
   (highlight-quoted-symbol :foreground orange :background (doom-darken (doom-blend 'orange 'bg 0.25) 0.55) :slant 'italic)
   (highlight-numbers-number :foreground magenta :background (doom-darken (doom-blend 'magenta 'bg 0.25) 0.55) :weight 'bold)


   (font-lock-comment-face
    :foreground comments
    :background (if doom-cyberpunk-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property :foreground green)
   (css-selector :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override) :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)

   ;; outline (affects org-mode)
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (doom-darken blue 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))

   ;; org-mode
   (org-hide :foreground hidden)
   (org-block :background base2)
   (org-block-begin-line :background base2 :foreground comments)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-cyberpunk-theme.el ends here