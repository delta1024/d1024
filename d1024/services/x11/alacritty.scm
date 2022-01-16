(define-module (d1024 services x11 alacritty)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)

  #:export (test-alacritty-service))

(define-maybe list)
(define-maybe string)
(define-maybe symbol)
(define-maybe bool)
(define-maybe pair)
(define-maybe integer)

(define bool? (lambda (value)
		(match value
		  (#t #t)
		  (#f #t)
		  (_ #f))))

(define (is-disabled? symbol)
  (match symbol
    ('disabled #t)
    (_ #f)))

(define (prettify-symbol symbol)
  (symbol->string symbol))

(define (serialize-bool field-name value)
  (let ((return-value (match value
			(#f "false")
			(#t "true"))))
    #~(string-append #$(symbol->string field-name) ": " #$return-value "\n")))

;; Env
(define (serialize-env-elm value)
  (define (serialize-pair pair)
    (string-append "  " (car pair) ": " (cdr pair) "\n"))
  #~(string-append  #$(serialize-pair value)))

(define (serialize-env  field-name value)
  (if (is-disabled? value)
      #~(string-append #$(empty-serializer field-name value))
      #~(string-append  "env:\n" #$@(map serialize-env-elm
					 value))))

;; Windows
(define (window-serialize-maybe-pair field-name value)
  (define (serialize-xy-pair field-name value)
    (string-append "  " (prettify-symbol field-name) ":\n"
		       (string-pad "" 4) "x: " (number->string (car value)) "\n"
		       (string-pad "" 4) "y: " (number->string (cdr value)) "\n"))
  (if (is-disabled? value)
      #~(string-append #$(empty-serializer field-name value))
      (let ((pair (serialize-xy-pair field-name value)))
      #~(string-append #$pair))))

(define (window-dimensions-serialize-pair field-name value)
  (if (is-disabled? value)
      #~(string-append #$(empty-serializer field-name value))
      #~(string-append "  " #$(prettify-symbol field-name) ":\n"
		       #$(string-pad "" 4) "columns: "
		       #$(number->string (car value)) "\n"
		       #$(string-pad "" 4) "lines: "
		       #$(number->string (cdr value)) "\n")))

(define (window-class-serialize-pair field-name value)
  (if (is-disabled? value)
      #~(string-append #$(empty-serializer field-name value))
      #~(string-append "  " #$(prettify-symbol field-name) ":\n"
		       #$(string-pad "" 4) "instance: "
		       #$(car value) "\n"
		       #$(string-pad "" 4) "general: "
		       #$(cdr value) "\n")))

(define (window-serialize-maybe-bool field-name value)
  (if (is-disabled? value)
      #~(string-append #$(empty-serializer field-name value))
      #~(string-append "  " #$(serialize-bool field-name value))))

(define (window-serialize-maybe-string field-name value)
  (if (is-disabled? value)
      #~(string-append #$(empty-serializer field-name value))
      #~(string-append "  " #$(prettify-symbol field-name) ": "
		       #$value "\n")))

(define (window-scrolling-serialize-pair field-name value)
  #~(string-append "  " #$(prettify-symbol field-name) ":\n"
		   #$(string-pad "" 4) "history: "
		   #$(number->string (car value)) "\n"
		   #$(string-pad "" 4) "multiplyer: "
		   #$(number->string (cdr value)) "\n"))

(define (serialize-list-of-alacritty-windows field-name configuration)
  (if (is-disabled? configuration)
      #~(string-append #$(empty-serializer field-name configuration))
      #~(string-append "window:\n" #$@(map (cut serialize-configuration
						<> alacritty-window-fields)
					   configuration))))

(define-configuration alacritty-window
  (dimensions
   (maybe-pair 'disabled)
   "Window dimensions (changes require restart)
Specified in number of columns/lines, not pixels.
If both are `0`, this setting is ignored.
@code{
dimensions:
     columns: 0
     lines: 0}"
   window-dimensions-serialize-pair)
  (position
   (maybe-pair 'disabled)
  " Window position (changes require restart)
  
Specified in number of pixels.
If the position is not set, the window manager will handle the placement.
@code{
position:
  x: 0
  y: 0}")
  (padding
   (maybe-pair 'disabled)
   " Window padding (changes require restart)

 Blank space added around the window in pixels. This padding is scaled
 by DPI and the specified value is always added at both opposing sides.
@code{padding:
  x: 0
  y: 0}")
  (dynamic_padding
   (maybe-bool 'disabled)
   "Spread additional padding evenly around the terminal content.
@code{dynamic_padding: false}")

  (decorations
   (maybe-string 'disabled)
   " Window decorations

  Values for `decorations`:
      - full: Borders and title bar
      - none: Neither borders nor title bar

  Values for `decorations` (macOS only):
      - transparent: Title bar, transparent background and title bar buttons
      - buttonless: Title bar, transparent background, but no title bar buttons
@code{ decorations: full}")
  (startup_mode
   (maybe-string 'disabled)
" Startup Mode (changes require restart)

 Values for `startup_mode`:
   - Windowed
   - Maximized
   - Fullscreen

 Values for `startup_mode` (macOS only):
   - SimpleFullscreen
code@{startup_mode: Windowed}")

  (title
   (maybe-string 'disabled)
  " Window title
  title: Alacritty")

  (class
    (maybe-pair 'disabled)
    " # Window class (Linux/BSD only):
 @code{class:}
  Application instance name
@code{  instance: Alacritty}
  General application class
@code{  general: Alacritty}"
    window-class-serialize-pair)

  (gtk_theme_variant
   (maybe-string 'disabled)
" GTK theme variant (Linux/BSD only)
 Override the variant of the GTK theme. Commonly supported values are `dark` and `light`.
 Set this to `None` to use the default theme variant.
  #@code{gtk_theme_variant: None}")

    (scrolling
     (maybe-pair 'disabled)
     "scrolling:
   Maximum number of lines in the scrollback buffer.
   Specifying '0' will disable scrolling.
  history: 10000

   Scrolling distance multiplier.
  multiplier: 3"
     window-scrolling-serialize-pair)
  (prefix window-))

(define-maybe list-of-font-configurations)
(define list-of-font-configurations? list?)

(define (serialize-list-of-font-configurations field-name configuration)
  (if (is-disabled? configuration)
      #~(string-append #$(empty-serializer field-name configuration))
      #~(string-append "font:\n" #$@(map (cut serialize-configuration <>
					      font-configuration-fields)
					 configuration))))

(define (font-serialize-maybe-pair field-name value)
  (if (is-disabled? value)
      #~(string-append #$(empty-serializer field-name value))
      #~(string-append "  " #$(prettify-symbol field-name) ":\n"
		       #$(string-pad "" 4) "family: " #$(car value) "\n"
		       #$(string-pad "" 4) "syle: " #$(cdr value) "\n")))

(define (font-serialize-xy-pair field-name value)
  (define (serialize-pair value)
    (string-append "  " (prettify-symbol field-name) ":\n"
		   (string-pad "" 4) "x: " (number->string (car value)) "\n"
		   (string-pad "" 4) "y: " (number->string (cdr value)) "\n"))
  (if (is-disabled? value)
      #~(string-append #$(empty-serializer field-name value))
      (let ((value (serialize-pair  value)))
	#~(string-append #$value))))


(define (font-serialize-maybe-integer field-name value)
  (if (is-disabled? value)
      #~(string-append #$(empty-serializer field-name value))
      #~(string-append "  " #$(symbol->string field-name) ": "
		       #$(number->string value) "\n")))

(define (font-serialize-maybe-bool field-name value)
  (if (is-disabled? value)
      #~(string-append #$(empty-serializer field-name value))
      #~(string-append "  " #$(serialize-bool field-name value)))) ;;": "

(define-configuration font-configuration
;; font:
;;   # Normal (roman) font face
  (normal
   (maybe-pair 'disabled)
  " normal:
      Font family
     
      Default:
        - (macOS) Menlo
        - (Linux/BSD) monospace
        - (Windows) Consolas
     family: Fira Code

     The `style` can be specified to pick a specific face.
     style: Regular")
  (bold
   (maybe-pair 'disabled)
" Bold font face
bold:
 # Font family
 #
 # If the bold family is not specified, it will fall back to the
 # value specified for the normal font.
 #family: monospace

 # The `style` can be specified to pick a specific face.
 #style: Bold")
  (italic
   (maybe-pair 'disabled)
" Italic font face
italic:
 # Font family
 #
 # If the italic family is not specified, it will fall back to the
 # value specified for the normal font.
 #family: monospace

 # The `style` can be specified to pick a specific face.
 #style: Italic")
  (bold_italic
   (maybe-pair 'disabled)
" Bold italic font face
bold_italic:
 # Font family
 #
 # If the bold italic family is not specified, it will fall back to the
 # value specified for the normal font.
 #family: monospace

 # The `style` can be specified to pick a specific face.
 #style: Bold Italic")

  (size
   (maybe-integer 'disabled)
   "# Point size
      size: 12.0")

  (offset
   (maybe-pair 'disabled)
"# Offset is the extra space around each character. `offset.y` can be thought of
# as modifying the line spacing, and `offset.x` as modifying the letter spacing.
#offset:
#  x: 0
#  y: 0"
   font-serialize-xy-pair)

  (glyph_offset
   (maybe-pair 'disabled)
"# Glyph offset determines the locations of the glyphs within their cells with
# the default being at the bottom. Increasing `x` moves the glyph to the right,
# increasing `y` moves the glyph upward.
#glyph_offset:
#  x: 0
#  y: 0"
   font-serialize-xy-pair)

  (use_thin_strokes
   (maybe-bool 'disabled)
   " Thin stroke font rendering (macOS only)

 Thin strokes are suitable for retina displays, but for non-retina screens
 it is recommended to set `use_thin_strokes` to `false`.
use_thin_strokes: true")

  (draw_bold_text_with_bright_colors
   (maybe-bool 'disabled)
"If `true`, bold text is drawn using the bright color variants.
draw_bold_text_with_bright_colors: false")

  (prefix font-))

(define list-of-color-groups? list?)

(define (colors-serialize-list-of-color-groups field-name configuration)
  (if (null? configuration)
      #~(string-append #$(empty-serializer field-name configuration))
      #~(string-append "  " #$(prettify-symbol field-name) ": \n"
                       #$@(map (cut serialize-configuration <>
                                    color-groups-fields)
                               configuration))))
(define (color-groups-serialize-maybe-string field-name value)
  (if (is-disabled? value)
      #~(string-append #$(empty-serializer field-name value))
      #~(string-append (string-pad "" 4) #$(prettify-symbol field-name) ": '" #$value "'\n"))) 

(define (serialize-list-of-color-configurations field-name configuration)
  (if (is-disabled? configuration)
      #~(string-append #$(empty-serializer field-name configuration))
      #~(string-append "colors:\n" #$@(map (cut serialize-configuration <>
                                                color-configuration-fields)
                                           configuration))))
(define (colors-serialize-primary-list field-name value)
  ;;TODO add guard to pre
  (if (is-disabled? value)
      #~(string-append #$(empty-serializer field-name value))
      (let ((background (list-ref value 0))
            (foreground (list-ref value 1))
            (dim-foreground (list-ref value 2))
            (bright-foreground (list-ref value 3)))
        #~(string-append  "  primary:\n"
                          #$(if (string-null? background)
                                ""
                                (string-append (string-pad "" 4)
                                               "background: '" background "'\n"))
                          #$(if (string-null? foreground)
                                ""
                                (string-append (string-pad "" 4)
                                               "foreground: '" foreground "'\n"))
                          #$(if (string-null? dim-foreground)
                                ""
                                (string-append (string-pad "" 4)
                                               "dim_foreground: '" dim-foreground "'\n"))
                          #$(if (string-null? bright-foreground)
                                ""
                                (string-append (string-pad "" 4)
                                               "bright_foreground: '" bright-foreground "'\n"))))))

(define (colors-serialize-cursor-pair field-name value)
  (if (is-disabled? value)
      #~(string-append #$(empty-serializer field-name value))
      #~(string-append "  " #$(symbol->string field-name) ":\n"
                       #$(string-pad "" 4) "text: " #$(car value) "\n"
                       #$(string-pad "" 4) "cursor: " #$(cdr value) "\n")))

(define (colors-serialize-selection-pair field-name value)
  (if (is-disabled? value)
      #~(string-append #$(empty-serializer (symbol->string field-name) value))
      #~(string-append "  " #$(symbol->string field-name) ":\n"
                       #$(string-pad "" 4) "text: "
                       #$(car value) "\n"
                       #$(string-pad "" 4) "background: "
                       #$(cdr value) "\n")))

(define (colors-serialize-search-list field-name value)
  (if (is-disabled? value)
      #~(string-append #$(empty-serializer field-name value))
      (let ((foreground (list-ref value 0))
            (background (list-ref value 1))
            (bar-foreground (list-ref value 2))
            (bar-background (list-ref value 3)))
        #~(string-append "  " #$(symbol->string field-name) ":\n"
                         #$(if (and (string-null? foreground) (string-null? background))
                               ""
                               (string-append (string-pad "" 4) "matches:\n"))
                         #$(if (string-null? foreground)
                               ""
                               (string-append (string-pad "" 6)
                                              "foreground: '" foreground "'\n"))
                         #$(if (string-null? foreground)
                               ""
                               (string-append (string-pad "" 6)
                                              "background: '" background "'\n"))
                         #$(if (and (string-null? bar-foreground) (string-null? bar-background))
                               ""
                               (string-append (string-pad "" 4) "bar:\n"))
                         #$(if (string-null? bar-foreground)
                               ""
                               (string-append (string-pad "" 6) "foreground: '" bar-foreground "'\n"))
                         #$(if (string-null? bar-background)
                               ""
                               (string-append (string-pad "" 6) "background: '" bar-background "'\n"))))))


(define (colors-serialize-indexed-lists field-name value)
  (define (serialize-pair pair)
    (string-append (string-pad "" 4) "- { index: " (car pair) ", color: '" (cdr pair) "' }\n"))
  (if (is-disabled? value)
      #~(string-append #$(empty-serializer field-name value))
      #~(string-append "  " #$(symbol->string field-name) ":\n"
                       #$@(map serialize-pair value))))

(define-configuration color-groups
  (black
   (maybe-string 'disabled)
   "the black value for the color group")
  (red
   (maybe-string 'disabled)
   "the red value for the color group")
  (green
   (maybe-string 'disabled)
   "the green value for the color group")
  (yellow
   (maybe-string 'disabled)
   "the yellow value for the color group")
  (blue
   (maybe-string 'disabled)
   "the blue value for the color group")
  (magenta
   (maybe-string 'disabled)
   "the magenta value for the color group")
  (cyan
   (maybe-string 'disabled)
   "the cyan value for the color group")
  (white
   (maybe-string 'disabled)
   "the white value for the color group")
  (prefix color-groups-))

(define-configuration color-configuration
  ;; # Colors (Tomorrow Night)
  ;; #colors:
  (primary-colors
   (maybe-list 'disabled)
   "# Default colors
    #primary:
    #  background: '#1d1f21'
    #  foreground: '#c5c8c6'

      # Bright and dim foreground colors
      #
      # The dimmed foreground color is calculated automatically if it is not present.
      # If the bright foreground color is not set, or `draw_bold_text_with_bright_colors`
      # is `false`, the normal foreground color will be used.
      #dim_foreground: '#828482'
      #bright_foreground: '#eaeaea'"
   colors-serialize-primary-list)

  (cursor
   (maybe-pair 'disabled)
   " Cursor colors

   Colors which should be used to draw the terminal cursor.

   Allowed values are CellForeground and CellBackground, which reference the
   affected cell, or hexadecimal colors like #ff00ff.
  cursor:
    text: CellBackground
    cursor: CellForeground"
   colors-serialize-cursor-pair)

  (vi_mode_cursor
   (maybe-pair 'disabled)
   "Vi mode cursor colors

   Colors for the cursor when the vi mode is active.

   Allowed values are CellForeground and CellBackground, which reference the
   affected cell, or hexadecimal colors like #ff00ff.
  vi_mode_cursor:
    text: CellBackground
    cursor: CellForeground"
   colors-serialize-cursor-pair)
  (selection
   (maybe-pair 'disabled)
   "Selection colors

   Colors which should be used to draw the selection area.

   Allowed values are CellForeground and CellBackground, which reference the
   affected cell, or hexadecimal colors like #ff00ff.
  selection:
    text: CellBackground
    background: CellForeground"
   colors-serialize-selection-pair)
  (search
   (maybe-list 'disabled)
   "Search colors

   Colors used for the search bar and match highlighting.

   Allowed values are CellForeground and CellBackground, which reference the
   affected cell, or hexadecimal colors like #ff00ff.
  search:
    matches:
      foreground: '#000000'
      background: '#ffffff'

    bar:
      background: CellForeground
      foreground: CellBackground"
   colors-serialize-search-list)
  (normal
   (list-of-color-groups '())
   " Normal colors
  normal:
    black:   '#1d1f21'
    red:     '#cc6666'
    green:   '#b5bd68'
    yellow:  '#f0c674'
    blue:    '#81a2be'
    magenta: '#b294bb'
    cyan:    '#8abeb7'
    white:   '#c5c8c6'")
  (bright
   (list-of-color-groups '())
   " Bright colors
  bright:
    black:   '#666666'
    red:     '#d54e53'
    green:   '#b9ca4a'
    yellow:  '#e7c547'
    blue:    '#7aa6da'
    magenta: '#c397d8'
    cyan:    '#70c0b1'
    white:   '#eaeaea'")
  (dim
   (list-of-color-groups '())
   " Dim colors

   If the dim colors are not set, they will be calculated automatically based
   on the `normal` colors.
  dim:
    black:   '#131415'
    red:     '#864343'
    green:   '#777c44'
    yellow:  '#9e824c'
    blue:    '#556a7d'
    magenta: '#75617b'
    cyan:    '#5b7d78'
    white:   '#828482'")
  (indexed_colors
   (maybe-list 'disabled)
   "Indexed Colors

 The indexed colors include all colors from 16 to 256.
 When these are not set, they're filled with sensible defaults.

 Example:
   `- { index: 16, color: '#ff00ff' }`

indexed_colors: []"
   colors-serialize-indexed-lists)
  (prefix colors-))

(define (serialize-alacritty-configuration configuration)
  (mixed-text-file
   "alacritty.yml"
   #~(string-append "# This file is generated by (d1024 services x11 alacritty)\n# please see commentary there\n"
                    #$(serialize-configuration
                       configuration alacritty-configuration-fields))))

(define-maybe list-of-alacritty-windows)
(define list-of-alacritty-windows? list?)
(define-maybe list-of-color-configurations)
(define list-of-color-configurations? list?)

(define-configuration alacritty-configuration
  ;; # Any items in the `env` entry below will be added as
  ;; # environment variables. Some entries may override variables
  ;; # set by alacritty itself.
  ;; #env:
  (env
   (maybe-list 'disabled)
   "TERM variab
 This value is used to set the `$TERM` environment variable for
 each instance of Alacritty. If it is not present, alacritty will

 available, otherwise `xterm-256color` is used.
@code{env:
  TERM: alacritty}"
   serialize-env)
  (window
   (maybe-list-of-alacritty-windows 'disabled)
   "Alacritty window config (requires restart on change)")
  (font-config
   (maybe-list-of-font-configurations 'disabled)
   "Font configuration for Alacritty")
  (color-config
   (maybe-list-of-color-configurations 'disabled)
   "color configuration field"))

(define alacritty-config
  (alacritty-configuration
   (env
    '(("USER" . "Bob")
      ("GUEST" . "Alice")))
   (window
    (list
     (alacritty-window
      (dimensions '(0 . 0))
      (position '(0 . 0))
      (padding '(0 . 0))
      (dynamic_padding #f)
      (decorations "full")
      (startup_mode "Windowed")
      (title "Alacritty")
      (class '("Alacritty" . "Alacritty"))
      (gtk_theme_variant "None")
      (scrolling '(1000 . 0)))))
   (font-config				;; keep
    (list				;; keep
     (font-configuration 		;; keep
      (normal '("Fira Code" . "Regular")) ;; keep
      (size 12.0) 
      (glyph_offset '(0 . 0))
      (use_thin_strokes #t)
      (draw_bold_text_with_bright_colors #f))))
   (color-config
    (list
     (color-configuration
      (primary-colors '("" "#c5c8c6"
                        "#828482" "#eaeaea"))
      (cursor '("CellBackground" . "CellForeground"))
      (vi_mode_cursor '("CellBackground" . "CellForeground"))
      (selection '("CellBackground" . "CellForeground"))
      (search '("#000000" "#ffffff"
                "CellForeground" "CellBackground"))
      (normal
       (list
        (color-groups
         (black "#1d1f21")
         (red "#cc6666")
         (green "#b5bd68")
         (yellow "#f0c674")
         (blue "#81a2be")
         (magenta "#b294bb")
         (cyan "#8abeb7")
         (white "#c5c8c6"))))
      (indexed_colors (list '("16" . "#ff00ff")
                            '("16" . "#ffffff" ))))))
   ));;)))


(define test-alacritty-service
  (list
   (simple-service 'alaritty-service
                   home-files-service-type
                   (list
                    `("config/test.yml"
                      ,(serialize-alacritty-configuration alacritty-config))))))
