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

;; TODO:
;;      * finish refactor of serialize-xy-pair
;; Configuration for Alacritty, the GPU enhanced terminal emulator.
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


(define-maybe list)

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
(define bool? (lambda (value)
		(match value
		  (#t #t)
		  (#f #t)
		  (_ #f))))

(define-maybe string)
(define-maybe symbol)
(define-maybe bool)
(define-maybe pair)

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

;; # Font configuration
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

(define-maybe integer)

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
					      

;; (define (colors-serialize-indexed-colors-lists))

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
  (prefix colors-))

;;   # Indexed Colors
;;   #
;;   # The indexed colors include all colors from 16 to 256.
;;   # When these are not set, they're filled with sensible defaults.
;;   #
;;   # Example:
;;   #   `- { index: 16, color: '#ff00ff' }`
;;   #
;;   #indexed_colors: []

;; # Bell
;; #
;; # The bell is rung every time the BEL control character is received.
;; #bell:
;;   # Visual Bell Animation
;;   #
;;   # Animation effect for flashing the screen when the visual bell is rung.
;;   #
;;   # Values for `animation`:
;;   #   - Ease
;;   #   - EaseOut
;;   #   - EaseOutSine
;;   #   - EaseOutQuad
;;   #   - EaseOutCubic
;;   #   - EaseOutQuart
;;   #   - EaseOutQuint
;;   #   - EaseOutExpo
;;   #   - EaseOutCirc
;;   #   - Linear
;;   #animation: EaseOutExpo

;;   # Duration of the visual bell flash. A `duration` of `0` will disable the
;;   # visual bell animation.
;;   #duration: 0

;;   # Visual bell animation color.
;;   #color: '#ffffff'

;;   # Bell Command
;;   #
;;   # This program is executed whenever the bell is rung.
;;   #
;;   # When set to `command: None`, no command will be executed.
;;   #
;;   # Example:
;;   #   command:
;;   #     program: notify-send
;;   #     args: ["Hello, World!"]
;;   #
;;   #command: None

;; # Background opacity
;; #
;; # Window opacity as a floating point number from `0.0` to `1.0`.
;; # The value `0.0` is completely transparent and `1.0` is opaque.
;; background_opacity: 0.8


;; #selection:
;;   #semantic_escape_chars: ",│`|:\"' ()[]{}<>\t"

;;   # When set to `true`, selected text will be copied to the primary clipboard.
;;   #save_to_clipboard: false

;; # Allow terminal applications to change Alacritty's window title.
;; #dynamic_title: true

;; cursor:
;;   # Cursor style
;;   #
;;   # Values for `style`:
;;   #   - ▇ Block
;;   #   - _ Underline
;;   #   - | Beam
;;   style: Block

;;   # Vi mode cursor style
;;   #
;;   # If the vi mode cursor style is `None` or not specified, it will fall back to
;;   # the style of the active value of the normal cursor.
;;   #
;;   # See `cursor.style` for available options.
;;   vi_mode_style: Beam 

;;   # If this is `true`, the cursor will be rendered as a hollow box when the
;;   # window is not focused.
;;   #unfocused_hollow: true

;;   # Thickness of the cursor relative to the cell width as floating point number
;;   # from `0.0` to `1.0`.
;;   #thickness: 0.15

;; # Live config reload (changes require restart)
;; #live_config_reload: true

;; # Shell
;; #
;; # You can set `shell.program` to the path of your favorite shell, e.g. `/bin/fish`.
;; # Entries in `shell.args` are passed unmodified as arguments to the shell.
;; #
;; # Default:
;; #   - (macOS) /bin/bash --login
;; #   - (Linux/BSD) user login shell
;; #   - (Windows) powershell
;; #shell:
;; #  program: /bin/bash
;; #  args:
;; #    - --login

;; # Startup directory
;; #
;; # Directory the shell is started in. If this is unset, or `None`, the working
;; # directory of the parent process will be used.
;; #working_directory: None

;; # WinPTY backend (Windows only)
;; #
;; # Alacritty defaults to using the newer ConPTY backend if it is available,
;; # since it resolves a lot of bugs and is quite a bit faster. If it is not
;; # available, the WinPTY backend will be used instead.
;; #
;; # Setting this option to `true` makes Alacritty use the legacy WinPTY backend,
;; # even if the ConPTY backend is available.
;; #winpty_backend: false

;; # Send ESC (\x1b) before characters when alt is pressed.
;; #alt_send_esc: true

;; #mouse:
;;   # Click settings
;;   #
;;   # The `double_click` and `triple_click` settings control the time
;;   # alacritty should wait for accepting multiple clicks as one double
;;   # or triple click.
;;   #double_click: { threshold: 300 }
;;   #triple_click: { threshold: 300 }

;;   # If this is `true`, the cursor is temporarily hidden when typing.
;;   #hide_when_typing: false

;;   #url:
;;     # URL launcher
;;     #
;;     # This program is executed when clicking on a text which is recognized as a URL.
;;     # The URL is always added to the command as the last parameter.
;;     #
;;     # When set to `launcher: None`, URL launching will be disabled completely.
;;     #
;;     # Default:
;;     #   - (macOS) open
;;     #   - (Linux/BSD) xdg-open
;;     #   - (Windows) explorer
;;     #launcher:
;;     #  program: xdg-open
;;     #  args: []

;;     # URL modifiers
;;     #
;;     # These are the modifiers that need to be held down for opening URLs when clicking
;;     # on them. The available modifiers are documented in the key binding section.
;;     #modifiers: None

;; # Mouse bindings
;; #
;; # Mouse bindings are specified as a list of objects, much like the key
;; # bindings further below.
;; #
;; # To trigger mouse bindings when an application running within Alacritty captures the mouse, the
;; # `Shift` modifier is automatically added as a requirement.
;; #
;; # Each mouse binding will specify a:
;; #
;; # - `mouse`:
;; #
;; #   - Middle
;; #   - Left
;; #   - Right
;; #   - Numeric identifier such as `5`
;; #
;; # - `action` (see key bindings)
;; #
;; # And optionally:
;; #
;; # - `mods` (see key bindings)
;; #mouse_bindings:
;; #  - { mouse: Middle, action: PasteSelection }

;; # Key bindings
;; #
;; # Key bindings are specified as a list of objects. For example, this is the
;; # default paste binding:
;; #
;; # `- { key: V, mods: Control|Shift, action: Paste }`
;; #
;; # Each key binding will specify a:
;; #
;; # - `key`: Identifier of the key pressed
;; #
;; #    - A-Z
;; #    - F1-F24
;; #    - Key0-Key9
;; #
;; #    A full list with available key codes can be found here:
;; #    https://docs.rs/glutin/*/glutin/event/enum.VirtualKeyCode.html#variants
;; #
;; #    Instead of using the name of the keys, the `key` field also supports using
;; #    the scancode of the desired key. Scancodes have to be specified as a
;; #    decimal number. This command will allow you to display the hex scancodes
;; #    for certain keys:
;; #
;; #       `showkey --scancodes`.
;; #
;; # Then exactly one of:
;; #
;; # - `chars`: Send a byte sequence to the running application
;; #
;; #    The `chars` field writes the specified string to the terminal. This makes
;; #    it possible to pass escape sequences. To find escape codes for bindings
;; #    like `PageUp` (`"\x1b[5~"`), you can run the command `showkey -a` outside
;; #    of tmux. Note that applications use terminfo to map escape sequences back
;; #    to keys. It is therefore required to update the terminfo when changing an
;; #    escape sequence.
;; #
;; # - `action`: Execute a predefined action
;; #
;; #   - ToggleViMode
;; #   - SearchForward
;; #   - SearchBackward
;; #   - Copy
;; #   - Paste
;; #   - PasteSelection
;; #   - IncreaseFontSize
;; #   - DecreaseFontSize
;; #   - ResetFontSize
;; #   - ScrollPageUp
;; #   - ScrollPageDown
;; #   - ScrollHalfPageUp
;; #   - ScrollHalfPageDown
;; #   - ScrollLineUp
;; #   - ScrollLineDown
;; #   - ScrollToTop
;; #   - ScrollToBottom
;; #   - ClearHistory
;; #   - Hide
;; #   - Minimize
;; #   - Quit
;; #   - ToggleFullscreen
;; #   - SpawnNewInstance
;; #   - ClearLogNotice
;; #   - ClearSelection
;; #   - ReceiveChar
;; #   - None
;; #
;; #   (`mode: Vi` only):
;; #   - Open
;; #   - Up
;; #   - Down
;; #   - Left
;; #   - Right
;; #   - First
;; #   - Last
;; #   - FirstOccupied
;; #   - High
;; #   - Middle
;; #   - Low
;; #   - SemanticLeft
;; #   - SemanticRight
;; #   - SemanticLeftEnd
;; #   - SemanticRightEnd
;; #   - WordRight
;; #   - WordLeft
;; #   - WordRightEnd
;; #   - WordLeftEnd
;; #   - Bracket
;; #   - ToggleNormalSelection
;; #   - ToggleLineSelection
;; #   - ToggleBlockSelection
;; #   - ToggleSemanticSelection
;; #   - SearchNext
;; #   - SearchPrevious
;; #   - SearchStart
;; #   - SearchEnd
;; #
;; #   (macOS only):
;; #   - ToggleSimpleFullscreen: Enters fullscreen without occupying another space
;; #
;; #   (Linux/BSD only):
;; #   - CopySelection: Copies into selection buffer
;; #
;; # - `command`: Fork and execute a specified command plus arguments
;; #
;; #    The `command` field must be a map containing a `program` string and an
;; #    `args` array of command line parameter strings. For example:
;; #       `{ program: "alacritty", args: ["-e", "vttest"] }`
;; #
;; # And optionally:
;; #
;; # - `mods`: Key modifiers to filter binding actions
;; #
;; #    - Command
;; #    - Control
;; #    - Option
;; #    - Super
;; #    - Shift
;; #    - Alt
;; #
;; #    Multiple `mods` can be combined using `|` like this:
;; #       `mods: Control|Shift`.
;; #    Whitespace and capitalization are relevant and must match the example.
;; #
;; # - `mode`: Indicate a binding for only specific terminal reported modes
;; #
;; #    This is mainly used to send applications the correct escape sequences
;; #    when in different modes.
;; #
;; #    - AppCursor
;; #    - AppKeypad
;; #    - Alt
;; #
;; #    A `~` operator can be used before a mode to apply the binding whenever
;; #    the mode is *not* active, e.g. `~Alt`.
;; #
;; # Bindings are always filled by default, but will be replaced when a new
;; # binding with the same triggers is defined. To unset a default binding, it can
;; # be mapped to the `ReceiveChar` action. Alternatively, you can use `None` for
;; # a no-op if you do not wish to receive input characters for that binding.
;; #
;; # If the same trigger is assigned to multiple actions, all of them are executed
;; # in the order they were defined in.
;; #key_bindings:
;;   #- { key: Paste,                                action: Paste          }
;;   #- { key: Copy,                                 action: Copy           }
;;   #- { key: L,         mods: Control,             action: ClearLogNotice }
;;   #- { key: L,         mods: Control, mode: ~Vi,  chars: "\x0c"          }
;;   #- { key: PageUp,    mods: Shift,   mode: ~Alt, action: ScrollPageUp,  }
;;   #- { key: PageDown,  mods: Shift,   mode: ~Alt, action: ScrollPageDown }
;;   #- { key: Home,      mods: Shift,   mode: ~Alt, action: ScrollToTop,   }
;;   #- { key: End,       mods: Shift,   mode: ~Alt, action: ScrollToBottom }

;;   # Vi Mode
;;   #- { key: Space,  mods: Shift|Control, mode: Vi, action: ScrollToBottom          }
;;   #- { key: Space,  mods: Shift|Control,           action: ToggleViMode            }
;;   #- { key: Escape,                      mode: Vi, action: ClearSelection          }
;;   #- { key: I,                           mode: Vi, action: ScrollToBottom          }
;;   #- { key: I,                           mode: Vi, action: ToggleViMode            }
;;   #- { key: Y,      mods: Control,       mode: Vi, action: ScrollLineUp            }
;;   #- { key: E,      mods: Control,       mode: Vi, action: ScrollLineDown          }
;;   #- { key: G,                           mode: Vi, action: ScrollToTop             }
;;   #- { key: G,      mods: Shift,         mode: Vi, action: ScrollToBottom          }
;;   #- { key: B,      mods: Control,       mode: Vi, action: ScrollPageUp            }
;;   #- { key: F,      mods: Control,       mode: Vi, action: ScrollPageDown          }
;;   #- { key: U,      mods: Control,       mode: Vi, action: ScrollHalfPageUp        }
;;   #- { key: D,      mods: Control,       mode: Vi, action: ScrollHalfPageDown      }
;;   #- { key: Y,                           mode: Vi, action: Copy                    }
;;   #- { key: Y,                           mode: Vi, action: ClearSelection          }
;;   #- { key: Copy,                        mode: Vi, action: ClearSelection          }
;;   #- { key: V,                           mode: Vi, action: ToggleNormalSelection   }
;;   #- { key: V,      mods: Shift,         mode: Vi, action: ToggleLineSelection     }
;;   #- { key: V,      mods: Control,       mode: Vi, action: ToggleBlockSelection    }
;;   #- { key: V,      mods: Alt,           mode: Vi, action: ToggleSemanticSelection }
;;   #- { key: Return,                      mode: Vi, action: Open                    }
;;   #- { key: K,                           mode: Vi, action: Up                      }
;;   #- { key: J,                           mode: Vi, action: Down                    }
;;   #- { key: H,                           mode: Vi, action: Left                    }
;;   #- { key: L,                           mode: Vi, action: Right                   }
;;   #- { key: Up,                          mode: Vi, action: Up                      }
;;   #- { key: Down,                        mode: Vi, action: Down                    }
;;   #- { key: Left,                        mode: Vi, action: Left                    }
;;   #- { key: Right,                       mode: Vi, action: Right                   }
;;   #- { key: Key0,                        mode: Vi, action: First                   }
;;   #- { key: Key4,                        mode: Vi, action: Last                    }
;;   #- { key: Key6,   mods: Shift,         mode: Vi, action: FirstOccupied           }
;;   #- { key: H,      mods: Shift,         mode: Vi, action: High                    }
;;   #- { key: M,      mods: Shift,         mode: Vi, action: Middle                  }
;;   #- { key: L,      mods: Shift,         mode: Vi, action: Low                     }
;;   #- { key: B,                           mode: Vi, action: SemanticLeft            }
;;   #- { key: W,                           mode: Vi, action: SemanticRight           }
;;   #- { key: E,                           mode: Vi, action: SemanticRightEnd        }
;;   #- { key: B,      mods: Shift,         mode: Vi, action: WordLeft                }
;;   #- { key: W,      mods: Shift,         mode: Vi, action: WordRight               }
;;   #- { key: E,      mods: Shift,         mode: Vi, action: WordRightEnd            }
;;   #- { key: Key5,   mods: Shift,         mode: Vi, action: Bracket                 }
;;   #- { key: Slash,                       mode: Vi, action: SearchForward           }
;;   #- { key: Slash,  mods: Shift,         mode: Vi, action: SearchBackward          }
;;   #- { key: N,                           mode: Vi, action: SearchNext              }
;;   #- { key: N,      mods: Shift,         mode: Vi, action: SearchPrevious          }

;;   # (Windows, Linux, and BSD only)
;;   #- { key: V,        mods: Control|Shift,           action: Paste            }
;;   #- { key: C,        mods: Control|Shift,           action: Copy             }
;;   #- { key: F,        mods: Control|Shift,           action: SearchForward    }
;;   #- { key: B,        mods: Control|Shift,           action: SearchBackward   }
;;   #- { key: C,        mods: Control|Shift, mode: Vi, action: ClearSelection   }
;;   #- { key: Insert,   mods: Shift,                   action: PasteSelection   }
;;   #- { key: Key0,     mods: Control,                 action: ResetFontSize    }
;;   #- { key: Equals,   mods: Control,                 action: IncreaseFontSize }
;;   #- { key: Add,      mods: Control,                 action: IncreaseFontSize }
;;   #- { key: Subtract, mods: Control,                 action: DecreaseFontSize }
;;   #- { key: Minus,    mods: Control,                 action: DecreaseFontSize }

;;   # (Windows only)
;;   #- { key: Return,   mods: Alt,           action: ToggleFullscreen }

;;   # (macOS only)
;;   #- { key: K,      mods: Command, mode: ~Vi, chars: "\x0c"            }
;;   #- { key: Key0,   mods: Command,            action: ResetFontSize    }
;;   #- { key: Equals, mods: Command,            action: IncreaseFontSize }
;;   #- { key: Add,    mods: Command,            action: IncreaseFontSize }
;;   #- { key: Minus,  mods: Command,            action: DecreaseFontSize }
;;   #- { key: K,      mods: Command,            action: ClearHistory     }
;;   #- { key: V,      mods: Command,            action: Paste            }
;;   #- { key: C,      mods: Command,            action: Copy             }
;;   #- { key: C,      mods: Command, mode: Vi,  action: ClearSelection   }
;;   #- { key: H,      mods: Command,            action: Hide             }
;;   #- { key: M,      mods: Command,            action: Minimize         }
;;   #- { key: Q,      mods: Command,            action: Quit             }
;;   #- { key: W,      mods: Command,            action: Quit             }
;;   #- { key: N,      mods: Command,            action: SpawnNewInstance }
;;   #- { key: F,      mods: Command|Control,    action: ToggleFullscreen }
;;   #- { key: F,      mods: Command,            action: SearchForward    }
;;   #- { key: B,      mods: Command,            action: SearchBackward   }

;; #debug:
;;   # Display the time it takes to redraw each frame.
;;   #render_timer: false

;;   # Keep the log file after quitting Alacritty.
;;   #persistent_logging: false

;;   # Log level
;;   #
;;   # Values for `log_level`:
;;   #   - None
;;   #   - Error
;;   #   - Warn
;;   #   - Info
;;   #   - Debug
;;   #   - Trace
;;   #log_level: Warn

;;   # Print all received window events.
;;   print_events: true

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
	 (white "#c5c8c6")))))))
      ));;)))
   

(define test-alacritty-service
  (list
   (simple-service 'alaritty-service
		   home-files-service-type
		   (list
		    `("config/test.yml"
		      ,(serialize-alacritty-configuration alacritty-config))))))