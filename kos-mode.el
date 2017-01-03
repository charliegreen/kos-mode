;;; kos-mode.el --- KSP kOS KerboScript major mode

;; Author: Charlie Green
;; URL: https://github.com/charliegreen/kos-mode
;; Version: 0.2.1

;;; Commentary:

;; A major mode for editing KerboScript program files, from the Kerbal Space Program mod
;; kOS.  I hope this is useful for someone!

;; TODO:
;; features:
;;   * potentially add custom faces for strings/comments
;;   * add AGn action group highlighting
;;   * add useful interactive commands (eg electric braces)
;; other:
;;   * yikes, clean up some of the regexes here....

;; TODO (long-term):
;;   * make sure I got the syntax right and included all global functions and variables
;;   * add some nice completion thing for completing fields of data structures (eg
;;     SHIP:VELOCITY autofills ":SURFACE", which autofills ":MAG", etc)
;;   * deal with highlighting function calls vs global variables, especially when they
;;     have the same name (eg STAGE)
;;   * add an optional auto-formatter

;;; Code:

(defgroup kos-mode ()
  "Options for `kos-mode'."
  :group 'languages)

(defgroup kos-mode-faces ()
  "Faces used by `kos-mode'."
  :group 'kos-mode)

(defcustom kos-indent (default-value 'tab-width)
  "Basic indent increment for indenting kOS code."
  :group 'kos-mode)

(defface kos-keyword-face
  '((t :inherit (font-lock-keyword-face)))
  "Face for keywords."
  :group 'kos-mode-faces)

(defface kos-operator-face
  '((t :inherit (font-lock-builtin-face)))
  "Face for operators."
  :group 'kos-mode-faces)

(defface kos-global-face
  '((t :inherit (font-lock-constant-face)))
  "Face for globally defined variables."
  :group 'kos-mode-faces)

(defface kos-constant-face
  '((t :inherit (font-lock-constant-face)))
  "Face for constants."
  :group 'kos-mode-faces)

(defface kos-function-name-face
  '((t :inherit (font-lock-function-name-face)))
  "Face for highlighting the names of functions in their definitions."
  :group 'kos-mode-faces)

(defmacro kos--opt (keywords)
  "Compile a regex matching any of KEYWORDS."
  `(eval-when-compile
     (regexp-opt ,keywords 'words)))

(eval-and-compile
  (defconst kos-keywords
    '("add" "all" "at" "batch" "break" "clearscreen" "compile" "copy" "declare"
      "delete" "deploy" "do" "do" "edit" "else" "file" "for" "from" "from"
      "function" "global" "if" "in" "is" "local" "lock" "log" "off" "on"
      "once" "parameter" "preserve" "print" "reboot" "remove" "rename" "run"
      "set" "shutdown" "stage" "step" "switch" "then" "to" "toggle" "unlock"
      "unset" "until" "volume" "wait" "when" "return" "lazyglobal")))

(eval-and-compile
  (defconst kos-globals
    '("ship" "target" "hastarget" "heading" "prograde" "retrograde" "facing"
      "maxthrust" "velocity" "geoposition" "latitude" "longitude" "up" "north"
      "body" "angularmomentum" "angularvel" "angularvelocity" "mass"
      "verticalspeed" "groundspeed" "surfacespeed" "airspeed" "altitude"
      "apoapsis" "periapsis" "sensors" "srfprograde" "srfretrograde" "obt"
      "status" "shipname"

      "terminal" "core" "archive" "nextnode" "hasnode" "allnodes"

      "liquidfuel" "oxidizer" "electriccharge" "monopropellant" "intakeair"
      "solidfuel"

      "alt" "eta" "encounter"

      "sas" "rcs" "gear" "lights" "brakes" "abort" "legs" "chutes" "chutessafe"
      "panels" "radiators" "ladders" "bays" "intakes" "deploydrills" "drills"
      "fuelcells" "isru" "ag1" "ag2" "ag3" "ag4" "ag5" "ag6" "ag7" "ag8" "ag9"
      "ag10"

      "throttle" "steering" "wheelthrottle" "wheelsteering"

      "missiontime" "version" "major" "minor" "build" "sessiontime"
      "homeconnection" "controlconnection"

      "kuniverse" "config" "warp" "warpmode" "mapview" "loaddistance"
      "solarprimevector" "addons"

      "red" "green" "blue" "yellow" "cyan" "magenta" "purple" "white" "black")))

(eval-and-compile
  (defconst kos-functions
    '("round" "mod" "abs" "ceiling" "floor" "ln" "log10" "max" "min" "random" "sqrt"
      "char" "unchar" "sin" "cos" "tan" "arcsin" "arccos" "arctan" "arctan2"

      "list" "rgb" "rgba" "hsv" "hsva"
      "clearscreen" "stage" "constant" "profileresult")))

(eval-and-compile
  (defconst kos-constants
    '("pi" "e" "g" "c" "atmtokpa" "kpatoatm" "degtorad" "radtodeg")))

(defun kos--opt-nomember (keywords)
  "Compile a regex matching any of KEYWORDS with no leading colon.

This is the same as `kos--opt', except it won't match any of
KEYWORDS if they are being accessed as a structure member (eg,
for `(kos--opt-nomember '(\"foo\"))`, 'foo' would be highlighted,
but 'bar:foo' would not)."
  (concat "\\(?:^\\|[^:]\\)" (kos--opt keywords)))

(defconst kos-font-lock-keywords
  ;; aren't these regexes beautiful?
  `((,(kos--opt-nomember kos-keywords) 1 'kos-keyword-face)

    (,(kos--opt-nomember kos-globals) 1 'kos-global-face)
    (,(kos--opt-nomember kos-constants) 1 'kos-constant-face)

    ;; for numbers; have this before operators so decimals are still highlighted
    ("\\b[[:digit:].]+\\(e[+-]?[:digit:]+\\)?\\b" . 'kos-constant-face)

    ;; ((rx (any ?+ ?- ?* ?/ ?^ ?( ?))) . 'kos-operator-face)   ; arithmetic ops
    ("\\+\\|-\\|\\*\\|/\\|\\^\\|(\\|)" . 'kos-operator-face) ; arithmetic ops

    ;; ((rx word-boundary
    ;; 	(or "not" "and" "or" "true" "false" "<>" "<=" ">=" "=" ">" "<")
    ;; 	word-boundary) 1 'kos-operator-face)				    ; logical ops
    ("\\b\\(not\\|and\\|or\\|true\\|false\\|<>\\|<=\\|>=\\|=\\|>\\|<\\)\\b" ; logical ops
     1 'kos-operator-face)

    ;;((rx (any ?{ ?} ?[ ?] ?, ?. ?: ?@)) . 'kos-operator-face) ; other ops
    ("{\\|}\\|\\[\\|\\]\\|,\\|\\.\\|:\\|@" . 'kos-operator-face) ; other ops
    
    ;; highlight function declarations
    ("\\bfunction\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)" 1 'kos-function-name-face))
  "Keyword highlighting specification for `kos-mode'.")

;; (defvar kos-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     ;(define-key map [foo] 'kos-do-foo)
;;     map)
;;   "Keymap for `kos-mode'.")

(defvar kos-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 12" st)	; // starts comments
    (modify-syntax-entry ?\n ">" st)	; newline ends comments
    st)
  "Syntax table for `kos-mode'.")

;; https://web.archive.org/web/20070702002238/http://two-wugs.net/emacs/mode-tutorial.html
(defun kos-indent-line ()
  "Indent the current line of kOS code."
  (interactive)

  (require 'cl)

  (let ((not-indented t) cur-indent
	(r-bob "^[^\n]*?{[^}]*$")	; beginning of block regex
	(r-nl "\\(?:\n\\|\r\n\\|$\\)")	; an actual newline ($ was acting funky)
	(ltss nil)) ; lines to start of (unterminated) statement; see `in-unterminated-p'

    (cl-flet* ((get-cur-line ()
			 (save-excursion
			   (let ((start (progn (beginning-of-line) (point)))
				 (end (progn (end-of-line) (point))))
			     (buffer-substring-no-properties start end))))
	       
	       (back-to-nonblank-line ()
				      (let ((cont t))
					(while cont
					  (forward-line -1)
					  (if (or (bobp) (not (looking-at "^\\s-*$")))
					      (setq cont nil)))))
	       
	       (set-indent (v &optional not-relative)
			   (setq cur-indent (if not-relative v
					      (+ (current-indentation)
						 (* v kos-indent))))
			   (setq not-indented nil))
	       
	       (strip-text
		(s) ;; remove strings and comments
		(while (string-match
			(rx (: ?\" (0+ (or (: ?\\ ?\")
					   (not (any ?\")))) ?\")) s)
		  (setq s (replace-match "" t t s)))
		(while (string-match (concat "//.*" r-nl) s)
		  (setq s (replace-match "" t t s))) s)

	       (in-unterminated-p
		()
		;; Returns nil if in unterminated statement and the number of
		;; lines back to the beginning of the statement otherwise
		(setq ltss nil)
		(save-excursion
		  (let ((loopp t)	; whether we should keep searching
			(rettp nil)	; whether we'll return a positive value
			(count 0)	; number of lines we've processed before
			(found-end-p nil) ; whether we've found a terminating line
			cur-line)	  ; the current line we're processing
		    (while (and loopp (not (bobp)))
		      (setq cur-line (strip-text (get-cur-line)))
		      (cond
		       ;; found an unterminated statement
		       ((string-match
			 (concat "^\\s-*" (kos--opt kos-keywords) "\\b\\s-*[^{.]*$")
			 cur-line)
			(setq loopp nil rettp t))
		       
		       ;; found a block opener
		       ((string-match "^\\s-*{[^}]*$" cur-line)
			(setq loopp nil))

		       ;; found a block closer (which counts as terminator,
		       ;; since it presumably has an opener) or a line ending
		       ;; with a dot
		       ((or
			 (string-match ".*\\.\\s-*$" cur-line)
			 (string-match "^\\s-*}[^{]*$" cur-line))
			
			;; check if count is zero so we don't indent whitespace
			;; past the terminator
			(if (and (not found-end-p) (zerop count))
			    (setq found-end-p t)
			  (setq loopp nil))))

		      ;; at end of each loop, if we don't want to break, update
		      ;; counter and move POINT to next line to process
		      (if loopp
			  (progn
			    (setq count (1+ count))
			    (forward-line -1))))
		    (if (not rettp) nil
		      (progn
			(setq ltss count)
			(not (= count 0)))))))

	       ;; for "looking-at-line"
	       (lal (r) (string-match r (strip-text (get-cur-line)))))

      (save-excursion
	(beginning-of-line)

	(cond ((bobp) (set-indent 0 t)) ; if at beginning of buffer, indent to 0
	      ((lal "^[ \t]*}")		; if closing a block
	       (progn	     ; then indent one less than previous line
		 ;; TODO: check if previous line is part of a line continuation
		 (back-to-nonblank-line)
		 (cond
		  ((looking-at r-bob) (set-indent 0)) ; if closing empty block, match it
		  ((in-unterminated-p)	    ; if last line unterminated, indent back two
		   (set-indent -2))
		  (t (set-indent -1))))) ; otherwise, indent back one
	      
	      ((lal "^[ \t]*{")		; if opening a block on a blank line
	       (progn			; then indent the same as last line
		 (back-to-nonblank-line)
		 (set-indent 0)))
	      
	      ((in-unterminated-p)	; if line part of unterminated statement
	       (progn			; then indent one more than beginning
		 (forward-line (- ltss))
		 (set-indent +1)))
	      
	      (t (while not-indented	; else search backwards for clues
		   (back-to-nonblank-line)
		   (cond
		    ((bobp) (setq not-indented nil)) ; perhaps we won't find anything
		    ((lal "^[ \t]*}[^{]*$") (set-indent 0)) ; found the end of a block
		    ((lal r-bob) (set-indent +1)))))))) ; found the beginning of a block

      (if (not cur-indent) (setq cur-indent 0))
      (if (< cur-indent 0) (setq cur-indent 0))
      
      ;; now actually indent to cur-indent
      (if (save-excursion		; if within indentation
	    (let ((point (point))
		  (start (progn (beginning-of-line) (point)))
		  (end (progn (back-to-indentation) (point))))
	      (and (<= start point) (<= point end))))
	  (indent-line-to cur-indent)	; then indent line and move point
	(save-excursion (indent-line-to cur-indent))))) ; else just indent line

;;;###autoload
(define-derived-mode kos-mode prog-mode "KerboScript"
  "Major mode for editing kOS program files, for the game Kerbal Space Program."
  :syntax-table kos-mode-syntax-table
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'indent-line-function)
  (setq comment-start "// ")
  (setq comment-start-skip "//+\\s-*")
  (setq font-lock-defaults
	'(kos-font-lock-keywords nil t)) ; t makes this case-insensitive
  (setq indent-line-function 'kos-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ks\\'" . kos-mode))

(provide 'kos-mode)

;;; kos-mode.el ends here
