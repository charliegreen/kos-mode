;;; kos-mode.el --- KSP kOS KerboScript major mode

;; Author: Charlie Green
;; URL: https://github.com/charliegreen/kos-mode
;; Version: 0.1

;;; Commentary:

;; A major mode for editing KerboScript program files, from the Kerbal Space Program mod
;; kOS. I hope this is useful for someone!

;; TODO:
;; bugs:
;;   * make unterminated statements indent line continuation
;;   * make indentation code stop ignoring all lines with strings
;; features:
;;   * potentially add custom faces for strings/comments
;;   * add AGn action group highlighting

;; TODO (long-term):
;;   * make sure I got the syntax right and included all global functions and variables
;;   * add some nice completion thing for completing fields of data structures (eg
;;     SHIP:VELOCITY autofills ":SURFACE", which autofills ":MAG", etc)
;;   * deal with highlighting function calls vs global variables, especially when they
;;     have the same name (eg STAGE)

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
  "Prepare KEYWORDS for `looking-at'."
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
  "Same as `kos--opt', except prepends a regex so that the resulting regex won't
   match any words that are being accessed as members of structures (eg if you
   had `(kos--opt-nomember '(\"foo\"))`, then 'foo' would be highlighted, but
   'bar:foo' would not)"
  (concat "\\(?:^\\|[^:]\\)" (kos--opt keywords)))

(defconst kos-font-lock-keywords
  ;; aren't these regexes beautiful?
  `((,(kos--opt-nomember kos-keywords) 1 'kos-keyword-face)

    (,(kos--opt-nomember kos-globals) 1 'kos-global-face)
    (,(kos--opt-nomember kos-constants) 1 'kos-constant-face)
    ;; have this before operators so decimals are still highlighted
    ("\\b[[:digit:].]+\\(e[+-]?[:digit:]+\\)?\\b" . 'kos-constant-face)

    ("\\+\\|-\\|\\*\\|/\\|\\^\\|(\\|)" . 'kos-operator-face) ; arithmetic ops
    ("\\b\\(not\\|and\\|or\\|true\\|false\\|<>\\|<=\\|>=\\|=\\|>\\|<\\)\\b" ; logical ops
     1 'kos-operator-face)
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
    (modify-syntax-entry ?_ "_" st)	; `_' is symbol-level, not word
    st)
  "Syntax table for `kos-mode'.")

;; https://web.archive.org/web/20070702002238/http://two-wugs.net/emacs/mode-tutorial.html
(defun kos-indent-line ()
  "Indent the current line of kOS code."
  (interactive)

  (require 'cl)

  (let ((not-indented t) cur-indent (full-text "")
	(r-bob "^[^\n]*?{[^}]*$")	; beginning of block regex
	(r-nl "\\(?:\n\\|\r\n\\)")	; an actual newline
	lb-str)	; set to whatever string the last `string-match' in `lb' was run on

    (cl-flet* ((cur-line ()
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
	       
	       (remove-strings (s)
			       (while (string-match
				       (rx (: ?\" (0+ (or (: ?\\ ?\")
							  (not (any ?\"))))
					      ?\")) s)
				 (setq s (replace-match "" t t s))) s)
	       
	       (update-full-text ()
				 (setq full-text
				       (concat (remove-strings (cur-line)) full-text)))
	       
	       (la (r) (string-match r full-text)) ; `la' for `looking-at'
	       (lal (r) (string-match r (remove-strings (cur-line))))

	       (lb (r) ; `lb' for `looking-back'
		   ;; (string-match
		   ;;  (concat "\\`\\(?:.\\|\n\\)*\\(" r "\\)\\'")
		   ;;  (buffer-substring-no-properties (point-min) (point)))))
		   
		   (setq r (concat r "\\'"))
		   (let ((pos (point))
		   	 (end (point))
		   	 (loopp t))
		     (while (and (>= pos (point-min)) loopp)
		       ;;(kos-dbg "  Trying `%s'" (buffer-substring-no-properties pos end))
		       (let ((text (remove-strings
				    (buffer-substring-no-properties pos end))))
			 (if (string-match r text)
			     (progn
			       (setq loopp nil)
			       (setq lb-str text)
			       (kos-dbg "--------------------")
			       (kos-dbg "  Raw: `%s'" (buffer-substring pos end))
			       (kos-dbg "  Matched `%s'" text))
			   (setq pos (1- pos)))))
		     (not loopp))))

      (save-excursion
	(beginning-of-line)
	(update-full-text)
	
	(cond ((bobp) (set-indent 0 t)) ; if at beginning of buffer, indent to 0
	      ((la "^[ \t]*}")		; if closing a block
	       (progn	     ; then indent one less than previous line
		 ;; TODO: check if previous line is part of a line continuation
		 (back-to-nonblank-line)
		 (if (looking-at r-bob) ; if we're closing an empty block, match indentation
		     (set-indent 0)
		   (set-indent -1))))
	      
	      ((la "^[ \t]*{")	; if opening a block on a blank line
	       (progn			; then indent the same as last line
		 (back-to-nonblank-line)
		 (set-indent 0)))
	      
	      ((lb		 ; if inside an unterminated statement
		;;(concat "^\\s-*" (kos--opt kos-keywords) "\\b\\s-*" "[^{.]*" r-nl))
		(concat "\\s-*" (kos--opt kos-keywords) "\\b\\s-*"
			"\\(?:[^{.]*"  	; content
			"\\(?://.*\\)?"	; an optional comment
			r-nl "\\)+"))
	       (progn			; then indent one more than statement starter
		 (kos-dbg "--------------------------------")
		 (kos-dbg "unterminated statement:")
		 (kos-dbg "    `%s'" (match-string 0 lb-str))
		 
		 ;;(goto-char (match-beginning 0))
		 (goto-char (+ (- (point) (length lb-str)) (match-beginning 0)))
		 (kos-dbg "starter: `%s'" (cur-line))
		 (set-indent +1)))
	      
	      (t
	       (while not-indented	     ; else search backwards for clues	 
		 (back-to-nonblank-line)
		 (update-full-text)

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
	  (indent-line-to cur-indent) ; then indent line and move point
	(save-excursion (indent-line-to cur-indent))))) ; else just indent line

;;;###autoload
(define-derived-mode kos-mode prog-mode "KerboScript"
  "Major mode for editing kOS program files, for the game Kerbal Space Program."
  :syntax-table kos-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//+\\s-*")
  (setq-local font-lock-defaults
	      '(kos-font-lock-keywords nil t)) ; t makes this case-insensitive
  (setq-local indent-line-function 'kos-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ks\\'" . kos-mode))

(provide 'kos-mode)

;; ==================== for debugging only; TODO remove this
(defun kos-mode-reload ()
  (interactive)
  (load "kos-mode/kos-mode")
  (kos-mode))
(global-set-key (kbd "C-c r") 'kos-mode-reload)

(defun kos-dbg (fmt &rest args)
  (let ((buf (current-buffer)))
    (pop-to-buffer (get-buffer-create "debug"))
    (end-of-buffer)
    (insert (apply 'format (cons fmt args)) "\n")
    (pop-to-buffer buf)))

(defun kos-test-debug ()
  (interactive)
  (kos-dbg "Hello!")
  (kos-dbg "and another, with formatting: `%d' is 0." 0))
(global-set-key (kbd "C-c d") 'kos-test-debug)

