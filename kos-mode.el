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

  (let ((not-indented t) cur-indent (full-text "")
	(r-bob "^[^\"\n]*?{[^}]*$")	; beginning of block regex
	(r-nl "\\(?:\n\\|\r\n\\)")	; an actual newline
	(cur-line #'(lambda ()
		      (save-excursion
			(let ((start (progn (beginning-of-line) (point)))
			      (end (progn (end-of-line) (point))))
			  (buffer-substring-no-properties start end)))))
	(back-to-nonblank-line #'(lambda ()
				   (let ((cont t))
				     (while cont
				       (forward-line -1)
				       (if (not (looking-at "^\\s-*$"))
					   (setq cont nil)))))))
    (save-excursion
      (beginning-of-line)
      (setq full-text (concat (funcall cur-line) full-text))

      (cond ((bobp) (setq cur-indent 0)) ; if at beginning of buffer, indent to 0
	    ((looking-at "^[ \t]*}")	 ; if closing a block
	     (progn			 ; then indent one less than previous line
	      (funcall back-to-nonblank-line)
	      (setq cur-indent
		    ;; if we're closing an empty block, match indentation
		    (if (looking-at r-bob) (current-indentation)
		      (- (current-indentation) kos-indent)))
	      (if (< cur-indent 0)
		  (setq cur-indent 0))))

	    ((looking-at "^[ \t]*{")	; if opening a block on a blank line
	     (progn			; then indent the same as last line
	      (funcall back-to-nonblank-line)
	      (setq cur-indent (current-indentation))))
	    
	    ((looking-back		; if inside an unterminated statement
	      ;;(concat "^\\s-*" (kos--opt kos-keywords) "\\b\\s-*" "[^{.]*" r-nl))
	      (concat "^\\s-*" (kos--opt kos-keywords) "\\b\\s-*"
		      "\\(?:[^{.]*" "\\(?://.*\\)?" r-nl "\\)+"))
	     (progn			; then indent one more than statement starter
	       ;;(message "unterminated statement: `%s'" (match-string 0))
	       (goto-char (match-beginning 0))
	       ;;(message "starter: `%s'" (funcall cur-line))
	       (setq cur-indent (+ (current-indentation) kos-indent))
	       (setq not-indented nil)))

	    (t	
	     (while not-indented	     ; else search backwards for clues
	       (funcall back-to-nonblank-line)
	       (setq full-text (concat (funcall cur-line) full-text))
	       (cond ((bobp) (setq not-indented nil)) ; perhaps we won't find anything
		     
		     ;; found the end of a block
		     ((looking-at "^[ \t]*}[^{]*$")
		      (progn
			;;(message "found end of block: `%s'" (funcall cur-line))
			(setq cur-indent (current-indentation))
			(setq not-indented nil)))
		     
		     ;; found the beginning of a block
		     ((looking-at r-bob)
		      (progn
			;;(message "found beginning of block: `%s'" (funcall cur-line))
			(setq cur-indent (+ (current-indentation) kos-indent))
			(setq not-indented nil))))))))
		  
    (if (not cur-indent) (setq cur-indent 0))
    (if (< cur-indent 0) (setq cur-indent 0))
    ;; indent to cur-indent
    (if (save-excursion			; if within indentation
	  (let ((point (point))
		(start (progn (beginning-of-line) (point)))
		(end (progn (back-to-indentation) (point))))
	    (and (<= start point) (<= point end))))
	(indent-line-to cur-indent)  ; then indent line and move point
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
