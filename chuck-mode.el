;;; downloaded from http://www.dtic.upf.edu/~gcoleman/chuck/svn/chuck-mode.el
;;; ChucK major mode for editing ChucK code and hopefully in the
;;; future also updating a running ChucK engine
;;;
;;; (c) 2004 Mikael Johansson
;;; revised by Graham Coleman 2006

;; mode hook for user defined actions
(defvar chuck-mode-hook nil)

;;; CHANGE THIS to something that fits your system!!!
(defvar chuck-exec "chuck")

;;; CHANGE THIS to list of commonly required classes
(defvar chuck-lib (list "fade.ck" "scale.ck" "tg.ck"))

;; call chuck with some arguments
(defun run-chuck (ch-action &rest args)
  (apply 'call-process chuck-exec
	 nil ;;no infile
	 ;;0 ;;discard output and return nil immediately
	 "*ChucK*" ;;don't throw away output just yet
	 nil ;;do not redisplay
	 ch-action ;;the action
	 args)) ;;apply spreads the args

;; make a temp ck dir if you need it
(defun make-ck-dir ()
  (if (not (file-exists-p ".ck")) ;;if no dir
      (make-directory ".ck" nil))) ;;create it

;; save into a temp file and return filename, under construction
(defun save-temp ()
  (make-ck-dir)
  (let ((bname (format ".ck/temp.ck")))
    (write-region (point-min) (point-max) ;;write the whole buffer
		  bname ;;temp name
		  nil nil nil) ;;no append, mustbenew, or visit
    bname)) ;;return the filename

;; ChucK as an internal listener does not work well. Run it externally
;; and control it internally.
;(defun run-chuck ()
;  "Start a ChucK listener"
;  (interactive)
;  (start-process "ChucK" "*ChucK*" chuck-exec "--loop"))
(defun kill-chuck ()
 "Kills a running ChucK listener"
 (interactive)
 (call-process chuck-exec nil 0 "--kill"))

;;try running chuck as an internal listener
(defun start-chuck ()
  "Start a ChucK listener"
  (interactive)
  (let ((cproc
	 (start-process "ChucK" "*ChucK*" chuck-exec "--loop")))
    ;;(set-process-coding-system cproc
    ;;			       (keyboard-coding-system)
    ;;			       (terminal-coding-system))
    ;;(setq chuck-proc cproc)
    ;;(process-send-eof cproc)
    ;;(process-kill-without-query cproc)
    ;;(set-process-filter cproc 'chuck-insert-filter)
    ;;(set-process-sentinel cproc nil)) ;;the null sentinel
    )
  (switch-to-buffer-other-window "*ChucK*") ;;open in bottom
  )

;;filter
(defun chuck-insert-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
	;; Insert the text, advancing the process marker.
	(goto-char (process-mark proc))
	(insert string)
	(set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun chuck-add-code ()
  "Add buffer to running ChucK"
  (interactive)
  (if (buffer-modified-p)
      (message "You need to save first")
    (run-chuck "--add" buffer-file-name)))

(defun chuck-add-code-nosave ()
  "Add buffer to running ChucK w/o requiring save"
  (interactive)
  (run-chuck "--add" (save-temp)))

(defun chuck-add-library ()
  "Add commonly required class files."
  (interactive)
  (dolist (f chuck-lib)
    (run-chuck "--add" f)))

(defun chuck-remove-code ()
  "Remove code snippet from running ChucK"
  (interactive)
  (run-chuck "--remove"
	     (read-string "Remove which shred? ")))

(defun chuck-replace-code ()
  "Replace code snippet in running ChucK with buffer"
  (interactive)
  (if (buffer-modified-p)
      (message "You need to save first")
    (run-chuck "--replace"
	       (read-string "Replace which shred? ")
	       buffer-file-name)))

(defun chuck-replace-code-nosave ()
  "Replace shred in running ChucK without saving"
  (interactive)
  (run-chuck "--replace"
	     (read-string "Replace which shred? ")
	     (save-temp)))

(defun chuck-status ()
  "Tell ChucK to report status"
  (interactive)
  (run-chuck "--status"))

(defun chuck-remove-all ()
  "Remove all shreds"
  (interactive)
  (run-chuck "--removeall"))

;; keymap for ChucK mode
(defvar chuck-mode-map
  (let ((chuck-mode-map (make-keymap)))
    (define-key chuck-mode-map (kbd "<RET>") 'newline-and-indent)

    (define-key chuck-mode-map [menu-bar chuck]
      (cons "ChucK" (make-sparse-keymap "ChucK")))

    (define-key chuck-mode-map "\C-c\C-c" 'chuck-remove-all)
    (define-key chuck-mode-map [menu-bar chuck chuck-remove-all]
      '("Remove all shreds from running ChucK" . chuck-remove-all))

    (define-key chuck-mode-map [menu-bar chuck chuck-add-library]
      '("Add library to running ChucK" . chuck-add-library))

    (define-key chuck-mode-map "\M-s" 'chuck-status)
    ;; M-s is normally undefined
    (define-key chuck-mode-map [menu-bar chuck chuck-status]
      '("Query ChucK status" . chuck-status))

    (define-key chuck-mode-map "\M-r" 'chuck-replace-code)
    ;; M-r normally move-to-window-line
    (define-key chuck-mode-map [menu-bar chuck chuck-replace-code]
      '("Replace code in running ChucK with buffer" . chuck-replace-code))

    (define-key chuck-mode-map "\M-e" 'chuck-remove-code)
    ;; M-e is normally sentence-end
    (define-key chuck-mode-map [menu-bar chuck chuck-remove-code]
      '("Remove code from running ChucK" . chuck-remove-code))

    (define-key chuck-mode-map "\M-a" 'chuck-add-code)
    ;; M-e is normally forward-sentence
    (define-key chuck-mode-map [menu-bar chuck chuck-add-code]
      '("Add buffer to running ChucK" . chuck-add-code))

    chuck-mode-map)
  "Keymap for ChucK major mode")

;; Filename binding
(add-to-list 'auto-mode-alist '("\\.ck\\'" . chuck-mode))

;; Come helper functions for creating font-lock entries.
(defun keyword-regexp (&rest word-list)
  (concat
   "\\<\\("
   (mapconcat 'identity word-list "\\|")
   "\\)\\>"))
(defun symbol-regexp (&rest symbol-list)
  (concat
   "\\_<\\("
   (mapconcat 'identity symbol-list "\\|")
   "\\)\\_>"))
(defun chuck-library-regexp (namespace &rest symbol-list)
  (concat
   "\\<" namespace "\\.\\("
   (mapconcat 'identity symbol-list "\\|")
   "\\)\\>"))

;; Syntax highlighting
(defconst chuck-font-lock-keywords-1
  (list
   (cons (keyword-regexp
	  ;; Primitive types
	  "int" "float" "time" "dur" "void" "same"
	  ;; Reference types
	  "Object" "array" "Event" "UGen" "string"
	  ;; Complex types
	  "polar" "complex"
	  ;; standard ChucK unit generators:
	  "SinOsc" "PulseOsc" "SqrOsc" "TriOsc"
	  "SawOsc" "Phasor" "Noise" "Impulse"
	  "Step" "Gain" "SndBuf" "SndBuf2" "HalfRect"
	  "FullRect" "ZeroX" "Mix2" "Pan2"
	  "GenX" "CurveTable" "WarpTable" "LiSa"
	  ;; filters:
	  "OneZero" "TwoZero" "OnePole" "TwoPole"
	  "PoleZero" "BiQuad" "Filter" "LPF"
	  "HPF" "BPF" "BRF" "ResonZ" "Dyno"
	  ;; STK unit generators in ChucK:
	  "Envelope" "ADSR" "Delay" "DelayA" "DelayL"
	  "Echo" "JCRev" "NRev" "PRCRev" "Chorus"
	  "Modulate" "PitShift" "SubNoise" "Blit"
	  "BlitSaw" "BlitSquare" "WvIn" "WaveLoop"
	  "WvOut" "WvOut2"
	  ;; STK instruments unit generators
	  "StkInstrument" "BandedWG" "BlowBotl"
	  "BlowHole" "Bowed" "Brass" "Clarinet"
	  "Flute" "Mandolin" "ModalBar" "Moog"
	  "Saxofony" "Shakers" "Sitar" "StifKarp"
	  "VoicForm" "FM" "BeeThree" "FMVoices"
	  "HevyMetl" "PercFlut" "Rhodey"
	  "TubeBell" "Wurley"
	  ;; Events and IO
	  "Event" "Object" "MidiIn" "MidiMsg"
	  "OscRecv" "OscSend" "OscEvent"
	  "FileIO" "IO" "Shred"
	  "Hid" "HidMsg" "KBHit"
	  "StringTokenizer" "ConsoleInput"
	  )
	 'font-lock-type-face)
   (cons (keyword-regexp
	  ;; Control structures
	  "if" "else" "while" "until" "for" "repeat"
	  "break" "continue" "return" "switch"
	  ;; Class keyword
	  "class" "extends" "public" "static" "pure"
	  "this" "super" "interface" "implements"
	  "protected" "private"
	  ;; Other keywords
	  "function" "fun" "spork" "const" "new")
	 'font-lock-keyword-face)
   (cons (keyword-regexp
	  ;; Special values
	  "now" "true" "false" "maybe"
	  "null" "NULL" "me" "pi"
	  ;; Special: default durations
	  "samp" "ms" "second" "minute" "hour"
	  "day" "week"
	  ;; Special: global ugens
	  "dac" "adc" "blackhole")
	 'font-lock-pseudo-keyword-face)

   ;; chuck operators and debug print
   (cons (symbol-regexp "=>" "=<" "!=>" "->"
			"<-" "+->" "-->" "*->"
			"/->" "&->" "|->" "^->"
			">>->" "<<->" "%->" "@=>"
			"+=>" "-=>" "*=>" "/=>"
			"&=>" "|=>" "^=>" ">>=>"
			"<<=>" "%=>" "<<<" ">>>")
	 'font-lock-operator-face)

   ;;  Upchuck operator. For some reason the regexp applied to other
   ;;  operators don't work
   (cons "\\_<\\(=\\^\\)" 'font-lock-operator-face)

   ;; Standard Library functions
   (list (chuck-library-regexp "Std"
			       ;; Std
			       "abs" "fabs"
			       ;; update for 1.3.1.3: deprecate Std.rand
			       ;;"rand" "rand2" "randf" "rand2f"
			       "sgn" "system" "atoi"
			       "atof" "getenv" "setenv"
			       "mtof" "ftom" "powtodb"
			       "rmstodb" "dbtopow" "dbtorms")
	 1 'font-lock-builtin-face)

   (list (chuck-library-regexp "Machine"
			       ;; Machine
			       "add" "spork" "remove"
			       "replace" "status" "crash")
	 1 'font-lock-builtin-face)

   (list (chuck-library-regexp "me"
			       ;; me
			       "arg" "args" "yield"
			       "exit" "sourcePath" "sourceDir")
	 1 'font-lock-builtin-face)

   (list (chuck-library-regexp "Math"
			       ;; Math
			       "random" "random2" "randomf" "random2f"
			       "srandom" "RANDOM_MAX"
			       "PI" "TWO_PI" "e" "E" "i" "I" "j" "J"
			       "INFINITY" "INT_MAX" "FLOAT_MAX"
			       "FLOAT_MIN_MAG"
			       "sin" "cos" "tan" "asin"
			       "acos" "atan" "atan2"
			       "sinh" "cosh" "tanh"
			       "hypot" "pow" "sqrt" "exp"
			       "log" "log2" "log10"
			       "floor" "ceil" "round"
			       "trunc" "fmod" "remainder"
			       "min" "max" "nextpow2"
			       "isinf" "isnan")
	 1 'font-lock-builtin-face)

   ;; Namespaces
   '("\\<\\(Math\\|Std\\|Machine\\)\\>\\." 1 'font-lock-constant-face)
   ;; Functions
   '("\\<\\(fun\\|function\\)[ \t]+[a-zA-Z_]+[a-zA-Z0-9_]*[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
     2 'font-lock-function-name))
  ;; '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Highlighting for ChucK mode")

(defvar chuck-font-lock-keywords chuck-font-lock-keywords-1
  "Default highlighting for ChucK mode")

;; Indenting for ChucK mode
(defun chuck-indent-line ()
  "Indent current line as ChucK code"
  (interactive)
  (beginning-of-line)
  (if (bobp)  ;; Start of buffer starts out unindented
      (indent-line-to 0)
    (let ((not-indented t)
		  cur-indent)
      (if (looking-at "[[:blank:]]*}") ; Closing a block
		  (progn
			(save-excursion
			  (forward-line -1)
			  (setq cur-indent (- (current-indentation) default-tab-width)))
			(if (< cur-indent 0)
				(setq cur-indent 0)))
		(save-excursion
		  (while not-indented
			(forward-line -1)
			(cond ((looking-at ".*{") ; In open block
				   (setq cur-indent (+ (current-indentation) default-tab-width))
				   (setq not-indented nil))
				  ((looking-at "[[:blank:]]*}") ; Closed block on blank line
				   (setq cur-indent (current-indentation))
				   (setq not-indented nil))
				  ((looking-at ".*}") ; Closed block on non-blank line
				   (setq cur-indent (- (current-indentation) default-tab-width))
				   (setq not-indented nil))
				  ((bobp)
				   (setq not-indented nil))))))
      (if cur-indent
		  (indent-line-to cur-indent)
		(indent-line-to 0)))))

;; Syntax table
(defvar chuck-mode-syntax-table nil "Syntax table for ChucK mode")
(setq chuck-mode-syntax-table
      (let ((chuck-mode-syntax-table (make-syntax-table)))
	(modify-syntax-entry ?_ "_" chuck-mode-syntax-table)
	(modify-syntax-entry ?/ ". 12" chuck-mode-syntax-table)
	(modify-syntax-entry ?\n ">" chuck-mode-syntax-table)
	chuck-mode-syntax-table))

;; Entry point
(defun chuck-mode ()
  "Major mode for editing ChucK music/audio scripts"
      (interactive)
  (kill-all-local-variables)
  (set-syntax-table chuck-mode-syntax-table)
  (use-local-map chuck-mode-map)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'font-lock-defaults)
       '(chuck-font-lock-keywords))
  (set (make-local-variable 'indent-line-function)
       'chuck-indent-line)

  (setq major-mode 'chuck-mode)
  (setq mode-name "ChucK")
  (setq default-tab-width 4)
  (run-hooks 'chuck-mode-hook))

(provide 'chuck-mode)
