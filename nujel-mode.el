;;; nujel-mode.el --- Tango-based custom theme -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Benjamin Vincent Schulenburg <ben@wolkenwelten.net>

;; Author: Benjamin Vincent Schulenburg
;; URL: https://github.com/Melchizedek6809/nujel-mode
;; Created: 2022
;; Version: 1.0
;; Keywords: language, lisp, scheme, clojure
;; License: GPL-3.0-or-later
;; Filename: nujel-mode.el
;; Package-Requires: ((emacs "24"))
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; This is a major-mode for the Nujel programming language.
;;
;; Heavily based on newlisp.el by Tim Johnson et al.
;; Thanks to Tim Johnson <tim@johnsons-web.com> (TJ)
;; Thanks to Stefan Monnier <monnier@iro.umontreal.ca> (SM)
;; Thanks also to: johan bockg�rd <bojohan+news@dd.chalmers.se> (JB)
;;
;;; Code:
;; ===========================================================================================
(require 'scheme)          ;; Inherit Scheme mode
;; =====================================================================================
;;  Top-level Values
;; =====================================================================================
(defconst nujel-binary-name "nujel" "Process executable.")
;; =====================================================================================
(defconst nujel-process-name "nujel" "Nujel Process Name.")
;; ===========================================================================================
(defvar nujel-mode-hook nil
  "*Hook called by `nujel-mode'.")
;; ==========================================================================
(defvar nujel-function-names '("defn" "defmacro" "defun" "defobject" "defproperty" "defmethod" "defhandler" "fn")
  "Names of Nujel definition definitions.")
;; ===========================================================================================
(defvar nujel-function-begin-regexp "\\[\\(?:def\\(?:n\\|macro\\|handler||\object\\|method\\|property\\)\\|fn\\)"
  "Used to find definition definitions.  NOTE: No whitespace after parens!")
;; ===========================================================================================
;;  'helper' functions
;; ===========================================================================================
(defun nujel-replace-newlines (S)
  "Replace newlines in string 'S' with spaces.
Use for sending code to nujel."
  (mapconcat (lambda (x) x) (split-string S "\n") " "))
;; ===========================================================================================
;; ===========================================================================================
(defun nujel-at-function-startp ()
  "Is cursor at the beginning of a definition?"
  (interactive)
  (cond ((string-equal (char-to-string (char-after)) "[") ;; cursor on '('
	 (forward-char 1)
	 (cond ((member (current-word) nujel-function-names)
		(backward-char 1) ;; found. Reset
		(message "found")
		t)
	       (t ;; not found. Reset
		(message "not found")
		(backward-char 1) nil)))
	(t  nil)))
;; ===========================================================================================
(put 'fn 'scheme-indent-function 2)  ;; treat fn,letn,letex as a functions
(put 'def 'scheme-indent-function 1)
(put 'defn 'scheme-indent-function 1)
(put 'for 'scheme-indent-function 0)
(put 'case 'scheme-indent-function 0)
(put 'while 'scheme-indent-function 0)
(put 'when 'scheme-indent-function 0)
(put 'let 'scheme-indent-function 0)
(put 'let* 'scheme-indent-function 0)
(put 'defmacro 'scheme-indent-function 1)
(put 'defobject 'scheme-indent-function 1)
(put 'defproperty 'scheme-indent-function 1)
(put 'defmethod 'scheme-indent-function 1)
(put 'defhandler 'scheme-indent-function 1)
;; ==========================================================================
(defconst nujel-function-names-regexp (regexp-opt '("defun" "defn" "def" "fn" "defmacro" "defobject" "defmethod" "defproperty" "defhandler")))
;; ==========================================================================
;; 2008-03-12 Jeff Ober: updated with 9.3.3's symbols [(map string (symbols))]
;; ==========================================================================
(defun nujel-indent-and-move-next ()
  "NOTE: Indentation is done via Lisp indentation rules.  Not 'default-tab-width."
  (lisp-indent-line)
  (forward-line 1))
;; ==========================================================================
(defun nujel-indent-and-move-back ()
  "NOTE: Indentation is done via Lisp indentation rules.  Not 'default-tab-width."
  (lisp-indent-line)
  (forward-line -1))
;; ==========================================================================
(defun nujel-prev-opening-parens ()
  "Move Point to the first previous opening bracket."
  (re-search-backward "\\["))
;; ==========================================================================
(defun nujel-next-opening-parens ()
  "Move Point to the next opening bracket."
  (if (eq (char-after) 40)
      (forward-char 1))
  (re-search-forward "\\[")
  (backward-char 1))
;; ==========================================================================
(defun nujel-sexp-start ()
  "Move point to nearest opening parens."
  (interactive)
  (if (not (eq (char-after) 40))
      (re-search-backward "\\[")))
;; ==========================================================================
(defun nujel-sexp-end()
  "Move point to nearest closing parens."
  (interactive)
  (re-search-forward "\\]"))
;; ==========================================================================
;;  Inferior process functions and constants
;; ==========================================================================
(defun nujel-select-sexp ()
  "Select the innermost sexp (closest to cursor)."
  (interactive)
  (nujel-sexp-start)
  (set-mark (point))
  (forward-sexp))
;; ==========================================================================
(defun nujel-select-function ()
  "Select enclosing or previous function if Point is not inside of one.
Cursor is moved to end of function."
  (interactive)
  (let ((found nil))
    (cond ((nujel-at-function-startp) (setq found t))
	  ((nujel-previous-functionp) (setq found t)))
    (cond (found (set-mark (point))
		 (forward-sexp))
	  (t (message "No enclosing or previous function to select")))))
;; ==========================================================================
(defun nujel-evaluate-function ()
  "Evaluate the enclosing (or previous) definition."
  (interactive)
  (save-excursion
    (let ((found nil))
      (cond ((nujel-at-function-startp)
	     (setq found t))
	    ((nujel-previous-functionp)
	     (setq found t)))
      (cond (found (forward-sexp)
                   (nujel-evaluate-prev-sexp))
	    (t (message
		"No enclosing or previous definition to select for evaluation"))))))
;; ==========================================================================
(defun nujel-evaluate-buffer()
  "Tell the inferior process to load the current buffer.
Uses the nujel 'load command."
  (interactive)
  (process-send-string
   nujel-process-name
   (concat "[load \"" (buffer-file-name) "\"]\n")))
;; ==========================================================================
;; CONTRIB: frontera000 provided 'nujel-surround-cmds
;; Maintainer: Wrapped code in 'let form
;; Functionality: Cleaner interpreter window.
;; ==========================================================================
(defun nujel-evaluate-region (beg end)
  "Send the region from BEG to END to the inferior Nujel, removing newlines."
  (interactive "r")
  (let ((str
         (nujel-surround-cmds
          (buffer-substring-no-properties beg end))))
    (process-send-string
     nujel-process-name str)))
;; ==========================================================================
;; CONTRIB: frontera000. Code evaluated, not displayed in interpreter window
;; ==========================================================================
(defun nujel-surround-cmds (str)
  "Provide 'cmd directive for code in STR."
  (concat "\n[cmd]\n" str "\n[/cmd]\n"))
;; ==========================================================================
(defun nujel-evaluate-prev-sexp()
  "Send the previous sexp to the inferior Scheme process.
Newlines removed."
  (interactive)
  (nujel-evaluate-region
   (save-excursion (backward-sexp) (point)) (point)))
;; =====================================================================================
(defcustom nujel-comment-prefix ";;"
  "*String used by \\[comment-region] to comment out a block of code."
  :type 'string
  :group 'nujel)
;; ==========================================================================
(defun nujel-previous-functionp ()
  "Look for the preceding function definition.
Move there and return t if found.
Reset to starting point and return nil if not found."
  (interactive)
  (let (res (start (point)))
    (setq res (re-search-backward nujel-function-begin-regexp nil 'move))
    (cond (res (if (nujel-at-function-startp)
		   (setq res t)
		 (goto-char start)
		 (setq res nil)))
	  (t (goto-char start)
	     (setq res nil)))
    res))
;; ==========================================================================
(defun nujel-next-functionp ()
  "Look for next function definition.
Move there and return t if found.
Reset to starting point and return nil if not found."
  (interactive)
  (if (or (eq 40 (char-after))
          (eq 91 (char-after)))
      (forward-char 1))
  (let (res (start (point)))
    (setq res
	  (re-search-forward nujel-function-begin-regexp nil 'move))
    (cond (res (re-search-backward "\\[")
	       (if (nujel-at-function-startp)
		   (setq res t)
		 (goto-char start)
		 (setq res nil)))
	  (t (goto-char start) ;; go back to where we started
	     (setq res nil)))
    res))
;; ==========================================================================
(defun nujel-previous-location()
  "Move point backwards to the beginning of the nearest function definition."
  (interactive)
  (let (res)
    (setq res (nujel-previous-functionp))
    (if (not res)
	(message "No previous function"))))
;; ==========================================================================
(defun nujel-next-location()
  "Move point backwards to the beginning of the nearest function definition."
  (interactive)
  (let (res)
    (setq res (nujel-next-functionp))
    (if (not res)
	(message "No function found while searching forward."))))
;; ===============================================================================================
;; Inferior process
;; ===============================================================================================
(defun nujel-clear-comint-buffer ()
  "Clear the Interpreter input/output window."
  (interactive)
  (nujel-visit-interpreter)
  (let (begin end)
    (goto-char (point-min))
    (setq begin (point))
    (goto-char (point-max))
    (setq end (point))
    (delete-region begin end)
    (other-window 1)))
;; ===============================================================================================
(defun nujel-show-interpreter()
  "Start and/or show interpreter in other window.
Cursor stays at point."
  (interactive)
  (switch-to-buffer-other-window
   (make-comint nujel-process-name nujel-binary-name))
  (other-window -1))
;; ===============================================================================================
(defun nujel-visit-interpreter()
  "Start and/or show interpreter in other window.
Then, put cursor in other window."
  (interactive)
  (switch-to-buffer-other-window
   (make-comint nujel-process-name nujel-binary-name)))
;; ==========================================================================
(defun nujel-delete-sexp ()
  "Delete outermost enclosing sexp."
  (interactive)
  (cond
   ((eq (char-after) 40)   ;; cursor on '('
    (kill-sexp 1))
   ((eq (char-after) 41)   ;; cursor on ')'
    (forward-char 1)
    (backward-sexp)
    (kill-sexp 1))
   (t (nujel-sexp-start) ;; find nearest preceding '('
      (kill-sexp 1))))
;; ==========================================================================
;; CONTRIB: Jeff Ober - allows selection by list and evaluation by list like
;; tuareg mode.
;; ==========================================================================
(defun nujel-list-open ()
  "We assume the nearest opening bracket at the start of a line to be the root."
  (interactive)
  (if (not (bobp)) (re-search-backward "^\\[")))

(defun nujel-list-close ()
  "Find the current list's closing paren."
  (interactive)
  (nujel-sexp-start)
  (forward-char 1)
  (let ((openers 1) (closers 0))
    (while (and (not (eobp)) (> openers closers))
      (cond
       ((eq (following-char) ?\[) (setq openers (+ 1 openers)))
       ((eq (following-char) ?\]) (setq closers (+ 1 closers))))
      (forward-char 1))))

(defun nujel-select-list ()
  "Select the current list."
  (interactive)
  (beginning-of-line)
  (forward-char 1)
  (nujel-list-open)
  (set-mark (point))
  (nujel-list-close))

(defun nujel-incremental-eval ()
  "Evaluate the current list that rooted at the beginning of a line.
Then move on to the next like Tuareg mode."
  (interactive)
  (nujel-select-list)
  (let ((b (mark)) (e (point)))
   (deactivate-mark)
    (nujel-evaluate-region b e)
    (re-search-forward "\\[")
    (backward-char 1)))
;; ==========================================================================
(defconst nujel-function-regexp
  (regexp-opt '("defn" "defun" "fn" "defmacro" "defhandler" "defobject" "defmethod" "defproperty"))
  "Nujel function names.")
;; =====================================================================================
(defconst nujel-keywords-regexp
  (regexp-opt '("def" "fn" "defn" "defmethod" "defobject" "defproperty" "defhandler"
                "while" "do" "for" "for-in" "return" "try" "throw" "eval-in" "apply"
                "list" "fmt" "fmtln" "pfmt" "pfmtln" "efmtln" "efmt" "ref"
                "defmacro" "map" "for-each" "read" "read/single" "if" "cond" "and" "or"
                "case" "type-of" "->" "->>" "cons" "car" "cdr" "cadr" "cddr" "when" "when-not" "if-not")))

(defvar nujel-font-lock-keywords
  `(,@scheme-font-lock-keywords  ;; note: backquote and splice operator!
    ;; add new keywords for highlighting in our sample face
    (,(concat "\\<\\(" nujel-keywords-regexp "\\)\\>")  ;; builtin keywords + word boundaries
     0 font-lock-keyword-face)  ;; removed 't as last argument
    (,(concat "\\<\\(" nujel-function-names-regexp "\\)\\>")  ;; function keywords + word boundaries
     0 font-lock-function-name-face t)
    ;; Multi-line string highlighting. HINT: use ctrl-c f to refontify
    ;;   NOTE: emacs does not handle multi-line string well in this manner.
    ;;     (JB) suggests looking at how perl and AUCTex handle this.
                                        ;("[^#]\\({[^{}]*}\\)" 0 'font-lock-string-face) ;; braces, {}
    ;;("[^#]\\({[^{}]*}\\)" 0 font-lock-string-face t) ; long string
    ;;("[^#]\\(\\[text\\][^{}]*\\[/text\\]\\)" 0 'font-lock-string-face t) ;; [text] [/text]
    ("[A-Za-z0-9-/-_*]*!" 0 'font-lock-builtin-face) ;; Setters and other symbols that produced side-effects
    ("[A-Za-z0-9-/_*]*[?]" 0 'font-lock-type-face) ;; Predicates
    ("#t" 0 'font-lock-keyword-face)
    ("#f" 0 'font-lock-builtin-face)
    ("#nil" 0 'font-lock-comment-face)
    ("#$[0-9a-fA-F]*" 0 'font-lock-function-name-face)
    (":[A-Za-z0-9-/_*]+" 0 'font-lock-constant-face)
    ("'[A-Za-z0-9-/_*]+" 0 'font-lock-constant-face)
    ("\\(^\\|[^\$\\\]\\);.*" 0 'font-lock-comment-face t) ;; `;;' comments
    )
  "List of nujel keywords and faces.")
;; ==========================================================================
; Construct a keymap for the mode.
;; ==========================================================================
(defvar nujel-mode-map
  (let ((map (make-sparse-keymap))) ;; c-h make-sparse-keymap <RET>
    ;; Here we may define any number of key sequences for our mode
    ;; c-h define-key <RET>
    (define-key map (kbd "C-c C-y") 'nujel-show-interpreter)
    (define-key map (kbd "C-c C-z") 'nujel-visit-interpreter)
    (define-key map (kbd "C-c M-o") 'nujel-clear-comint-buffer)
    ;; --------------------------------------------------------------
    (define-key map (kbd "C-c C-b") 'nujel-evaluate-buffer)
    (define-key map (kbd "C-x C-e") 'nujel-evaluate-prev-sexp)
    (define-key map (kbd "C-c C-r") 'nujel-evaluate-region)
    (define-key map (kbd "C-c C-c") 'nujel-evaluate-function)
    ;; -----------------------------------------------------------------------
    (define-key map (kbd "C-M-q") 'nujel-indent-sexp)
    (define-key map (kbd "C-M-k") 'nujel-delete-sexp)
    ;;(define-key map [(control c) (control i) (\;)] 'nujel-context-qualify)
    ;; -----------------------------------------------------------------------
    (define-key map (kbd "C-M-.") 'nujel-next-location)
    (define-key map (kbd "C-M-,") 'nujel-previous-location)
    (define-key map (kbd "C-,") 'nujel-sexp-start)
    (define-key map (kbd "C-.") 'nujel-sexp-end)
    ;; -----------------------------------------------------------------------
    (define-key map (kbd "C-M-SPC") 'nujel-select-sexp)
    (define-key map (kbd "C-M-h") 'nujel-select-function)
    (define-key map (kbd "C-x C-;") 'comment-or-uncomment-region)
    ;; -----------------------------------------------------------------------
    ;; 2008-03-12 Jeff Ober: a few more simple shortcuts
    ;; -----------------------------------------------------------------------
    (define-key map (kbd "C-c C-e") 'nujel-incremental-eval)
    ;; -----------------------------------------------------------------------
    map)
  "Keymap for `nujel-mode'.")
;; ==========================================================================
;; Define the menu using 'easy-menu-define for
;; best compatibility for both forks.
;; ==========================================================================
(easy-menu-define    ;; c-h f easy-menu-define <RET>
  nujel-menu nujel-mode-map "Nujel Mode Menu."
  '("Nujel"
    ["Show Interpreter" nujel-show-interpreter]
    ["Visit Interpreter" nujel-visit-interpreter]
    ["Clear Interpreter" nujel-clear-comint-buffer]
    ["Evaluate Buffer" nujel-evaluate-buffer]
    ["Evaluate Region" nujel-evaluate-region]
    ["Evaluate Prev Sexp" nujel-evaluate-prev-sexp]
    ["Evaluate Function" nujel-evaluate-function]
    ["Evaluate Sexp" nujel-incremental-eval]
    "-" ;; seperator
    ("Text Operations"  ;; submenu
     ["Delete Sexp" nujel-delete-sexp]
     ["Select Sexp" nujel-select-sexp]
     ["Context" nujel-context-qualify])
    "-" ;; seperator
    ["Next function" nujel-next-function]
    ["Previous function" nujel-previous-function]
    ["Nearest Start of Sexp" nujel-sexp-start]
    ["Nearest End of Sexp" nujel-sexp-end]
    ["Forward Sexp" forward-sexp]
    ["Backward Sexp" backward-sexp]
    "-" ;; seperator
    ["Select function" nujel-select-function]
    ["Select Sexp" nujel-select-sexp]
    ["Comment or uncomment region or line" comment-or-uncomment-region]))
;; ==========================================================================
(define-derived-mode nujel-mode scheme-mode "nujel"
  "A major mode for Nujel."
  (imenu-add-menubar-index)    ;; install imenu with title "Index"
  (setq imenu-sort-function 'imenu--sort-by-name)  ;; alternatively: 'imenu--sort-by-position
  (run-hooks 'nujel-mode-hook)
  (use-local-map nujel-mode-map)
  ;; Highly Recommended: c-h v font-lock-keywords <RET>
  (set (make-local-variable 'font-lock-defaults)
       (cons 'nujel-font-lock-keywords
             (or (cdr font-lock-defaults)
                 '(nil t ;; syntax table modifications follow: You may wish to use
                       ;; For help: C-h f modify-syntax-entry <RET>
                       ;; Bind non-alpha characters to the 'word' syntax class
                       ((?+ . "w") (?- . "w") (?* . "w") (?/ . "w")
                        (?. . "w") (?< . "w") (?> . "w") (?= . "w")
                        (?? . "w") (?$ . "w") (?% . "w") (?_ . "w")
                                        ;(?& . "w") (?~ . "w") (?^ . "w") (?: . "w"))))))
                        (?& . "w") (?~ . "w") (?^ . "w") )))))
  ;; NOTE: Emacs accepts a more compact approach.
  ;; The cons-cell list approach used here is for XEmacs compatibility.
  (define-key scheme-mode-map [menu-bar scheme] nil))

(add-to-list 'auto-mode-alist '("\\.nuj\\'" . nujel-mode))
(add-to-list 'auto-mode-alist '("\\.no\\'" . nujel-mode))
(provide 'nujel-mode)
;;; nujel-mode.el ends here
