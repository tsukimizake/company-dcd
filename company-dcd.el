;;; company-dcd.el --- Company backend for Dlang using DCD.

;; Author: tsukimizake <shomasd_at_gmail.com>
;; Version: 0.1
;; Package-Requires: ((company "0.9") (flycheck-dmd-dub "0.7") (yasnippet "0.8") (popwin "0.7") (cl-lib "0.5") (helm "1.5.6"))
;; Keywords: languages
;; URL: http://github.com/tsukimizake/company-dcd

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Company backend for DCD.

;;; Usage:

;; (add-to-list 'exec-path "/path/to/DCD/bin")
;; (require 'company-dcd)
;; (add-hook 'd-mode-hook 'company-dcd-mode)


;;; Code:

(require 'company)
(require 'rx)
(require 'yasnippet)
(require 'flycheck-dmd-dub)
(require 'ring)
(require 'cl-lib)
(require 'popwin)
(require 'helm)
(require 'company-dcd-helm-file-type)

(defgroup company-dcd nil "company-mode backend for DCD." :group 'company)

(defcustom company-dcd-client-executable
  "dcd-client"
  "Location of dcd-client executable."
  :group 'company-dcd
  :type 'file)

(defcustom company-dcd--flags nil
  "Extra flags to pass to the dcd-server.
This variable will typically contain include paths,
e.g., (\"-I~/MyProject\", \"-I.\").
You can't put port number flag here.  Set `company-dcd--server-port' instead."
  :group 'company-dcd
  :type '(repeat (string :tag "Argument" "")))

(defconst company-dcd--completion-pattern
  (rx bol (submatch (1+ nonl)) "\t" (submatch (any "cisuvmkfgePMaAltT")) eol)
  "Regex to parse dcd output.
\\1 is candidate itself, \\2 is kind of candidate.")

(defconst company-dcd--error-buffer-name "*dcd-error*")
(defconst company-dcd--output-buffer-name "*dcd-output*")
(defconst company-dcd--document-buffer-name "*dcd-document*")
(defcustom company-dcd-server-executable
  "dcd-server"
  "Location of dcd-server executable."
  :group 'company-dcd
  :type 'file)

(defcustom company-dcd--server-port 9166
  "Port number of dcd-server.  default is 9166."
  :group 'company-dcd)

(defvar company-dcd--delay-after-kill-process 200
  "Duration after killing server process in milli second.
If `company-dcd-restart-server' doesn't work correctly, please set bigger number for this variable.")

(defvar company-dcd--version nil
  "Version of dcd server.  This variable is automatically set when company-dcd--get-version is called.")

(defcustom company-dcd--ignore-template-argument nil
  "If non-nil, ignore template argument of calltip candidate."
  :group 'company-dcd)

;;server handle functions

(defun company-dcd-stop-server ()
  "Stop dcd-server manually.  Ordinary, you don't have to call it.
If you want to restart server, use `company-dcd-restart-server' instead."
  (interactive)
  (interrupt-process "dcd-server"))

(defsubst company-dcd--start-server ()
  "Start dcd-server."

  (unless (executable-find company-dcd-server-executable)
    (error "company-dcd error! dcd-server is not found."))
  
  (let ((buf (get-buffer-create "*dcd-server*")))
    (with-current-buffer buf (apply 'start-process "dcd-server" (current-buffer)
				    company-dcd-server-executable
				    "-p"
				    (format "%s" company-dcd--server-port)
				    company-dcd--flags
				    ))))

(defun company-dcd--server-is-alive-p ()
  "If dcd-server is alive, return t.  Else, nil."
  (if (or (get-process "dcd-server") (not (zerop (string-to-number (shell-command-to-string "pidof dcd-server")))))
      t
    nil))

(defun company-dcd-maybe-start-server ()
  "Start dcd-server.  When the server process is already running, do nothing."
  (unless (company-dcd--server-is-alive-p)
    (company-dcd--start-server)))

(defun company-dcd-restart-server ()
  "Start dcd-server.  When the server process is already running, restart it."
  (interactive)
  (when (company-dcd--server-is-alive-p)
    (company-dcd-stop-server)
    (sleep-for 0 company-dcd--delay-after-kill-process))
  (company-dcd--start-server)
  (setq company-dcd--version nil))

(defun company-dcd--get-version ()
  "Get dcd version.  If company-dcd--version is set, use it as a cache."
  (if company-dcd--version
      company-dcd--version
    (progn
      (company-dcd--call-process '("--version"))
      (let* ((buf (get-buffer company-dcd--output-buffer-name))
	     (str (with-current-buffer buf (buffer-string)))
	     verstr)
	(string-match (rx "v" (submatch (* nonl)) (or "-" "\n")) str)
	(setq verstr (match-string 1 str))
	(setq company-dcd--version (string-to-number verstr))
	))))

;; output parser functions

(defun company-dcd--parse-output-for-completion ()
  "Parse dcd output of normal completion."
  (with-current-buffer company-dcd--output-buffer-name
    (goto-char (point-min))
    (let ((pattern company-dcd--completion-pattern)
	  lines match detailed-info
	  )
      (while (re-search-forward pattern nil t)
	(setq match (match-string-no-properties 1))
	
	(setq detailed-info (match-string-no-properties 2))
	(when detailed-info
	  (setq match (propertize match 'company-dcd--help detailed-info)))
	(push match lines))
      lines)))

(defun company-dcd--get-help (cand)
  "Get type of candidate `CAND'."
  (get-text-property 0 'company-dcd--help cand))

(defvar company-dcd--error-message-regexp
  (rx (and (submatch (* nonl))  ": " (submatch (* nonl)) ": " (submatch (* nonl) eol)))
  "If it matches first line of dcd-output, it would be error message.")

(defun company-dcd--handle-error (res args)
  "Notify error with result RES and arguments ARGS."
  (let* ((errbuf (get-buffer-create company-dcd--error-buffer-name))
         (outbuf (get-buffer company-dcd--output-buffer-name))
         (cmd (concat company-dcd-client-executable " " (mapconcat 'identity args " ")))
         (errstr
          (with-current-buffer outbuf
            (goto-char (point-min))
            (re-search-forward company-dcd--error-message-regexp)
            (concat
             (match-string 2) " : " (match-string 3)))
          ))
    (with-current-buffer errbuf
      (erase-buffer)
      (insert (current-time-string)
              "\n\"" cmd "\" failed."
              (format "\nError type is: %s\n" errstr)
              )
      (goto-char (point-min)))
    (display-buffer errbuf)))

(defun company-dcd--output-buf-string ()
  "Return contents of dcd-output buffer."
  (with-current-buffer company-dcd--output-buffer-name
    (buffer-string)))

;; utility functions to call process
(defun company-dcd--call-process (args)
  "Call dcd-client with ARGS.  Output string is inserted to dcd-output buf."
  (let ((buf (get-buffer-create company-dcd--output-buffer-name))
        res)
    (with-current-buffer buf (erase-buffer))
    (setq res (if (null (executable-find company-dcd-client-executable))
                  (progn
                    (message "company-dcd error: could not find dcd-client executable")
                    0)
		(apply 'call-process-region (point-min) (point-max)
		       company-dcd-client-executable nil buf nil args)))
    (with-current-buffer buf
      (unless (eq 0 res)
        (company-dcd--handle-error res args))
      )))

(defsubst company-dcd--cursor-position ()
  "Get cursor position to pass to dcd-client."
  (position-bytes (point)))

(defsubst company-dcd--build-args (&optional pos)
  "Build argument list to pass to dcd-client for position POS.
If pos was not provided or nil, it will do what you mean."
  (if pos 
      (list
       "-c"
       (format "%s" pos)
       "-p"
       (format "%s" company-dcd--server-port)
       )
    (list
     "-p"
     (format "%s" company-dcd--server-port)
     )))

(defsubst company-dcd--in-string/comment ()
  "Return non-nil if point is in a literal (a comment or string)."
  (nth 8 (syntax-ppss)))

(defsubst company-dcd--adjust-cursor-on-completion (point)
  "If it was not member completion, goto the head of query before call process.
`POINT' is the point to complete in D src."

  ;; I'm not sure if it is exactly 0.4. If the completion doesn't work on older dcd, please report.
  (when (> 0.4 (company-dcd--get-version))
    (return))
  
  (while (not (string-match (rx (or blank "." "\n")) (char-to-string (char-before (point)))))
    (backward-char)))

;; Interface functions to company-mode.
(defun company-dcd--get-candidates ()
  "Get ordinary auto-complete candidates."
  (unless (company-dcd--in-string/comment)
    (save-restriction
      (widen)
      (save-excursion
	(company-dcd--adjust-cursor-on-completion (point))
	(company-dcd--call-process
	 (company-dcd--build-args (company-dcd--cursor-position))))
      (company-dcd--parse-output-for-completion))))

(defun company-dcd--document (item)
  "Return popup document of `ITEM'."
  (if (stringp item)
      (let ((s (company-dcd--get-help item)))
        (cond
	 ((equal s "c") "class name")
	 ((equal s "i") "interface name")
	 ((equal s "s") "struct name")
	 ((equal s "u") "union name")
	 ((equal s "v") "variable name")
	 ((equal s "m") "member variable name")
	 ((equal s "k") "keyword, built-in version, scope statement")
	 ((equal s "f") "function or method")
	 ((equal s "g") "enum name")
	 ((equal s "e") "enum member")
	 ((equal s "P") "package name")
	 ((equal s "M") "module name")
	 ((equal s "a") "array")
	 ((equal s "A") "associative array")
	 ((equal s "l") "alias name")
	 ((equal s "t") "template name")
	 ((equal s "T") "mixin template name")
         (t (format "candidate kind undetected: %s" s))
         ))))

;; Q: Why run-with-timer?
;; A: See https://github.com/company-mode/company-mode/issues/320
(defun company-dcd--action (lastcompl)
  "Exec calltip expansion."
  (let ((candidate-type (company-dcd--get-help lastcompl)))
    (cond
     ((string= "f" candidate-type) ; when it was a function
      (run-with-idle-timer 0 nil (lambda ()
				   (company-begin-backend 'company-dcd--calltips)
				   (let ((this-command 'company-idle-begin))
				     (company-post-command)))))
     ((string= "s" candidate-type) ; when it was a struct
      (run-with-idle-timer 0 nil (lambda ()
				   (company-begin-backend 'company-dcd--calltips-for-struct-constructor)
				   (let ((this-command 'company-idle-begin))
				     (company-post-command))))
      )
     ((string= "c" candidate-type) ; when it was a class
      (run-with-idle-timer 0 nil (lambda ()
				   (company-begin-backend 'company-dcd--calltips-for-struct-constructor)
				   (let ((this-command 'company-idle-begin))
				     (company-post-command))))
      ))))

(defvar company-dcd-mode nil)

(defun company-dcd (command &optional arg &rest ignored)
  "company-mode backend for DCD."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-dcd))
    (prefix (and company-dcd-mode (company-grab-symbol)))
    (candidates (company-dcd--get-candidates))
    (annotation (format " %s" (company-dcd--get-help arg)))
    (meta (company-dcd--document arg))
    (post-completion (company-dcd--action arg))))


;; function calltip expansion with yasnippet
(defun company-dcd--get-calltip-candidates ()
  "Return calltip completion candidates of the Dlang symbol at point.
The cursor must be at the end of a Dlang symbol.
When the symbol is not a function, return nil."
  (let ((buf (get-buffer-create company-dcd--output-buffer-name)))
    (company-dcd--call-process-for-calltips)
    (with-current-buffer buf (company-dcd--parse-calltips))
    ))

(defun company-dcd--call-process-for-calltips ()
  "Call process to get calltips of the function at point."
  (insert "( ;")
  (backward-char 2)

  (company-dcd--call-process
   (company-dcd--build-args (company-dcd--cursor-position)))

  (forward-char 2)
  (delete-char -3)
  )


(defconst company-dcd--normal-calltip-pattern
  (rx bol (submatch (* nonl)) (submatch "(" (* nonl) ")") eol)
  "Regexp to parse calltip completion.
\\1 is function return type (if exists) and name, and \\2 is args.")
(defconst company-dcd--template-pattern (rx (submatch (* nonl)) (submatch "(" (*? nonl) ")") (submatch "(" (* nonl)")"))
  "Regexp to parse template calltips.  
\\1 is function return type (if exists) and name, \\2 is template args, and \\3 is args.")
(defconst company-dcd--calltip-pattern
  (rx  (or (and bol (* nonl) "(" (* nonl) ")" eol)
	   (and bol (* nonl) "(" (*? nonl) ")" "(" (* nonl)")" eol))))
(defcustom company-dcd--ignore-template-argument t
  "If non-nil, ignore template argument on calltip expansion."
  :group 'company-dcd)

(defsubst company-dcd--cleanup-function-candidate (s)
  "This is a helper function for parser.
Remove return type of the head of the function.
`S' is candidate string."
  (let (res)
    (with-temp-buffer
      (insert s)

      ;;goto beggining of function name
      (progn
        (end-of-line)
        (backward-sexp)
        (re-search-backward (rx (or bol " "))))

      (setq res (buffer-substring
                 (point)
                 (progn
                   (end-of-line)
                   (point))))
      (when (equal " " (substring res 0 1))
        (setq res (substring res 1)))
      res
      )))

(defsubst company-dcd--cleanup-template-candidate (s)
  "This is a helper function for parser.
Remove return type of the head of the template function.
`S' is candidate string."
  (let (res)
    (with-temp-buffer
      (insert s)

      ;;goto beggining of function name
      (progn
        (end-of-line)        (backward-sexp)
	(backward-sexp)
        (re-search-backward (rx (or bol " "))))

      (setq res (buffer-substring
                 (point)
                 (progn
                   (end-of-line)
                   (point))))
      (when (equal " " (substring res 0 1))
        (setq res (substring res 1)))
      res
      )))

(defsubst company-dcd--candidate-is-tempalte-p (s)
  "This is a helper function for parser.
If candidate string `S' is a template, return t."
  (with-temp-buffer
    (insert s)
    (backward-sexp)
    (equal ")" (char-to-string (char-before)))))

(defun company-dcd--parse-calltips ()
  "Parse dcd output for calltip completion.
It returns a list of calltip candidates."
  (goto-char (point-min))
  (let ((pattern company-dcd--calltip-pattern)
        lines
        match
	)
    (while (re-search-forward pattern nil t)
      (setq match (match-string 0))
      (if (company-dcd--candidate-is-tempalte-p match)
	  (progn
	    (string-match company-dcd--template-pattern match)
	    (add-to-list 'lines (company-dcd--cleanup-function-candidate (format "%s%s" (match-string 1 match) (match-string 3 match)))) ;remove template argument
	    (unless company-dcd--ignore-template-argument
	      (string-match company-dcd--template-pattern match)
	      (add-to-list 'lines (company-dcd--cleanup-template-candidate (format "%s!%s%s" (match-string 1 match) (match-string 2 match) (match-string 3 match))))) ; candidate with template argument
	    )
	(progn
	  (string-match company-dcd--normal-calltip-pattern match)
	  (add-to-list 'lines (company-dcd--cleanup-function-candidate (format "%s%s" (match-string 1 match) (match-string 2 match))))) ; when it was not template argument
	))
    lines
    ))

(defsubst company-dcd--format-calltips (str)
  "Format calltips `STR' in parenthesis to yasnippet style."
  (let (yasstr)
    
    ;;remove parenthesis
    (setq str (substring str 1 (- (length str) 1)))

    (setq yasstr
	  (mapconcat
	   (lambda (s) "format each args to yasnippet style" (concat "${" s "}"))
	   (split-string str ", ")
	   ", "))
    (setq yasstr (concat "(" yasstr ")"))
    ))

(defun company-dcd--calltip-action (lastcompl)
  "Format and insert the calltip using yasnippet.
This function should be called at *dcd-output* buf."
  (let* ((end (point))
	 (arg-beg (save-excursion
		    (backward-sexp)
		    (point)))
	 (template-beg
	  (if (company-dcd--candidate-is-tempalte-p lastcompl)
	      (save-excursion
		(backward-sexp 2)
		(point))
	    nil))
	 (args (buffer-substring arg-beg end))
	 res)
    (delete-region arg-beg end)
    (setq res (company-dcd--format-calltips args))
    
    (when template-beg
      (let ((template-args (buffer-substring template-beg arg-beg)))
	(delete-region template-beg arg-beg)
	(setq res (format "%s%s" (company-dcd--format-calltips template-args) res))))
    (yas-expand-snippet res)))

(defun company-dcd--calltip-completion-available ()
  (if (company-dcd--get-calltip-candidates)
      (company-grab-symbol)
    nil))

(defun company-dcd--calltips (command &optional arg &rest ignored)
  "dcd calltip completion."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-dcd--calltips))
    (prefix (company-dcd--calltip-completion-available))
    (candidates
     (company-dcd--get-calltip-candidates)
     )
    (post-completion (company-dcd--calltip-action arg))
    ))

;; struct constructor calltip expansion

(defsubst company-dcd--replace-this-to-struct-name (struct-name)
  "Replace \"this\" with STRUCT-NAME.
dcd-client outputs candidates which begin with \"this\" when completing struct constructor calltips."
  (goto-char (point-min))
  (while (search-forward "this" nil t)
    (replace-match struct-name)))

(defun company-dcd--get-calltip-candidate-for-struct-constructor (lastcompl)
  "Almost the same as `company-dcd--get-calltip-candidates', but call `company-dcd--replace-this-to-struct-name' before parsing."
  (let ((buf (get-buffer-create company-dcd--output-buffer-name)))
    (company-dcd--call-process-for-calltips)
    (with-current-buffer buf
      (company-dcd--replace-this-to-struct-name lastcompl)
      (company-dcd--parse-calltips))
    ))

(defun company-dcd--calltips-for-struct-constructor (command &optional arg &rest ignored)
  "dcd calltip completion for struct constructor."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-dcd--calltips))
    (prefix (company-dcd--calltip-completion-available))
    (candidates
     (company-dcd--get-calltip-candidate-for-struct-constructor arg))
    (post-completion (company-dcd--calltip-action arg))
    ))


;;show document

(defun company-dcd--reformat-document ()
  "Currently, it just unescape \n and \\n."
  (with-current-buffer (get-buffer company-dcd--document-buffer-name)

    ;; replace '\n' with newline
    ;;doit twice to catch '\n\n'
    (goto-char (point-min))
    (while (re-search-forward (rx (and (not (any "\\")) (submatch "\\n"))) nil t)
      (replace-match "\n" nil nil nil 1))

    (goto-char (point-min))
    (while (re-search-forward (rx (and (not (any "\\")) (submatch "\\n"))) nil t)
      (replace-match "\n" nil nil nil 1))

    ;; replace '\\n' in D src to '\n'
    (goto-char (point-min))
    (while (re-search-forward (rx "\\\\n") nil t)
      (replace-match "\\\\n"))
    (goto-char (point-min))
    ))

(defun company-dcd--get-ddoc ()
  "Get document with `dcd-client --doc'."
  (save-buffer)
  (let ((args
         (append
          (company-dcd--build-args (company-dcd--cursor-position))
          '("-d")
          (list (buffer-file-name))))
        )

    (with-current-buffer (get-buffer-create company-dcd--output-buffer-name)
      (erase-buffer)
      (company-dcd--call-process args)
      (when (or
	     (string= (buffer-string) "\n\n\n")	; when symbol has no doc
	     (string= (buffer-string) "") ; when there is no symbol at cursor, or something.
	     )
	(error "No document for the symbol at point!"))
      (buffer-string))
    ))

(defun company-dcd-show-ddoc-with-buffer ()
  "Display Ddoc at point using `display-buffer'."
  (interactive)

  (let ((raw-doc (company-dcd--get-ddoc)))
    (with-current-buffer (get-buffer-create company-dcd--document-buffer-name)
      (insert raw-doc)))
  (company-dcd--reformat-document)
  (display-buffer (get-buffer-create company-dcd--document-buffer-name)))


;; goto definition
;; thanks to jedi.el by Takafumi Arakaki

(defcustom company-dcd--goto-definition-marker-ring-length 16
  "Length of marker ring to store `company-dcd-goto-definition' call positions."
  :group 'company-dcd)

(defvar company-dcd--goto-definition-marker-ring
  (make-ring company-dcd--goto-definition-marker-ring-length)
  "Ring that stores company-dcd--goto-symbol-declaration.")

(defsubst company-dcd--goto-def-push-marker ()
  "Push marker at point to goto-def ring."
  (ring-insert company-dcd--goto-definition-marker-ring (point-marker)))

(defun company-dcd-goto-def-pop-marker ()
  "Goto the point where `company-dcd-goto-definition' was last called."
  (interactive)
  (if (ring-empty-p company-dcd--goto-definition-marker-ring)
      (error "Marker ring is empty. Can't pop.")
    (let ((marker (ring-remove company-dcd--goto-definition-marker-ring 0)))
      (switch-to-buffer (or (marker-buffer marker)
                            (error "Buffer has been deleted")))
      (goto-char (marker-position marker))
      ;; Cleanup the marker so as to avoid them piling up.
      (set-marker marker nil nil))))

(cl-defstruct company-dcd--position-data file type offset)

(defun company-dcd-goto-definition ()
  "Goto declaration of symbol at point."
  (interactive)
  (save-buffer)
  (company-dcd--call-process-for-symbol-declaration)
  (let* ((data (company-dcd--parse-output-for-get-symbol-declaration))
         (file (company-dcd--position-data-file data))
         (offset (company-dcd--position-data-offset data)))
    (if (equal data '(nil . nil))
        (message "Not found")
      (progn
        (company-dcd--goto-def-push-marker)
        (unless (string=  file "stdin") ; the declaration is in the current file
          (find-file file))
        (goto-char (byte-to-position (string-to-number offset)))))))


;; utilities for goto-definition

(defun company-dcd--call-process-for-symbol-declaration ()
  "Call process for `dcd-client --symbolLocation'."
  (let ((args
         (append
          (company-dcd--build-args (company-dcd--cursor-position))
          '("-l")
          (list (buffer-file-name))))
        (buf (get-buffer-create company-dcd--output-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (company-dcd--call-process args)
      (buffer-string)
      )))

(defun company-dcd--parse-output-for-get-symbol-declaration ()
  "Parse output of `company-dcd--get-symbol-declaration'.
output is a company-dcd--position-data, whose `type' is nil."
  (let ((buf (get-buffer-create company-dcd--output-buffer-name)))
    (with-current-buffer buf
      (goto-char (point-min))
      (if (not (string= "Not found\n" (buffer-string)))
          (progn (re-search-forward (rx (submatch (* nonl)) "\t" (submatch (* nonl)) "\n"))
                 (make-company-dcd--position-data :file (match-string 1) :offset (match-string 2)))
        nil))
    ))

;;; symbol search.

(defvar company-dcd--symbol-search-pattern
  (rx (and bol (submatch (* nonl)) "\t" (submatch char) "\t" (submatch (* digit)) eol))
  "Regex pattern to parse dcd output for symbol location.")

(defun company-dcd--parse-output-for-symbol-search ()
  "Return a list of company-dcd--position-data."
  (with-current-buffer company-dcd--output-buffer-name
    (goto-char (point-min))
    (let (res)
      (while (re-search-forward company-dcd--symbol-search-pattern nil t)
	(add-to-list 'res
		     (make-company-dcd--position-data
		      :file (match-string 1)
		      :type (match-string 2)
		      :offset (string-to-number (match-string 3)))
		     ))
      res)))

(defun company-dcd--call-process-for-symbol-search (str)
  "Call DCD process to search symbol."
  (let ((args
         (append
          (company-dcd--build-args)
          '("-s")
	  (list str))))
    
    (with-current-buffer (get-buffer-create company-dcd--output-buffer-name)
      (company-dcd--call-process args))))

(defun company-dcd-symbol-search (str)
  "Search symbol using DCD with query `STR'.
Return a list of `company-dcd--position-data'."
  (company-dcd--call-process-for-symbol-search str)
  (company-dcd--parse-output-for-symbol-search))

(defun company-dcd--find-file-of-pos-data (pos-data)
  (find-file-noselect (company-dcd--position-data-file pos-data)))

(defun company-dcd--goto-char-of-pos-data (pos-data)
  (goto-char (byte-to-position (company-dcd--position-data-offset pos-data))))

(defun company-dcd--line-string-at-pos ()
    (let ((beg (point-at-bol))
	  (end (point-at-eol)))
      (buffer-substring-no-properties beg end)))


(defun company-dcd--format-helm-dcd-search-result (pos-data)
  "Format `POS-DATA' to helm's file-line candidate style."
  
  
  
  (with-current-buffer (company-dcd--find-file-of-pos-data pos-data)
    (company-dcd--goto-char-of-pos-data pos-data)
    (let ((fname (company-dcd--position-data-file pos-data))
	  (line (line-number-at-pos))
	  (str (company-dcd--line-string-at-pos)))
      
      (format "%s:%s:%s" fname line str))))

(defun company-dcd--read-query-or-region-str ()
  "If region is active, return the region string.
Else, read query."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "query: ")))

(defvar helm-c-source-company-dcd-search
  '(
    (name . "dcd-search")
    (init . (lambda ()
	      (let* ((query (company-dcd--read-query-or-region-str))
		     (res (company-dcd-symbol-search query)))
		(with-current-buffer (helm-candidate-buffer 'local)
		  (insert (mapconcat 'company-dcd--format-helm-dcd-search-result res "\n"))))))
    (candidates-in-buffer)
    (type . company-dcd-file-line)))

(defun company-dcd-helm-search-symbol ()
  "DCD symbol search with helm interface."
  (interactive)
  (company-dcd--goto-def-push-marker)
  (helm 'helm-c-source-company-dcd-search)
  (recenter))


;;; automatic add-imports.

(defun company-dcd--parent-directory (dir)
  "Return parent directory of DIR."
  (when dir
    (file-name-directory (directory-file-name (expand-file-name dir)))))

(defun company-dcd--search-file-up (name &optional path)
  "Search for file NAME in parent directories recursively."
  (let* ((tags-file-name (concat path name))
         (parent (company-dcd--parent-directory path))
         (path (or path default-directory))
         )
    (cond
     ((file-exists-p tags-file-name) tags-file-name)
     ((string= parent path) nil)
     (t (company-dcd--search-file-up name parent)))))

(defun company-dcd--find-imports-dub ()
  "Extract import flags from \"dub describe\" output."
  (let* ((basedir (fldd--get-project-dir)))
    (if basedir
	(mapcar (lambda (x) (concat "-I" x)) (fldd--get-dub-package-dirs))
      nil)))

(defun company-dcd--find-imports-std ()
  "Extract import flags from dmd.conf file."
  (let ((dmd-conf-filename
         (cl-find-if 'file-exists-p
		     (list
		      ;; TODO: the first directory to look into should be dmd's current
		      ;; working dir
		      (concat (getenv "HOME") "/dmd.conf")
		      (concat (company-dcd--parent-directory (executable-find "dmd")) "dmd.conf")
		      (concat (company-dcd--parent-directory
                	(company-dcd--parent-directory
                	(executable-find "dmd"))) "etc/dmd.conf")
		      (concat (company-dcd--parent-directory
                	(company-dcd--parent-directory
                	(executable-find "dmd"))) "etc/dmd/dmd.conf")
		      "/etc/dmd.conf"))))

    ;; TODO: this extracting procedure is pretty rough, it just searches for
    ;; the first occurrence of the DFLAGS
    (save-window-excursion
      (with-temp-buffer
        (insert-file-contents dmd-conf-filename)
        (goto-char (point-min))
        (search-forward "\nDFLAGS")
        (skip-chars-forward " =")
        (let ((flags-list (split-string (buffer-substring-no-properties
                                         (point) (line-end-position)))))
          (cl-remove-if-not (lambda (s)
			      (string-prefix-p "-I" s))
			    flags-list))))))

(defun company-dcd--add-imports ()
  "Send import flags of the current DUB project to dcd-server.

The root of the project is determined by the \"closest\" dub.json
or package.json file."
  (interactive)
  (company-dcd--call-process
   (append
    (company-dcd--find-imports-std)
    (company-dcd--find-imports-dub))))

(defvar company-dcd-mode-map (make-keymap))
(define-key company-dcd-mode-map (kbd "C-c ?") 'company-dcd-show-ddoc-with-buffer)
(define-key company-dcd-mode-map (kbd "C-c .") 'company-dcd-goto-definition)
(define-key company-dcd-mode-map (kbd "C-c ,") 'company-dcd-goto-def-pop-marker)
(define-key company-dcd-mode-map (kbd "C-c s") 'company-dcd-helm-search-symbol)

;;;###autoload
(define-minor-mode company-dcd-mode "company-backend for Dlang Completion Demon, aka DCD."
  :init-value nil
  :lighter " DCD"
  :keymap company-dcd-mode-map
  (if company-dcd-mode
      (progn (company-mode-on)
	     (yas-minor-mode-on)
	     (company-dcd-maybe-start-server)
	     (company-dcd--add-imports)
	     (add-to-list 'company-backends 'company-dcd)
	     (add-to-list 'popwin:special-display-config
			  `(,company-dcd--error-buffer-name :noselect t))
	     (add-to-list 'popwin:special-display-config
			  `(,company-dcd--document-buffer-name :position right :width 80)))
    ))

(provide 'company-dcd)
;;; company-dcd.el ends here
