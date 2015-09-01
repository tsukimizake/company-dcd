
;;; Commentary:
;; use helm's obsolete type-attribute `file-line'.
;; This code is from http://rubikitch.com/f/mylisp-helm-file-line.el , which is released as a free software.

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


;;; Code:

(require 'helm-plugin)
(require 'helm-utils)


(defun company-dcd--helm-display-to-real-numbered-line (candidate)
  "This is used to display a line in occur style in helm sources.
e.g \"    12:some_text\".
It is used with type attribute 'line'."
  (if (string-match "^ *\\([0-9]+\\):\\(.*\\)$" candidate)
      (list (string-to-number (match-string 1 candidate))
            (match-string 2 candidate))
    (error "Line number not found")))

;;; Type attributes
;;
;;
(define-helm-type-attribute 'company-dcd-line
    '((display-to-real . company-dcd--helm-display-to-real-numbered-line)
      (action ("Go to Line" . company-dcd--helm-action-line-goto)))
  "LINENO:CONTENT string, eg. \"  16:foo\".

Optional `target-file' attribute is a name of target file.

Optional `before-jump-hook' attribute is a function with no
arguments which is called before jumping to position.

Optional `after-jump-hook' attribute is a function with no
arguments which is called after jumping to position.

If `adjust' attribute is specified, searches the line whose
content is CONTENT near the LINENO.

If `recenter' attribute is specified, the line is displayed at
the center of window, otherwise at the top of window.")

(define-helm-type-attribute 'company-dcd-file-line
    `((filtered-candidate-transformer company-dcd--helm-filtered-candidate-transformer-file-line)
      (multiline)
      (action ("Go to" . company-dcd--helm-action-file-line-goto)))
  "FILENAME:LINENO:CONTENT string, eg. \"~/.emacs:16:;; comment\".

Optional `default-directory' attribute is a default-directory
FILENAME is interpreted.

Optional `before-jump-hook' attribute is a function with no
arguments which is called before jumping to position.

Optional `after-jump-hook' attribute is a function with no
arguments which is called after jumping to position.

If `adjust' attribute is specified, searches the line whose
content is CONTENT near the LINENO.

If `recenter' attribute is specified, the line is displayed at
the center of window, otherwise at the top of window.")


(helm-document-attribute 'default-directory "type . file-line"
  "  `default-directory' to interpret file.")

(helm-document-attribute 'before-jump-hook "type . file-line / line"
  "  Function to call before jumping to the target location.")

(helm-document-attribute 'after-jump-hook "type . file-line / line"
  "  Function to call after jumping to the target location.")

(helm-document-attribute 'adjust "type . file-line"
  "  Search around line matching line contents.")

(helm-document-attribute 'recenter "type . file-line / line"
  "  `recenter' after jumping.")

(helm-document-attribute 'target-file "type . line"
  "  Goto line of target-file.")

(defun company-dcd--helm-action-line-goto (lineno-and-content)
  (apply #'company-dcd--helm-goto-file-line
         (append lineno-and-content
                 (list (helm-interpret-value (helm-attr 'target-file))
                       (if (and (helm-attr-defined 'target-file)
                                (not helm-in-persistent-action))
                           'find-file-other-window
                         'find-file)))))

(cl-defun company-dcd--helm-action-file-line-goto (file-line-content)
  (apply #'company-dcd--helm-goto-file-line
         (if (stringp file-line-content)
             ;; Case: filtered-candidate-transformer is skipped
             (cdr (company-dcd--company-dcd--helm-filtered-candidate-transformer-file-line-1
                   file-line-content))
           file-line-content)))

(defun company-dcd--helm-filtered-candidate-transformer-file-line (candidates _source)
  (delq nil (mapcar 'company-dcd--company-dcd--helm-filtered-candidate-transformer-file-line-1
                    candidates)))

(defun company-dcd--company-dcd--helm-filtered-candidate-transformer-file-line-1 (candidate)
  (when (string-match "^\\(.+?\\):\\([0-9]+\\):\\(.*\\)$" candidate)
    (let ((filename (match-string 1 candidate))
          (lineno (match-string 2 candidate))
          (content (match-string 3 candidate)))
      (cons (format "%s:%s\n %s"
                    (propertize filename 'face compilation-info-face)
                    (propertize lineno 'face compilation-line-face)
                    content)
            (list (string-to-number lineno) content
                  (expand-file-name
                   filename
                   (or (helm-interpret-value (helm-attr 'default-directory))
                       (and (helm-candidate-buffer)
                            (buffer-local-value
                             'default-directory (helm-candidate-buffer))))))))))

(cl-defun company-dcd--helm-goto-file-line (lineno &optional content file (find-file-function #'find-file))
  (helm-aif (helm-attr 'before-jump-hook)
      (funcall it))
  (when file (funcall find-file-function file))
  (if (helm-attr-defined 'adjust)
      (company-dcd--helm-goto-line-with-adjustment lineno content)
    (helm-goto-line lineno))
  (unless (helm-attr-defined 'recenter)
    (set-window-start (get-buffer-window helm-current-buffer) (point)))
  (helm-aif (helm-attr 'after-jump-hook)
      (funcall it))
  (when helm-in-persistent-action
    (helm-highlight-current-line)))

(defun company-dcd--helm-goto-line-with-adjustment (line line-content)
  (let ((startpos)
        offset found pat)
    ;; This constant is 1/2 the initial search window.
    ;; There is no sense in making it too small,
    ;; since just going around the loop once probably
    ;; costs about as much as searching 2000 chars.
    (setq offset 1000
          found nil
          pat (concat (if (eq selective-display t)
                          "\\(^\\|\^m\\) *" "^ *") ;allow indent
                      (regexp-quote line-content)))
    ;; If no char pos was given, try the given line number.
    (setq startpos (progn (helm-goto-line line) (point)))
    (or startpos (setq startpos (point-min)))
    ;; First see if the tag is right at the specified location.
    (goto-char startpos)
    (setq found (looking-at pat))
    (while (and (not found)
                (progn
                  (goto-char (- startpos offset))
                  (not (bobp))))
      (setq found
            (re-search-forward pat (+ startpos offset) t)
            offset (* 3 offset)))       ; expand search window
    (or found
        (re-search-forward pat nil t)
        (error "not found")))
  ;; Position point at the right place
  ;; if the search string matched an extra Ctrl-m at the beginning.
  (and (eq selective-display t)
       (looking-at "\^m")
       (forward-char 1))
  (forward-line 0))

(provide 'company-dcd-helm-file-type)
;;; company-dcd-helm-file-type.el ends here

