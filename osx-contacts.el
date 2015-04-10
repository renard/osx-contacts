;;; osx-contacts.el --- Read contact address from Contacts.app

;; Copyright © 2013 Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>

;; Author: Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>
;; Keywords: emacs, 
;; Created: 2013-04-07
;; Last changed: 2013-11-13 12:31:24
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;; 


;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'bbdb nil t))

(defgroup osx-contacts nil
  "Configuration group for `osx-contacts'."
  :group nil)

(defcustom osx-contacts-sqlite-bin (executable-find "sqlite3")
  "Path to `sqlite3' binary file."
  :group 'osx-contacts
  :type 'string)

(defcustom osx-contacts-base
  "~/Library/Application Support/AddressBook/AddressBook-v22.abcddb"
  "Location of Contacts.app database."
  :group 'osx-contacts
  :type 'string)

(defvar osx-contacts-query
  "SELECT ZABCDRECORD.ZFIRSTNAME,
          ZABCDRECORD.ZLASTNAME,
          ZABCDRECORD.ZNICKNAME,
          (SELECT GROUP_CONCAT(ZABCDEMAILADDRESS.ZADDRESS)
                  FROM ZABCDEMAILADDRESS
                  WHERE ZABCDEMAILADDRESS.ZOWNER = ZABCDRECORD.Z_PK)
          AS EMAIL,
          (SELECT GROUP_CONCAT(ZNAME)
                  FROM ZABCDRECORD AS ZABCDR
                  WHERE ZABCDR.Z_PK IN (SELECT Z_17PARENTGROUPS1
                        FROM Z_21PARENTGROUPS
                        WHERE Z_21CONTACTS=ZABCDRECORD.Z_PK))
          AS GROUPS
    FROM ZABCDRECORD
    WHERE NOT(EMAIL IS NULL);"
  "Query to run")


(defun osx-contacts-parse-buffer ()
  "Parse the result of `osx-contacts-sync' and save result to
`bbdb-file'."
  (let* ((str (buffer-substring-no-properties
	       (point-min) (point-max))))
    (with-temp-file (if (boundp 'bbdb-file) bbdb-file "~/.bbdb")
      (insert ";; -*- mode: Emacs-Lisp; coding: utf-8; -*-\n"
	      ";;; file-format: 7\n")
      (loop for l in (split-string str "\n" t)
	    do (destructuring-bind
		   (first-name name nick email) (split-string l "|")
		 (insert
		  (format
		   "%S\n"
		   (vector first-name
			   name
			   nil ;; middle
			   (when (> (length nick) 0) (list nick)) ;; aka (list)
			   nil ;; Organization (list)
			   nil ;; phone (list of vector ["type" 0 0 0 num])
			   nil ;; address (list of vector [ "type" ("line 1"
			   ;; "line 2") "city" "State" "postcode"
			   ;; "Country"]
			   (split-string email "," t) ;; email (list)
			   nil ;; Last change
			   nil ;; ?
			   ))))))))
    

(defun osx-contacts-run-sentinel (proc change)
  "`osx-contacts-run' process sentinel."
  (when (eq (process-status proc) 'exit)
    (let ((status  (process-exit-status proc))
	  (cmd-buf (process-get proc :cmd-buf)))
      (if (not (eq 0 status))
	  (progn
	    (when (process-buffer proc)
	      (set-window-buffer (selected-window) cmd-buf))
	    (error "OSX Contacts ERROR"))
	(with-current-buffer cmd-buf
	  (osx-contacts-parse-buffer))
	(kill-buffer cmd-buf)))))

;;;###autoload
(defun osx-contacts-sync ()
  "Synchronize contacts from Contacts.app to `bbdb'.

Warning, this is a destructive operation. All your existing
contacts would be destroyed."
  (interactive)
  (let* ((cmd-line (list osx-contacts-sqlite-bin
			 (expand-file-name osx-contacts-base)
			 osx-contacts-query))
	 (cmd-buf (get-buffer-create " *OSX Contacts*"))
	 (proc (apply 'start-process (car cmd-line)
		      cmd-buf (car cmd-line) (cdr cmd-line))))
    (process-put proc :cmd-buf cmd-buf)
    (set-process-sentinel proc 'osx-contacts-run-sentinel)))



(defun osx-contacts-fetch()
  ""
  (with-temp-buffer
    (call-process
     osx-contacts-sqlite-bin nil
     (current-buffer) nil
     (expand-file-name osx-contacts-base)
     osx-contacts-query)
    (loop for l in (split-string
		    (buffer-substring-no-properties
		     (point-min) (point-max))
		    "\n" t)
	  nconc (destructuring-bind
		      (first-name name nick email groups)
		      (split-string l "|")
		  (loop for e in (split-string email "," t)
			collect
			(ietf-drums-make-address
			 (if (string= "" nick)
			     (concat first-name " " name)
			   (concat first-name " " name " (" nick ")"))
			 e))))))

(defun osx-contacts-thing-at-point-bounds-of-email-address ()
  "Return a cons of begin and end position of email address at
point, including full name."
  (save-excursion
    (let* ((search-point (point))
	   (start (re-search-backward "[:,]" (line-beginning-position) 'move))
	   (dummy (goto-char search-point))
	   (end   (re-search-forward  "[:,]" (line-end-position) t)))
      (setq start
	    (progn
	      (goto-char (if start (+ 1 start)
			   (line-beginning-position)))
	      (skip-chars-forward "[ \t]" (point-at-eol))
	      (point)))
      (unless end (setq end (line-end-position)))
      (cons start end))))
 
(defun osx-contacts-thing-at-point-email-address-maybe (&optional bounds)
  "Return full email address at point."
  (let ((bounds
	 (or bounds
	     (osx-contacts-thing-at-point-bounds-of-email-address))))
    (when (and bounds (not (= (car bounds) (cdr bounds))))
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun osx-contacts-thing-at-point-email-address (&optional bounds)
  "Return full email address at point."
  (let* ((email-address-text (osx-contacts-thing-at-point-email-address-maybe bounds)))
    (when email-address-text
      (mail-header-parse-address email-address-text))))

(defun osx-contacts-header-at-point ()
  "Find which header is at point"
  (unless (message-in-body-p)
    (save-restriction
      (save-excursion
	(mail-header-narrow-to-field)
	(goto-char (point-min))
	(let ((colon (search-forward ":" (point-at-eol) t)))
	  (when colon
	    (downcase
	     (buffer-substring-no-properties
	      (point-min) (- colon 1)))))))))

;;;###autoload
(defun osx-contacts-complete-address ()
  "Complete email address."
  (interactive)
  (if (member
	 (osx-contacts-header-at-point)
	 '("to" "cc" "bcc"))
      (let* ((bounds (osx-contacts-thing-at-point-bounds-of-email-address))
	     (email-at-point
	      ;;(or (osx-contacts-thing-at-point-email-address bounds)
	      (osx-contacts-thing-at-point-email-address-maybe bounds))
	     (email
	      (completing-read "Email: " (osx-contacts-fetch) nil t email-at-point)))
	(when email
	  (save-restriction
	    (widen)
	    (narrow-to-region (car bounds) (cdr bounds))
	    (delete-region (point-min) (point-max)) 
	    (goto-char (point-min))
	    (insert email))))
    (when (message-in-body-p)
      (indent-relative))))


;; http://www.mactech.com/articles/mactech/Vol.21/21.10/ScriptingAddressBook/index.html
(defun osx-contacts-add (&optional text)
  ""
  (interactive)
  (let* ((text (or text (osx-contacts-thing-at-point-email-address-maybe)))
	 (email-cons (mail-header-parse-address text))
	 (address (car email-cons))
	 (name (cdr email-cons))
	 (script (format "
tell application \"Contacts\"
   set thePerson to make new person with properties {first name:\"%s\", last name:\"%s\"}
   make new email at end of emails of thePerson with properties  {label:\"Work\", value:\"%s\"}
   save addressbook
end tell" name name address)))
    (do-applescript script)))
      
		  


(provide 'osx-contacts)

;; osx-contacts.el ends here
