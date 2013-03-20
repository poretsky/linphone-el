;;; linphone-contacts.el --- Linphone address book manager
;;; Author: Igor B. Poretsky <poretsky@mlbox.ru>
;;; Keywords: Linphone, web phone, voip, network sip telephone

;;{{{  Copyright

;;; Copyright (C) 2013  Igor B. Poretsky

;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; This file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ Introduction

;;; Commentary:

;;; This module provides Linphone address book management facilities.
;;; It will be automatically loaded by the main Linphone interface
;;; when necessary, so the file should be available on the load path.

;;; Code:

;;}}}
;;{{{ Requirements

(require 'custom)
(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(require 'linphone)

;;}}}
;;{{{ Customizations

(defcustom linphone-contacts-get-command "friend list"
  "Linphone command to get contact list."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-contacts-call-command-format "friend call %d"
  "Format string to construct a contact call command.
The number placeholder is to be replaced by the contact index."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-contacts-add-command-format "friend add \"%s\" %s"
  "Format string to construct a contact add command.
String placeholders are to be replaced by the name and address
of a new contact."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-contacts-delete-command-format "friend delete %d"
  "Format string to construct a contact delete command.
The number placeholder is to be replaced by the contact index."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-contacts-clear-command "friend delete all"
  "Command string to clear all contacts info."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-contacts-item-extractor "^\\*+ Friend \\([0-9]+\\) \\*+$"
  "Regular expression to parse item header in the backend supplied list."
  :type 'regexp
  :group 'linphone-backend)

(defcustom linphone-contacts-name-extractor "^name: \\(.*\\)$"
  "Regular expression to extract name info from the backend supplied list."
  :type 'regexp
  :group 'linphone-backend)

(defcustom linphone-contacts-address-extractor "^address: <\\(.*\\)>$"
  "Regular expression to extract address info from the backend supplied list."
  :type 'regexp
  :group 'linphone-backend)

;;}}}
;;{{{ Request data from the backend

(defun linphone-contacts-refresh ()
  "Refresh contacts info."
  (linphone-command linphone-contacts-get-command)
  (setq linphone-contacts-requested t))

;;}}}
;;{{{ Fill out contacts list

(defvar linphone-contacts-list nil
  "Address book content.")

(defun linphone-contacts-extract ()
  "Parse backend supplied contacts info in current buffer
and store it in the internal list."
  (goto-char (point-min))
  (let ((contacts nil))
    (while (re-search-forward linphone-contacts-item-extractor nil t)
      (let* ((item-start (point))
             (index (string-to-number (match-string 1)))
             (item-end (re-search-forward linphone-contacts-item-extractor nil t))
             (name (progn (goto-char item-start)
                          (and (re-search-forward linphone-contacts-name-extractor item-end t)
                               (match-string 1))))
             (address (progn (goto-char item-start)
                             (and (re-search-forward linphone-contacts-address-extractor item-end t)
                                  (match-string 1)))))
        (goto-char item-start)
        (add-to-list 'contacts
                     (vector index name address nil))))
    (setq linphone-contacts-list
          (sort contacts
                (lambda (first second)
                  (string-lessp (aref first 1) (aref second 1)))))))

;;}}}
;;{{{ Utility functions

(defvar linphone-contacts-obsolete nil
  "An obsolete contact index to delete.")

(defun linphone-contacts-delete (index)
  "Delete contact by index."
  (linphone-command (format linphone-contacts-delete-command-format index)))

(defun linphone-contacts-delete-obsolete ()
  "Delete obsolete contact if any."
  (when linphone-contacts-obsolete
    (linphone-contacts-delete linphone-contacts-obsolete)
    (setq linphone-contacts-obsolete nil)))

(defun linphone-contacts-add (name address)
  "Add a new contact."
  (interactive "sName: \nsAddress: ")
  (unless (and name (string-match-p "\\w" name))
    (error "Invalid contact name"))
  (unless (and address (string-match-p "\\w" address))
    (error "Invalid contact address"))
  (linphone-command (format linphone-contacts-add-command-format name address))
  (setq linphone-pending-actions (list 'linphone-contacts-refresh)))

;;}}}
;;{{{ Control widgets

(defvar linphone-contacts-display-position nil
  "Display position of the contact list to return after modification.")

(defun linphone-contacts-call-button (item)
  "Button to call the item."
  (widget-create 'push-button
                 :tag "Call"
                 :help-echo (concat "Make a call to " (aref item 1))
                 :notify (lambda (button &rest ignore)
                           (linphone-command (format linphone-contacts-call-command-format
                                                     (widget-value button))))
                 (aref item 0)))

(defun linphone-contacts-edit-button (item)
  "Button to edit the item."
  (widget-create 'push-button
                 :tag "Edit"
                 :help-echo (concat "Edit contact info for " (aref item 1))
                 :notify (lambda (button &rest ignore)
                           (linphone-contacts-add
                            (read-string "Name: " (aref (widget-value button) 1))
                            (read-string "Address: " (aref (widget-value button) 2)))
                           (setq linphone-contacts-obsolete (aref (widget-value button) 0))
                           (push 'linphone-contacts-delete-obsolete linphone-pending-actions)
                           (when linphone-contacts-display-position
                             (goto-char linphone-contacts-display-position)))
                 item))

(defun linphone-contacts-delete-button (item)
  "Button to delete the item."
  (widget-create 'push-button
                 :tag "Delete"
                 :help-echo (concat "Delete contact info for " (aref item 1))
                 :notify (lambda (button &rest ignore)
                           (linphone-contacts-delete (widget-value button))
                           (setq linphone-pending-actions (list 'linphone-contacts-refresh))
                           (forward-line 0))
                 (aref item 0)))

(defun linphone-contacts-add-button ()
  "Button to add a new contact."
  (widget-create 'push-button
                 :tag "Add new"
                 :help-echo "Add a new contact"
                 :notify (lambda (&rest ignore)
                           (call-interactively 'linphone-contacts-add)
                           (when linphone-contacts-display-position
                             (goto-char linphone-contacts-display-position)))
                 "Add new"))

(defun linphone-contacts-clear-button ()
  "Button to clear all contacts info."
  (widget-create 'push-button
                 :tag "Clear all"
                 :help-echo "Clear all contacts info"
                 :notify (lambda (&rest ignore)
                           (linphone-command linphone-contacts-clear-command)
                           (setq linphone-pending-actions (list 'linphone-contacts-refresh))
                           (when linphone-contacts-display-position
                             (goto-char linphone-contacts-display-position)))
                 "Clear all"))

;;}}}
;;{{{ Show contact list

(defun linphone-contacts-show ()
  "Make the address book view."
  (setq linphone-contacts-display-position (point))
  (if linphone-contacts-list
      (mapc (lambda (item)
              (widget-insert (aref item 1) " ")
              (when (aref item 3)
                (widget-insert "(" (aref item 3) ") "))
              (widget-insert "<" (aref item 2) ">")
              (when (and linphone-online (not linphone-call-active))
                (widget-insert " ")
                (linphone-contacts-call-button item))
              (widget-insert " ")
              (linphone-contacts-edit-button item)
              (widget-insert " ")
              (linphone-contacts-delete-button item)
              (widget-insert "\n"))
            linphone-contacts-list)
    (widget-insert "Empty list\n"))
  (linphone-contacts-add-button)
  (when linphone-contacts-list
    (widget-insert "  ")
    (linphone-contacts-clear-button))
  (widget-insert "\n"))

;;}}}

(provide 'linphone-contacts)

;;; linphone-contacts.el ends here
