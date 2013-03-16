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

(defgroup linphone-contacts nil
  "Log viewing customization."
  :group 'linphone)

(defcustom linphone-contacts-call-command-format "friend call %d"
  "Format string to construct a contact call command.
The number placeholder is to be replaced by the contact index."
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
;;{{{ Control widgets

(defun linphone-contacts-call-button (item)
  "Button to call the item."
  (widget-create 'push-button
                 :tag "Call"
                 :help-echo (concat "Make a call to " (aref item 1))
                 :notify (lambda (button &rest ignore)
                           (linphone-command (format linphone-contacts-call-command-format
                                                     (widget-value button)))
                 (aref item 0))))

;;}}}
;;{{{ Show contact list

(defun linphone-contacts-show ()
  "Make the address book view."
  (if linphone-contacts-list
      (mapc (lambda (item)
              (widget-insert (aref item 1) " ")
              (when (aref item 3)
                (widget-insert "(" (aref item 3) ") "))
              (widget-insert "<" (aref item 2) ">")
              (unless linphone-call-active
                (widget-insert " ")
                (linphone-contacts-call-button item))
              (widget-insert "\n"))
            linphone-contacts-list)
    (widget-insert "Empty list\n")))

;;}}}

(provide 'linphone-contacts)

;;; linphone-log.el ends here
