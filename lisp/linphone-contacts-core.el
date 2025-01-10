;;; linphone-contacts-core.el --- Linphone address book core functions
;;; Author: Igor B. Poretsky <poretsky@mlbox.ru>
;;; Keywords: Linphone, web phone, voip, network sip telephone

;;{{{  Copyright

;;; Copyright (C) 2013-2025  Igor B. Poretsky

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

;;; This module provides Linphone address book core functions
;;; used by other modules mainly for caller identification.
;;; It will be automatically loaded by the main Linphone interface
;;; when necessary, so the file should be available on the load path.

;;; Code:

;;}}}
;;{{{ Requirements

(require 'cl-lib)

(cl-eval-when (load)
  (require 'linphone))

;;}}}
;;{{{ Forward declarations

(declare-function linphone-command "linphone" (command))

(defvar linphone-contacts-requested)
(defvar linphone-contacts-loaded)

;;}}}
;;{{{ Control data

(defconst linphone-contacts-get-command "friend list"
  "Linphone command to get contact list.")

(defconst linphone-contacts-item-extractor "^\\*+ Friend \\([0-9]+\\) \\*+$"
  "Regular expression to parse item header in the backend supplied list.")

(defconst linphone-contacts-name-extractor "^name: \\(.*\\)$"
  "Regular expression to extract name info from the backend supplied list.")

(defconst linphone-contacts-address-extractor "^address: <\\(.*\\)>$"
  "Regular expression to extract address info from the backend supplied list.")

(defconst linphone-contacts-info-parser "\\(\"\\(.*\\)\" \\)?<\\(.*\\)>"
  "Regular expression to parse caller id.")

;;}}}
;;{{{ Request data from the backend

;;;###autoload
(defun linphone-contacts-refresh ()
  "Refresh contacts info."
  (linphone-command linphone-contacts-get-command)
  (setq linphone-contacts-requested t))

;;}}}
;;{{{ Fill out contacts list

(defvar linphone-contacts-list nil
  "Address book content.")

;;;###autoload
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
                  (string-lessp (aref first 1) (aref second 1))))))
  (setq linphone-contacts-loaded t))

;;}}}
;;{{{ Utility functions

;;;###autoload
(defun linphone-contacts-recognize (info)
  "Search names in the contact list by specified info.
Returns vector in the format [id name address]
or nil if specified info string is incorrect."
  (save-match-data
    (when (and (stringp info)
               (string-match linphone-contacts-info-parser info))
      (let ((found nil)
            (id nil)
            (name (match-string 2 info))
            (address (match-string 3 info)))
        (mapc (lambda (item)
                (and (stringp (aref item 2))
                     (stringp (aref item 1))
                     (string-equal (aref item 2) address)
                     (add-to-list 'found item 'append)))
              linphone-contacts-list)
        (when (and found
                   (or (null name)
                       (= (length found) 1)))
          (setq id (aref (car found) 0)
                name (aref (car found) 1)))
        (vector id name address)))))

;;}}}

(provide 'linphone-contacts-core)

;;; linphone-contacts-core.el ends here
