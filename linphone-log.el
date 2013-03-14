;;; linphone-log.el --- Linphone calls log viewer
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

;;; This module provides Linphone call log viewing facilities.

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

(defcustom linphone-log-incoming-call-detector "Incoming"
  "Incoming call detector string for matching."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-log-outgoing-call-detector "Outgoing"
  "Outgoing call detector string for matching."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-log-call-info-header-format "^\\(%s\\) call at \\(.*\\)$"
  "Regular expression pattern for call info block header matching.
The string placeholder is to be replaced by a call type detector string."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-log-call-issuer-retriever "^From: \\(\"\\(.*\\)\" \\)?<\\(.*\\)>$"
  "Regular expression to retrieve and parse call issuer info."
  :type 'regexp
  :group 'linphone-backend)

(defcustom linphone-log-call-target-retriever "^To: \\(\"\\(.*\\)\" \\)?<\\(.*\\)>$"
  "Regular expression to retrieve and parse call target info."
  :type 'regexp
  :group 'linphone-backend)

(defcustom linphone-log-call-duration-retriever "^Duration: \\(.*\\)$"
  "Regular expression to retrieve and parse call duration info."
  :type 'regexp
  :group 'linphone-backend)

(defcustom linphone-log-call-status-retriever "^Status: \\(.*\\)$"
  "Regular expression to retrieve and parse call status info."
  :type 'regexp
  :group 'linphone-backend)

;;}}}
;;{{{ Parse log content

(defvar linphone-log-call-list nil
  "List of calls retrieved from the Linphone log.")

(defun linphone-log-acquire ()
  "Parse the log and fill out the call list."
  (setq linphone-log-call-list nil)
  (goto-char (point-min))
  (let ((header (format linphone-log-call-info-header-format
                        (concat linphone-log-incoming-call-detector
                                "\\|" linphone-log-outgoing-call-detector))))
    (while (re-search-forward header nil t)
      (let* ((item-start (point))
             (type (match-string 1))
             (date (match-string 2))
             (item-end (re-search-forward header nil t))
             (partner (cond
                       ((string-equal type linphone-log-incoming-call-detector)
                        (goto-char item-start)
                        (if (re-search-forward linphone-log-call-issuer-retriever item-end t)
                            (cons (match-string 2) (match-string 3))
                          (cons nil nil)))
                       ((string-equal type linphone-log-outgoing-call-detector)
                        (goto-char item-start)
                        (if (re-search-forward linphone-log-call-target-retriever item-end t)
                            (cons (match-string 2) (match-string 3))
                          (cons nil nil)))
                       (t (cons nil nil))))
             (duration (progn (goto-char item-start)
                              (and (re-search-forward linphone-log-call-duration-retriever item-end t)
                                   (match-string 1))))
             (status (progn (goto-char item-start)
                            (and (re-search-forward linphone-log-call-status-retriever item-end t)
                                 (match-string 1)))))
        (goto-char item-start)
        (add-to-list 'linphone-log-call-list
                     (vector type date (car partner) (cdr partner) duration status)
                     'append)))))

;;}}}
;;{{{ Control widgets

(defun linphone-log-call-button (item)
  "Button to call the item."
  (widget-create 'push-button
                 :tag "Call now"
                 :help-echo (concat "Call "
                                    (or (aref item 2) (aref item 3))
                                    " just now")
                 :notify (lambda (button &rest ignore)
                           (linphone-call (widget-value button)))
                 (aref item 3)))

;;}}}
;;{{{ Show call history

(defun linphone-log-show ()
  "Insert call history into the current buffer."
  (widget-insert "Recent calls:\n")
  (if linphone-log-call-list
      (mapc (lambda (item)
              (widget-insert "\n" (aref item 1) "\n")
              (cond
               ((string-equal linphone-log-incoming-call-detector (aref item 0))
                (widget-insert "From "))
               ((string-equal linphone-log-outgoing-call-detector (aref item 0))
                (widget-insert "To "))
               (t nil))
              (when (aref item 2)
                (widget-insert (aref item 2) " "))
              (widget-insert "<" (aref item 3) ">")
              (unless linphone-call-active
                (widget-insert " ")
                (linphone-log-call-button item))
              (widget-insert "\n" (aref item 5))
              (when (aref item 4)
                (widget-insert " " (aref item 4)))
              (widget-insert "\n"))
            linphone-log-call-list)
    (widget-insert "\nEmpty list\n")))

;;}}}

(provide 'linphone-log)

;;; linphone-log.el ends here
