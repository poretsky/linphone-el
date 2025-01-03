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
;;; It will be automatically loaded by the main Linphone interface
;;; when necessary, so the file should be available on the load path.

;;; Code:

;;}}}
;;{{{ Requirements

(require 'cl-lib)
(require 'custom)
(require 'widget)
(eval-when-compile
  (require 'cl)
  (require 'wid-edit))

(cl-eval-when (load)
  (require 'linphone)
  (require 'linphone-contacts-core)
  (require 'linphone-contacts-lib))

;;}}}
;;{{{ Forward declarations

(declare-function linphone-command "linphone" (command))
(declare-function linphone-call "linphone-control" (addr))
(declare-function linphone-contacts-refresh "linphone-contacts-core")
(declare-function linphone-contacts-recognize "linphone-contacts-core" (info))
(declare-function linphone-contacts-add "linphone-contacts-lib" (name address))
(declare-function linphone-contacts-call-button "linphone-contacts-lib" (id name))

(defvar linphone-log-requested)
(defvar linphone-contacts-loaded)
(defvar linphone-pending-actions)
(defvar linphone-current-control)
(defvar linphone-backend-ready)
(defvar linphone-online)
(defvar linphone-call-active)

;;}}}
;;{{{ Customizations

;;;###autoload
(defgroup linphone-logs nil
  "Log viewing customization."
  :group 'linphone)

(defcustom linphone-log-show-missed-calls nil
  "Show missed calls list by default.
In fact, the list visibility can be toggled at any time from the
control panel. This option merely defines the state to start with."
  :type 'boolean
  :group 'linphone-logs)

(defcustom linphone-log-show-received-calls nil
  "Show received calls list by default.
In fact, the list visibility can be toggled at any time from the
control panel. This option merely defines the state to start with."
  :type 'boolean
  :group 'linphone-logs)

(defcustom linphone-log-show-dialed-calls nil
  "Show dialed calls list by default.
In fact, the list visibility can be toggled at any time from the
control panel. This option merely defines the state to start with."
  :type 'boolean
  :group 'linphone-logs)

(defcustom linphone-log-show-unclassified-calls nil
  "Show unclassified calls list by default.
In fact, the list visibility can be toggled at any time from the
control panel. This option merely defines the state to start with."
  :type 'boolean
  :group 'linphone-logs)

(defcustom linphone-log-show-all-calls nil
  "Show all calls list by default.
In fact, the list visibility can be toggled at any time from the
control panel. This option merely defines the state to start with."
  :type 'boolean
  :group 'linphone-logs)

(defcustom linphone-log-max-list-size nil
  "Maximum call list size.
Zero or negative value means no restrictions."
  :type '(choice (const :tag "Unrestricted" nil) integer)
  :group 'linphone-logs)

(defcustom linphone-log-get-command "call-logs"
  "Linphone command to get log info."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-log-incoming-call-detector "Incoming"
  "Incoming call detector string for matching."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-log-outgoing-call-detector "Outgoing"
  "Outgoing call detector string for matching."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-log-missed-call-detector "missed"
  "Outgoing call detector string for matching."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-log-call-info-header-format "^\\(%s\\) call at \\(.*\\)$"
  "Regular expression pattern for call info block header matching.
The string placeholder is to be replaced by a call type detector string."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-log-call-issuer-retriever "^From: \\(.*\\)$"
  "Regular expression to retrieve and parse call issuer info."
  :type 'regexp
  :group 'linphone-backend)

(defcustom linphone-log-call-target-retriever "^To: \\(.*\\)$"
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
;;{{{ Utilities

(defun linphone-log-request-info ()
  "Request log info from the backend."
  (linphone-command linphone-log-get-command)
  (setq linphone-log-requested t))

;;;###autoload
(defun linphone-log-refresh ()
  "Refresh log info."
  (if linphone-contacts-loaded
      (linphone-log-request-info)
    (add-to-list 'linphone-pending-actions 'linphone-log-request-info 'append)
    (linphone-contacts-refresh)))

;;}}}
;;{{{ Parse log content

(defvar linphone-log-call-list nil
  "List of calls retrieved from the Linphone log.")

(defvar linphone-log-missed-calls (cons linphone-log-show-missed-calls 0)
  "Visibility state and number of missed calls in history.")

(defvar linphone-log-received-calls (cons linphone-log-show-received-calls 0)
  "Visibility state and number of received calls in history.")

(defvar linphone-log-dialed-calls (cons linphone-log-show-dialed-calls 0)
  "Visibility state and number of dialed calls in history.")

(defvar linphone-log-unclassified-calls (cons linphone-log-show-unclassified-calls 0)
  "Visibility state and number of unclassified calls in history.")

(defvar linphone-log-all-calls (cons linphone-log-show-all-calls 0)
  "Visibility state and number of all calls in history.")

;;;###autoload
(defun linphone-log-acquire ()
  "Parse the log and fill out the call list."
  (setq linphone-log-call-list nil)
  (setcdr linphone-log-missed-calls 0)
  (setcdr linphone-log-received-calls 0)
  (setcdr linphone-log-dialed-calls 0)
  (setcdr linphone-log-unclassified-calls 0)
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
                            (match-string 1)
                          nil))
                       ((string-equal type linphone-log-outgoing-call-detector)
                        (goto-char item-start)
                        (if (re-search-forward linphone-log-call-target-retriever item-end t)
                            (match-string 1)
                          nil))
                       (t nil)))
             (duration (progn (goto-char item-start)
                              (and (re-search-forward linphone-log-call-duration-retriever item-end t)
                                   (match-string 1))))
             (status (progn (goto-char item-start)
                            (and (re-search-forward linphone-log-call-status-retriever item-end t)
                                 (match-string 1)))))
        (cond
         ((string-equal type linphone-log-outgoing-call-detector)
          (incf (cdr linphone-log-dialed-calls)))
         ((string-equal type linphone-log-incoming-call-detector)
          (if (string-equal status linphone-log-missed-call-detector)
              (incf (cdr linphone-log-missed-calls))
            (incf (cdr linphone-log-received-calls))))
         (t (incf (cdr linphone-log-unclassified-calls))))
        (goto-char item-start)
        (add-to-list 'linphone-log-call-list
                     (vector type date partner duration status)
                     'append))))
  (setcdr linphone-log-all-calls
          (if linphone-log-call-list
              (length linphone-log-call-list)
            0)))

;;}}}
;;{{{ Control widgets

(defun linphone-log-list-summary (control label)
  "List summary widget."
  (when (> (cdr control) 0)
    (widget-insert "\n")
    (widget-create 'toggle
                   :tag label
                   :value (car control)
                   :on "Hide"
                   :off "Show"
                   :format (format "%%t %d %%[[%%v]%%]" (cdr control))
                   :control control
                   :notify (lambda (widget &rest ignore)
                             (setcar (widget-get widget ':control)
                                     (widget-value widget))
                             (let ((position (point)))
                               (funcall linphone-current-control)
                               (goto-char position))))))

(defun linphone-log-refresh-button ()
  "Button to refresh log info."
  (widget-create 'push-button
                 :tag "Refresh"
                 :help-echo "Refresh log info"
                 :notify (lambda (&rest ignore)
                           (if linphone-backend-ready
                               (linphone-log-refresh)
                             (add-to-list 'linphone-pending-actions 'linphone-log-refresh 'append)))
                 "Refresh"))

(defun linphone-log-call-button (name address)
  "Button to call the item."
  (widget-create 'push-button
                 :tag "Call"
                 :help-echo (format "Call %s just now" (or name address))
                 :notify (lambda (button &rest ignore)
                           (linphone-call (widget-value button)))
                 address))

(defun linphone-log-remember-button (name address)
  "Button to remember the item as a contact."
  (widget-create 'push-button
                 :tag "Remember"
                 :help-echo (format "Remember %s as a contact" (or name address))
                 :notify (lambda (button &rest ignore)
                           (linphone-contacts-add
                            (read-string "Name: " (cdr (widget-value button)))
                            (car (widget-value button))))
                 (cons address name)))

;;}}}
;;{{{ Calls classification predicates

(defun linphone-log-missed-p (item)
  "Test if the item represents missed call."
  (and (string-equal (aref item 0) linphone-log-incoming-call-detector)
       (string-equal (aref item 4) linphone-log-missed-call-detector)))

(defun linphone-log-received-p (item)
  "Test if the item represents received call."
  (and (string-equal (aref item 0) linphone-log-incoming-call-detector)
       (not (string-equal (aref item 4) linphone-log-missed-call-detector))))

(defun linphone-log-dialed-p (item)
  "Test if the item represents dialed call."
  (string-equal (aref item 0) linphone-log-outgoing-call-detector))

(defun linphone-log-unclassified-p (item)
  "Test if the item represents unclassified call."
  (not (or (string-equal (aref item 0) linphone-log-incoming-call-detector)
           (string-equal (aref item 0) linphone-log-outgoing-call-detector))))

(defun linphone-log-any-p (item)
  "Always true."
  t)

;;}}}
;;{{{ Show call history

(defun linphone-log-show-list (control label predicate)
  "Insert particular calls info into the current buffer."
  (linphone-log-list-summary control label)
  (when (car control)
    (widget-insert "\n")
    (let ((count 0))
      (mapc (lambda (item)
              (when (and (funcall predicate item)
                         (or (not (integerp linphone-log-max-list-size))
                             (<= linphone-log-max-list-size 0)
                             (< count linphone-log-max-list-size)))
                (setq count (1+ count))
                (widget-insert "\n" (aref item 1) "\n")
                (cond
                 ((string-equal linphone-log-incoming-call-detector (aref item 0))
                  (widget-insert "From "))
                 ((string-equal linphone-log-outgoing-call-detector (aref item 0))
                  (widget-insert "To "))
                 (t nil))
                (let ((partner (linphone-contacts-recognize (aref item 2))))
                  (when (aref partner 1)
                    (widget-insert (aref partner 1) " "))
                  (widget-insert "<" (aref partner 2) ">")
                  (when (and (aref partner 2) (string-match-p "\\w" (aref partner 2)))
                    (when (and linphone-online (not linphone-call-active))
                      (widget-insert " ")
                      (if (aref partner 0)
                          (linphone-contacts-call-button (aref partner 0) (aref partner 1))
                        (linphone-log-call-button (aref partner 1) (aref partner 2))))
                    (unless (aref partner 0)
                      (widget-insert " ")
                      (linphone-log-remember-button (aref partner 1) (aref partner 2)))))
                (widget-insert "\n" (aref item 4))
                (when (aref item 3)
                  (widget-insert " after " (aref item 3)))
                (widget-insert "\n")))
            linphone-log-call-list)
      (widget-insert (format "\nShown %d of %d\n" count (cdr control))))))

;;;###autoload
(defun linphone-log-show ()
  "Insert call history into the current buffer."
  (if (null linphone-log-call-list)
      (widget-insert "The log is empty")
    (linphone-log-show-list linphone-log-missed-calls "Missed" 'linphone-log-missed-p)
    (linphone-log-show-list linphone-log-received-calls "Received" 'linphone-log-received-p)
    (linphone-log-show-list linphone-log-dialed-calls "Dialed" 'linphone-log-dialed-p)
    (linphone-log-show-list linphone-log-unclassified-calls "Unclassified" 'linphone-log-unclassified-p)
    (linphone-log-show-list linphone-log-all-calls "Total" 'linphone-log-any-p))
  (widget-insert "\n")
  (linphone-log-refresh-button))

;;}}}

(provide 'linphone-log)

;;; linphone-log.el ends here
