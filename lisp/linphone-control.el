;;; linphone-control.el --- Linphone control widgets
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

;;; This module provides general Linphone control functionality.
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

(defcustom linphone-call-command-format "call %s"
  "Linphone call command format.
The string placeholder is to be replaced by the actual target address."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-answer-command "answer"
  "Linphone answer incoming call command string."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-cancel-command "terminate"
  "Linphone current call cancellation command string."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-unregister-command "unregister"
  "Command to cancel registration."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-check-registration-command "status register"
  "Command to check registration status."
  :type 'string
  :group 'linphone-backend)

;;}}}
;;{{{ Backend commands

(defun linphone-call (addr)
  "Arrange an outgoing call to specified address."
  (interactive "sAddress to call: ")
  (linphone-command
   (format linphone-call-command-format addr)))

(defun linphone-check-registration-status ()
  "Check registration status."
  (linphone-command linphone-check-registration-command))

;;}}}
;;{{{ Control widgets

(defun linphone-standby-button ()
  "Linphone standby mode button."
  (widget-create 'push-button
                 :tag "Standby"
                 :help-echo "Run in background waiting for incoming calls"
                 :notify (lambda (&rest ignore)
                           (condition-case nil
                               (kill-buffer-and-window)
                             (error nil)))
                 "Standby"))

(defun linphone-arrange-call-button ()
  "Button to make a call."
  (widget-create 'push-button
                 :tag "Call"
                 :help-echo "Arrange an outgoing call"
                 :notify (lambda (&rest ignore)
                           (call-interactively 'linphone-call))
                 "Call"))

(defun linphone-answer-button ()
  "Answer incoming call button."
  (widget-create 'push-button
                 :tag "Accept"
                 :notify (lambda (&rest ignore)
                           (linphone-command linphone-answer-command))
                 "Accept"))

(defun linphone-cancel-button (label)
  "Current call cancellation button."
  (widget-create 'push-button
                 :tag label
                 :notify (lambda (&rest ignore)
                           (linphone-command linphone-cancel-command))
                 label))

(defun linphone-unregister-button ()
  "Button to cancel registration."
  (widget-create 'push-button
                 :tag "Unregister"
                 :help-echo "Cancel or change subscription"
                 :notify (lambda (&rest ignore)
                           (linphone-command linphone-unregister-command)
                           (setq linphone-pending-actions (list 'linphone-check-registration-status)))
                 "Unregister"))

(defun linphone-notification-close-button ()
  "Button to close notification window."
  (widget-create 'push-button
                 :tag "Ok"
                 :notify (lambda (&rest ignore)
                           (if linphone-call-active
                               (setq linphone-current-control 'linphone-active-call-control)
                             (setq linphone-current-control 'linphone-general-control))
                           (funcall linphone-current-control))
                 "Ok"))

(defun linphone-online-controls ()
  "Make up general controls for online mode."
  (linphone-arrange-call-button)
  (widget-insert "    ")
  (linphone-standby-button)
  (widget-insert "\n")
  (linphone-quit-button)
  (widget-insert "    ")
  (linphone-customize-button)
  (widget-insert "\n\n    ")
  (linphone-unregister-button))

(defun linphone-incoming-call-control ()
  "Incoming call control panel popup."
  (linphone-arrange-control-panel linphone-backend-response)
  (with-current-buffer linphone-control-panel
    (linphone-answer-button)
    (widget-insert "    ")
    (linphone-cancel-button "Cancel")
    (widget-insert "\n")
    (widget-setup)
    (widget-forward 1))
  (display-buffer linphone-control-panel))

(defun linphone-outgoing-call-control ()
  "Outgoing call control panel popup."
  (linphone-arrange-control-panel linphone-backend-response)
  (with-current-buffer linphone-control-panel
    (widget-insert "        ")
    (linphone-cancel-button "Cancel")
    (widget-insert "\n")
    (widget-setup)
    (widget-forward 1))
  (pop-to-buffer linphone-control-panel))

(defun linphone-notification ()
  "Notification window popup."
  (linphone-arrange-control-panel linphone-backend-response)
  (with-current-buffer linphone-control-panel
    (linphone-notification-close-button)
    (widget-insert "\n")
    (widget-setup)
    (widget-forward 1))
  (pop-to-buffer linphone-control-panel))

;;}}}

(provide 'linphone-control)

;;; linphone-control.el ends here
