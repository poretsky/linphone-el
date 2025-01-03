;;; linphone-display.el --- Linphone control panel
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

;;; This module provides general Linphone control functionality.
;;; It will be automatically loaded by the main Linphone interface
;;; when necessary, so the file should be available on the load path.

;;; Code:

;;}}}
;;{{{ Requirements

(require 'cl-lib)
(require 'custom)
(require 'widget)
(require 'wid-edit)

(cl-eval-when (load)
  (require 'linphone))

;;}}}
;;{{{ Forward declarations

(declare-function linphone-command "linphone" (command))
(declare-function linphone-contacts-show "linphone-contacts")
(declare-function linphone-log-show "linphone-log")
(declare-function linphone-online-controls "linphone-control")

(defvar linphone-backend-ready)
(defvar linphone-pending-actions)
(defvar linphone-backend-quit-command)
(defvar linphone-quit-command)
(defvar linphone-process)
(defvar linphone-current-control)
(defvar linphone-displayed-control)
(defvar linphone-control-panel)
(defvar linphone-online)

;;}}}
;;{{{ Customizations

(defcustom linphone-show-contacts nil
  "Show address book content on startup.
In fact, address book visibility can be toggled at any time from the
control panel. This option merely defines the state to start with."
  :type 'boolean
  :group 'linphone)

(defcustom linphone-show-log nil
  "Show log content on startup.
In fact, log visibility can be toggled at any time from the
control panel. This option merely defines the state to start with."
  :type 'boolean
  :group 'linphone)

(defcustom linphone-register-command-format "register %s %s %s"
  "Linphone register command format.
The string placeholders are to be replaced by your identity,
proxy host name and your password."
  :type 'string
  :group 'linphone-backend)

;;}}}
;;{{{ Utilities

(defvar linphone-display-contacts (cons linphone-show-contacts 'linphone-contacts-refresh)
  "Linphone log visibility control.")

(defvar linphone-display-log (cons linphone-show-log 'linphone-log-refresh)
  "Linphone log visibility control.")

(defun linphone-refresh (control)
  "Refresh info by specified control."
  (when (car control)
    (if linphone-backend-ready
        (funcall (cdr control))
      (add-to-list 'linphone-pending-actions (cdr control) 'append))))

;;;###autoload
(defun linphone-schedule-log-update ()
  "Schedule log update if it is visible."
  (let ((linphone-backend-ready nil))
    (linphone-refresh linphone-display-log)))

(defun linphone-validate-sip-address (address &optional host-only)
  "Check and validate supplied sip address.
If optional second argument is not nil, only host name is required.
Return fully qualified address string."
  (unless (string-match "^\\(sip:\\)?\\([^@ ]+@\\)?[^@ ]+\\(\\.[^@. ]+\\)*$" address)
    (error "Invalid SIP address"))
  (unless (or (match-string 2 address) host-only)
    (error "Invalid SIP identity"))
  (if (match-string 1 address)
      address
    (concat "sip:" address)))

;;}}}
;;{{{ Major mode definition

(defvar linphone-control-mode-map (make-sparse-keymap)
  "Keymap for Linphone control panels.")

(set-keymap-parent linphone-control-mode-map widget-keymap)

(define-derived-mode linphone-control-mode fundamental-mode
  "Control panel"
  "This is a Linphone control panel.
Navigate around and press buttons.

\\{linphone-control-mode-map}")

;;}}}
;;{{{ Control widgets

(defun linphone-register-button ()
  "Button to register or change account."
  (widget-create 'push-button
                 :tag "Register"
                 :help-echo "Register now"
                 :notify (lambda (&rest ignore)
                           (let ((identity
                                  (linphone-validate-sip-address
                                   (read-string "Your identity: "))))
                             (linphone-command
                              (format linphone-register-command-format identity
                                      (linphone-validate-sip-address
                                       (read-string "SIP proxy: "
                                                    (and (string-match "@\\(.+\\)$" identity)
                                                         (match-string 1 identity))) t)
                                      (read-passwd "Password: ")))))
                 "Register"))

(defun linphone-customize-button ()
  "Button to enter customization mode."
  (widget-create 'push-button
                 :tag "Customize"
                 :help-echo "Set up some options"
                 :notify (lambda (&rest ignore)
                           (customize-group 'linphone))
                 "Customize"))

(defun linphone-quit-button ()
  "Linphone quit button."
  (widget-create 'push-button
                 :tag "Quit"
                 :help-echo "Stop and exit telephone completely"
                 :notify (lambda (&rest ignore)
                           (if linphone-backend-quit-command
                               (call-process-shell-command linphone-backend-quit-command)
                             (linphone-command linphone-quit-command))
                           (set-process-filter linphone-process t)
                           (condition-case nil
                               (kill-buffer-and-window)
                             (error nil)))
                 "Quit"))

(defun linphone-toggle-list-visibility-button (control label)
  "Toggle list visibility button."
  (widget-create 'toggle
                 :tag label
                 :value (car control)
                 :on "Hide"
                 :off "Show"
                 :format "%[[%v %t]%]"
                 :control control
                 :notify (lambda (widget &rest ignore)
                           (if (setcar (widget-get widget ':control)
                                       (widget-value widget))
                               (linphone-refresh (widget-get widget ':control))
                             (let ((position (point)))
                               (funcall linphone-current-control)
                               (goto-char position))))))

(defun linphone-arrange-control-panel (header)
  "Arrange fresh control panel with specified header."
  (setq linphone-displayed-control linphone-current-control)
  (unless (get-buffer linphone-control-panel)
    (linphone-refresh linphone-display-contacts)
    (linphone-refresh linphone-display-log))
  (with-current-buffer (get-buffer-create linphone-control-panel)
    (kill-all-local-variables)
    (let ((inhibit-readonly t))
      (when (fboundp 'remove-overlays)
        (remove-overlays))
      (erase-buffer))
    (linphone-control-mode)
    (widget-insert header "\n\n")))

(defun linphone-panel-footer ()
  "Make up a panel footer part."
  (widget-insert "\n")
  (linphone-toggle-list-visibility-button linphone-display-contacts "Address book")
  (widget-insert "\n")
  (when (car linphone-display-contacts)
    (linphone-contacts-show)
    (widget-insert "\n"))
  (linphone-toggle-list-visibility-button linphone-display-log "Recent calls")
  (when (car linphone-display-log)
    (widget-insert "\n")
    (linphone-log-show))
  (widget-insert "\n"))

;;;###autoload
(defun linphone-general-control (&optional show)
  "General control panel constructor. If optional argument is non-nil
the constructed panel will be popped up."
  (linphone-arrange-control-panel
   (format "Internet telephone %s"
           (if linphone-online
               (if (stringp linphone-online)
                   (format "at %s" linphone-online)
                 "online")
             "offline")))
  (with-current-buffer linphone-control-panel
    (if linphone-online
        (linphone-online-controls)
      (linphone-register-button)
      (widget-insert "    ")
      (linphone-quit-button)
      (widget-insert "    ")
      (linphone-customize-button))
    (widget-insert "\n")
    (linphone-panel-footer)
    (widget-setup)
    (widget-forward 1))
  (when show
    (pop-to-buffer linphone-control-panel)))

;;}}}

(provide 'linphone-display)

;;; linphone-display.el ends here
