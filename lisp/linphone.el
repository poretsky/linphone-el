;;; linphone.el --- Emacs frontend for the Linphone program
;;; Author: Igor B. Poretsky <poretsky@mlbox.ru>
;;; Keywords: Linphone, web phone, voip, network sip telephone

;;{{{  Copyright

;;; Copyright (C) 2008  Igor B. Poretsky

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

;;; This module provides more or less intuitive and simple Emacs interface
;;; for well-known network telephone program named Linphone. It actually
;;; uses linphonec executable (that is console version of Linphone)
;;; as a backend. Debian or Ubuntu users should install package linphone-nox
;;; in order to use it.

;;; Usage:

;;; To make this module to be loaded automatically when Emacs starts
;;; place it in a directory mentioned in the load-path variable
;;; and include into your Emacs startup file the next string:
;;;
;;; (require 'linphone)
;;;
;;; Of course, you may byte-compile this file previously if you like.
;;;
;;; Then you can activate Linphone either via menu or by typing
;;; "M-x linphone <RET>". The control panel with several buttons
;;; will be popped up. To make an outgoing call press "Call" button.
;;; If you just want to stand by waiting for incoming calls,
;;; press "Standby" button. The control panel will vanish
;;; and Linphone will continue proceeding in background.
;;; When incoming call will be encountered, corresponding control panel
;;; will appear in a separate window. Then you can switch to this window
;;; and accept or reject the call by pressing an appropriate button.

;;; Note:

;;; This interface doesn't allow configuring the backend program itself.
;;; You should do it either using native means or by editing configuration
;;; file directly.

;;; Code:

;;}}}
;;{{{ Requirements

(require 'custom)
(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(autoload 'linphone-online-controls "linphone-control")
(autoload 'linphone-incoming-call-control "linphone-control")
(autoload 'linphone-outgoing-call-control "linphone-control")
(autoload 'linphone-notification "linphone-control")
(autoload 'linphone-active-call-control "linphone-conversation")
(autoload 'linphone-mute "linphone-conversation")
(autoload 'linphone-unmute "linphone-conversation")
(autoload 'linphone-contacts-extract "linphone-contacts")
(autoload 'linphone-contacts-show "linphone-contacts")
(autoload 'linphone-log-acquire "linphone-log")
(autoload 'linphone-log-show "linphone-log")

;;}}}
;;{{{ Customizations

(defvar linphone-sounds-directory
  (file-name-as-directory (expand-file-name "../sounds" (file-name-directory load-file-name)))
  "Directory where sound icons are stored.")

(defgroup linphone nil
  "Internet telephone."
  :group 'applications)

(defgroup linphone-sounds nil
  "Sound icons for various events."
  :group 'linphone)

(defgroup linphone-backend nil
  "Communications with external backend programs.
Don't touch this stuff unless you really know what you are doing."
  :group 'linphone)

(defcustom linphone-online-sound (expand-file-name "online.wav" linphone-sounds-directory)
  "Sound file played when going online."
  :type '(choice (const :tag "No sound")
                 (file :must-match t))
  :group 'linphone-sounds)

(defcustom linphone-offline-sound (expand-file-name "offline.wav" linphone-sounds-directory)
  "Sound file played when going offline."
  :type '(choice (const :tag "No sound")
                 (file :must-match t))
  :group 'linphone-sounds)

(defcustom linphone-hangup-sound (expand-file-name "hangup.wav" linphone-sounds-directory)
  "Sound file played when call is finished or connection is lost."
  :type '(choice (const :tag "No sound")
                 (file :must-match t))
  :group 'linphone-sounds)

(defcustom linphone-backend-program "linphonec"
  "Backend executable program name."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-backend-args '("--pipe")
  "List of additional arguments for the backend program."
  :type '(repeat string)
  :group 'linphone-backend)

(defcustom linphone-backend-quit-command "linphonecsh exit"
  "External command to quit backend.
It can help when the native one doesn't do the work properly."
  :type '(choice (const :tag "Not specified" nil) string)
  :group 'linphone-backend)

(defcustom linphone-sound-play-program "aplay"
  "Program used to play audio icons."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-sound-play-args '("-q")
  "List of additional arguments for sound playing command."
  :type '(repeat string)
  :group 'linphone-backend)

(defcustom linphone-register-command-format "register %s %s %s"
  "Linphone register command format.
The string placeholders are to be replaced by your identity,
proxy host name and your password."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-get-contacts-command "friend list"
  "Linphone command to get contact list."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-get-log-command "call-logs"
  "Linphone command to get log info."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-quit-command "quit"
  "Linphone quit command string."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-prompt-pattern "^linphonec> "
  "Regular expression that matches against Linphone backend prompt."
  :type 'regexp
  :group 'linphone-backend)

(defcustom linphone-call-request-pattern "^\\(.*\\) is contacting you"
  "Regular expression that matches against incoming call request."
  :type 'regexp
  :group 'linphone-backend)

(defcustom linphone-call-progress-pattern "^Contacting \\(.*\\)$"
  "Regular expression that matches progress notification message."
  :type 'regexp
  :group 'linphone-backend)

(defcustom linphone-call-connection-pattern "Connected\\.$"
  "Regular expression that matches against connection acknowledge."
  :type 'regexp
  :group 'linphone-backend)

(defcustom linphone-call-failure-pattern
  "User is temporarily unavailable\
\\|Not Acceptable Here\
\\|Forbidden\
\\|Could not reach destination\
\\|Could not resolve this number\
\\|Call failed\
\\|Terminate current call first\
\\|Request Timeout\
\\|Call declined\
\\|Internal Server Error\
\\|Bad request"
  "Regular expression that matches against call failure messages."
  :type 'regexp
  :group 'linphone-backend)

(defcustom linphone-call-termination-pattern "Call \\(terminat\\|end\\)ed\\|No active call"
  "Regular expression that matches against call termination message."
  :type 'regexp
  :group 'linphone-backend)

(defcustom linphone-online-state-string "successful"
  "Online status indication string."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-offline-state-pattern "failed"
  "Regular expression that matches against offline indication strings."
  :type 'regexp
  :group 'linphone-backend)

(defcustom linphone-registration-result-pattern "Registration on \\(.*\\) \\(%s\\)"
  "Regular expression pattern that matches against registration result
messages. The string placeholder is to be replaced by the status
matching regexp constructed from the online and offline patterns."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-unreg-state-pattern "registered=-?[0-9]+"
  "Regexp matching no registration status message."
  :type 'regexp
  :group 'linphone-backend)

;;}}}
;;{{{ Utilities

(defun linphone-play-sound (icon)
  "Play a sound icon."
  (when icon
    (apply 'call-process linphone-sound-play-program
           nil 0 nil
           (append linphone-sound-play-args (list icon)))))

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
;;{{{ Backend commands

(defvar linphone-process nil
  "The Linphone backend process handle.")

(defvar linphone-backend-ready nil
  "Indicates that Linphone backend is ready to accept commands.")

(defvar linphone-log-requested nil
  "Indicates that calls log was requested.")

(defvar linphone-contacts-requested nil
  "Indicates that contact list was requested.")

(defun linphone-command (command)
  "Send given string as a command to the Linphone backend."
  (if (and linphone-process
           (eq (process-status linphone-process) 'run))
      (if (not linphone-backend-ready)
          (error "Linphone backend is not ready to accept commands")
        (setq linphone-backend-ready nil)
        (process-send-string linphone-process
                             (format "%s\n" command)))
    (error "No running Linphone backend")))

(defun linphone-refresh-log ()
  "Refresh log info."
  (linphone-command linphone-get-log-command)
  (setq linphone-log-requested t))

(defun linphone-refresh-contacts ()
  "Refresh contacts info."
  (linphone-command linphone-get-contacts-command)
  (setq linphone-contacts-requested t))

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

(defconst linphone-control-panel "*Linphone*"
  "Name of the buffer for Linphone control widgets.")

(defvar linphone-current-control nil
  "Current control panel popup function.")

(defvar linphone-backend-response nil
  "Save last significant backend response.")

(defvar linphone-current-call nil
  "Current call target id string.")

(defvar linphone-call-active nil
  "Indicates active call state.")

(defvar linphone-online nil
  "Indicates Linphone online state.
When online, this variable contains provider address.")

(defvar linphone-pending-actions nil
  "A queue of pending actions to be done to accomplish the task in progress.
Each action should be represented by function without arguments.")

(defvar linphone-log-visibility-control (cons nil 'linphone-refresh-log)
  "Linphone log visibility control.")

(defvar linphone-contacts-visibility-control (cons nil 'linphone-refresh-contacts)
  "Linphone log visibility control.")

(defun linphone-register-button ()
  "Button to register or change account."
  (widget-create 'push-button
                 :tag "Register"
                 :help-echo "Register now"
                 :notify (lambda (&rest ignore)
                           (linphone-command
                            (format linphone-register-command-format
                                    (linphone-validate-sip-address
                                     (read-string "Your SIP address: "))
                                    (linphone-validate-sip-address
                                     (read-string "SIP proxy host name: ") t)
                                    (read-passwd "Password: "))))
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
                               (funcall (cdr (widget-get widget ':control)))
                             (let ((position (point)))
                               (funcall linphone-current-control)
                               (goto-char position))))))

(defun linphone-arrange-control-panel (header)
  "Arrange fresh control panel with specified header."
  (with-current-buffer (get-buffer-create linphone-control-panel)
    (kill-all-local-variables)
    (let ((inhibit-readonly t))
      (erase-buffer))
    (when (fboundp 'remove-overlays)
      (remove-overlays))
    (linphone-control-mode)
    (widget-insert header "\n\n")))

(defun linphone-panel-footer ()
  "Make up a panel footer part."
  (widget-insert "\n")
  (linphone-toggle-list-visibility-button linphone-contacts-visibility-control "Address book")
  (widget-insert "\n")
  (when (car linphone-contacts-visibility-control)
    (linphone-contacts-show)
    (widget-insert "\n"))
  (linphone-toggle-list-visibility-button linphone-log-visibility-control "Recent calls")
  (when (car linphone-log-visibility-control)
    (widget-insert "\n")
    (linphone-log-show))
  (widget-insert "\n"))

(defun linphone-general-control ()
  "General control panel popup."
  (linphone-arrange-control-panel
   (format "Internet telephone %s"
           (if linphone-online
               (format "at %s" linphone-online)
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
  (pop-to-buffer linphone-control-panel))

;;}}}
;;{{{ Parsing backend responses

(defvar linphone-control-change nil
  "Indicates that current control panel is changed and should be updated.")

(defun linphone-output-parser (proc string)
  "Filter function to parse Linphone backend output."
  (with-current-buffer (process-buffer proc)
    (goto-char (process-mark proc))
    (insert string)
    (when (re-search-backward linphone-prompt-pattern nil t)
      (replace-match "")
      (setq linphone-backend-ready t))
    (goto-char (point-max))
    (setq linphone-control-change nil)
    (when (re-search-backward (format linphone-registration-result-pattern
                                      (concat linphone-online-state-string
                                              "\\|" linphone-offline-state-pattern))
                              nil t)
      (if (string-equal (match-string 2) linphone-online-state-string)
          (unless linphone-online
            (setq linphone-online (match-string 1)
                  linphone-control-change t)
            (linphone-play-sound linphone-online-sound)
            (message "%s" (match-string 0)))
        (when linphone-online
          (setq linphone-online nil
                linphone-control-change t)
          (linphone-play-sound linphone-offline-sound)
          (message "%s" (match-string 0)))))
    (goto-char (point-max))
    (setq linphone-control-change
          (or (cond
               (linphone-contacts-requested
                (when linphone-backend-ready
                  (linphone-contacts-extract) t))
               (linphone-log-requested
                (when linphone-backend-ready
                  (linphone-log-acquire) t))
               ((re-search-backward linphone-unreg-state-pattern nil t)
                (when linphone-online
                  (linphone-play-sound linphone-offline-sound)
                  (setq linphone-online nil) t))
               ((re-search-backward linphone-call-connection-pattern nil t)
                (linphone-unmute)
                (setq linphone-call-active t)
                (setq linphone-current-control 'linphone-active-call-control))
               ((re-search-backward linphone-call-termination-pattern nil t)
                (setq linphone-call-active nil)
                (linphone-mute)
                (linphone-play-sound linphone-hangup-sound)
                (setq linphone-current-control 'linphone-general-control))
               ((re-search-backward linphone-call-request-pattern nil t)
                (setq linphone-backend-response (match-string 0))
                (setq linphone-current-call (match-string 1))
                (setq linphone-current-control 'linphone-incoming-call-control))
               ((re-search-backward linphone-call-progress-pattern nil t)
                (setq linphone-backend-response (match-string 0))
                (setq linphone-current-call (match-string 1))
                (setq linphone-current-control 'linphone-outgoing-call-control))
               ((re-search-backward linphone-call-failure-pattern nil t)
                (setq linphone-backend-response (match-string 0))
                (linphone-play-sound linphone-hangup-sound)
                (setq linphone-current-control 'linphone-notification))
               (t nil))
              linphone-control-change))
    (when (or linphone-backend-ready
              (not (or linphone-contacts-requested linphone-log-requested)))
      (goto-char (point-max))
      (forward-line 0)
      (delete-region (point-min) (point)))
    (set-marker (process-mark proc) (point-max)))
  (if (and linphone-backend-ready linphone-pending-actions)
      (let ((action (car linphone-pending-actions)))
        (setq linphone-pending-actions (cdr linphone-pending-actions))
        (funcall action))
    (let ((position (and (or linphone-contacts-requested linphone-log-requested)
                         (string-equal (buffer-name) linphone-control-panel) (point))))
      (when linphone-backend-ready
        (setq linphone-contacts-requested nil
              linphone-log-requested nil))
      (when (and linphone-current-control linphone-control-change)
        (funcall linphone-current-control)
        (when position
          (goto-char position))))))

;;}}}
;;{{{ Backend process control

(defconst linphone-buffer " *Linphone interaction*"
  "Linphone interaction buffer name.")

(defun linphone-sentinel (proc event)
  "Cleanup after exiting."
  (when (eq (process-status proc) 'exit)
    (setq linphone-process nil)
    (kill-buffer (process-buffer proc))))

(defun linphone-launch ()
  "Launch and start Linphone backend process."
  (let ((process-connection-type t)
        (default-directory (file-name-as-directory (getenv "HOME"))))
    (setq linphone-process
          (apply 'start-process "Linphone backend"
                 (get-buffer-create linphone-buffer)
                 linphone-backend-program linphone-backend-args)))
  (if (not (and linphone-process
                (eq (process-status linphone-process) 'run)))
      (error "Cannot run Linphone backend program")
    (setq linphone-backend-ready nil
          linphone-online nil
          linphone-call-active nil
          linphone-pending-actions nil
          linphone-contacts-requested nil
          linphone-log-requested nil)
    (set-process-filter linphone-process 'linphone-output-parser)
    (set-process-sentinel linphone-process 'linphone-sentinel)))

;;}}}
;;{{{ Interactive commands

(defun linphone ()
  "The main Linphone entry point.
Start the backend program if necessary and popup control panel."
  (interactive)
  (if (and linphone-process
           (eq (process-status linphone-process) 'run))
      (if (get-buffer linphone-control-panel)
          (pop-to-buffer linphone-control-panel)
        (unless linphone-current-control
          (setq linphone-current-control 'linphone-general-control))
        (funcall linphone-current-control))
    (linphone-launch)
    (setq linphone-current-control 'linphone-general-control)
    (funcall linphone-current-control)))

;;}}}

(provide 'linphone)

;;; linphone.el ends here
