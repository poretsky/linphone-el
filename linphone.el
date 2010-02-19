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

;;}}}
;;{{{ Customizations

(defgroup linphone nil
  "Internet telephone."
  :group 'applications)

(defcustom linphone-backend-program "linphonec"
  "Backend executable program name."
  :type 'string
  :group 'linphone)

(defcustom linphone-mute-command "amixer set Capture,0 nocap"
  "Shell command that effectively mutes microphone."
  :type 'string
  :group 'linphone)

(defcustom linphone-unmute-command "amixer set Capture,0 cap"
  "Shell command that effectively unmutes microphone."
  :type 'string
  :group 'linphone)

;;}}}
;;{{{ Utilities

(defvar linphone-sounds-directory (file-name-directory load-file-name)
  "Directory where sound icons are stored.")

(defvar linphone-sound-play-program "aplay"
  "Program used to play audio icons.")

(defvar linphone-online-sound (expand-file-name "online.wav" linphone-sounds-directory)
  "Sound played when going online.")

(defvar linphone-offline-sound (expand-file-name "offline.wav" linphone-sounds-directory)
  "Sound played when going offline.")

(defvar linphone-hangup-sound (expand-file-name "hangup.wav" linphone-sounds-directory)
  "Sound played when connection is lost.")

(defun linphone-play-sound (icon)
  "Play a sound icon."
  (call-process linphone-sound-play-program
                nil 0 nil "-q" icon))

(defun linphone-mute ()
  "Mute microphone."
  (call-process-shell-command linphone-mute-command))

(defun linphone-unmute ()
  "Unmute microphone."
  (call-process-shell-command linphone-unmute-command))

;;}}}
;;{{{ Backend commands

(defconst linphone-call-command "call %s"
  "Linphone call command format.")

(defconst linphone-answer-command "answer"
  "Linphone answer incoming call command string.")

(defconst linphone-cancel-command "terminate"
  "Linphone current call cancellation command string.")

(defconst linphone-quit-command "quit"
  "Linphone quit command string.")

(defvar linphone-process nil
  "The Linphone backend process handle.")

(defvar linphone-backend-ready nil
  "Indicates that Linphone backend is ready to accept commands.")

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

(defun linphone-call (addr)
  "Arrange outgoing call to specified address."
  (interactive "sAddress to call: ")
  (linphone-command
   (format linphone-call-command addr)))

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
  "Name of buffer for Linphone control widgets.")

(defvar linphone-backend-response nil
  "Save last significant backend response.")

(defvar linphone-current-call nil
  "Current call target id string.")

(defvar linphone-call-active nil
  "Indicates active call state.")

(defun linphone-arrange-control-panel (header)
  "Arrange fresh control panel with specified header."
  (with-current-buffer (get-buffer-create linphone-control-panel)
    (kill-all-local-variables)
    (let ((inhibit-readonly t))
      (erase-buffer))
    (when (fboundp 'remove-overlays)
      (remove-overlays))
    (linphone-control-mode)
    (widget-insert header "\n")))

(defun linphone-mute-button ()
  "Mute/unmute button."
  (widget-create 'toggle
                 :tag "Microphone"
                 :value t
                 :on "Mute"
                 :off "Unmute"
                 :format "%t: %[[%v]%]"
                 :notify (lambda (widget &rest ignore)
                           (if (widget-value widget)
                               (linphone-unmute)
                             (linphone-mute)))))

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

(defun linphone-quit-button ()
  "Linphone quit button."
  (widget-create 'push-button
                 :tag "Quit"
                 :help-echo "Stop and exit telephone completely"
                 :notify (lambda (&rest ignore)
                           (condition-case nil
                               (kill-buffer-and-window)
                             (error nil))
                           (set-process-filter linphone-process t)
                           (linphone-command linphone-quit-command))
                 "Quit"))

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

(defun linphone-customize-button ()
  "Button to enter customization mode."
  (widget-create 'push-button
                 :tag "Customize"
                 :help-echo "Set up some options"
                 :notify (lambda (&rest ignore)
                           (customize-group 'linphone))
                 "Customize"))

(defun linphone-general-control ()
  "General control panel popup."
  (linphone-arrange-control-panel "Internet telephone")
  (with-current-buffer linphone-control-panel
    (linphone-arrange-call-button)
    (widget-insert "    ")
    (linphone-standby-button)
    (widget-insert "\n")
    (linphone-quit-button)
    (widget-insert "    ")
    (linphone-customize-button)
    (widget-insert "\n")
    (widget-setup)
    (widget-forward 1))
  (pop-to-buffer linphone-control-panel))

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

(defun linphone-active-call-control ()
  "Active call control panel popup."
  (linphone-arrange-control-panel (format "Talking with %s"
                                          linphone-current-call))
  (with-current-buffer linphone-control-panel
    (widget-insert "            ")
    (linphone-cancel-button "Hang up")
    (widget-insert "\n")
    (linphone-mute-button)
    (widget-insert "\n")
    (widget-setup)
    (widget-backward 1))
  (pop-to-buffer linphone-control-panel))

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
;;{{{ Parsing backend responses

(defconst linphone-prompt-pattern "^linphonec> "
  "Regular expression that matches against Linphone backend prompt.")

(defconst linphone-call-request-pattern "^\\(.*\\) is contacting you"
  "Regular expression that matches against incoming call request.")

(defconst linphone-call-progress-pattern "^Contacting \\(.*\\)$"
  "Regular expression that matches progress notification message.")

(defconst linphone-call-connection-pattern "Connected"
  "Regular expression that matches against connection acknowledge.")

(defconst linphone-call-failure-pattern
  "User is temporarily unavailable\
\\|Not Acceptable Here\
\\|Forbidden\
\\|Could not reach destination\
\\|Terminate current call first\
\\|Request Timeout\
\\|Call declined\
\\|Internal Server Error\
\\|Bad request"
  "Regular expression that matches against call failure messages.")

(defconst linphone-call-termination-pattern "Call \\(terminat\\|end\\)ed\\|No active call"
  "Regular expression that matches against call termination message.")

(defconst linphone-online-state-string "successful"
  "Online status indication string.")

(defconst linphone-registration-result-pattern
  (format "Registration on .* \\(%s\\|failed\\)" linphone-online-state-string)
  "Regular expression that matches against registration result messages.")

(defconst linphone-offline-pattern "Registration on .* failed"
  "Regular expression that matches against offline indication messages.")

(defvar linphone-online nil
  "Indicates Linphone online state.")

(defvar linphone-current-control nil
  "Current control panel popup function.")

(defvar linphone-control-change nil)

(defun linphone-output-parser (proc string)
  "Filter function to parse Linphone backend output."
  (with-current-buffer (process-buffer proc)
    (goto-char (process-mark proc))
    (insert string)
    (when (re-search-backward linphone-prompt-pattern nil t)
      (replace-match "")
      (setq linphone-backend-ready t))
    (goto-char (point-max))
    (when (re-search-backward linphone-registration-result-pattern nil t)
      (if (string-equal (match-string 1) linphone-online-state-string)
          (unless linphone-online
            (setq linphone-online t)
            (linphone-play-sound linphone-online-sound)
            (message "%s" (match-string 0)))
        (when linphone-online
          (setq linphone-online nil)
          (linphone-play-sound linphone-offline-sound)
          (message "%s" (match-string 0)))))
    (goto-char (point-max))
    (setq linphone-control-change
          (cond
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
            (setq linphone-current-control 'linphone-notification))))
    (goto-char (point-max))
    (forward-line 0)
    (delete-region (point-min) (point))
    (set-marker (process-mark proc) (point-max)))
  (when (and linphone-current-control linphone-control-change)
    (funcall linphone-current-control)))

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
          (start-process "Linphone backend"
                         (get-buffer-create linphone-buffer)
                         linphone-backend-program)))
  (if (not (and linphone-process
                (eq (process-status linphone-process) 'run)))
      (error "Cannot run Linphone backend program")
    (setq linphone-backend-ready nil)
    (setq linphone-online nil)
    (setq linphone-call-active nil)
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
        (if linphone-current-control
            (funcall linphone-current-control)
          (linphone-general-control)))
    (linphone-launch)
    (linphone-general-control)))

;;}}}

(provide 'linphone)

;;; linphone.el ends here
