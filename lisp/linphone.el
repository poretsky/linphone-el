;;; linphone.el --- Emacs frontend for the Linphone program
;;; Author: Igor B. Poretsky <poretsky@mlbox.ru>
;;; Keywords: Linphone, web phone, voip, network sip telephone

;;{{{  Copyright

;;; Copyright (C) 2008-2025  Igor B. Poretsky

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

(require 'cl-lib)
(require 'custom)

;;}}}
;;{{{ Forward declarations

(declare-function linphone-contacts-extract "linphone-contacts-core")
(declare-function linphone-contacts-recognize "linphone-contacts-core" (info))
(declare-function linphone-log-acquire "linphone-log")
(declare-function linphone-schedule-log-update "linphone-display")
(declare-function linphone-general-control "linphone-display" (&optional show))

;;}}}
;;{{{ Customizations

(defvar linphone-sounds-directory
  (file-name-as-directory (expand-file-name "../sounds" (file-name-directory load-file-name)))
  "Directory where sound icons are stored.")

;;;###autoload
(defgroup linphone '((linphone-logs custom-group))
  "Internet telephone."
  :group 'applications)

(defgroup linphone-sounds nil
  "Sound icons for various events."
  :group 'linphone)

(defgroup linphone-backend
  '((linphone-call-command-format custom-variable)
    (linphone-log-get-command custom-variable)
    (linphone-contacts-delete-command-format custom-variable)
    (linphone-register-command-format custom-variable))
  "Communications with external backend programs.
Don't touch this stuff unless you really know what you are doing."
  :group 'linphone)

(defcustom linphone-autostart nil
  "Run Linphone automatically in background
every time when Emacs starts up. There is no sense to set this option
for current session. It affects only the startup sequence."
  :type 'boolean
  :set (lambda (symbol value)
         (custom-set-default symbol value)
         (when value
           (add-hook 'emacs-startup-hook 'linphone)))
  :initialize 'custom-initialize-default
  :require 'linphone
  :group 'linphone)

(defcustom linphone-answer-mode nil
  "Initial answer mode to use on startup.
Actual answer mode can be toggled from the main control panel
at any time. This option only defines the state to start with."
  :type '(radio
          (const :tag "Manual" nil)
          (const :tag "Automatic" t))
  :group 'linphone)

(defcustom linphone-mic-gain nil
  "Microphone gain level in percents.
The value must be in the range 0 through 100 inclusively.
if nil is specified, then the current level will be left untouched."
  :type '(choice (const :tag "Untouched" nil)
                 (integer :tag "Explicit value in percents"))
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (cl-declare (special linphone-mic-tune-command))
         (when value
           (when (or (< value 0) (> value 100))
             (error "Linphone microphone gain value is out of range"))
           (call-process-shell-command (format linphone-mic-tune-command value)))
         (custom-set-default symbol value))
  :set-after '(linphone-mic-tune-command)
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

(defcustom linphone-mute-command "amixer set Capture,0 nocap"
  "Shell command that effectively mutes microphone."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-unmute-command "amixer set Capture,0 cap"
  "Shell command that effectively unmutes microphone."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-mic-tune-command "amixer set Capture,0 %d%%"
  "Format of the shell command to control microphone gain.
This format specification must have one placeholder for numeric
parameter to be replaced by actual gain value."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-autoanswer-enable-command "autoanswer enable"
  "The command string to turn autoanswer mode on."
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

(defcustom linphone-call-failure-patterns
  '("User is \\(busy\\|temporarily unavailable\\)"
    "Not Acceptable Here"
    "Forbidden"
    "Could not \\(reach destination\\|resolve this number\\)"
    "Call \\(failed\\|declined\\)"
    "Terminate current call first"
    "Request Timeout"
    "Internal Server Error"
    "Bad request")
  "Regular expressions that matches against call failure messages."
  :type '(repeat regexp)
  :group 'linphone-backend)

(defcustom linphone-call-termination-pattern "Call \\(terminat\\|end\\)ed\\|No active call"
  "Regular expression that matches against call termination message."
  :type 'regexp
  :group 'linphone-backend)

(defcustom linphone-missed-call-pattern "^You have missed [0-9]+ calls?"
  "Regular expression that matches against missed call message."
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

(defcustom linphone-answer-mode-change-pattern "Auto answer \\(en\\|dis\\)abled"
  "Regexp matching answer mode change messages."
  :type 'regexp
  :group 'linphone-backend)

;;}}}
;;{{{ Utilities

(defvar linphone-mic-muted nil
  "Indicates that the microphone is muted.")

(defun linphone-mute ()
  "Mute microphone."
  (setq linphone-mic-muted t)
  (call-process-shell-command linphone-mute-command))

(defun linphone-unmute ()
  "Unmute microphone."
  (setq linphone-mic-muted nil)
  (when (numberp linphone-mic-gain)
    (call-process-shell-command (format linphone-mic-tune-command linphone-mic-gain)))
  (call-process-shell-command linphone-unmute-command))

(defun linphone-play-sound (icon)
  "Play a sound icon."
  (when icon
    (apply 'call-process linphone-sound-play-program
           nil 0 nil
           (append linphone-sound-play-args (list icon)))))

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

(defvar linphone-autoanswer nil
  "Indicates automatic answer mode.")

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

(defun linphone-autoanswer-enable ()
  "Turn automatic answer mode on."
  (linphone-command linphone-autoanswer-enable-command))

;;}}}
;;{{{ Control means

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
When online, this variable usually contains provider address.")

(defvar linphone-pending-actions nil
  "A queue of pending actions to be done to accomplish the task in progress.
Each action should be represented by function without arguments.")

;;}}}
;;{{{ Parsing backend responses

(defvar linphone-displayed-control nil
  "Stores currently displayed control.")

(defvar linphone-control-change nil
  "Indicates that current control panel is changed and should be updated.")

(defvar linphone-contacts-loaded nil
  "Indicates that the contact list has been loaded already.")

(defun linphone-list-display-update ()
  "Generates special value for linphone-control-change when control
panel should be updated after updating log or contact list info."
  (and (or (eq linphone-current-control 'linphone-general-control)
           (eq linphone-current-control 'linphone-active-call-control))
       (or (not (eq linphone-displayed-control linphone-current-control))
           'save-position)))

(defun linphone-output-parser (proc string)
  "Filter function to parse Linphone backend output."
  (with-current-buffer (process-buffer proc)
    (goto-char (process-mark proc))
    (insert string)
    (while (re-search-backward linphone-prompt-pattern nil t)
      (replace-match "")
      (goto-char (point-max))
      (setq linphone-backend-ready t))
    (when (re-search-backward (format linphone-registration-result-pattern
                                      (concat linphone-online-state-string
                                              "\\|" linphone-offline-state-pattern))
                              nil t)
      (if (string-equal (match-string 2) linphone-online-state-string)
          (unless (and (stringp linphone-online)
                       (string-equal (match-string 1) linphone-online))
            (setq linphone-online (match-string 1)
                  linphone-control-change t)
            (linphone-play-sound linphone-online-sound)
            (message "%s" (match-string 0))
            (unless (or linphone-contacts-loaded
                        (and linphone-autoanswer
                             (null (get-buffer linphone-control-panel))))
              (add-to-list 'linphone-pending-actions 'linphone-contacts-refresh 'append)))
        (when linphone-online
          (when linphone-call-active
            (linphone-mute)
            (linphone-play-sound linphone-hangup-sound))
          (unless (eq linphone-current-control 'linphone-notification)
            (setq linphone-current-control 'linphone-general-control))
          (setq linphone-online nil
                linphone-control-change t)
          (linphone-play-sound linphone-offline-sound)
          (message "%s" (match-string 0))))
      (goto-char (point-max)))
    (when (re-search-backward linphone-missed-call-pattern nil t)
      (message "%s in this session" (match-string 0))
      (goto-char (point-max)))
    (cond
     (linphone-contacts-requested
      (when linphone-backend-ready
        (linphone-contacts-extract)
        (setq linphone-contacts-requested nil
              linphone-control-change (or linphone-control-change
                                          (linphone-list-display-update)))))
     (linphone-log-requested
      (when linphone-backend-ready
        (linphone-log-acquire)
        (setq linphone-log-requested nil
              linphone-control-change (or linphone-control-change
                                          (linphone-list-display-update)))))
     ((re-search-backward linphone-unreg-state-pattern nil t)
      (when linphone-online
        (linphone-play-sound linphone-offline-sound)
        (setq linphone-online nil
              linphone-control-change t)))
     ((re-search-backward linphone-answer-mode-change-pattern nil t)
      (message "%s" (match-string 0)) nil)
     ((and linphone-online
           (re-search-backward linphone-call-connection-pattern nil t))
      (linphone-unmute)
      (setq linphone-call-active t
            linphone-current-control 'linphone-active-call-control
            linphone-control-change t))
     ((and linphone-online
           (re-search-backward linphone-call-termination-pattern nil t))
      (when linphone-call-active
        (linphone-mute))
      (linphone-play-sound linphone-hangup-sound)
      (when (get-buffer linphone-control-panel)
        (linphone-schedule-log-update))
      (setq linphone-call-active nil
            linphone-current-control 'linphone-general-control
            linphone-control-change t))
     ((and linphone-online
           (re-search-backward linphone-call-request-pattern nil t))
      (when linphone-contacts-loaded
        (let ((caller (linphone-contacts-recognize (match-string 1))))
          (when (and caller (aref caller 0))
            (replace-match (format "\"%s\" <%s>"
                                   (aref caller 1) (aref caller 2))
                           t t nil 1)
            (goto-char (point-max))
            (re-search-backward linphone-call-request-pattern nil t))))
      (setq linphone-backend-response (match-string 0)
            linphone-current-call (match-string 1)
            linphone-current-control 'linphone-incoming-call-control
            linphone-control-change t))
     ((and linphone-online
           (re-search-backward linphone-call-progress-pattern nil t))
      (setq linphone-backend-response (match-string 0)
            linphone-current-call (match-string 1)
            linphone-current-control 'linphone-outgoing-call-control
            linphone-control-change t))
     ((and linphone-online
           (re-search-backward
            (let ((pattern (car linphone-call-failure-patterns)))
              (mapc (lambda (reason)
                      (setq pattern (concat pattern "\\|" reason)))
                    (cdr linphone-call-failure-patterns))
              pattern)
            nil t))
      (when linphone-call-active
        (linphone-mute))
      (linphone-play-sound linphone-hangup-sound)
      (when (get-buffer linphone-control-panel)
        (linphone-schedule-log-update))
      (setq linphone-backend-response (match-string 0)
            linphone-call-active nil
            linphone-current-control 'linphone-notification
            linphone-control-change t))
     (t nil))
    (when linphone-backend-ready
      (goto-char (point-max))
      (forward-line 0)
      (delete-region (point-min) (point)))
    (set-marker (process-mark proc) (point-max)))
  (if (and linphone-backend-ready linphone-pending-actions)
      (let ((action (car linphone-pending-actions)))
        (setq linphone-pending-actions (cdr linphone-pending-actions))
        (funcall action))
    (let ((position (and (eq linphone-control-change 'save-position)
                         (string-equal (buffer-name) linphone-control-panel) (point))))
      (when (and linphone-current-control linphone-control-change
                 (or (get-buffer linphone-control-panel)
                     (not (or linphone-autoanswer
                              (eq linphone-current-control 'linphone-general-control)))))
        (funcall linphone-current-control)
        (when position
          (goto-char position))))
    (setq linphone-control-change nil)))

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
          linphone-contacts-requested nil
          linphone-log-requested nil
          linphone-autoanswer linphone-answer-mode
          linphone-pending-actions (when linphone-autoanswer
                                     (list 'linphone-autoanswer-enable)))
    (set-process-filter linphone-process 'linphone-output-parser)
    (set-process-sentinel linphone-process 'linphone-sentinel)))

;;}}}
;;{{{ Interactive commands

;;;###autoload
(defun linphone (&optional show)
  "The main Linphone entry point.
Start the backend program if necessary and popup control panel if
optional argument is non-nil or the function is called interactively."
  (interactive "p")
  (if (and linphone-process
           (eq (process-status linphone-process) 'run))
      (when show
        (if (get-buffer linphone-control-panel)
            (pop-to-buffer linphone-control-panel)
          (unless linphone-current-control
            (setq linphone-current-control 'linphone-general-control))
          (if (eq linphone-current-control 'linphone-general-control)
              (linphone-general-control show)
            (funcall linphone-current-control))))
    (linphone-launch)
    (setq linphone-current-control 'linphone-general-control)
    (when show
      (linphone-general-control show))))

;;}}}

(provide 'linphone)

;;; linphone.el ends here
