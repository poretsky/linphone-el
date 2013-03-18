;;; linphone-conversation.el --- Linphone conversation control panel
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

;;; This module provides control panel for the Linphone conversation mode.
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
(require 'linphone-control)

;;}}}
;;{{{ Customizations

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

(defcustom linphone-mic-gain nil
  "Microphone gain level in percents.
The value must be in the range 0 through 100 inclusively.
if nil is specified, then the current level will be left untouched."
  :type '(choice (const :tag "Untouched" nil)
                 (integer :tag "Explicit value in percents"))
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (when value
           (when (or (< value 0) (> value 100))
             (error "Linphone microphone gain value is out of range"))
           (call-process-shell-command (format linphone-mic-tune-command value)))
         (custom-set-default symbol value))
  :set-after '(linphone-mic-tune-command)
  :group 'linphone)

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

;;}}}
;;{{{ Control widgets

(defun linphone-mute-button ()
  "Mute/unmute button."
  (widget-create 'toggle
                 :tag "Microphone"
                 :value (not linphone-mic-muted)
                 :on "Mute"
                 :off "Unmute"
                 :format "%t: %[[%v]%]"
                 :notify (lambda (widget &rest ignore)
                           (if (widget-value widget)
                               (linphone-unmute)
                             (linphone-mute)))))

(defun linphone-mic-control-button ()
  "Button to adjust microphone gain."
  (widget-create 'push-button
                 :tag "Gain"
                 :help-echo "Adjust microphone gain"
                 :notify (lambda (&rest ignore)
                           (customize-option 'linphone-mic-gain))
                 "Gain"))

(defun linphone-active-call-control ()
  "Active call control panel popup."
  (linphone-arrange-control-panel (format "Talking with %s"
                                          linphone-current-call))
  (with-current-buffer linphone-control-panel
    (widget-insert "            ")
    (linphone-cancel-button "Hang up")
    (widget-insert "\n")
    (linphone-mute-button)
    (widget-insert "  ")
    (linphone-mic-control-button)
    (widget-insert "\n")
    (linphone-panel-footer)
    (widget-setup)
    (widget-forward 2))
  (pop-to-buffer linphone-control-panel))

;;}}}

(provide 'linphone-conversation)

;;; linphone-conversation.el ends here
