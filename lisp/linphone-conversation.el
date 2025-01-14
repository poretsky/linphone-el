;;; linphone-conversation.el --- Linphone conversation control panel
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

;;; This module provides control panel for the Linphone conversation mode.
;;; It will be automatically loaded by the main Linphone interface
;;; when necessary, so the file should be available on the load path.

;;; Code:

;;}}}
;;{{{ Requirements

(require 'widget)
(require 'wid-edit)

(require 'linphone)
(require 'linphone-display)
(require 'linphone-control)

;;}}}
;;{{{ Control data

(defconst linphone-mute-command "mute"
  "Linphone microphone mute command.")

(defconst linphone-unmute-command "unmute"
  "Linphone microphone unmute command.")

(defconst linphone-transfer-command-format "transfer %s"
  "Linphone transfer command format.
The string placeholder is to be replaced by the actual target address.")

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
                           (linphone-command
                            (if (setq linphone-mic-muted (not (widget-value widget)))
                                linphone-mute-command
                              linphone-unmute-command)))))

(defun linphone-mic-control-button ()
  "Button to adjust microphone gain."
  (widget-create 'push-button
                 :tag "Gain"
                 :help-echo "Adjust microphone gain"
                 :notify (lambda (&rest ignore)
                           (customize-option 'linphone-mic-gain))
                 "Gain"))

(defun linphone-transfer-call-button ()
  "Button to transfer current call."
  (widget-create 'push-button
                 :tag "Transfer this call"
                 :help-echo "Transfer current call to another subscriber"
                 :notify (lambda (&rest ignore)
                           (linphone-command
                            (format linphone-transfer-command-format
                                    (read-string "Address to transfer to: "))))
                 "Transfer this call"))

;;;###autoload
(defun linphone-active-call-control ()
  "Active call control panel popup."
  (linphone-arrange-control-panel (format "Talking with %s"
                                          linphone-current-call))
  (with-current-buffer linphone-control-panel
    (linphone-cancel-button "Hang up")
    (widget-insert "   ")
    (linphone-transfer-call-button)
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
