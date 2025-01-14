;;; linphone-codecs.el --- Linphone codecs control
;;; Author: Igor B. Poretsky <poretsky@mlbox.ru>
;;; Keywords: Linphone, web phone, voip, network sip telephone

;;{{{  Copyright

;;; Copyright (C) 2025  Igor B. Poretsky

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

;;; This module provides Linphone codecs control functions
;;; providing a facility to enable or disable them individually.
;;; It will be automatically loaded by the main Linphone interface
;;; when necessary, so the file should be available on the load path.

;;; Code:

;;}}}
;;{{{ Requirements

(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(require 'linphone)

;;}}}
;;{{{ Control data

(defconst linphone-codec-general-command-template "codec %s"
  "General template for codec commands constructing.")

(defconst linphone-codecs-list-command
  (format linphone-codec-general-command-template "list")
  "The command for codecs list retrieving.")

(defconst linphone-codec-enable-command-format
  (format linphone-codec-general-command-template "enable %d")
  "The format to construct a command enabling specified codec.")

(defconst linphone-codec-disable-command-format
  (format linphone-codec-general-command-template "disable %d")
  "The format to construct a command disabling specified codec.")

(defconst linphone-codec-item-extractor
  "^ *\\([0-9]+\\): \\(.+\\) \\(\\(?:en\\|dis\\)abled\\)$"
  "Codec list item matching regexp.")

;;}}}
;;{{{ Request data from the backend

;;;###autoload
(defun linphone-codecs-list-refresh ()
  "Refresh codecs info."
  (linphone-command linphone-codecs-list-command)
  (setq linphone-codecs-list-requested t))

;;}}}
;;{{{ Fill out codecs list

(defvar linphone-codecs-list nil
  "List of audio codecs.")

;;;###autoload
(defun linphone-codecs-list-extract ()
  "Parse backend supplied codecs info in current buffer
and store it in the internal list."
  (setq linphone-codecs-list nil)
  (while (re-search-backward linphone-codec-item-extractor nil t)
    (push (vector
           (string-to-number (match-string 1))
           (match-string 2)
           (string-equal (match-string 3) "enabled"))
          linphone-codecs-list)))

;;}}}
;;{{{ Utility functions

(defun linphone-codec-set-status (enable index)
  "Enable or disable codec by index."
  (linphone-command (format
                     (if enable
                         linphone-codec-enable-command-format
                       linphone-codec-disable-command-format)
                     index)))

;;}}}
;;{{{ Control widgets

(defun linphone-codec-item-show (codec-info)
  "Display codec status control widget."
  (widget-create 'checkbox
                 :index (aref codec-info 0)
                 :notify (lambda (widget &rest ignore)
                           (let ((index (widget-get widget ':index))
                                 (value (widget-value widget)))
                             (linphone-codec-set-status value index)
                             (setf (aref (nth index linphone-codecs-list) 2) value)))
                 (aref codec-info 2))
  (widget-insert " " (aref codec-info 1) "\n"))

;;}}}
;;{{{ Show codecs list

;;;###autoload
(defun linphone-codecs-show ()
  "Make the codecs list view."
  (message "Showing linphone codecs")
  (if linphone-codecs-list
      (mapc 'linphone-codec-item-show linphone-codecs-list)
    (widget-insert "No codecs available\n")))

;;}}}

(provide 'linphone-codecs)

;;; linphone-codecs.el ends here
