;;; linphone-contacts-lib.el --- Linphone address book shared functions
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

;;; This module provides some Linphone address book functions
;;; and widgets, that are shared by several modules.
;;; It will be automatically loaded by the main Linphone interface
;;; when necessary, so the file should be available on the load path.

;;; Code:

;;}}}
;;{{{ Requirements

(require 'cl-lib)
(require 'custom)
(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(cl-eval-when (load)
  (require 'linphone)
  (require 'linphone-contacts-core))

;;}}}
;;{{{ Forward declarations

(declare-function linphone-command "linphone" (command))

(defvar linphone-pending-actions)

;;}}}
;;{{{ Customizations

(defcustom linphone-contacts-call-command-format "friend call %d"
  "Format string to construct a contact call command.
The number placeholder is to be replaced by the contact index."
  :type 'string
  :group 'linphone-backend)

(defcustom linphone-contacts-add-command-format "friend add \"%s\" %s"
  "Format string to construct a contact add command.
String placeholders are to be replaced by the name and address
of a new contact."
  :type 'string
  :group 'linphone-backend)

;;}}}
;;{{{ Utility functions

(defun linphone-contacts-add (name address)
  "Add a new contact."
  (interactive "sName: \nsAddress: ")
  (unless (and name (string-match-p "\\w" name))
    (error "Invalid contact name"))
  (unless (and address (string-match-p "\\w" address))
    (error "Invalid contact address"))
  (linphone-command (format linphone-contacts-add-command-format name address))
  (setq linphone-pending-actions (list 'linphone-contacts-refresh)))

;;}}}
;;{{{ Control widgets

(defun linphone-contacts-call-button (id name)
  "Button to call the item."
  (widget-create 'push-button
                 :tag "Call"
                 :help-echo (format "Make a call to %s" name)
                 :notify (lambda (button &rest ignore)
                           (linphone-command (format linphone-contacts-call-command-format
                                                     (widget-value button))))
                 id))

;;}}}

(provide 'linphone-contacts-lib)

;;; linphone-contacts-lib.el ends here
