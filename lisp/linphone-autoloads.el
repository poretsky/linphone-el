;;; linphone-autoloads.el --- linphone autoload bindings
;;; Author: Igor B. Poretsky <poretsky@mlbox.ru>

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

;; This file should be loaded from startup to have access
;; to the linphone functionality.

;;}}}
;;{{{ Code:

(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

(require 'linphone-loaddefs)

;;}}}
;;{{{ Global menu bindings:

(define-key global-map [menu-bar linphone] '("Linphone" . linphone))

;;}}}
