; bkuhn-org-mode.el                                     -*- Emacs Lisp -*-
;     Various org-mode things that bkuhn uses
;
; Copyright (C) 2014, Bradley M. Kuhn
;
; This program's license grants you software freedom; you can copy, modify,
; convey, and/or redistribute it under the terms of the GNU General Public
; License as published by the Free Software Foundation; either version 3 of
; the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License along
; with this program in a file called 'GPLv3'.  If not, write to the:
;    Free Software Foundation, Inc., 51 Franklin St, Fifth Floor
;                                    Boston, MA 02110-1301, USA.


; bkuhn/most-recent-org-mode-file returns the most recently used org-mode
; buffer on the buffer-list.

(defun bkuhn/most-recent-org-mode-file ()
  (bkuhn/most-recent-org-mode-file-helper (buffer-list)))

(defun bkuhn/most-recent-org-mode-file-helper (buffers)
  (cond ( (null buffers) nil)
        ( (eq (buffer-local-value 'major-mode (car buffers)) 'org-mode)
          (car buffers))
        (t (bkuhn/most-recent-org-mode-file-helper (cdr buffers)))))

