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


(require 'org-install)
(load-library "org")
(load-library "org-agenda")

;********************* POSSIBLY REUSABLE FUNCTIONS *****************

; bkuhn/most-recent-org-mode-file returns the most recently used org-mode
; buffer on the buffer-list.

(defun bkuhn/most-recent-org-mode-file ()
  (bkuhn/most-recent-org-mode-file-helper (buffer-list)))

(defun bkuhn/most-recent-org-mode-file-helper (buffers)
  (cond ( (null buffers) nil)
        ( (eq (buffer-local-value 'major-mode (car buffers)) 'org-mode)
          (car buffers))
        (t (bkuhn/most-recent-org-mode-file-helper (cdr buffers)))))

(defun bkuhn/org-goto-from-anywhere ()
  "Interactive function that prompts user for all targets in
  `org-refile-targets' and goto that target, which will possibly
  include a buffer switch to the appropriate buffer.  The default
  org-mode file is considered the most recently visited buffer
  currently with its current mode set to org-mode"
  (interactive)
  (let ( (org-goto-start-pos (point))
         (new-position (progn (let*
                          ( (my-selection
                             (org-refile-get-location "Goto "
                                (bkuhn/most-recent-org-mode-file) nil t))
                             (my-pos (nth 3 my-selection))
                             (my-file (nth 1 my-selection)))
                           (find-file my-file)
                           (org-refile-check-position my-selection)
                           my-pos))))
    (if new-position
        (progn
	  (org-mark-ring-push org-goto-start-pos)
          (goto-char new-position)
          (if (or (outline-invisible-p) (org-invisible-p2))
              (org-show-context 'org-goto))))))

(defun bkuhn/org-insert-subheading-at-top-always-hack (arg)
"This hack is designed to force org-insert-subheading to always
force a new subheading at the top immediately below the current
item.  This is done by simply moving forward one character when
we're right on top of the main heading we want a subheading for"
(interactive "P")
(if (looking-at "^\\* ") (forward-char))
(org-insert-subheading arg))

(defun bkuhn/org-todo-done-state ()
  (interactive)
  (org-todo 6))

(defun bkuhn/dairy-style-sunrise-only ()
  "Return Sunrise time only for org-mode-agenda in a diary-ish style"
  (let ((l (solar-sunrise-sunset date)))
    (format
     "%s (%s hours of daylight)"
     (if (car l)
         (concat "Sunrise " (apply 'solar-time-string (car l)))
       "No sunrise")
     (nth 2 l))))
(defun bkuhn/dairy-style-sunset-only ()
  "Return Sunrise time only for org-mode-agenda in a diary-ish style"
  (let ((l (solar-sunrise-sunset date)))
    (format
     "%s (%s hours of daylight)"
     (if (cadr l)
         (concat "Sunset " (apply 'solar-time-string (cadr l)))
       "no sunset")
     (nth 2 l))))


;********************* PERSONAL KEY CONFIGURATIONS *****************

(global-set-key "\C-co" 'bkuhn/org-mode-goto-from-anywhere)

(org-defkey org-agenda-keymap "\C-ct" 'bkuhn/org-todo-done-state)
(org-defkey org-mode-map (kbd "M-C-f") 'org-metaright)
(org-defkey org-mode-map (kbd "M-C-f") 'org-metaleft)
(org-defkey org-agenda-mode-map "S" 'org-agenda-schedule) ; was org-agenda-sunrise-sunset
(org-defkey org-agenda-keymap "S" 'org-agenda-schedule)   ;    ^ which maybe I want automatic?
(org-defkey org-agenda-mode-map "D" 'org-agenda-deadline)  ; was org-agenda-toggle-diary
(org-defkey org-agenda-keymap "D" 'org-agenda-deadline)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-refile-targets '((nil . (:level . 1))))
(setq org-yank-adjusted-subtrees t)
(setq org-log-done t)
(setq org-log-done 'time)
(setq org-log-done 'note)
(setq org-agenda-include-diary t)   ; Note this can and may be overriden by custom agenda commands.
(setq org-fontify-done-headline t)
(setq org-use-fast-todo-selection t)
(setq org-hide-leading-stars t)
(setq org-todo-keywords
       '((sequence "TODO(t@)" "STARTED(s@/!)" "WAITING(w@/!)" "DELEGATED(l@/!)" "APPT(a@/!)" "|" "DONE(d@/!)" "DEFERRED(f@/!)" "CANCELLED(c@/!)")))


; New way to turn on appoitment notification.  This must happen *after* all
; org-mode-y stuff is done.

; This doesn't work.  I probably have to patch org-mode to fix this.

(org-agenda-to-appt '((headline "APPT")))


(appt-activate 1)
