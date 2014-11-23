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

(defun bkuhn/org-goto-from-anywhere (&optional arg)
  "Interactive function that prompts user with a list of possible
targets, and then switches buffers and moves the point directly
to the chosen target.

By default, the choices will be bound by the user's of
`org-refile-targets'.

The user may add a unversal argument, possibly with an integer,
and that value will be used to set the number of levels deep of
targets to be used in completion.

The function can be called from any buffer (not necessarily one
currently in org-mode).

`org-refile' is used to select the target, so user configuration
 settings related to that should work here."
  (interactive "P")
  (let ((old-org-refile-targets org-refile-targets))
      (let ((org-refile-targets
              (if (null arg) org-refile-targets
                (if (listp arg)
                    `((,(car (car org-refile-targets)) . (:level . ,(truncate (+ 1 (log (car arg) 4))))))
                  `((,(car (car org-refile-targets)) . (:level . ,arg)))))))
        (org-refile '(4) nil nil "Goto "))
        ; Finalize by cleaning up the org-refile-target-table.  It seems that
        ; org-mode effectively caches this, and it has nothing to do with
        ; org-refile-cache, so it must be cleared if we mess with org-refile-targets
      (if (not (eq old-org-refile-targets org-refile-targets)) (setq org-refile-target-table nil))))

; bkuhn/most-recent-org-mode-file returns the most recently used org-mode
; buffer on the buffer-list.

(defun bkuhn/most-recent-org-mode-file ()
  (bkuhn/most-recent-org-mode-file-helper (buffer-list)))

(defun bkuhn/most-recent-org-mode-file-helper (buffers)
  (cond ( (null buffers) nil)
        ( (eq (buffer-local-value 'major-mode (car buffers)) 'org-mode)
          (car buffers))
        (t (bkuhn/most-recent-org-mode-file-helper (cdr buffers)))))

(defun bkuhn/org-goto-from-anywhere-force-default-buffer (&optional arg)
  "Interactive function that prompts user with a list of possible
targets, and then switches buffers and moves the point directly
to the chosen target.

By default, the choices will be bound by the user's of
`org-refile-targets'.

The user may add a unversal argument, possibly with an integer,
and that value will be used to set the number of levels deep of
targets to be used in completion.

The function can be called from any buffer (not necessarily one
currently in org-mode), and the most recently visited buffer that
currently has its mode set to org-mode will be considered the
default buffer.

`org-refile-get-location' is used to select the target, so user
 configuration settings related to that should work here."
  (interactive "P")
  (let ((old-org-refile-targets org-refile-targets))
    (progn
      (let* ((org-refile-targets
              (if (null arg) org-refile-targets
                (if (listp arg)
                    `((,(car (car org-refile-targets)) . (:level . ,(truncate (+ 1 (log (car arg) 4))))))
                  `((,(car (car org-refile-targets)) . (:level . ,arg))))))
             (org-goto-start-pos (point))
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
                  (org-show-context 'org-goto)))))

        ; Finalize by cleaning up the org-refile-target-table.  It seems that
        ; org-mode effectively caches this, and it has nothing to do with
        ; org-refile-cache, so it must be cleared if we mess with org-refile-targets
      (if (not (eq old-org-refile-targets org-refile-targets)) (setq org-refile-target-table nil)))))

(defun bkuhn/org-insert-heading-above-current ()
  "Insert new heading at current level above the current one"
  (interactive)
  (org-back-to-heading t)
  (org-insert-heading nil))

(defun bkuhn/org-insert-heading-below-current ()
  "Insert new heading at current level above below current one"
  (interactive)
  (org-back-to-heading t)
  (org-insert-heading '(16)))

(defun bkuhn/org-insert-subheading-at-top ()
  "Insert a new heading one level below current, after current subtree."
  (interactive)
  (org-back-to-heading t)
  (outline-next-heading)
  (org-insert-heading))

(defun bkuhn/org-insert-subheading-at-bottom ()
  "Insert a new heading one level below current, after current subtree."
  (interactive)
  (org-back-to-heading t)
  (org-insert-heading-after-current)
  (org-demote))

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

(defun bkuhn/skip-unless-appt-or-diary ()
       "Skip trees that are not APPT"
       (org-back-to-heading t)
       (let ( (beg (point))
               (end (progn (outline-next-heading) (1- (point)))))
         (goto-char beg)
         (if (re-search-forward "^\\**[ \t]*\\(%%(diary-\\|APPT\\)" end t)
               nil end)))

(defun bkuhn/org-find-conservancy-project-name ()
  (save-excursion
    (let ((buf (find-file-noselect org-conservancy-bookkeeping-file)))
      (with-current-buffer buf
        (let* ( (headings
                (mapcar (lambda (a) (org-no-properties (car a))) (org-refile-get-targets)))
               (m
                (org-find-olp (list org-conservancy-bookkeeping-file (ido-completing-read "Category: " headings)))))
          (org-capture-put-target-region-and-position)
	  (widen)
	  (goto-char m)
          (forward-line)
          (point-marker))))))


(defun bkuhn/org-template-erc-determine-datetime-of-current-region ()
"bkuhn/org-template-erc-determine-datetime-of-current-region
expects the current region to be a highlighted porition of an ERC
buffer (although the buffer need not be in ERC mode).  This
function attempts to determine the correct date and times of the
beginning and end of the chat conversation, to return a string of
the form:
 \"from <org-mode-formatted-start> to <org-mode-mode-formatted-end>\"

This is used as an %(sexp ) call in bkuhn's org-capture template called erg-log.org-capture-template.
"
  (interactive)
  (let ( (beg (region-beginning))
         (end (region-end))
         (start-date (format-time-string "%a %b %d %Y"))
         (end-date (format-time-string "%a %b %d %Y"))
         (start-time (format-time-string "%H:%M"))
         (end-time (format-time-string "%H:%M"))
         (date-format "<%Y-%m-%d %H:%M>")
         (date-regexp "^\\s-*\\[\\([[:alpha:]]\\{3\\}\\s-*[[:alpha:]]\\{3\\}\\s-*[[:digit:]]\\{1,3\\}\s-*[[:digit:]]\\{1,4\\}\\s-*\\)\\]")
         (time-regexp "\\[\\([[:digit:]]+\\s-*:\\s-*[[:digit:]]+\\)\\]$"))
    (save-excursion
      (goto-char beg)
      (re-search-forward "^[<\\*]" end t)
      (if (re-search-backward date-regexp beg t)
          (setq start-date (match-string 1)))
      (goto-char beg)
      (if (re-search-forward time-regexp end t)
          (setq start-time (match-string 1)))
      (goto-char end)
      (if (re-search-backward date-regexp beg t)
          (setq end-date (match-string 1)))
      (goto-char end)
      (if (re-search-backward time-regexp beg t)
          (setq end-time (match-string 1)))
      (concat "from "
       (format-time-string date-format (apply 'encode-time (org-read-date-analyze (concat start-date " " start-time) nil nil)))
       " to "
       (format-time-string date-format (apply 'encode-time (org-read-date-analyze (concat end-date " " end-time) nil nil)))))))

;********************* PERSONAL KEY CONFIGURATIONS *****************

(global-set-key "\C-cr" 'org-capture)
(global-set-key "\C-co" 'bkuhn/org-goto-from-anywhere)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(org-defkey org-agenda-keymap "\C-ct" 'bkuhn/org-todo-done-state)
(org-defkey org-mode-map (kbd "M-C-f") 'org-metaright)
(org-defkey org-mode-map (kbd "M-C-b") 'org-metaleft)
; Note: S-return was bound to org-table-copy-down when I changed it, but I never used it.
(org-defkey org-mode-map (kbd "<S-return>") 'bkuhn/org-insert-heading-above-current)
(org-defkey org-mode-map (kbd "<C-return>") 'bkuhn/org-insert-heading-below-current)
(org-defkey org-mode-map (kbd "<M-return>") 'bkuhn/org-insert-subheading-at-top)
(org-defkey org-mode-map (kbd "<C-M-return>") 'bkuhn/org-insert-subheading-at-bottom)

(org-defkey org-mode-map (kbd "M-n")    'outline-next-visible-heading)
(org-defkey org-mode-map (kbd "M-p")    'outline-previous-visible-heading)
(org-defkey org-mode-map (kbd "C-M-n")  'org-forward-heading-same-level)
(org-defkey org-mode-map (kbd "C-M-p")  'org-backward-heading-same-level)

(org-defkey org-agenda-mode-map "S" 'org-agenda-schedule) ; was org-agenda-sunrise-sunset
(org-defkey org-agenda-keymap "S" 'org-agenda-schedule)   ;    ^ which maybe I want automatic?
(org-defkey org-agenda-mode-map "D" 'org-agenda-deadline)  ; was org-agenda-toggle-diary
(org-defkey org-agenda-keymap "D" 'org-agenda-deadline)
(org-defkey org-agenda-mode-map (kbd "M-C-b") 'org-agenda-earlier)
(org-defkey org-agenda-mode-map (kbd "M-C-f") 'org-agenda-later)

;********************* VARIABLE SETTINGS *****************

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-refile-targets '((org-agenda-files . (:level . 1))))
(setq org-outline-path-complete-in-steps t)
(setq org-yank-adjusted-subtrees t)
(setq org-completion-use-ido t)
(setq org-log-done t)
(setq org-log-done 'time)
(setq org-log-done 'note)
(setq org-agenda-include-diary t)   ; Note this can and may be overriden by custom agenda commands.
(setq org-fontify-done-headline t)
(setq org-use-fast-todo-selection t)
(setq org-hide-leading-stars t)
(setq org-enforce-todo-dependencies t)
(setq org-todo-keywords
       '((sequence "TODO(t@)" "STARTED(s@/!)" "WAITING(w@/!)" "DELEGATED(l@/!)" "APPT(a@/!)" "|" "DONE(d@/!)" "DEFERRED(f@/!)" "CANCELLED(c@/!)")))


; New way to turn on appoitment notification.  This must happen *after* all
; org-mode-y stuff is done.

; This doesn't work.  I probably have to patch org-mode to fix this.

; (org-agenda-to-appt '((headline "APPT")))


(setq appt-display-format 'window)
(appt-activate 1)
