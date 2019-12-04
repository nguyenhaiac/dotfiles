(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))


(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

  (setq org-refile-use-outline-path 'file)
  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-agenda-time-grid
	(quote
	 ((daily today remove-match)
	  (900 1100 1300 1500 1700)
	  "......" "----------------")))
(require 'org-habit)
(setq spacemacs-theme-org-agenda-height nil
      org-agenda-start-day "-1d"
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-include-diary t
      org-agenda-block-separator nil
      org-agenda-compact-blocks t
      org-agenda-start-with-log-mode t)

(setq  org-habit-following-days 7
       org-habit-preceding-days 10
       org-habit-show-habits-only-for-today t)
(setq org-agenda-tags-column -102)
(setq org-habit-graph-column 50)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-out-when-done t)
(setq org-clock-persist t)
  (custom-declare-face '+org-todo-active '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
  (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
  (custom-declare-face '+org-todo-onhold '((t (:inherit (bold warning org-todo)))) "")
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "NEXT(n)"
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something is holding up this task; or it is paused
           "|"
           "DONE(d)"  ; Task successfully completed
           "PHONE(p)"
           "MEETING(m)"
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")) ; Task was completed
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ))
  (setq org-agenda-files (list "~/Dropbox/orggtd/todo.org"))
  (use-package org-super-agenda
    :config
    (org-super-agenda-mode))
  (setq org-agenda-custom-commands
	'(("z" "Super zaen view"
	   ((agenda "" ((org-agenda-span 3)
			(org-super-agenda-groups
			 '((:name "Habit"
				  :habit t)

			   (:name "Schedule"
				  :time-grid t
				  :scheduled t
				  :order 2)
			   (:discard (:anything t))))))

	    (alltodo "" ((org-agenda-overriding-header "Project Task")
			(org-agenda-skip-function 'bh/skip-non-project-tasks)
			(org-super-agenda-groups
			 '((:name none
				   :todo t
				   :order 1)))))

	    (alltodo "" ((org-agenda-overriding-header "Active Project")
			 (org-super-agenda-groups
			  '((:name none
				   :children "NEXT"
				   :order 1)
			    (:discard (:anything t))))))

	    (alltodo "" ((org-agenda-overriding-header "Next Task")
			 (org-super-agenda-groups
			  '((:name none
				   :discard (:not (:todo "NEXT"))
				   :discard (:habit)
				   :order 1)
			    (:name none
				   :todo "NEXT"
				   :face (:background "" :underline t))
			    ))))

	    (alltodo "" ((org-agenda-overriding-header "Doing")
			 (org-super-agenda-groups
			  '((:name none
				   :discard (:not (:todo ("STRT" "[-]")))
				   :discard (:habit)
				   :order 1)
			    (:name none
				   :todo t
				   :face (:background "blue" :underline t))
			    ))))

	    (alltodo "" ((org-agenda-overriding-header "Standalone Task")
			 (org-agenda-skip-function 'bh/skip-project-tasks)
			 (org-super-agenda-groups
			  '((:name none
				   :todo ("TODO" "[ ]" "WAIT" "[?]")
				   :order 1)
			    (:discard (:anything t))))))

	    (alltodo "" ((org-agenda-overriding-header "Stuck Project")
			 (org-super-agenda-groups
			  '((:name none
				   :discard (:children "NEXT")
				   :order 1)
			    (:name none
				   :discard (:children nil)
				   :order 1)
			    (:name none
				   :children todo)))))
	    ))))

(setq org-capture-templates
      (quote (("t" "todo" entry (file+headline "~/Dropbox/orggtd/todo.org" "Inbox")
               "* [ ] %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("n" "note" entry (file "~/Dropbox/orggtd/todo.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/Dropbox/orggtd/journal.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file "~/Dropbox/orggtd/todo.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/Dropbox/orggtd/todo.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/Dropbox/orggtd/todo.org")
	       "* NEXT %?\nSCHEDULED: <%<%Y-%m-%d %a .+1d>>\n:PROPERTIES:\n:CREATED: %U\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:LOGGING: DONE(!)\n:ARCHIVE: %%s_archive::* Habits\n:END:\n%U\n"
	       ))))

(provide 'orginit)
