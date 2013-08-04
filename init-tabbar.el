;; init-tabbar.el

;; version 1.00 -- frames / tab-groups:  SYSTEM, MAIN, WANDERLUST, ORG
;; Tested with Tabbar version 2.0; and Emacs Trunk version 24.3.50 (9.0).

;; If using (desktop-save-mode 1), then also use (setq desktop-restore-frames nil)

;; authored, in part, by lawlist -- modifying functions at the following links:
;; http://www.emacswiki.org/emacs/TabBarMode
;; https://github.com/bamanzi/dotemacs-full/blob/master/init.d/25-tabbar.el
;; https://gist.github.com/Johniel/4324127
;; http://stackoverflow.com/questions/17823448/if-frame-named-xyz-exists-then-switch-to-that-frame
;; http://blog.andy.glew.ca/2012_10_02_archive.html

(require 'tabbar)
(tabbar-mode t)
(setq tabbar-cycle-scope 'tabs)
(setq ido-enable-flex-matching t)

(global-set-key [(f5)] (function (lambda () (interactive) (refresh-frames-and-tab-groups)))) ;; manual refresh
(define-key global-map [?\s-\~] 'cycle-backward-frames-groups)
(define-key global-map [?\s-\`] 'cycle-forward-frames-groups)
(global-set-key [(control shift tab)] 'tabbar-backward-group)
(global-set-key [(control tab)]       'tabbar-forward-group) 
(define-key global-map [?\s-w] (function (lambda () (interactive) (delete-frame-if-empty) )))

;; Users will need to add additional hooks for specific modes that do not open files
;; and some not so commonly used functions such as `rename-buffer`.  Rather than use
;; a kill-buffer-hook, I linked the manual refresh to a define-key function that kills
;; a buffer -- an empty frame will be deleted if there are no tab groups remaining.

(add-hook 'find-file-hook
  (lambda()
    (frames-and-tab-groups)
  ))

(add-hook 'org-agenda-mode-hook
  (lambda ()
    (frames-and-tab-groups)
  ))

(add-hook 'wl-draft-mode-hook
  (lambda ()
    (frames-and-tab-groups)
  ))

(add-hook 'wl-summary-mode-hook
  (lambda ()
    (frames-and-tab-groups)
  ))

(add-hook 'wl-folder-mode-hook
  (lambda ()
    (frames-and-tab-groups)
  ))

(add-hook 'emacs-startup-hook
  (lambda ()
(frames-and-tab-groups) ;; needed if desktop restores a file to a tab group other than "system".
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DIAGNOSTIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tabbar-info ()
"Diagnostic tabbar data."
(interactive)
(tabbar-current-tabset 't) ;; refresh
(tabbar-display-update) ;; refresh
(message "---------------------- tabbar-info ---------------------")
(message "Tab - Focus:  %s" (tabbar-selected-tab tabbar-current-tabset))
(message "Tabs Group Focus:  %s" (tabbar-tabs tabbar-current-tabset))
(message "Group - Focus:  %s" tabbar-current-tabset)
(message "Frame - Focus:  %s" (frame-parameter nil 'name))
(message "All Frames:  %s" (mapcar (lambda (frame) (frame-parameter frame 'name)) (frame-list)) )
(message "Buffer - Focus:  %s" (buffer-name))
(message "Buffers Group Focus:  %s" (mapcar (lambda (tab) (buffer-name (tabbar-tab-value tab))) (tabbar-tabs (tabbar-current-tabset t))))
(message "All Groups:  %s" (mapcar #'(lambda (group) (format "%s" (cdr group))) (tabbar-tabs tabbar-tabsets-tabset)))
(message "All Tabs (Per Group) -- Focus:  %s" (tabbar-tabs (tabbar-get-tabsets-tabset)))
(message "Focus + Next:  %s" (cdr (tabbar-tabs (tabbar-get-tabsets-tabset))))
(message "Previous:  %s" (car (tabbar-tabs (tabbar-get-tabsets-tabset))))
(message "--------------------------------------------------------")
(switch-to-buffer "*Messages*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BUFFER MODIFICATION STATE INDICATOR
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
   (setq ad-return-value
         (if (and (buffer-modified-p (tabbar-tab-value tab))
                  (buffer-file-name (tabbar-tab-value tab)))
             (concat " + " (concat ad-return-value " "))
           (concat " " (concat ad-return-value " ")))))
(defun ztl-modification-state-change ()
   (tabbar-set-template tabbar-current-tabset nil)
   (tabbar-display-update))
(defun ztl-on-buffer-modification ()
   (set-buffer-modified-p t)
   (ztl-modification-state-change))
(add-hook 'after-save-hook 'ztl-modification-state-change)
(add-hook 'first-change-hook 'ztl-on-buffer-modification)


;; The variable `tabbar+displayed-buffers` is used to show
;; buffers with astericks when switching between tab groups
;; with either `tabbar-forward-group` or `tabbar-backward-group`.
;; The function `buffer-lawlist-function` hides buffers with
;; astericks -- tabbar+displayed-buffers show select buffers.
;; For example, the buffers "*scratch*" and "*Messages* can be
;; made visible by specifically defining the names within the
;; variable `tabbar+displayed-buffers` AND also by adding the
;; following entry in `tabbar-buffer-groups`:
;;   ((member (buffer-name) '("*scratch*" "*Messages*")) "SYSTEM")
;; If *scratch* is a text-mode file and not otherwise defined into
;; a particular tabbar-buffer-group based upon "buffer-name", then
;; *scratch* will appear in the tab group for text-mode files.
(defvar tabbar+displayed-buffers '("*scratch*" "*Messages*" "*TODO*" "*Org Agenda*"
  "*BBDB*" "*bbdb*" "*Completions*" "*Org-toodledo-log*" "*Calendar*" "*Buffer List*"
"*BUFFER LIST*" "*Help*" "*Compile-Log*")
  "*Reagexps matches buffer names always included tabs.")

;; The list of buffers put in tabs is provided by the function
;; specified in the variable `tabbar-buffer-list-function'.  The
;; default function: `tabbar-buffer-list', excludes buffers whose name
;; starts with a space, when they are not visiting a file.
(setq tabbar-buffer-list-function 'buffer-lawlist-function)
(defun buffer-lawlist-function ()
  (let* ((hides (list ?\ ?\*))
  (re (regexp-opt tabbar+displayed-buffers))
  (cur-buf (current-buffer))
  (tabs (delq nil
  (mapcar (lambda (buf)
  (let ((name (buffer-name buf)))
  (when (and (not (string-match "*lawlist:.**" name)) ;; still not sure what this line does
  (or (string-match re name)
  (not (memq (aref name 0) hides))))
  buf)))
  (buffer-list)))))
  (if (memq cur-buf tabs)
  tabs
  (cons cur-buf tabs))))

(defun tabbar-choice ()
(interactive)
  "Switch between tabbar modes."
  (message "Choose:  [d]efault | [c]ourt | [a]ll | ido- [t]ab [f]rame [g]roup | [v/h] Tile | [G]roups " )

  (let*

  ( (a (read-char-exclusive))

  (choice (case a

  ((?d)
  (setq tabbar-buffer-list-function 'buffer-lawlist-function)
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
  (define-key global-map [?\s-\~] 'cycle-backward-frames-groups)
  (define-key global-map [?\s-\`] 'cycle-forward-frames-groups)
  (tabbar-display-update)
  (sit-for 0)
  (message "You have chosen: \"primary grouping\""))
  
  ((?c)
  (setq tabbar-buffer-list-function 'buffer-lawlist-function)
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
  (define-key global-map [?\s-\`] nil)
  (define-key global-map [?\s-\`] (function (lambda () (interactive)
    (if (equal "MAIN" (frame-parameter nil 'name))
      (progn
        (get-frame "ORG")
        (get-group "org"))
      (get-frame "MAIN")
      (get-group "main")))))
  (tabbar-display-update)
  (sit-for 0)
  (message "You have chosen: \"COURT\""))
  
  ((?a)
  (setq tabbar-buffer-list-function 'tabbar-buffer-list)
  (setq tabbar-buffer-groups-function (lambda () (list "lawlist")))
  (define-key global-map [?\s-\~] 'tabbar-backward-tab)
  (define-key global-map [?\s-\`] 'tabbar-forward-tab)
  (tabbar-display-update)
  (sit-for 0)
  (message "You have chosen: \"everything\""))
  
  ((?t)
  (tabbar-display-update)
  (sit-for 0)
  (ido-tab))
  
  ((?g)
  (tabbar-display-update)
  (sit-for 0)
  (ido-group))
  
  ((?f)
  (tabbar-display-update)
  (sit-for 0)
  (ido-frame))

  ;; This function requires installation of frame-bufs by Al Parker and substantial
  ;; modifications if using a current version of Emacs -- see notes down below.
  ;; http://www.gnu.org/software/emacs/
  ((?F)
  (tabbar-display-update)
  (sit-for 0)
  (ido-frame-bufs))

  ;; requires installation of both frame-cmds and frame-fns
  ;; http://www.emacswiki.org/emacs/frame-cmds.el
  ;; http://www.emacswiki.org/emacs/frame-fns.el
  ((?v)
  (refresh-frames-and-tab-groups)
  (tile-frames-vertically)
  (message "You have selected tile-frames-vertically."))

  ;; requires installation of both frame-cmds and frame-fns
  ;; http://www.emacswiki.org/emacs/frame-cmds.el
  ;; http://www.emacswiki.org/emacs/frame-fns.el
  ((?h)
  (refresh-frames-and-tab-groups)
  (tile-frames-horizontally)
  (message "You have selected tile-frames-vertically."))

  ((?G)
  (tabbar-buffer-show-groups-toggle-switch)
  (tabbar-display-update)
  (message "All groups have been revealed."))
  
  (t (message "No changes have been made."))

)))))

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
  Return a list of one element based on major mode."
  (list
   (cond

    ((or (get-buffer-process (current-buffer))
         ;; Check if the major mode derives from `comint-mode' or
         ;; `compilation-mode'.
         (tabbar-buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "process")

    ((eq major-mode 'org-mode)
     "org")

    ((member (buffer-name)
      '("*TODO*" "*Org Agenda*"))
        "org")

;; TRUMPS ALL ATTEMPTS AT OTHERWISE CATEGORIZING BUFFERS WITH ASTERICKS
;;       ((string-equal "*" (substring (buffer-name) 0 1))
;;       "system")

    ((member (buffer-name)
      '("*scratch*" "*Messages*" "*bbdb*" "*Org-toodledo-log*" "*Calendar*" "*Buffer List*" "*BUFFER LIST*" "*Help*" "*Compile-Log*"))
        "system")

    ((eq major-mode 'dired-mode)
     "dired")

    ((member (buffer-name)
      '("Folder" "Summary" "Email"))
        "wanderlust")

    ((memq major-mode
        '(wl-summary-mode wl-original-message-mode wl-draft-mode mime-view-mode wl-message-mode wl-folder-mode
        rail-mode rmail-edit-mode vm-summary-mode vm-mode mail-mode mh-letter-mode mh-show-mode mh-folder-mode
        gnus-summary-mode message-mode gnus-group-mode gnus-article-mode score-mode gnus-browse-killed-mode))
     "wanderlust")

    ((memq major-mode
           '(text-mode latex-mode help-mode apropos-mode Info-mode Man-mode))
     "main")

    (t
     ;; Return `mode-name' if not blank, `major-mode' otherwise.
      (if (and (stringp mode-name)
              ;; Take care of preserving the match-data because this
              ;; function is called when updating the header line.
              (save-match-data (string-match "[^ ]" mode-name)))
         mode-name
       (symbol-name major-mode))
     ))))

(defun tabbar-buffer-show-groups-toggle-switch ()
  (interactive)
  (if (and tabbar-mode tabbar--buffer-show-groups)
      (tabbar-buffer-show-groups nil)
    (tabbar-buffer-show-groups t) )  )

(defun ido-group ()
  "Jump to a tabbar group."
  (interactive)
  (if (< emacs-major-version 24)
      (ido-common-initialization))
  (unless (and (featurep 'tabbar)
               tabbar-mode)
    (error "Error: tabbar-mode not turned on."))
  (set tabbar-tabsets-tabset (tabbar-map-tabsets 'tabbar-selected-tab)) ;; refresh 1 of 3
  (tabbar-scroll tabbar-tabsets-tabset 0)  ;; refresh 2 of 3
  (tabbar-set-template tabbar-tabsets-tabset nil)  ;; refresh 3 of 3
  (let* ( (groups (mapcar #'(lambda (group)
                              (format "%s" (cdr group)))
                          (tabbar-tabs tabbar-tabsets-tabset)))
          (group-name (ido-completing-read "Groups: " groups)) )
    (mapc #'(lambda (group)
              (when (string= group-name (format "%s" (cdr group)))
                  (message "Switch to group '%s', current buffer: %s" (cdr group) (car group))
                  (switch-to-buffer (car group)) ))
          (tabbar-tabs tabbar-tabsets-tabset)))
  (frames-and-tab-groups) )

(defun get-group (group-name)
  "Jump to a specific tabbar group."
  (unless (and (featurep 'tabbar)
               tabbar-mode)
    (error "Error: tabbar-mode not turned on."))
  (set tabbar-tabsets-tabset (tabbar-map-tabsets 'tabbar-selected-tab)) ;; refresh 1 of 3
  (tabbar-scroll tabbar-tabsets-tabset 0)  ;; refresh 2 of 3
  (tabbar-set-template tabbar-tabsets-tabset nil)  ;; refresh 3 of 3
  (let* ( (groups (mapcar #'(lambda (group) (format "%s" (cdr group))) (tabbar-tabs tabbar-tabsets-tabset))))
    (mapc #'(lambda (group)
              (when (string= group-name (format "%s" (cdr group)))
                  (if (not (equal (format "%s" (car group)) "#<killed buffer>") )
                    (progn
                      (switch-to-buffer (car group))
                      (message "Switch to group '%s', current buffer: %s" (cdr group) (car group))
                    )
                    ;; else
                    (message "(car group):  %s" (car group))
                    (and (eq header-line-format tabbar-header-line-format)
                      (eq tabbar-current-tabset-function 'tabbar-buffer-tabs)
                      (eq (current-buffer) (window-buffer (selected-window)))
                      (let ((bl (tabbar-tab-values (tabbar-current-tabset)))
                            (b  (current-buffer))
                            found sibling)
                      (while (and bl (not found))
                        (if (eq b (car bl))
                          (setq found t)
                            (setq sibling (car bl)))
                        (setq bl (cdr bl)))
                        (when (and (setq sibling (or (car bl) sibling))
                              (buffer-live-p sibling))
                           ;; Move sibling buffer in front of the buffer list.
                          (save-current-buffer (switch-to-buffer sibling))
                          (message "sibling:  %s" sibling))) )
                  )
               )
            )
      (tabbar-tabs tabbar-tabsets-tabset)
    )
  )
 )

(defun ido-tab ()
  "Jump to a buffer in current tabbar group."
  (interactive)
  (if (< emacs-major-version 24)
      (ido-common-initialization))
  (unless (and (featurep 'tabbar)
               tabbar-mode)
    (error "Error: tabbar-mode not turned on."))
  (let* ( ;; Swaps the current buffer name with the next one along.
         (visible-buffers (mapcar (lambda (tab) (buffer-name (tabbar-tab-value tab)))
                                  (tabbar-tabs (tabbar-current-tabset t))))
         (buffer-name (ido-completing-read "Buffer: " visible-buffers))
         window-of-buffer)
    (if (not (member buffer-name visible-buffers))
        (error "'%s' does not have a visible window" buffer-name)
      (switch-to-buffer buffer-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FRAMES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lawlist-new-frame (frame-name)
(interactive "sSelect a frame name: ")
(make-frame)
(set-frame-name frame-name))

(defun ido-frame ()
  (interactive)
  (setq frame-to (ido-completing-read "Select Frame:  " (mapcar (lambda (frame) (frame-parameter frame 'name)) (frame-list))))
;;  (setq frame-to (read-string (format "From: (%s) => To: %s.  Select: "
;;    ;;  From:
;;    (frame-parameter nil 'name)
;;    ;;  To:
;;    (mapcar
;;      (lambda (frame) "print frame"
;;        (reduce 'concat
;;          (mapcar (lambda (s) (format "%s" s))
;;            (list "|" (frame-parameter frame 'name) "|" )
;;          )
;;        )
;;      )
;;    (frame-list) )
;;  )))
  (setq frame-from (frame-parameter nil 'name))
  (let ((frames (frame-list)))
    (catch 'break
      (while frames
        (let ((frame (car frames)))
          (if (equal (frame-parameter frame 'name) frame-to)
            (throw 'break 
              (progn
                (select-frame-set-input-focus frame)
                (message "Switched -- From: \"%s\"  To: \"%s\"." frame-from frame-to)
              )
            )
            (setq frames (cdr frames)))))) )
  (if (equal "MAIN" (frame-parameter nil 'name))
    (get-group "main"))
  (if (equal "SYSTEM" (frame-parameter nil 'name))
    (get-group "system"))
  (if (equal "ORG" (frame-parameter nil 'name))
    (get-group "org"))
  (if (equal "WANDERLUST" (frame-parameter nil 'name))
    (get-group "wanderlust")))

(defun frame-exists (frame-name)
  (not (eq nil (get-frame frame-name))))
(defun get-frame (frame-to)
  (setq frame-from (frame-parameter nil 'name) )
  (let ((frames (frame-list)))
    (catch 'break
      (while frames
        (let ((frame (car frames)))
          (if (equal (frame-parameter frame 'name) frame-to)
            ;; then
            (throw 'break (progn
              (select-frame-set-input-focus frame)
              (message "Switched -- From: \"%s\"  To: \"%s\"." frame-from frame-to)) )
            ;; else
            (setq frames (cdr frames))
          )
        )
      )
    )
  )
)

(defun frame-exists-system ()
(interactive)
  (if (frame-exists "SYSTEM")
    ;; then
    (progn
      (message "The frame named \"SYSTEM\" already exists -- do not create.")
      (get-group "system")
    )
    ;; else
    (message "The frame named \"SYSTEM\" does not exist -- create frame." )
    ;; take control of an existing frame not yet specially named.
    (if
      (and
        (not (equal "MAIN" (frame-parameter nil 'name)))
        (not (equal "SYSTEM" (frame-parameter nil 'name)))
        (not (equal "ORG" (frame-parameter nil 'name)))
        (not (equal "WANDERLUST" (frame-parameter nil 'name)))
      )
      ;; then
      (progn
        (toggle-frame-maximized)
        (set-frame-name "SYSTEM")
      )
      ;; else
        (new-frame)
        (toggle-frame-maximized)
        (set-frame-name "SYSTEM") )

    (get-group "system")
  )
)

(defun frame-exists-main ()
(interactive)
  (if (frame-exists "MAIN")
    ;; then
    (progn
      (message "The frame named \"MAIN\" already exists -- do not create.")
      (get-group "main")
    )
    ;; else
    (message "The frame named \"MAIN\" does not exist -- create frame." )
    ;; take control of an existing frame not yet specially named.
    (if
      (and
        (not (equal "MAIN" (frame-parameter nil 'name)))
        (not (equal "SYSTEM" (frame-parameter nil 'name)))
        (not (equal "ORG" (frame-parameter nil 'name)))
        (not (equal "WANDERLUST" (frame-parameter nil 'name)))
      )
      ;; then
      (progn
        (toggle-frame-maximized)
        (set-frame-name "MAIN")
      )
      ;; else
        (new-frame)
        (toggle-frame-maximized)
        (set-frame-name "MAIN") )
    (get-group "main")
  )
)

(defun frame-exists-org ()
(interactive)
  (if (frame-exists "ORG")
    ;; then
    (progn
      (message "The frame named \"ORG\" exists -- do not create.")
      (get-group "org")
    )
    ;; else
    (message "The frame named \"ORG\" does not exist -- create frame." )
    ;; take control of an existing frame not yet specially named.
    (if
      (and
        (not (equal "MAIN" (frame-parameter nil 'name)))
        (not (equal "SYSTEM" (frame-parameter nil 'name)))
        (not (equal "ORG" (frame-parameter nil 'name)))
        (not (equal "WANDERLUST" (frame-parameter nil 'name)))
      )
      ;; then
      (progn
        (toggle-frame-maximized)
        (set-frame-name "ORG")
      )
      ;; else
        (new-frame)
        (toggle-frame-maximized)
        (set-frame-name "ORG") )
    (get-group "org")
  )
)

(defun frame-exists-wanderlust ()
(interactive)
  (if (frame-exists "WANDERLUST")
    ;; then
    (progn
      (message "The frame named \"WANDERLUST\" already exists -- do not create.")
      (get-group "wanderlust")
    )
    ;; else
    (message "The frame named \"WANDERLUST\" does not exist -- create frame." )
    ;; take control of an existing frame not yet specially named.
    (if
      (and
        (not (equal "MAIN" (frame-parameter nil 'name)))
        (not (equal "SYSTEM" (frame-parameter nil 'name)))
        (not (equal "ORG" (frame-parameter nil 'name)))
        (not (equal "WANDERLUST" (frame-parameter nil 'name)))
      )
      ;; then
      (progn
        (toggle-frame-maximized)
        (set-frame-name "WANDERLUST")
      )
      ;; else
        (new-frame)
        (toggle-frame-maximized)
        (set-frame-name "WANDERLUST") )
    (get-group "wanderlust")
  )
)

(defun frames-and-tab-groups ()
(interactive)
  (tabbar-current-tabset t)
  (if (equal (format "%s" tabbar-current-tabset) "main")
      (frame-exists-main))
  (if (equal (format "%s" tabbar-current-tabset) "system")
      (frame-exists-system))
  (if (equal (format "%s" tabbar-current-tabset) "org")
      (frame-exists-org))
  (if (equal (format "%s" tabbar-current-tabset) "wanderlust")
      (frame-exists-wanderlust)) )

(defun refresh-frames-and-tab-groups ()
(interactive)
  (setq current-frame (frame-parameter nil 'name))
  (get-group "wanderlust") (frames-and-tab-groups)
  (get-group "system") (frames-and-tab-groups)
  (get-group "main") (frames-and-tab-groups)
  (get-group "org") (frames-and-tab-groups)
  (get-frame "WANDERLUST") (get-group "wanderlust")
  (get-frame "SYSTEM") (get-group "system")
  (get-frame "MAIN") (get-group "main")
  (get-frame "ORG") (get-group "org")
  (get-frame current-frame))

(defun cycle-forward-frames-groups ()
  "Cycle to next available fame / group."
(interactive)
  (other-frame 1)
  (if (equal "MAIN" (frame-parameter nil 'name))
    (get-group "main"))
  (if (equal "SYSTEM" (frame-parameter nil 'name))
    (get-group "system"))
  (if (equal "ORG" (frame-parameter nil 'name))
    (get-group "org"))
  (if (equal "WANDERLUST" (frame-parameter nil 'name))
    (get-group "wanderlust")))

(defun cycle-backward-frames-groups ()
  "Cycle to next available fame / group."
(interactive)
  (other-frame -1)
  (if (equal "MAIN" (frame-parameter nil 'name))
    (get-group "main"))
  (if (equal "SYSTEM" (frame-parameter nil 'name))
    (get-group "system"))
  (if (equal "ORG" (frame-parameter nil 'name))
    (get-group "org"))
  (if (equal "WANDERLUST" (frame-parameter nil 'name))
    (get-group "wanderlust")))

;;  (tabbar-current-tabset 't) ;; refresh
;;  (tabbar-display-update) ;; refresh
;;  Tabbar 2.0 uses `tabbar-buffer-track-killed` (linked to a `kill-buffer-hook`) to cleanup
;;  and select the post-kill buffer.  There are many buffers throughout Emacs that exist only
;;  for a brief moment in time, which are not visible to the naked eye.  While it may be true
;;  that `tabbar-buffer-track-killed` serves a useful purpose, it is nevertheless responsible
;;  for setting existing tabs to groups of "nil" when a buffer is killed manually -- this
;;  makes it extremely difficult to detect whether a frame is still associated with a
;;  particular tab group.  Temporarily removing the `kill-buffer-hook` (linked to
;;  `tabbar-buffer-track-killed`) appears to fix this dilemma.
;;
;; TODO -- add an exception for when *Buffer List* (group system) is called when a different
;; frame / group is has focus -- merely buried (or keyboard-quit...) versus killed.
;; 
;; TODO -- *Help* buffer -- same thing.
;;
(defun delete-frame-if-empty ()
(interactive)
  (if
    (and
      (not (equal "SYSTEM" (frame-parameter nil 'name))) ;; CONSIDER USING ONLY THIS CONDITION
      (not (equal (buffer-name) "*scratch*"))
      (not (equal (buffer-name) "*bbdb*"))
      (not (equal (buffer-name) "*Messages*"))
    )
  ;; THEN
  (progn
    (remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)
    (kill-buffer nil)
    (add-hook 'kill-buffer-hook 'tabbar-buffer-track-killed)
    (if
      (and
        (equal "WANDERLUST" (frame-parameter nil 'name))
        (not (equal (format "%s" tabbar-current-tabset) "wanderlust")))
        (delete-frame))
    (if
      (and
        (equal "ORG" (frame-parameter nil 'name))
        (not (equal (format "%s" tabbar-current-tabset) "org")))
        (delete-frame))
    (if
      (and
        (equal "MAIN" (frame-parameter nil 'name))
        (not (equal (format "%s" tabbar-current-tabset) "main")))
        (delete-frame))
  )
  ;; ELSE
  (if (not (equal (buffer-name) "*Messages*"))
    ;; then
    (kill-buffer nil)
    ;; else
    (message "Please leave the *Messages* buffer open, or use M-x kill-buffer or kill-this-buffer, or use C-x k.") )
;;    (kill-buffer nil)
;;    (if (buffer-exists "*scratch*")
;;      (switch-to-buffer "*scratch*")
;;      (find-file "~/.0.data/.0.emacs/*scratch*")) 
  ))

(defun print-frame-info ()
 (interactive)
  (message "%s"
    (mapcar
      (lambda (frame) "print frame"
        (reduce 'concat
          (mapcar (lambda (s) (format "%s" s))
            (list
            "TITLE=" (frame-parameter frame 'title) "\n"
            "   NAME=" (frame-parameter frame 'name) "\n"
            "   explicit-name=" (frame-parameter frame 'explicit-name) "\n"
            "   display=" (frame-parameter frame 'display) "\n"
            "   frame-height X frame-width=" (frame-height frame) "x" (frame-width frame) "\n"
            "   frame-pixel-height X frame-pixel-width=" (frame-pixel-height frame) "x" (frame-pixel-width frame) "\n"
            "   visibility=" (frame-parameter frame 'visibility) "\n"
            )
          )
        )
      )
    (frame-list)
    )
  )
)

;;  ;; list of frames -- one per line
;;  (let* ((lawlist (make-vector (length frames) nil))
;;    (z 0))
;;      (dolist (frame frames)
;;        (aset lawlist z
;;          (nconc
;;            (list
;;              (message "%s" (frame-parameter frame 'name))
;;            )
;;           )
;;        ) 
;;      (setq z (1+ z)) ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FRAME BUFS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE:  There are a couple of functions that are related to using frame-bufs by Al Paker
;; i.e., `ido-frame-bufs` and `tabbar-buffer-grouping-simple-with-frame-bufs`.
;; These functions are NOT needed to associate tab groups with specific frames.  The source
;; for the Al Parker code can be found here:  https://github.com/alpaker/Frame-Bufs
;; If the user wishes to install frame-bufs with a current version of Emacs, then a custom
;; build will be required because frame-bufs was written around the time of Emacs 23.4,
;; and buff-menu.el is hard-coded into the executable of Emacs during the build process:
;; a.  Before building from source, replace the current version of .../lisp/buff-menu.el with
;;     .../lisp/buff-menu.el from Emacs version 23.4 at http://www.gnu.org/software/emacs/
;; b.  Then, build the application.  Of course, the user will lose the benefits of recent
;;     improvements to buff-menu.el (post-23.4) unless other modifications are made.

(defun ido-frame-bufs ()
  "Switch buffer, within buffers associated with current frame (`frame-bufs-buffer-list')
  Other buffers are excluded."
  (interactive)
  (frame-bufs-mode t)
      (let* ( (buffers (mapcar 'buffer-name (frame-bufs-buffer-list (selected-frame))))
              (buffers-rotated (append (cdr buffers) (cons (car buffers) nil)))
              (target (ido-completing-read "Buffer: " buffers-rotated)) )
        (switch-to-buffer target))
    (call-interactively 'ido-switch-buffer))

(eval-after-load "frame-bufs"
  `(progn
     (add-hook 'frame-bufs-mode-on-hook
               #'(lambda ()
                   (global-set-key (kbd "C-x b") 'ido-frame-bufs)
                   (global-set-key (kbd "C-x B") 'ido-switch-buffer)))
     (add-hook 'frame-bufs-mode-off-hook
               #'(lambda ()
                   (global-set-key (kbd "C-x b") 'ido-switch-buffer)))
     ))

;;and you need to modify your 'tabbar-buffer-groups-function'
(defun tabbar-buffer-grouping-simple-with-frame-bufs ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (setq last-tabbar-ruler-tabbar-buffer-groups
        (list
         (cond
          ((= (aref (buffer-name) 0) ?*)
           "Emacs")
          ((or (memq major-mode '(dired-mode
                                  eshell-mode
                                  shell-mode
                                  occur-mode
                                  grep-mode
                                  compilation-mode)))
           "Utils")
          (t
           "Files"
           ))))
  (if (and (featurep 'frame-bufs)
           frame-bufs-mode
           (memq (current-buffer) (frame-bufs-buffer-list (selected-frame))))
      (symbol-value 'last-tabbar-ruler-tabbar-buffer-groups)))

;;  (eval-after-load "frame-bufs"
;;    `(progn
;;       ;;(fset 'bmz/tabbar-buffer-groups-function 'tabbar-buffer-grouping-simple-with-frame-bufs)
;;       (setq tabbar-buffer-groups-function 'tabbar-buffer-grouping-simple-with-frame-bufs)
;;       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(provide 'init-tabbar)
