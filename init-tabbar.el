;; init-tabbar.el

;; version 1.00 -- frames / tab-groups:  common, main, wanderlust, org-mode
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
(require 'dash) ;; needed for miscellaneous functions towards end of file

(global-set-key [(f5)] 'toggle-frames-and-tab-groups) ;; manual refresh
(define-key global-map [?\s-\~] 'cycle-backward-frames-groups)
(define-key global-map [?\s-\`] 'cycle-forward-frames-groups)

;; Users will need to add additional hooks for specific modes that do not open files
;; and some not so commonly used functions such as `rename-buffer`.
(add-hook 'find-file-hook
  (lambda()
    (toggle-frames-and-tab-groups)
  ))

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
;; The function `tabbar+buffer-list-function` hides buffers with
;; astericks -- tabbar+displayed-buffers show select buffers.
;; For example, the buffers "*scratch*" and "*Messages* can be
;; made visible by specifically defining the names within the
;; variable `tabbar+displayed-buffers` AND also by adding the
;; following entry in `tabbar-buffer-groups`:
;;   ((member (buffer-name) '("*scratch*" "*Messages*")) "common")
;; If *scratch* is a text-mode file and not otherwise defined into
;; a particular tabbar-buffer-group based upon "buffer-name", then
;; *scratch* will appear in the tab group for text-mode files.
(defvar tabbar+displayed-buffers '("*scratch*" "*Messages*" "*TODO*" "*Org Agenda*" "*BBDB*" "*bbdb*" "*Completions*")
  "*Reagexps matches buffer names always included tabs.")

;; The list of buffers put in tabs is provided by the function
;; specified in the variable `tabbar-buffer-list-function'.  The
;; default function: `tabbar-buffer-list', excludes buffers whose name
;; starts with a space, when they are not visiting a file.
(setq tabbar-buffer-list-function 'tabbar+buffer-list-function)
(defun tabbar+buffer-list-function ()
  (let* ((hides (list ?\ ?\*))
  (re (regexp-opt tabbar+displayed-buffers))
  (cur-buf (current-buffer))
  (tabs (delq nil
  (mapcar (lambda (buf)
  (let ((name (buffer-name buf)))
  (when (and (not (string-match "*lawlist:.**" name))
  (or (string-match re name)
  (not (memq (aref name 0) hides))))
  buf)))
  (buffer-list)))))
  (if (memq cur-buf tabs)
  tabs
  (cons cur-buf tabs))))

(defconst tabbar+default-group-name "DEFAULT")
 
(defun tabbar+init-group ()
""
(--map (with-current-buffer it
(setq tabbar+group tabbar+default-group-name))
(buffer-list)))

(defvar tabbar+const-group-list
(--map (cons it tabbar+default-group-name)
'("*scratch*"
"*Messages*"
"*Packages*")))


(defun tabbar-choice ()
(interactive)
  "Switch between tabbar modes."
  (message "Choose:  [d]efault | plan [b] | [a]ll | [i/I]do | [f/F]rame | [g]roups " )

  (let*

  ( (a (read-char-exclusive))

  (choice (case a

  ((?d)
  (setq tabbar-buffer-list-function 'tabbar+buffer-list-function)
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
  (tabbar-display-update)
  (message "You have chosen: \"primary grouping\""))
  
  ((?b)
  (setq tabbar-buffer-list-function 'tabbar-buffer-list)
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
  (tabbar-display-update)
  (message "You have chosen: \"Plan B\""))
  
  ((?a)
  (tabbar+init-group)
  (setq tabbar-buffer-list-function 'tabbar-buffer-list)
  (setq tabbar-buffer-groups-function '(lambda () (list tabbar+default-group-name)))
  (tabbar-display-update)
  (message "You have chosen: \"everything\""))
  
  ((?i)
  (ido-jump-to-tab)
  (message "You have selected ido-jump-to-tab."))
  
  ((?I)
  (tabbar-display-update)
  (sit-for 0)
  (ido-jump-to-tab-group)
  (message "You have selected ido-jump-to-tab-group."))
  
  ((?f)
  (switch-to-frame)
  (message "You have selected switch-to-frame."))
  
  ((?F)
  (ido-frame-bufs-switch-buffer)
  (message "You have selected ido-frame-bufs-switch-buffer."))
  
  ((?g)
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
       "Process")

      ((eq major-mode 'org-mode)
       "org-mode")

      ((member (buffer-name)
                '("*TODO*"))
        "org-mode")

;; TRUMPS ALL ATTEMPTS AT OTHERWISE CATEGORIZING BUFFERS WITH ASTERICKS
;;       ((string-equal "*" (substring (buffer-name) 0 1))
;;       "common")

      ((member (buffer-name)
        '("*scratch*" "*Messages*"))
          "common")

      ((eq major-mode 'dired-mode)
       "dired")

      ((memq major-mode
             '(bbdb-mode wl-summary-mode wl-original-message-mode wl-draft-mode mime-view-mode wl-message-mode wl-folder-mode rmail-mode rmail-edit-mode vm-summary-mode vm-mode mail-mode mh-letter-mode mh-show-mode mh-folder-mode gnus-summary-mode message-mode gnus-group-mode gnus-article-mode score-mode gnus-browse-killed-mode))
       "wanderlust")

      ((member (buffer-name)
               '("*BBDB*" "*bbdb*" "*Completions*"))
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

(defun ido-jump-to-tab-group ()
  "Jump to a tabbar group."
  (interactive)
  (if (< emacs-major-version 24)
      (ido-common-initialization))
  (unless (and (featurep 'tabbar)
               tabbar-mode)
    (error "Error: tabbar-mode not turned on."))
  (set tabbar-tabsets-tabset (tabbar-map-tabsets 'tabbar-selected-tab)) ;; refresh groups
  (let* ( (groups (mapcar #'(lambda (group)
                              (format "%s" (cdr group)))
                          (tabbar-tabs tabbar-tabsets-tabset)))
          (group-name (ido-completing-read "Groups: " groups)) )
    (mapc #'(lambda (group)
              (when (string= group-name (format "%s" (cdr group)))
                  (message "Switch to group '%s', current buffer: %s" (cdr group) (car group))
                  (switch-to-buffer (car group)) ))
          (tabbar-tabs tabbar-tabsets-tabset))) )

;; (message "%s" tabbar-current-tabset) ;; tab group currently selected
;; (message "%s" (tabbar-tabs tabbar-tabsets-tabset)) ;; all buffers by group
;; (message "%s" groups) ) ;; all groups

(defun goto-tab-group (group-name)
  "Jump to a specific tabbar group."
  (unless (and (featurep 'tabbar)
               tabbar-mode)
    (error "Error: tabbar-mode not turned on."))
  (set tabbar-tabsets-tabset (tabbar-map-tabsets 'tabbar-selected-tab)) ;; refresh groups
  (let* ( (groups (mapcar #'(lambda (group)
                              (format "%s" (cdr group)))
                          (tabbar-tabs tabbar-tabsets-tabset))))
    (mapc #'(lambda (group)
              (when (string= group-name (format "%s" (cdr group)))
                  (message "Switch to group '%s', current buffer: %s" (cdr group) (car group))
                  (switch-to-buffer (car group)) ))
          (tabbar-tabs tabbar-tabsets-tabset))) )

(defun example-using-goto-tab-group ()
(interactive)
(goto-tab-group "org-mode"))


(defun ido-jump-to-tab ()
  "Jump to a buffer in current tabbar group."
  (interactive)
  (if (< emacs-major-version 24)
      (ido-common-initialization))
  (unless (and (featurep 'tabbar)
               tabbar-mode)
    (error "Error: tabbar-mode not turned on."))
  (let* ( ;; Swaps the current buffer name with the next one along.
         (visible-buffers (mapcar '(lambda (tab) (buffer-name (tabbar-tab-value tab)))
                                  (tabbar-tabs (tabbar-current-tabset t))))
         (buffer-name (ido-completing-read "Buffer: " visible-buffers))
         window-of-buffer)
    (if (not (member buffer-name visible-buffers))
        (error "'%s' does not have a visible window" buffer-name)
      (switch-to-buffer buffer-name))))


(defun ido-frame-bufs-switch-buffer ()
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
                   (global-set-key (kbd "C-x b") 'ido-frame-bufs-switch-buffer)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FRAMES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lawlist-new-frame (frame-name)
(interactive "sSelect a frame name: ")
(make-frame)
(set-frame-name frame-name))

(defun switch-to-frame ()
  (interactive)
  (setq frame-to (read-string (format "From: (%s) => To: %s.  Select: "
    ;;  From:
    (frame-parameter nil 'name)
    ;;  To:
    (mapcar
      (lambda (frame) "print frame"
        (reduce 'concat
          (mapcar (lambda (s) (format "%s" s))
            (list "|" (frame-parameter frame 'name) "|" )
          )
        )
      )
    (frame-list) )
  )))
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
            (setq frames (cdr frames)))))) ))


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

(defun frame-exists-common ()
(interactive)
  (if (frame-exists "common")
    ;; then
    (progn
      (message "Congratulations -- frame \"common\" exists!")
      (goto-tab-group "common")
    )
    ;; else
    (message "Sorry -- frame \"common\" does not exist :\(" )
    ;; take control of an existing frame not yet specially named.
    (if
      (and
        (not (equal "main" (frame-parameter nil 'name)))
        (not (equal "common" (frame-parameter nil 'name)))
        (not (equal "org-mode" (frame-parameter nil 'name)))
        (not (equal "wanderlust" (frame-parameter nil 'name)))
      )
      ;; then
      (progn
        (set-frame-name "common")
        (toggle-frame-maximized)
      )
      ;; else
        (new-frame)
        (set-frame-name "common")
        (toggle-frame-maximized) )

    (goto-tab-group "common")
  )
)

(defun frame-exists-main ()
(interactive)
  (if (frame-exists "main")
    ;; then
    (progn
      (message "Congratulations -- frame \"main\" exists!")
      (goto-tab-group "main")
    )
    ;; else
    (message "Sorry -- frame \"main\" does not exist :\(" )
    ;; take control of an existing frame not yet specially named.
    (if
      (and
        (not (equal "main" (frame-parameter nil 'name)))
        (not (equal "common" (frame-parameter nil 'name)))
        (not (equal "org-mode" (frame-parameter nil 'name)))
        (not (equal "wanderlust" (frame-parameter nil 'name)))
      )
      ;; then
      (progn
        (set-frame-name "main")
        (toggle-frame-maximized)
      )
      ;; else
        (new-frame)
        (set-frame-name "main")
        (toggle-frame-maximized) )
    (goto-tab-group "main")
  )
)


(defun frame-exists-org-mode ()
(interactive)
  (if (frame-exists "org-mode")
    ;; then
    (progn
      (message "Congratulations -- frame \"org-mode\" exists!")
      (goto-tab-group "org-mode")
    )
    ;; else
    (message "Sorry -- frame \"org-mode\" does not exist :\(" )
    ;; take control of an existing frame not yet specially named.
    (if
      (and
        (not (equal "main" (frame-parameter nil 'name)))
        (not (equal "common" (frame-parameter nil 'name)))
        (not (equal "org-mode" (frame-parameter nil 'name)))
        (not (equal "wanderlust" (frame-parameter nil 'name)))
      )
      ;; then
      (progn
        (set-frame-name "org-mode")
        (toggle-frame-maximized)
      )
      ;; else
        (new-frame)
        (set-frame-name "org-mode")
        (toggle-frame-maximized) )
    (goto-tab-group "org-mode")
  )
)

(defun frame-exists-wanderlust ()
(interactive)
  (if (frame-exists "wanderlust")
    ;; then
    (progn
      (message "Congratulations -- frame \"wanderlust\" exists!")
      (goto-tab-group "wanderlust")
    )
    ;; else
    (message "Sorry -- frame \"wanderlust\" does not exist :\(" )
    ;; take control of an existing frame not yet specially named.
    (if
      (and
        (not (equal "main" (frame-parameter nil 'name)))
        (not (equal "common" (frame-parameter nil 'name)))
        (not (equal "org-mode" (frame-parameter nil 'name)))
        (not (equal "wanderlust" (frame-parameter nil 'name)))
      )
      ;; then
      (progn
        (set-frame-name "wanderlust")
        (toggle-frame-maximized)
      )
      ;; else
        (new-frame)
        (set-frame-name "wanderlust")
        (toggle-frame-maximized) )
    (goto-tab-group "wanderlust")
  )
)

(defun toggle-frames-and-tab-groups ()
(interactive)
  (tabbar-current-tabset t)
  (if (equal (format "%s" tabbar-current-tabset) "main")
      (frame-exists-main))
  (if (equal (format "%s" tabbar-current-tabset) "common")
      (frame-exists-common))
  (if (equal (format "%s" tabbar-current-tabset) "org-mode")
      (frame-exists-org-mode))
  (if (equal (format "%s" tabbar-current-tabset) "wanderlust")
      (frame-exists-wanderlust)) )

(defun cycle-forward-frames-groups ()
  "Cycle to next available fame / group."
(interactive)
  (other-frame 1)
  (if (equal "main" (frame-parameter nil 'name))
    (goto-tab-group "main"))
  (if (equal "common" (frame-parameter nil 'name))
    (goto-tab-group "common"))
  (if (equal "org-mode" (frame-parameter nil 'name))
    (goto-tab-group "org-mode"))
  (if (equal "wanderlust" (frame-parameter nil 'name))
    (goto-tab-group "wanderlust"))
 )

(defun cycle-backward-frames-groups ()
  "Cycle to next available fame / group."
(interactive)
  (other-frame -1)
  (if (equal "main" (frame-parameter nil 'name))
    (goto-tab-group "main"))
  (if (equal "common" (frame-parameter nil 'name))
    (goto-tab-group "common"))
  (if (equal "org-mode" (frame-parameter nil 'name))
    (goto-tab-group "org-mode"))
  (if (equal "wanderlust" (frame-parameter nil 'name))
    (goto-tab-group "wanderlust"))
 )



(defun print-frame-list ()
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

;; list of frames on the same line without delimiters
;; (message "%s" (mapcar (lambda (frame) (frame-parameter frame 'name)) (frame-list)) )
;; (mapcar (lambda (frame) (frame-parameter frame 'name)) (frame-list))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recently-used-buffer ()
(interactive)
(other-buffer (current-buffer) 1))
 
(defun tabbar+sort-tab ()
"Sort current tab group lexicographically"
(interactive)
(let* ((ctabset (tabbar-current-tabset 't))
(ctabs (tabbar-tabs ctabset)))
(if (and ctabset ctabs)
(progn
(set ctabset (sort ctabs (lambda (b1 b2)
(string-lessp (buffer-name (car b1))
(buffer-name (car b2))))))
(put ctabset 'template nil)
(tabbar-display-update)))))

(defun tabbar+buffer-help-on-tab (tab)
"Return the help string shown when mouse is on a TAB."
(if tabbar--buffer-show-groups
(let* ((tabset (tabbar-tab-tabset tab))
(tab (tabbar-selected-tab tabset)))
(format "mouse-1: switch to buffer %S in group [%s]"
(buffer-name (tabbar-tab-value tab)) tabset))
(format "\
mouse-1: switch to buffer %S\n\
mouse-2: kill this buffer\n\
mouse-3: delete other windows"
(buffer-name (tabbar-tab-value tab)))))
 
(defun tabbar+buffer-select-tab (event tab)
"On mouse EVENT, select TAB."
(let ((mouse-button (event-basic-type event))
(buffer (tabbar-tab-value tab)))
(cond
((eq mouse-button 'mouse-2)
(with-current-buffer buffer
(kill-buffer)))
((eq mouse-button 'mouse-3)
(delete-other-windows))
(t
(switch-to-buffer buffer)))
;; Don't show groups.
(tabbar-buffer-show-groups nil)))
 
(setq tabbar-help-on-tab-function 'tabbar+buffer-help-on-tab)
(setq tabbar-select-tab-function 'tabbar+buffer-select-tab)

;;
;; Tab Position
;;
 
(defun tabbar+get-current-buffer-index ()
(let* ((ctabset (tabbar-current-tabset 't))
(ctabs (tabbar-tabs ctabset))
(ctab (tabbar-selected-tab ctabset)))
(length (--take-while (not (eq it ctab)) ctabs))))
 
(defun insert- (list-object index element)
(append (-take index list-object) (list element) (-drop index list-object)))
 
(defun tabbar+move (direction)
"Move current tab to (+ index DIRECTION)"
(interactive)
(let* ((ctabset (tabbar-current-tabset 't))
(ctabs (tabbar-tabs ctabset))
(ctab (tabbar-selected-tab ctabset))
(index (tabbar+get-current-buffer-index))
(others (--remove (eq it ctab) ctabs))
(ins (mod (+ index direction (+ 1 (length others))) (+ 1 (length others)))))
(set ctabset (insert- others ins ctab))
(put ctabset 'template nil)
(tabbar-display-update)))
 
(defun tabbar+move-right ()
"Move current tab to right"
(interactive)
(tabbar+move +1))
 
(defun tabbar+move-left ()
"Move current tab to left"
(interactive)
(tabbar+move -1))
 
;;
;; Kill Buffer or Group
;;
 
(defun tabbar+remove-right ()
"Remove right side buffers"
(interactive)
(let* ((ctabset (tabbar-current-tabset 't))
(ctabs (tabbar-tabs ctabset))
(ctab (tabbar-selected-tab ctabset)))
(--map (kill-buffer (car it)) (cdr (--drop-while (not (eq ctab it)) ctabs)))))
 
(defun tabbar+remove-left ()
"Remove left side buffers"
(interactive)
(let* ((ctabset (tabbar-current-tabset 't))
(ctabs (tabbar-tabs ctabset))
(ctab (tabbar-selected-tab ctabset)))
(--map (kill-buffer (car it)) (--take-while (not (eq ctab it)) ctabs))))
 
(defun tabbar+kill-group (group)
"Kill all buffers belonging to GROUP."
(interactive
(list (completing-read "Tab Group: " (tabbar+get-all-group-name))))
(->> (buffer-list)
(--filter (string= group (tabbar+get-group it)))
(-map 'kill-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-tabbar)
