;; init-tabbar.el

;; Authored (in part) by lawlist -- modifying various functions found at the following
;; links, and with the indirect help of many skilled programmers at the forums of
;; http://www.stackoverflow.com, and indirectly with the assistance of several skilled
;; programers who responded to a few bug reports lawlist submitted to the Emacs team.
;; http://www.emacswiki.org/emacs/TabBarMode
;; https://github.com/bamanzi/dotemacs-full/blob/master/init.d/25-tabbar.el
;; https://gist.github.com/Johniel/4324127
;; http://stackoverflow.com/questions/18346785/how-to-intercept-a-file-before-it-opens-and-decide-which-frame
;; http://stackoverflow.com/questions/17823448/if-frame-named-xyz-exists-then-switch-to-that-frame
;; http://blog.andy.glew.ca/2012_10_02_archive.html
;; https://github.com/alpaker/Frame-Bufs
;; http://www.emacswiki.org/emacs/frame-cmds.el
;; http://www.emacswiki.org/emacs/frame-fns.el
;; https://github.com/kentaro/auto-save-buffers-enhanced

;; Place these two (2) lines of code somewhere in the .emacs or init.el
;; (require 'tabbar)
;; (require 'init-tabbar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HOOKS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook (lambda ()
  (tabbar-mode t)
  (setq tabbar-use-images nil)
  (setq tabbar-cycle-scope 'tabs)
  (setq tabbar-buffer-groups-function (lambda () (list
    (cond
      ((memq (current-buffer) (lawlist-buffer-list (selected-frame))) "lawlist")
      (t "unassigned") )))) ))

;;;;;;;;;;;;;;;;; DISPLAY-BUFFER-ALIST and DISPLAY-BUFFER ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun example ()
  (interactive)
  ;; condition # 3 | file-visiting buffer
  (lawlist-find-file "*bar*")
  (set-frame-height (selected-frame) 20)
  (set-frame-width (selected-frame) 80)
  (set-frame-position (selected-frame) 0 0)
  (message "\*bar\* appears in frame name SYSTEM.")
  (sit-for 3)
  ;; condition # 4(a) | no-file-visiting buffer
  (display-buffer (get-buffer-create "*NO-FILE-special-buffer-regexp*"))
  (message "NO-FILE buffer existing frame, without other windows.")
  (sit-for 3)
  ;; condition # 2(a) | file-visiting buffer
  (lawlist-find-file "foo.txt")
  (set-frame-height (selected-frame) 20)
  (set-frame-width (selected-frame) 80)
  (set-frame-position (selected-frame) 100 100)
  (message "\"foo.txt\" appears in frame name MAIN.")
  (sit-for 3)
  ;; condition # 1 | file-visiting buffer
  (lawlist-find-file "doe.org")
  (set-frame-height (selected-frame) 20)
  (set-frame-width (selected-frame) 80)
  (set-frame-position (selected-frame) 200 200)
  (message "\"doe.org\" appears in frame name ORG.")
  (sit-for 3)
  ;; condition # 4(b) | file-visiting buffer
  (lawlist-find-file "*FILE-special-buffer-regexp*")
  (message "FILE buffer existing frame, without other windows.")
  (sit-for 3)
  ;; condition # 6 | no-file-visiting buffer default display
  (calendar)
  (message "Default for no-file-visiting-buffers.")
  (sit-for 3)
  ;; condition # 5 | file-visiting buffer with no pre-defined regexp.
  (lawlist-find-file "*FILE-undefined-regexp*")
  (set-frame-height (selected-frame) 20)
  (set-frame-width (selected-frame) 80)
  (set-frame-position (selected-frame) 300 300)
  (message "\*IS\* buffer-filename.  \*NOT\* defined by any particular regexp.")
  (sit-for 3)
  ;; condition # 2(b) | no-file-visiting buffer
  (display-buffer (get-buffer-create "*NO-FILE-main-buffer-regexp*"))
  (set-frame-height (selected-frame) 20)
  (set-frame-width (selected-frame) 80)
  (set-frame-position (selected-frame) 400 400)
  (message "\*NOT\* buffer-filename.  \*IS\* defined by main-buffer-regexp.")
  (sit-for 3)
  (kill-buffer "*bar*")
  (kill-buffer "foo.txt")
  (kill-buffer "doe.org")
  (kill-buffer "*FILE-undefined-regexp*")
  (kill-buffer "*NO-FILE-main-buffer-regexp*")
  (kill-buffer "*Calendar*")
  (kill-buffer "*FILE-special-buffer-regexp*")
  (kill-buffer "*NO-FILE-special-buffer-regexp*")
  (make-frame)
  (delete-frame (get-frame "SYSTEM"))
  (delete-frame (get-frame "MAIN"))
  (delete-frame (get-frame "ORG"))
  (delete-frame (get-frame "MISCELLANEOUS"))
  (message "THE END."))

(defun lawlist-make-frame ()
  (make-frame)
  (cond
    ((eq system-type 'darwin)
      (toggle-frame-maximized))
    ((eq system-type 'windows-nt)
      (switch-to-frame "emacs@PARALLELS")
      (w32-send-sys-command 61488)))
  (lawlist-buffer-list-reset))

(defvar regexp-frame-names "^\\(?:MAIN\\|SYSTEM\\|ORG\\|MISCELLANEOUS\\|WANDERLUST\\)$"
    "Regexp matching frames with specific names.")

(defvar wanderlust-buffer-regexp nil
  "Regexp of file / buffer names displayed in frame `WANDERLUST`.")
(setq wanderlust-buffer-regexp '("Folder" "Summary" "INBOX" "SENT" "JUNK" "TRASH" "DRAFTS" "\\*WL:Message\\*" "\\*draft\\*"))

(defvar system-buffer-regexp nil
  "Regexp of file / buffer names displayed in frame `SYSTEM`.")
(setq system-buffer-regexp '("*scratch*" "*bbdb*" "*bar*" ".scratch"))

(defvar main-buffer-regexp nil
  "Regexp of file / buffer names displayed in frame `MAIN`.")
(setq main-buffer-regexp
  '("\\.txt" "\\.tex" "\\.el" "\\.yasnippet" "\\*NO-FILE-main-buffer-regexp\\*"))

(defvar org-buffer-regexp nil
  "Regexp of file / buffer names displayed in frame  `ORG`.")
(setq org-buffer-regexp '("\\*TODO\\*" "\\.todo" "\\.done" "\\*DONE\\*" "\\*Org Agenda\\*" "\\.org_archive" "\\.org"))

(defvar special-buffer-regexp nil
  "Regexp of file / buffer names that will display in the current frame without other windows.")
(setq special-buffer-regexp
  '("\\*NO-FILE-special-buffer-regexp\\*" "\\*FILE-special-buffer-regexp\\*"))

(defvar buffer-filename nil)

(defun lawlist-find-file (&optional buffer-filename)
  "With assistance from the display-buffer-alist, locate or create a specific frame,
  and then open the file."
  (interactive)
  (unless buffer-filename (setq buffer-filename
    (cond
      ((eq system-type 'darwin)
        (ns-read-file-name "Select File:" "/Users/HOME/.0.data/" t nil nil))
      ((eq system-type 'windows-nt)
        (xp-read-file-name "Select File: " "y:/" nil nil nil nil)))))
  (if buffer-filename
    (display-buffer (find-file-noselect buffer-filename))))

;; The following is a `pinpoint` alternative to using the `display-buffer-alist`.
;; (display-buffer (get-buffer-create "foo.txt") '(lawlist-display-buffer-pop-up-frame))
(setq display-buffer-alist '((".*" . (lawlist-display-buffer-pop-up-frame))))

(defun lawlist-display-buffer-pop-up-frame (buffer alist)
  (cond
    ;; condition # 0 -- either file-visiting or no-file buffers
    ((regexp-match-p wanderlust-buffer-regexp (buffer-name buffer))
      (if (get-frame "WANDERLUST")
          (switch-to-frame "WANDERLUST")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "WANDERLUST")
              (lawlist-buffer-list-reset))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (get-frame "WANDERLUST"))
          (progn
            (lawlist-make-frame)
            (set-frame-name "WANDERLUST"))) )
      (lawlist-add-buffer (get-buffer buffer) (selected-frame))
      (set-window-buffer (selected-window) (buffer-name buffer))
      (set-buffer (buffer-name buffer)) )
    ;; condition # 1 -- either file-visiting or no-file buffers
    ((regexp-match-p org-buffer-regexp (buffer-name buffer))
      (if (get-frame "ORG")
          (switch-to-frame "ORG")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "ORG")
              (lawlist-buffer-list-reset))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (get-frame "ORG"))
          (progn
            (lawlist-make-frame)
            (set-frame-name "ORG"))) )
      (lawlist-add-buffer (get-buffer buffer) (selected-frame))
      (set-window-buffer (selected-window) (buffer-name buffer))
      (set-buffer (buffer-name buffer)) )
    ;; condition # 2 -- either file-visiting or no-file buffers
    ((regexp-match-p main-buffer-regexp (buffer-name buffer))
      (if (get-frame "MAIN")
          (switch-to-frame "MAIN")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "MAIN")
              (lawlist-buffer-list-reset))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (get-frame "MAIN"))
          (progn
            (lawlist-make-frame)
            (set-frame-name "MAIN"))) )
      (lawlist-add-buffer (get-buffer buffer) (selected-frame))
      (set-window-buffer (selected-window) (buffer-name buffer))
      (set-buffer (buffer-name buffer)) )
    ;; condition # 3 -- either file-visiting or no-file buffers
    ((regexp-match-p system-buffer-regexp (buffer-name buffer))
      (if (get-frame "SYSTEM")
          (switch-to-frame "SYSTEM")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "SYSTEM")
              (lawlist-buffer-list-reset))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (get-frame "SYSTEM"))
          (progn
            (lawlist-make-frame)
            (set-frame-name "SYSTEM"))) )
      (lawlist-add-buffer (get-buffer buffer) (selected-frame))
      (set-window-buffer (selected-window) (buffer-name buffer))
      (set-buffer (buffer-name buffer)) )
    ;; condition # 4
    ;; display buffer in the existing frame, without other windows
    ((regexp-match-p special-buffer-regexp (buffer-name buffer))
      (lawlist-add-buffer (get-buffer buffer) (selected-frame))
      (set-window-buffer (selected-window) (buffer-name buffer))
      (set-buffer (buffer-name buffer)) )
    ;; condition # 5
    ;; file-visiting buffers that do NOT match any pre-defined regexp
    ((and (not (regexp-match-p org-buffer-regexp (buffer-name buffer)))
          (not (regexp-match-p main-buffer-regexp (buffer-name buffer)))
          (not (regexp-match-p system-buffer-regexp (buffer-name buffer)))
          (not (regexp-match-p special-buffer-regexp (buffer-name buffer)))
          (not (regexp-match-p wanderlust-buffer-regexp (buffer-name buffer)))
          buffer-filename )
      (if (get-frame "MISCELLANEOUS")
          (switch-to-frame "MISCELLANEOUS")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "MISCELLANEOUS")
              (lawlist-buffer-list-reset))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (get-frame "MISCELLANEOUS"))
          (progn
            (lawlist-make-frame)
            (set-frame-name "MISCELLANEOUS"))))
      (lawlist-add-buffer (get-buffer buffer) (selected-frame))
      (set-window-buffer (selected-window) (buffer-name buffer))
      (set-buffer (buffer-name buffer)) )
    ;; condition # 6
    ;; default display for no-file-visiting buffers
    (t nil) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FRAME UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/kentaro/auto-save-buffers-enhanced
(defun regexp-match-p (regexps string)
  (catch 'matched
    (dolist (regexp regexps)
      (if (string-match regexp string)
        (throw 'matched t)))))

;; http://www.emacswiki.org/emacs/frame-fns.el
(defun get-frame-name (&optional frame)
  "Return the string that names FRAME (a frame).  Default is selected frame."
  (unless frame (setq frame (selected-frame)))
  (if (framep frame)
      (cdr (assq 'name (frame-parameters frame)))
    (error "Function `get-frame-name': Argument not a frame: `%s'" frame)))

;; http://www.emacswiki.org/emacs/frame-fns.el
(defun get-frame (frame)
  "Return a frame, if any, named FRAME (a frame or a string).
  If none, return nil.
  If FRAME is a frame, it is returned."
  (cond ((framep frame) frame)
        ((stringp frame)
         (catch 'get-a-frame-found
           (dolist (fr (frame-list))
             (when (string= frame (get-frame-name fr))
               (throw 'get-a-frame-found fr)))
           nil))
        (t
         (error
          "Function `get-frame-name': Arg neither a string nor a frame: `%s'"
          frame))))

;; http://stackoverflow.com/questions/17823448/if-frame-named-xyz-exists-then-switch-to-that-frame
(defun switch-to-frame (frame-name)
  (let ((frames (frame-list)))
    (catch 'break
      (while frames
        (let ((frame (car frames)))
          (if (equal (frame-parameter frame 'name) frame-name)
              (throw 'break (select-frame-set-input-focus frame))
            (setq frames (cdr frames))))))))

;; https://github.com/alpaker/Frame-Bufs
(defun lawlist-buffer-list (frame)
  ;; Remove dead buffers.
  (set-frame-parameter frame 'lawlist-buffer-list
    (delq nil (mapcar #'(lambda (x) (if (buffer-live-p x) x))
      (frame-parameter frame 'lawlist-buffer-list))))
  ;; Return the associated-buffer list.
  (frame-parameter frame 'lawlist-buffer-list) )

;; https://github.com/alpaker/Frame-Bufs
(defun lawlist-remove-buffer (buf frame)
  "Remove BUF from FRAME's associated-buffer list."
  (set-frame-parameter frame 'lawlist-buffer-list
    (delq buf (frame-parameter frame 'lawlist-buffer-list))))

;; https://github.com/alpaker/Frame-Bufs
(defun lawlist-add-buffer (buf frame)
  "Add BUF to FRAME's associated-buffer list if not already present."
  (unless (bufferp buf)
    (signal 'wrong-type-argument (list 'bufferp buf)))
  (let ((associated-bufs (frame-parameter frame 'lawlist-buffer-list)))
    (unless (memq buf associated-bufs)
      (set-frame-parameter frame 'lawlist-buffer-list (cons buf associated-bufs))))
  (if (and (featurep 'tabbar) tabbar-mode)
    (tabbar-display-update)))

;; https://github.com/alpaker/Frame-Bufs
(defun associate-current-buffer ()
(interactive)
  (lawlist-add-buffer (get-buffer (current-buffer)) (selected-frame)))

;; https://github.com/alpaker/Frame-Bufs
(defun disassociate-current-buffer (&optional buf frame)
(interactive)
  (unless buf (setq buf (current-buffer)))
  (unless frame (setq frame (selected-frame)))
  (lawlist-remove-buffer (current-buffer) (selected-frame))
  (dolist (win (get-buffer-window-list buf 'no-minibuf frame))
    (set-window-buffer win (other-buffer buf)))
  (if (not (equal (car (lawlist-buffer-list (selected-frame))) nil))
    (switch-to-buffer (format "%s" (car (lawlist-buffer-list (selected-frame)))))))

;; https://github.com/alpaker/Frame-Bufs
(defun lawlist-buffer-list-reset ()
  "Wipe the entire slate clean for the selected frame."
(interactive)
  (modify-frame-parameters (selected-frame) (list (cons 'lawlist-buffer-list nil))) 
   (if (and (featurep 'tabbar) tabbar-mode)
    (tabbar-display-update)))

;;;;;;;;;;;;;;;;;;;;;;;; IF BUILT --with-ns, THEN ALSO USE ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'ns-find-file 'lawlist-ns-find-file)
(defun lawlist-ns-find-file ()
  "Do a `find-file' with the `ns-input-file' as argument."
  (interactive)
  (let* ((f (file-truename
    (expand-file-name (pop ns-input-file)
      command-line-default-directory)))
    (file (find-file-noselect f))
    (bufwin1 (get-buffer-window file 'visible))
    (bufwin2 (get-buffer-window "*scratch*" 'visible)))
  (cond
    (bufwin1
      (select-frame (window-frame bufwin1))
      (raise-frame (window-frame bufwin1))
      (select-window bufwin1))
    ((and (eq ns-pop-up-frames 'fresh) bufwin2)
      (ns-hide-emacs 'activate)
      (select-frame (window-frame bufwin2))
      (raise-frame (window-frame bufwin2))
      (select-window bufwin2)
      (lawlist-find-file f))
    (t
      (ns-hide-emacs 'activate)
      (lawlist-find-file f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  MISCELLANEOUS FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://www.emacswiki.org/emacs/TabBarMode
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

(defun cycle-forward-frames-groups ()
  (interactive)
  (other-frame 1)
  (if (and 
        (not (equal (buffer-name) (car (lawlist-buffer-list (selected-frame)))))
        (not (equal (car (lawlist-buffer-list (selected-frame))) nil)))
    (switch-to-buffer (format "%s" (car (lawlist-buffer-list (selected-frame)))))))

(defun cycle-backward-frames-groups ()
(interactive)
  (other-frame -1)
  (if (and 
        (not (equal (buffer-name) (car (lawlist-buffer-list (selected-frame)))))
        (not (equal (car (lawlist-buffer-list (selected-frame))) nil)))
    (switch-to-buffer (format "%s" (car (lawlist-buffer-list (selected-frame)))))))

(defun lawlist-exit-draft ()
  "Choices for directories and files."
  (interactive)
  (message "[k]ill or [s]ave draft?")
  (let* (
    (a (read-char-exclusive)) )
    (cond
      ((eq a ?k)
      (with-current-buffer (get-buffer current-buffer-name) (wl-draft-kill)))
      ((eq a ?s)
      (with-current-buffer (get-buffer current-buffer-name) (wl-draft-save-and-exit)))
     (t (error "You must select either wl-draft-kill or wl-draft-save-and-exit.")))))

(defun lawlist-kill-buffer ()
(interactive)
  (setq current-buffer-name (buffer-name))
  (tabbar-backward)
  (setq previous-buffer-name (buffer-name))
  (if (eq (with-current-buffer current-buffer-name major-mode) 'wl-draft-mode)
    (progn
      (tabbar-forward)
      (lawlist-exit-draft)
      (switch-to-buffer previous-buffer-name)
      (if (get-buffer current-buffer-name)
        (kill-buffer current-buffer-name)))
    (kill-buffer current-buffer-name))
  (if (get-buffer-window "*Calendar*" (selected-frame))
    (progn
      (kill-buffer "*Calendar*")
      (delete-other-windows) ))
  (if (equal current-buffer-name previous-buffer-name)
    (condition-case e
      (delete-frame)
        (error
          (if (string= "Attempt to delete the sole visible or iconified frame" 
              (cadr e))
            (bury-buffer)))))
  (if (equal (buffer-name) "*scratch*")
    (progn
      (find-file (concat root.d ".scratch"))
      (kill-buffer "*scratch*")))
  (tabbar-display-update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-tabbar)
