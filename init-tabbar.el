;; init-tabbar.el

;; Version (alpha) -- frames / tab-groups:  SYSTEM, MAIN, WANDERLUST, ORG

;; Requires a current version of Emacs Trunk (24.3.50 (9.0)) and
;; modified versions of frame-bufs and buff-menu (consolidated into one file)
;; and a modified version of tabbar, all of which are contained within the lawlist
;; repository:  https://github.com/lawlist/tabbar-lawlist

;; The manual sorting of tabs requires installation of `dash` from `marmalade`.

;; If using (desktop-save-mode 1), then also use (setq desktop-restore-frames nil)

;; Place these three lines somewhere in the .emacs or init.el
;; (require 'tabbar)
;; (require 'init-frames)
;; (require 'init-tabbar)

;; Authored (in part) by lawlist -- modifying various functions found at the following
;; links, and with the indirect help of many skilled programmers at the forums of
;; http://www.stackoverflow.com, and indirectly with the assistance of several skilled
;; programers who responded to a few bug reports lawlist submitted to the Emacs team.
;; http://www.emacswiki.org/emacs/TabBarMode
;; https://github.com/bamanzi/dotemacs-full/blob/master/init.d/25-tabbar.el
;; https://gist.github.com/Johniel/4324127
;; http://stackoverflow.com/questions/17823448/if-frame-named-xyz-exists-then-switch-to-that-frame
;; http://blog.andy.glew.ca/2012_10_02_archive.html
;; https://github.com/alpaker/Frame-Bufs
;; http://www.emacswiki.org/emacs/frame-cmds.el
;; http://www.emacswiki.org/emacs/frame-fns.el

;; REFRESH TABBAR
;; (tabbar-current-tabset 't)
;; (tabbar-display-update)
;; (sit-for 0)
;;
;; set current buffer -- switch to tab -- function must contain (&optional type)
;; (setq wl-selected-message-buffer (car (tabbar-tabs (tabbar-get-tabsets-tabset))))
;; (tabbar-click-on-tab wl-selected-message-buffer type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HOOKS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook (lambda ()
  (tabbar-mode t)
  (setq tabbar-cycle-scope 'tabs)
  (setq frame-bufs-mode t)
  (setq tabbar-buffer-list-function 'tabbar-buffer-list)
  (setq tabbar-buffer-groups-function (lambda () (list
    (cond 
    ((memq (current-buffer) (frame-bufs-buffer-list (selected-frame))) "frame-bufs")
    (t "non-associated") ))))
  ))

(add-hook 'emacs-startup-hook
  (lambda ()
    (kill-buffer "*scratch*")
    (lawlist-find-file "~/.0.data/.0.emacs/*scratch*")
    (if (and (featurep 'init-frames) frame-bufs-mode)
      (frame-bufs-add-buffer (get-buffer "*Messages*") (selected-frame)))
    (lawlist-desktop-read)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KEYBOARD SHORTCUTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [?\s-w] 'lawlist-kill-buffer)
(global-set-key [?\s-o] 'lawlist-find-file)

(global-set-key [?\s-1] 'goto-unread-folder)
(global-set-key [?\s-2] 'goto-sent-recently-folder)
(global-set-key [?\s-3] 'activate-wanderlust)
(global-set-key [?\s-4] 'goto-inbox-folder)
(global-set-key [?\s-5] 'goto-sent-folder)

(global-set-key [?\s-\~] 'cycle-backward-frames-groups)
(global-set-key [?\s-\`] 'cycle-forward-frames-groups)
(global-set-key [(control shift tab)] 'tabbar-backward-group)
(global-set-key [(control tab)] 'tabbar-forward-group) 
(global-set-key (kbd "<M-s-right>") 'tabbar-forward)
(global-set-key (kbd "<M-s-left>") 'tabbar-backward)

(global-set-key [(f5)] 'tabbar-sort-tab)
(global-set-key (kbd "<C-M-s-right>") 'tabbar-move-right)
(global-set-key (kbd "<C-M-s-left>") 'tabbar-move-left)

;; control+option+command+r
(global-set-key (kbd "<C-M-s-268632082>") 'lawlist-frame-bufs-reset)

;; control+option+command+a
(global-set-key (kbd "<C-M-s-268632065>") 'associate-current-buffer)

;; control+option+command+n
;; Do NOT modify `frame-bufs-diss-buffer`, which is used "as-is" with `frame-bufs-menu-execute`.
(global-set-key (kbd "<C-M-s-268632078>") (lambda ()
  (interactive)
  (if (and (featurep 'init-frames) frame-bufs-mode)
    (progn
      (frame-bufs-dismiss-buffer)
      (switch-to-buffer (format "%s" (car (frame-bufs-buffer-list (selected-frame)))))
      ;;  NOTE:  The "nil" buffer is caused when there is no buffer assigned to
      ;;  the frame-bufs-buffer-list -- i.e., result when it is empty.
      (if (buffer-exists "nil")
        (kill-buffer "nil"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LAWLIST CHOICES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar wanderlust-insert nil)
(defvar system-insert nil)
(defvar main-insert nil)
(defvar org-insert nil)
(defun tabbar-choice ()
(interactive)
  "Different choices relating to tab groups, frame-bufs-mode, and the like."
  (message "[F]rame-Bufs | [d]efault | [c]ourt | [a]ll | ido- [t/T]ab [f]rame [g]roup | [v/h] Tile | [G]roups" )
  (let* (
    (a (read-char-exclusive))
    (choice (case a
      ((?d)
        (setq frame-bufs-mode nil)
        (setq tabbar-buffer-list-function 'buffer-lawlist-function)
        (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
        (global-set-key [?\s-\~] 'lawlist-tabbar-backward-group)
        (global-set-key [?\s-\`] 'lawlist-tabbar-forward-group)
        (tabbar-display-update)
        (message "You have chosen: \"primary grouping\"") )
      ((?c)
;;        (setq frame-bufs-mode t)
;;        (setq tabbar-buffer-list-function 'buffer-lawlist-function)
;;        (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
        (global-set-key [?\s-\`] nil)
        (global-set-key [?\s-\`] (lambda () (interactive)
          (if (equal "MAIN" (frame-parameter nil 'name))
            (switch-to-frame "ORG")
            (switch-to-frame "MAIN")) ))
;;        (if (equal "MAIN" (frame-parameter nil 'name))
;;            (progn
;;              (switch-to-frame "ORG")
;;              (get-group "org"))
;;            (switch-to-frame "MAIN")
;;            (get-group "main"))))
        (if (not (or (equal "MAIN" (frame-parameter nil 'name))
                     (equal "ORG" (frame-parameter nil 'name))))
          (switch-to-frame "MAIN"))
        (tabbar-display-update)
        (message "You have chosen: \"COURT\""))
      ((?a)
        (setq frame-bufs-mode nil)
        (setq tabbar-buffer-list-function 'tabbar-buffer-list)
        (setq tabbar-buffer-groups-function (lambda () (list "lawlist")))
        (global-set-key [?\s-\~] 'tabbar-backward-tab)
        (global-set-key [?\s-\`] 'tabbar-forward-tab)
        (switch-to-frame "SYSTEM")
        (tabbar-display-update)
        (message "You have chosen: \"everything\""))
      ((?t)
        (tabbar-display-update)
        (sit-for 0)
        (ido-tab))
      ((?T)
        ;; A modified version of frame-bufs by Al Parker is included in the lawlist repository.
        (tabbar-display-update)
        (sit-for 0)
        (ido-frame-bufs))
      ((?g)
        (tabbar-display-update)
        (sit-for 0)
        (ido-group))
      ((?f)
        (tabbar-display-update)
        (sit-for 0)
        (ido-frame))
      ((?v)
        ;; requires installation of both frame-cmds and frame-fns
        ;; http://www.emacswiki.org/emacs/frame-cmds.el
        ;; http://www.emacswiki.org/emacs/frame-fns.el
        (record-frame-buffer)
        (refresh-frames-buffers)
        (tile-frames-vertically)
        (restore-frame-buffer)
        (tabbar-display-update)
        (sit-for 0))
      ((?h)
        ;; requires installation of both frame-cmds and frame-fns
        ;; http://www.emacswiki.org/emacs/frame-cmds.el
        ;; http://www.emacswiki.org/emacs/frame-fns.el
        (record-frame-buffer)
        (refresh-frames-buffers)
        (tile-frames-horizontally)
        (restore-frame-buffer)
        (tabbar-display-update)
        (sit-for 0))
      ((?F)
        ;; A modified version of frame-bufs by Al Parker is included in the lawlist repository.
        (unless (not (and (featurep 'init-frames) frame-bufs-mode))
          (error "Error: frame-bufs-mode is already active."))
        (global-set-key [?\s-\~] 'cycle-backward-frames-groups)
        (global-set-key [?\s-\`] 'cycle-forward-frames-groups)
        (record-frame-buffer)
        (if (frame-exists "WANDERLUST" )
          (progn
          (get-group "wanderlust")
          (setq wanderlust-insert (mapcar 'tabbar-tab-value (tabbar-tabs (tabbar-current-tabset t))))))
        (if (frame-exists "SYSTEM")
          (progn
          (get-group "system")
          (setq system-insert (mapcar 'tabbar-tab-value (tabbar-tabs (tabbar-current-tabset t))))))
        (if (frame-exists "MAIN")
          (progn
          (get-group "main")
          (setq main-insert (mapcar 'tabbar-tab-value (tabbar-tabs (tabbar-current-tabset t))))))
        (if (frame-exists "ORG")
          (progn
          (get-group "org")
          (setq org-insert (mapcar 'tabbar-tab-value (tabbar-tabs (tabbar-current-tabset t))))))
        (setq frame-bufs-mode t)
        (setq tabbar-buffer-list-function 'tabbar-buffer-list) ;; 'tabbar-buffer-list or 'buffer-lawlist-function
        (setq tabbar-buffer-groups-function (lambda () (list (cond 
          ((memq (current-buffer) (frame-bufs-buffer-list (selected-frame))) "frame-bufs") 
          (t "non-associated") ))))
        (if (frame-exists "WANDERLUST")
          (progn
            (switch-to-frame "WANDERLUST")
            (modify-frame-parameters (selected-frame) (list (cons 'frame-bufs-buffer-list wanderlust-insert)))))
        (if (frame-exists "SYSTEM")
          (progn
            (switch-to-frame "SYSTEM")
            (modify-frame-parameters (selected-frame) (list (cons 'frame-bufs-buffer-list system-insert)))))
        (if (frame-exists "MAIN")
          (progn
            (switch-to-frame "MAIN")
            (modify-frame-parameters (selected-frame) (list (cons 'frame-bufs-buffer-list main-insert)))))
        (if (frame-exists "ORG")
          (progn
            (switch-to-frame "ORG")
            (modify-frame-parameters (selected-frame) (list (cons 'frame-bufs-buffer-list org-insert)))))
        (tabbar-display-update)
        (dolist (frame (frame-list))
          (switch-to-frame (frame-parameter frame 'name) )
          (switch-to-buffer (format "%s" (car (frame-bufs-buffer-list (selected-frame)))))
          ;;  NOTE:  The "nil" buffer is caused when there is no buffer assigned to
          ;;  the frame-bufs-buffer-list -- i.e., result when it is empty.
          (if (buffer-exists "nil")
            (kill-buffer "nil")) )
        (restore-frame-buffer) )
      ((?G)
        (tabbar-buffer-show-groups-toggle-switch)
        (tabbar-display-update))
      (t (message "No changes have been made.")) )))))

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

(defvar regexp-frame-names "^\\(?:MAIN\\|SYSTEM\\|ORG\\|MISCELLANEOUS\\|WANDERLUST\\)$"
    "Regexp matching frames with specific names.")

(defvar system-buffer-regexp nil
  "Regexp of file / buffer names displayed in frame `SYSTEM`.")
(setq system-buffer-regexp '("*scratch*" "*bbdb*" "*bar*"))

(defvar main-buffer-regexp nil
  "Regexp of file / buffer names displayed in frame `MAIN`.")
(setq main-buffer-regexp
  '("\\.txt" "\\.tex" "\\.el" "\\.yasnippet" "\\*NO-FILE-main-buffer-regexp\\*"))

(defvar org-buffer-regexp nil
  "Regexp of file / buffer names displayed in frame  `ORG`.")
(setq org-buffer-regexp '("\\*TODO\\*" "\\*Org Agenda\\*" "\\.org_archive" "\\.org"))

(defvar special-buffer-regexp nil
  "Regexp of file / buffer names that will .")
(setq special-buffer-regexp
  '("\\*NO-FILE-special-buffer-regexp\\*" "\\*FILE-special-buffer-regexp\\*"))

(defvar buffer-filename nil)

(defun lawlist-find-file (&optional buffer-filename)
  "With assistance from the display-buffer-alist, locate or create a specific frame,
  and then open the file."
  (interactive)
  ;;  (unless buffer-filename (setq buffer-filename
  ;;    (read-file-name "Select File: " "~/" nil nil nil nil)))
  ;; If using a version of Emacs built `--with-ns`, then user may substitute:
  (unless buffer-filename (setq buffer-filename
    (ns-read-file-name "Select File:" "~/.0.data/" t nil nil)))
  (if buffer-filename
    (display-buffer (find-file-noselect buffer-filename))))

(setq display-buffer-alist '((".*" . (lawlist-display-buffer-pop-up-frame))))

(defun lawlist-display-buffer-pop-up-frame (buffer alist)
  (cond
    ;; condition # 1 -- either file-visiting or no-file buffers
    ((regexp-match-p org-buffer-regexp (buffer-name buffer))
      (if (frame-exists "ORG")
          (switch-to-frame "ORG")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "ORG")
              (toggle-frame-maximized)
              (lawlist-frame-bufs-reset))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (frame-exists "ORG"))
          (progn
            (make-frame)
            (set-frame-name "ORG")
            (toggle-frame-maximized)
            (lawlist-frame-bufs-reset))) )
      (if (and (featurep 'init-frames) frame-bufs-mode)
        (frame-bufs-add-buffer (get-buffer buffer) (selected-frame)))
      (set-window-buffer (frame-selected-window) (buffer-name buffer)))
    ;; condition # 2 -- either file-visiting or no-file buffers
    ((regexp-match-p main-buffer-regexp (buffer-name buffer))
      (if (frame-exists "MAIN")
          (switch-to-frame "MAIN")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "MAIN")
              (toggle-frame-maximized)
              (lawlist-frame-bufs-reset))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (frame-exists "MAIN"))
          (progn
            (make-frame)
            (set-frame-name "MAIN")
            (toggle-frame-maximized)
            (lawlist-frame-bufs-reset))) )
      (if (and (featurep 'init-frames) frame-bufs-mode)
        (frame-bufs-add-buffer (get-buffer buffer) (selected-frame)))
      (set-window-buffer (frame-selected-window) (buffer-name buffer)))
    ;; condition # 3 -- either file-visiting or no-file buffers
    ((regexp-match-p system-buffer-regexp (buffer-name buffer))
      (if (frame-exists "SYSTEM")
          (switch-to-frame "SYSTEM")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "SYSTEM")
              (toggle-frame-maximized)
              (lawlist-frame-bufs-reset))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (frame-exists "SYSTEM"))
          (progn
            (make-frame)
            (set-frame-name "SYSTEM")
            (toggle-frame-maximized)
            (lawlist-frame-bufs-reset))) )
      (if (and (featurep 'init-frames) frame-bufs-mode)
        (frame-bufs-add-buffer (get-buffer buffer) (selected-frame)))
      (set-window-buffer (frame-selected-window) (buffer-name buffer)))
    ;; condition # 4
    ;; display buffer in the existing frame, without other windows
    ((regexp-match-p special-buffer-regexp (buffer-name buffer))
      (if (and (featurep 'init-frames) frame-bufs-mode)
        (frame-bufs-add-buffer (get-buffer buffer) (selected-frame)))
      (set-window-buffer (frame-selected-window) (buffer-name buffer)))
    ;; condition # 5
    ;; file-visiting buffers that do NOT match any pre-defined regexp
    ((and (not (regexp-match-p org-buffer-regexp (buffer-name buffer)))
          (not (regexp-match-p main-buffer-regexp (buffer-name buffer)))
          (not (regexp-match-p system-buffer-regexp (buffer-name buffer)))
          (not (regexp-match-p special-buffer-regexp (buffer-name buffer)))
          buffer-filename )
      (if (frame-exists "MISCELLANEOUS")
          (switch-to-frame "MISCELLANEOUS")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "MISCELLANEOUS")
              (toggle-frame-maximized)
              (lawlist-frame-bufs-reset))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (frame-exists "MISCELLANEOUS"))
          (progn
            (make-frame)
            (set-frame-name "MISCELLANEOUS")
            (toggle-frame-maximized)
            (lawlist-frame-bufs-reset))))
      (if (and (featurep 'init-frames) frame-bufs-mode)
        (frame-bufs-add-buffer (get-buffer buffer) (selected-frame)))
      (set-window-buffer (frame-selected-window) (buffer-name buffer)))
    ;; condition # 6
    ;; default display for no-file-visiting buffers
    (t nil) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GENERIC REGEXP FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun regexp-match-p (regexps string)
"Before the lisp function, define the variable like this:\n
(defvar example-regexp nil
  \"Regexps matching `buffer-name buffer` for frame name `SYSTEM`.\")
    (setq example-regexp '(\"\\(\\*foo\\*\\|\\*bar\\*\\)\"))
\nWithin the lisp function, use something like this:\n
(regexp-match-p example-regexp (buffer-name buffer))
\nOr, this:\n
(regexp-match-p example-regexp buffer-filename)"
  (setq case-fold-search nil) ;; take case into consideration
  (catch 'matched
    (dolist (regexp regexps)
      (if (string-match regexp string)
        (throw 'matched t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GENERIC FRAME UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun frame-exists (frame-name)
  (not (eq nil (get-frame frame-name))))

(defun get-frame-name (&optional frame)
  "Return the string that names FRAME (a frame).  Default is selected frame."
  (unless frame (setq frame (selected-frame)))
  (if (framep frame)
      (cdr (assq 'name (frame-parameters frame)))
    (error "Function `get-frame-name': Argument not a frame: `%s'" frame)))

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

(defun switch-to-frame (frame-name)
  (let ((frames (frame-list)))
    (catch 'break
      (while frames
        (let ((frame (car frames)))
          (if (equal (frame-parameter frame 'name) frame-name)
              (throw 'break (select-frame-set-input-focus frame))
            (setq frames (cdr frames))))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WANDERLUST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wanderlust-display-buffer-pop-up-frame ()
  (if (frame-exists "WANDERLUST")
      (switch-to-frame "WANDERLUST")
    ;; If unnamed frame exists, then take control of it.
    (catch 'break (dolist (frame (frame-list))
      (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
        (throw 'break (progn
          (switch-to-frame (frame-parameter frame 'name))
          (set-frame-name "WANDERLUST")
          (toggle-frame-maximized)
          (lawlist-frame-bufs-reset))))))
    ;; If dolist found no unnamed frame, then create / name it.
    (if (not (frame-exists "WANDERLUST"))
      (progn
        (make-frame)
        (set-frame-name "WANDERLUST")
        (toggle-frame-maximized)
        (lawlist-frame-bufs-reset)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GENERIC BUFFER UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun buffer-exists (bufname)
  (not (eq nil (get-buffer bufname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  MISCELLANEOUS FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ido-frame ()
  (interactive)
  (setq frame-to (ido-completing-read "Select Frame:  "
    (mapcar (lambda (frame) (frame-parameter frame 'name)) (frame-list))))
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
            (setq frames (cdr frames))))))))

(defun cycle-forward-frames-groups ()
  "Cycle to next frame-bufs frame."
(interactive)
  (unless (and (featurep 'init-frames) frame-bufs-mode)
    (error "Error: frame-bufs-mode must be active for this function to work."))
  (other-frame 1)
  (if (not (equal (buffer-name) (car (frame-bufs-buffer-list (selected-frame)))))
    (switch-to-buffer (format "%s" (car (frame-bufs-buffer-list (selected-frame))))))
  ;;  NOTE:  The "nil" buffer is caused when there is no buffer assigned to
  ;;  the frame-bufs-buffer-list -- i.e., result when it is empty.
  (if (buffer-exists "nil")
    (kill-buffer "nil")) )

(defun cycle-backward-frames-groups ()
  "Cycle to previous frame-bufs frame."
(interactive)
  (unless (and (featurep 'init-frames) frame-bufs-mode)
    (error "Error: frame-bufs-mode must be active for this function to work."))
  (other-frame -1)
  (if (not (equal (buffer-name) (car (frame-bufs-buffer-list (selected-frame)))))
    (switch-to-buffer (format "%s" (car (frame-bufs-buffer-list (selected-frame))))))
  ;;  NOTE:  The "nil" buffer is caused when there is no buffer assigned to
  ;;  the frame-bufs-buffer-list -- i.e., result when it is empty.
  (if (buffer-exists "nil")
    (kill-buffer "nil")) )

(defun lawlist-kill-buffer ()
(interactive)
  (setq current-buffer-name (buffer-name))
  (tabbar-backward)
  (setq previous-buffer-name (buffer-name))
  (kill-buffer current-buffer-name)
  (if (equal current-buffer-name previous-buffer-name)
    (condition-case e
      (delete-frame)
        (error
          (if (string= "Attempt to delete the sole visible or iconified frame" 
              (cadr e))
            (bury-buffer)))))
  (if (and
        (equal (buffer-name) "*scratch*")
        ;; (eq major-mode 'lisp-interaction-mode)
        (file-exists-p "/Users/HOME/.0.data/.0.emacs/*scratch*")
        (not (equal buffer-file-name "/Users/HOME/.0.data/.0.emacs/*scratch*")))
    (progn
      (rename-buffer "*to-be-deleted*")
      (lawlist-find-file "~/.0.data/.0.emacs/*scratch*")
      (kill-buffer "*to-be-deleted*")))
(tabbar-display-update))

(defun tabbar-buffer-show-groups-toggle-switch ()
  (interactive)
  (if (and tabbar-mode tabbar--buffer-show-groups)
      (tabbar-buffer-show-groups nil)
    (tabbar-buffer-show-groups t) )  )

(defun ido-tab ()
  "Jump to a buffer in current tabbar group."
  (interactive)
  (unless (and (featurep 'tabbar) tabbar-mode)
    (error "Error: tabbar-mode not turned on."))
  (let* ( ;; Swaps the current buffer name with the next one along.
         (visible-buffers (mapcar (lambda (tab) (buffer-name (tabbar-tab-value tab)))
                                  (tabbar-tabs (tabbar-current-tabset t))))
         (buffer-name (ido-completing-read "Buffer: " visible-buffers))
         window-of-buffer)
    (if (not (member buffer-name visible-buffers))
        (error "'%s' does not have a visible window" buffer-name)
      (switch-to-buffer buffer-name))))

(defun ido-frame-bufs ()
  "Switch buffer, within buffers associated with current frame (`frame-bufs-buffer-list')
  Other buffers are excluded."
  (interactive)
  (if (and (featurep 'init-frames) frame-bufs-mode)
    (progn
      (let* ( (buffers (mapcar 'buffer-name (frame-bufs-buffer-list (selected-frame))))
              (buffers-rotated (append (cdr buffers) (cons (car buffers) nil)))
              (target (ido-completing-read "Buffer: " buffers-rotated)) )
        (switch-to-buffer target)))
    (error "\"frame-bufs-mode\" must first be enabled in order to use this function.")))

(defun ido-group ()
  "Jump to a tabbar group."
  (interactive)
  (unless (and (and (featurep 'tabbar) tabbar-mode)
              (not (and (featurep 'init-frames) frame-bufs-mode)))
    (error "Either tabbar-mode is not active, or frame-bufs-mode is also active."))
  (let* ( (groups (mapcar #'(lambda (group)
                              (format "%s" (cdr group)))
                          (tabbar-tabs (tabbar-get-tabsets-tabset))))
          (group-name (ido-completing-read "Groups: " groups)) )
    (mapc #'(lambda (group)
              (when (string= group-name (format "%s" (cdr group)))
                  (message "Switch to group '%s', current buffer: %s" (cdr group) (car group))
                  (switch-to-buffer (car group)) ))
          (tabbar-tabs (tabbar-get-tabsets-tabset)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SORTING TABS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The manual sorting of tabs requires installation of `dash` from `marmalade`.

(defun tabbar-sort-tab ()
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

(defun tabbar-get-current-buffer-index ()
(let* ((ctabset (tabbar-current-tabset 't))
(ctabs (tabbar-tabs ctabset))
(ctab (tabbar-selected-tab ctabset)))
(length (--take-while (not (eq it ctab)) ctabs))))
 
(defun insert- (list-object index element)
(append (-take index list-object) (list element) (-drop index list-object)))
 
(defun tabbar-move (direction)
"Move current tab to (+ index DIRECTION)"
(interactive)
(let* ((ctabset (tabbar-current-tabset 't))
(ctabs (tabbar-tabs ctabset))
(ctab (tabbar-selected-tab ctabset))
(index (tabbar-get-current-buffer-index))
(others (--remove (eq it ctab) ctabs))
(ins (mod (+ index direction (+ 1 (length others))) (+ 1 (length others)))))
(set ctabset (insert- others ins ctab))
(put ctabset 'template nil)
(tabbar-display-update)))
 
(defun tabbar-move-right ()
"Move current tab to right"
(interactive)
(tabbar-move +1))
 
(defun tabbar-move-left ()
"Move current tab to left"
(interactive)
(tabbar-move -1))

;;;;;;;;;;;;;;;;;;;;;;; (setq frame-bufs-mode nil)   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar tabbar+displayed-buffers '("*scratch*" "*Messages*" "*TODO*" "*Org Agenda*"
  "*BBDB*" "*bbdb*" "*Completions*" "*Org-toodledo-log*" "*Calendar*" "*Buffer List*"
  "*BUFFER LIST*" "*Help*" "*Compile-Log*")
  "*Reagexps matches buffer names always included tabs.")

;; (setq tabbar-buffer-list-function 'buffer-lawlist-function)
(defun buffer-lawlist-function ()
  (let* ((hides (list ?\ ?\*))
  (re (regexp-opt tabbar+displayed-buffers))
  (cur-buf (current-buffer))
  (tabs (delq nil
  (mapcar (lambda (buf)
  (let ((name (buffer-name buf)))
  (when (or 
    (string-match "^*WL:Message*" name)
    (string-match "*~/.0.data/.** output*" name)
    (string-match re name)
    (not (memq (aref name 0) hides)))
  buf)))
  (buffer-list)))))
  (if (memq cur-buf tabs)
  tabs
  (cons cur-buf tabs))))

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
  Return a list of one element based on major mode."
  (list
    (cond
      ((or (get-buffer-process (current-buffer))
        (tabbar-buffer-mode-derived-p
         major-mode '(comint-mode compilation-mode)))
        "system")
      ((member (buffer-name)
        '("*Completions*" "*BBDB*" "*scratch*" "*Messages*" "*bbdb*" "*Org-toodledo-log*" "*Calendar*"
        "*Buffer List*" "*BUFFER LIST*" "*Help*"))
        "system")
      ((eq major-mode '(run-latexmk dired-mode help-mode apropos-mode Info-mode Man-mode)) "system")
         ;; TRUMPS ALL ATTEMPTS AT OTHERWISE CATEGORIZING BUFFERS WITH ASTERICKS
      ;; ((string-equal "*" (substring (buffer-name) 0 1)) "system")
      ((eq major-mode 'org-mode) "org")
      ((member (buffer-name) '("*TODO*" "*Org Agenda*")) "org")
      ((member (buffer-name) '("Folder" "Summary" "Email")) "wanderlust")
      ((memq major-mode
        '(wl-summary-mode wl-original-message-mode wl-draft-mode mime-view-mode wl-message-mode wl-folder-mode
        rail-mode rmail-edit-mode vm-summary-mode vm-mode mail-mode mh-letter-mode mh-show-mode mh-folder-mode
        gnus-summary-mode message-mode gnus-group-mode gnus-article-mode score-mode gnus-browse-killed-mode))
        "wanderlust")
      ((memq major-mode '(text-mode latex-mode emacs-lisp-mode)) "main")
      (t
        (if (and (stringp mode-name) (save-match-data (string-match "[^ ]" mode-name)))
          mode-name
          (symbol-name major-mode))) )))

(defun record-frame-buffer ()
  (setq current-frame (frame-parameter nil 'name))
  (setq restore-buffer (buffer-name)) )

(defun restore-frame-buffer ()
  (switch-to-frame current-frame)
  (switch-to-buffer restore-buffer) )

(defun refresh-frames-buffers ()
(interactive)
  (if (not (and (featurep 'init-frames) frame-bufs-mode))
    (progn
      (get-group "wanderlust") (frames-and-tab-groups)
      (get-group "system") (frames-and-tab-groups)
      (get-group "main") (frames-and-tab-groups)
      (get-group "org") (frames-and-tab-groups)
      (dolist (frame (frame-list))
        (when frame (lawlist-tabbar-forward-group))) ))
  (if (and (featurep 'init-frames) frame-bufs-mode)
    (progn
      (dolist (frame (frame-list))
        (switch-to-frame (frame-parameter frame 'name) )
        (switch-to-buffer (format "%s" (car (frame-bufs-buffer-list (selected-frame)))))
        ;;  NOTE:  The "nil" buffer is caused when there is no buffer assigned to
        ;;  the frame-bufs-buffer-list -- i.e., result when it is empty.
      (if (buffer-exists "nil")
        (kill-buffer "nil")) ))))

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

(defun lawlist-tabbar-cycle (&optional backward type)
  "Cycle to the next available tab.
  The scope of the cyclic navigation through tabs is specified by the
  option `tabbar-cycle-scope'.
  If optional argument BACKWARD is non-nil, cycle to the previous tab
  instead.
  Optional argument TYPE is a mouse event type (see the function
  `tabbar-make-mouse-event' for details)."
  (let* ((tabset (tabbar-current-tabset t))
         (ttabset (tabbar-get-tabsets-tabset))
         ;; If navigation through groups is requested, and there is
         ;; only one group, navigate through visible tabs.
         (cycle (if (and (eq tabbar-cycle-scope 'groups)
                         (not (cdr (tabbar-tabs ttabset))))
                    'tabs
                  tabbar-cycle-scope))
         selected tab)
    (when tabset
      (setq selected (tabbar-selected-tab tabset))
      (cond
       ;; Cycle through visible tabs only.
       ((eq cycle 'tabs)
        (setq tab (tabbar-tab-next tabset selected backward))
        ;; When there is no tab after/before the selected one, cycle
        ;; to the first/last visible tab.
        (unless tab
          (setq tabset (tabbar-tabs tabset)
                tab (car (if backward (last tabset) tabset))))
        )
       ;; Cycle through tab groups only.
       ((eq cycle 'groups)
        (setq tab (tabbar-tab-next ttabset selected backward))
        ;; When there is no group after/before the selected one, cycle
        ;; to the first/last available group.
        (unless tab
          (setq tabset (tabbar-tabs ttabset)
                tab (car (if backward (last tabset) tabset))))
        (if (equal (format "%s" (cdr tab)) "main")
          (if (frame-exists "MAIN")
            (switch-to-frame "MAIN")
            (frame-exists-main)))
        (if (equal (format "%s" (cdr tab)) "org")
          (if (frame-exists "ORG")
            (switch-to-frame "ORG")
            (frame-exists-org)))
        (if (equal (format "%s" (cdr tab)) "system")
          (if (frame-exists "SYSTEM")
            (switch-to-frame "SYSTEM")
            (frame-exists-system)))
        (if (equal (format "%s" (cdr tab)) "wanderlust")
          (if (frame-exists "WANDERLUST")
            (switch-to-frame "WANDERLUST")
            (frame-exists-wanderlust)))
       )
       (t
        ;; Cycle through visible tabs then tab groups.
        (setq tab (tabbar-tab-next tabset selected backward))
        ;; When there is no visible tab after/before the selected one,
        ;; cycle to the next/previous available group.
        (unless tab
          (setq tab (tabbar-tab-next ttabset selected backward))
          ;; When there is no next/previous group, cycle to the
          ;; first/last available group.
          (unless tab
            (setq tabset (tabbar-tabs ttabset)
                  tab (car (if backward (last tabset) tabset))))
          ;; Select the first/last visible tab of the new group.
          (setq tabset (tabbar-tabs (tabbar-tab-tabset tab))
                tab (car (if backward (last tabset) tabset))))
        ))
      (tabbar-click-on-tab tab type))))

(defun lawlist-tabbar-forward-group ()
  "Go to selected tab in the next available group."
  (interactive)
  (let ((tabbar-cycle-scope 'groups))
    (lawlist-tabbar-cycle)))

(defun lawlist-tabbar-backward-group ()
  "Go to selected tab in the previous available group."
  (interactive)
  (let ((tabbar-cycle-scope 'groups))
    (lawlist-tabbar-cycle t)))

(defvar tab-group nil)
(defun get-group (group-name)
  "Jump to a specific tabbar group."
  (unless (and (featurep 'tabbar) tabbar-mode)
    (error "Error: tabbar-mode not turned on."))
  (let* ( (groups (mapcar #'(lambda (group)
      (format "%s" (cdr group))) (tabbar-tabs (tabbar-get-tabsets-tabset)))))
    (mapc #'(lambda (group)
              (when (string= group-name (format "%s" (cdr group)))
              (setq tab-group group)
              ) ;; end of when
            ) ;; end of lambda
      (tabbar-tabs (tabbar-get-tabsets-tabset))
    ) ;; end of mapc
    (if (not (equal (format "%s" (car tab-group)) "#<killed buffer>") )
      (progn
        (switch-to-buffer (car tab-group))
        ;; (message "Switched to tab-group \"%s\",
        ;;   current buffer: %s" (cdr tab-group) (car tab-group))
      )
      ;; else
      (message "(car tab-group):  %s" (car tab-group))
      (and
        (eq header-line-format tabbar-header-line-format)
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
            (message "\"%s\" moved to the front of the buffer-list." sibling))) )
    ) ;; end of if
  ) ;; end of let
 ) ;; end of defun

(defun frame-exists-system ()
(interactive)
  (if (frame-exists "SYSTEM")
    ;; then
    (progn
      (switch-to-frame "SYSTEM")
      (get-group "system") )
    ;; else -- take control of an existing frame not yet specially named.
    (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
      ;; then
      (progn
        (toggle-frame-maximized)
        (set-frame-name "SYSTEM") )
      ;; else
        (make-frame)
        (toggle-frame-maximized)
        (set-frame-name "SYSTEM") )
    (get-group "system") ))

(defun frame-exists-main ()
(interactive)
  (if (frame-exists "MAIN")
    ;; then
    (progn
      (switch-to-frame "MAIN")
      (get-group "main") )
    ;; else -- take control of an existing frame not yet specially named.
    (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
      ;; then
      (progn
        (toggle-frame-maximized)
        (set-frame-name "MAIN") )
      ;; else
        (make-frame)
        (toggle-frame-maximized)
        (set-frame-name "MAIN") )
    (get-group "main") ))

(defun frame-exists-org ()
(interactive)
  (if (frame-exists "ORG")
    ;; then
    (progn
      (switch-to-frame "ORG")
      (get-group "org") )
    ;; else -- take control of an existing frame not yet specially named.
    (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
      ;; then
      (progn
        (toggle-frame-maximized)
        (set-frame-name "ORG") )
      ;; else
        (make-frame)
        (toggle-frame-maximized)
        (set-frame-name "ORG") )
    (get-group "org") ))

(defun frame-exists-wanderlust ()
(interactive)
  (if (frame-exists "WANDERLUST")
    ;; then
    (progn
      (switch-to-frame "WANDERLUST")
      (get-group "wanderlust") )
    ;; else -- take control of an existing frame not yet specially named.
    (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
      ;; then
      (progn
        (toggle-frame-maximized)
        (set-frame-name "WANDERLUST") )
      ;; else
        (make-frame)
        (toggle-frame-maximized)
        (set-frame-name "WANDERLUST") )
    (get-group "wanderlust") ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DESKTOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lawlist-desktop-restore-file-buffer (buffer-filename
                                    _buffer-name
                                    _buffer-misc)
  "Alternative version for desktop.el restore a file buffer."
  (when buffer-filename
    (if (or (file-exists-p buffer-filename)
      (let ((msg (format "Desktop: File \"%s\" no longer exists."
             bufferfilename)))
        (if desktop-missing-file-warning
      (y-or-n-p (concat msg " Re-create buffer? "))
    (message "%s" msg)
    nil)))
  (let* ((auto-insert nil) ; Disable auto insertion
         (coding-system-for-read
    (or coding-system-for-read
        (cdr (assq 'buffer-file-coding-system
             desktop-buffer-locals))))
         (buf (lawlist-find-file buffer-filename)))
    (and (not (eq major-mode desktop-buffer-major-mode))
         (functionp desktop-buffer-major-mode)
         (funcall desktop-buffer-major-mode))
    buf)
      nil)))

(defvar desktop-modes-not-to-save nil)

(defun lawlist-desktop-read (&optional dirname)
  (interactive)
(defalias 'desktop-restore-file-buffer 'lawlist-desktop-restore-file-buffer)
(setq desktop-save-mode t)
(desktop-auto-save-set-timer)
(setq desktop-restore-frames nil)
(setq desktop-dirname           "~/.0.data/.0.emacs/"
    desktop-base-file-name      ".desktop"
    desktop-base-lock-name      ".lock"
    desktop-path                (list desktop-dirname)
    desktop-save                t
    desktop-files-not-to-save   "\\*scratch\\*\\|\\*bbdb\\*\\|\\*BBDB\\*\\|\\*TODO\\*\\|\\*DONE\\*" ;; "^$"  reload tramp paths
    desktop-load-locked-desktop nil )
(setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
                "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble"
                "\\)$"))
    (add-to-list 'desktop-modes-not-to-save 'dired-mode)
    (add-to-list 'desktop-modes-not-to-save 'Info-mode)
    (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
    (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  (if (file-exists-p (desktop-full-file-name))
;;  (if (file-exists-p "~/.0.data/.0.emacs/.desktop")
  (let ((desktop-first-buffer nil)
        (desktop-buffer-ok-count 0)
        (desktop-buffer-fail-count 0)
        (owner (desktop-owner))
        (desktop-save nil))
    (if (and owner
       (memq desktop-load-locked-desktop '(nil ask))
       (or (null desktop-load-locked-desktop)
           (daemonp)
           (not (y-or-n-p (format "Warning: desktop file appears to be in use by PID %s.\n\
      Using it may cause conflicts.  Use it anyway? " owner)))))
        (let ((default-directory desktop-dirname))
    (setq desktop-dirname nil)
    (run-hooks 'desktop-not-loaded-hook)
    (unless desktop-dirname
      (message "Desktop file in use; not loaded.")))
      (desktop-lazy-abort)
      (load (desktop-full-file-name) t t t)
      (setq desktop-file-modtime (nth 5 (file-attributes (desktop-full-file-name))))
      (unless owner
        (condition-case nil
      (desktop-claim-lock)
    (file-error (message "Couldn't record use of desktop file")
          (sit-for 1))))
      (run-hooks 'desktop-delay-hook)
      (setq desktop-delay-hook nil)
      (desktop-restore-frameset)
      (run-hooks 'desktop-after-read-hook)
       (setq desktop-saved-frameset nil)
      t))
      (desktop-clear)
      (let ((default-directory desktop-dirname))
        (run-hooks 'desktop-no-desktop-file-hook))
      (message "No desktop file.")
      nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DIAGNOSTIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tabbar-info ()
"Diagnostic tabbar data."
(interactive)
  (setq frame-bufs-car (car (frame-bufs-buffer-list (selected-frame))))
  (setq frame-bufs-cdr (cdr (frame-bufs-buffer-list (selected-frame))))
  (setq frame-bufs-associated-frame (mapcar 'buffer-name (frame-bufs-buffer-list (selected-frame))))
  (setq frame-bufs-full-list-frame (mapcar 'buffer-name (frame-bufs-buffer-list (selected-frame) t))) ;; hides system buffers
  (setq tab-focus (tabbar-selected-tab tabbar-current-tabset))
  (setq tabs-group-focus (tabbar-tabs tabbar-current-tabset))
  (setq group-focus tabbar-current-tabset)
  (setq frame-focus (frame-parameter nil 'name))
  (setq all-frames (mapcar (lambda (frame) (frame-parameter frame 'name)) (frame-list)) )
  (setq buffer-focus (buffer-name))
  (setq buffers-group-focus (mapcar (lambda (tab) (buffer-name (tabbar-tab-value tab))) (tabbar-tabs (tabbar-current-tabset t))))
  (setq all-buffers (cdr (tabbar-buffer-list)))
;;  (setq all-groups (mapcar #'(lambda (group) (format "%s" (cdr group))) (tabbar-tabs tabbar-tabsets-tabset)))
  (setq all-tabs-per-group-focus (tabbar-tabs (tabbar-get-tabsets-tabset)))
  (setq cdr-all-tabs-per-group-focus (cdr (tabbar-tabs (tabbar-get-tabsets-tabset))))
  (setq car-all-tabs-per-group-focus (car (tabbar-tabs (tabbar-get-tabsets-tabset))))
  (if (buffer-exists "*Messages*")
      (display-buffer "*Messages*")
    (display-buffer (get-buffer-create "*Messages*")))
  (if (not (equal (buffer-name) "*BUFFER LIST*"))
    (other-window 1))
  (message "\n---------------------- tabbar-info --------------------- \n")
  (message "frame-bufs-car:  %s \n" frame-bufs-car)
  (message "frame-bufs-cdr:  %s \n" frame-bufs-cdr)
  (message "Selected Frame Buffer List:  %s \n" (frame-parameter (selected-frame) 'buffer-list))
  (message "Buffer List -- all:  %s \n" (buffer-list))
  (message "Selected Frame Buried Buffer List:  %s \n" (frame-parameter (selected-frame) 'buried-buffer-list))
  (message "Frame-Bufs Associated Frame:  %s \n" frame-bufs-associated-frame)
  (message "Frame-Bufs Full List Frame:  %s \n" frame-bufs-full-list-frame)
  (message "Tab - Focus:  %s \n" tab-focus)
  (message "Tabs Group Focus:  %s \n" tabs-group-focus)
  (message "Group - Focus:  %s \n" group-focus)
  (message "Frame - Focus:  %s \n" frame-focus)
  (message "All Frames:  %s \n" all-frames)
  (message "Buffer - Focus:  %s \n" buffer-focus)
  (message "Buffers Group Focus:  %s \n" buffers-group-focus)
  (message "All Buffers:  %s \n" all-buffers)
  ;; (message "All Groups:  %s \n" all-groups)
  (message "All Tabs (Per Group) -- Focus:  %s \n" all-tabs-per-group-focus)
  (message "\"cdr\" of \"All Tabs (Per Group) -- Focus\":  %s \n" cdr-all-tabs-per-group-focus)
  (message "\"car\" of \"All Tabs (Per Group) -- Focus\":  %s \n" car-all-tabs-per-group-focus)
  (message "-------------------------------------------------------- \n")
  (message "\"M-x delete-window\" to close this window.")
  (goto-char (point-max))
  (scroll-down 20))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-tabbar)
