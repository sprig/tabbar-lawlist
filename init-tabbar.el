;; init-tabbar.el

;; Version (alpha) -- frames / tab-groups:  SYSTEM, MAIN, WANDERLUST, ORG

;; Requires a current version of Emacs Trunk (24.3.50 (9.0)) and
;; modified versions of frame-bufs and buff-menu (consolidated into one file)
;; and a modified version of tabbar, all of which are contained within the lawlist
;; repository:  https://github.com/lawlist/tabbar-lawlist

;; If using (desktop-save-mode 1), then also use (setq desktop-restore-frames nil)

;; Authored (in part) by lawlist -- modifying various functions found at the following links:
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
;; set current buffer -- switch to tab (function must contain &optional type)
;; (setq wl-selected-message-buffer (car (tabbar-tabs (tabbar-get-tabsets-tabset))))
;; (tabbar-click-on-tab wl-selected-message-buffer type)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'tabbar)

(require 'init-frames) ;; Use the modified version in the lawlist repository.

(setq tabbar-cycle-scope 'tabs)

(setq ido-enable-flex-matching t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KEYBOARD SHORTCUTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key [?\s-w] (lambda () (interactive) (delete-frame-if-empty) ))
;; (global-set-key [?\s-w] 'kill-this-buffer)
(global-set-key [?\s-o] 'lawlist-find-file)

(global-set-key [?\s-1] 'goto-unread-folder)
(global-set-key [?\s-2] 'goto-sent-recently-folder)
(global-set-key [?\s-3] 'activate-wanderlust)
(global-set-key [?\s-4] 'goto-inbox-folder)
(global-set-key [?\s-5] 'goto-sent-folder)

(global-set-key [(f5)] (lambda () (interactive) (refresh-frames-buffers)))
(global-set-key [?\s-\~] 'cycle-backward-frames-groups)
(global-set-key [?\s-\`] 'cycle-forward-frames-groups)
(global-set-key [(control shift tab)] 'tabbar-backward-group)
(global-set-key [(control tab)] 'tabbar-forward-group) 
(global-set-key (kbd "<M-s-right>") 'tabbar-forward)
(global-set-key (kbd "<M-s-left>") 'tabbar-backward)
;; (define-key Buffer-menu-mode-map "\e\e\e" 'delete-window)
;; (define-key buff-menu-mode-map "\e\e\e" (lambda () (interactive) (kill-buffer nil) (delete-window) ))
;; (define-key lawlist-calendar-mode-map "\e\e\e" 'delete-window)

;; M-x associate-current-buffer -- control+option+command+r
(global-set-key (kbd "<C-M-s-268632082>") 'lawlist-frame-bufs-reset)

;; M-x associate-current-buffer -- control+option+command+a
(global-set-key (kbd "<C-M-s-268632065>") 'associate-current-buffer)

;; M-x frame-bufs-dismiss-buffer -- control+option+command+n
;; Do NOT modify `frame-bufs-diss-buffer`, which is used "as-is" with `frame-bufs-menu-execute`.
(global-set-key (kbd "<C-M-s-268632078>") (lambda ()
  (interactive)
  (if (and (featurep 'init-frames) frame-bufs-mode)
    (progn
      (frame-bufs-dismiss-buffer)
      (switch-to-buffer (format "%s" (car (frame-bufs-buffer-list (selected-frame)))))
      ;;  NOTE:  The "nil" buffer is caused when there is no buffer assigned to
      ;;  the frame-bufs-buffer-list -- i.e., result when it is empty.
      ;;  I already have the `buffer-exists` function elsewhere:
      ;;  (defun buffer-exists (bufname) (not (eq nil (get-buffer bufname))))
      (if (buffer-exists "nil")
        (kill-buffer "nil"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; HOOKS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook
  (lambda ()
    (tabbar-mode t)
    (setq frame-bufs-mode t)
    (setq tabbar-buffer-list-function 'tabbar-buffer-list) ;; 'tabbar-buffer-list or 'buffer-lawlist-function
    (setq tabbar-buffer-groups-function (lambda () (list (cond 
      ((memq (current-buffer) (frame-bufs-buffer-list (selected-frame))) "frame-bufs") 
      (t "non-associated") ))))
  ))

(add-hook 'emacs-startup-hook
  (lambda ()
    (require 'init-tabbar)
    (kill-buffer "*scratch*")
    (associate-current-buffer) ;; i.e., *Messages*
    (lawlist-find-file "~/.0.data/.0.emacs/*bbdb*")
    (lawlist-find-file "~/.0.data/.0.emacs/*scratch*")
    (desktop-save-mode 1)
    (lawlist-desktop-read)
    (desktop-auto-save-set-timer)
  ))

(add-hook 'find-file-hook
  (lambda ()

  ))

(add-hook 'mime-view-mode-hook
  (lambda ()
;;    (frames-and-tab-groups)
  ))

(add-hook 'org-agenda-mode-hook
  (lambda ()
;;    (frames-and-tab-groups)
  ))

(add-hook 'wl-draft-mode-hook
  (lambda ()
;;    (frames-and-tab-groups)
  ))

(add-hook 'wl-summary-mode-hook
  (lambda ()
;;    (frames-and-tab-groups)
  ))

(add-hook 'wl-folder-mode-hook
  (lambda ()
;;    (frames-and-tab-groups)
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LAWLIST FIND FILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar system-buffer-regexp nil
  "Regexps matching `buffer-filename` for frame name `SYSTEM`.")
(setq system-buffer-regexp '("*scratch*" "*bbdb*"))

(defvar main-buffer-regexp nil
  "Regexps matching `buffer-filename` for frame name `MAIN`.")
(setq main-buffer-regexp '("\\.txt" "\\.tex" "\\.el" "\\.yasnippet"))

(defvar org-buffer-regexp nil
  "Regexps matching `buffer-filename` for frame name `ORG`.")
(setq org-buffer-regexp '("[*]TODO[*]" "\\.org_archive" "\\.org"))

(defvar buffer-filename nil)

(defun lawlist-find-file (&optional buffer-filename)
  "Locate or create a specific frame, and then open the file."
  (interactive)
  ;; If not Emacs built --with-ns, then `read-file-name´ may be used.
  ;; (unless buffer-filename (setq buffer-filename (read-file-name "Select File:  ")))
  (unless buffer-filename (setq buffer-filename (ns-read-file-name "Select File:" "~/.0.data/" t nil)))
  (if buffer-filename (progn
    (when (lawlist-regexps-match-p org-buffer-regexp buffer-filename)
      (if (frame-exists "ORG")
          (switch-to-frame "ORG")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (and
              (not (equal "MAIN" (frame-parameter frame 'name)))
              (not (equal "SYSTEM" (frame-parameter frame 'name)))
              (not (equal "ORG" (frame-parameter frame 'name)))
              (not (equal "WANDERLUST" (frame-parameter frame 'name)))
              (not (equal "MISCELLANEOUS" (frame-parameter frame 'name))) )
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (toggle-frame-maximized)
              (set-frame-name "ORG"))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (frame-exists "ORG"))
          (progn
            (make-frame)
            (toggle-frame-maximized)
            (set-frame-name "ORG"))) ))
    (when (lawlist-regexps-match-p main-buffer-regexp buffer-filename)
      (if (frame-exists "MAIN")
          (switch-to-frame "MAIN")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (and
              (not (equal "MAIN" (frame-parameter frame 'name)))
              (not (equal "SYSTEM" (frame-parameter frame 'name)))
              (not (equal "ORG" (frame-parameter frame 'name)))
              (not (equal "WANDERLUST" (frame-parameter frame 'name)))
              (not (equal "MISCELLANEOUS" (frame-parameter frame 'name))) )
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (toggle-frame-maximized)
              (set-frame-name "MAIN"))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (frame-exists "MAIN"))
          (progn
            (make-frame)
            (toggle-frame-maximized)
            (set-frame-name "MAIN"))) ))
    (when (lawlist-regexps-match-p system-buffer-regexp buffer-filename)
      (if (frame-exists "SYSTEM")
          (switch-to-frame "SYSTEM")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (and
              (not (equal "MAIN" (frame-parameter frame 'name)))
              (not (equal "SYSTEM" (frame-parameter frame 'name)))
              (not (equal "ORG" (frame-parameter frame 'name)))
              (not (equal "WANDERLUST" (frame-parameter frame 'name)))
              (not (equal "MISCELLANEOUS" (frame-parameter frame 'name))) )
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (toggle-frame-maximized)
              (set-frame-name "SYSTEM"))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (frame-exists "SYSTEM"))
          (progn
            (make-frame)
            (toggle-frame-maximized)
            (set-frame-name "SYSTEM"))) ))
    (when (and (not (lawlist-regexps-match-p org-buffer-regexp buffer-filename))
            (not (lawlist-regexps-match-p main-buffer-regexp buffer-filename))
            (not (lawlist-regexps-match-p system-buffer-regexp buffer-filename)) )
      (if (frame-exists "MISCELLAENOUS")
          (switch-to-frame "MISCELLAENOUS")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (and
              (not (equal "MAIN" (frame-parameter frame 'name)))
              (not (equal "SYSTEM" (frame-parameter frame 'name)))
              (not (equal "ORG" (frame-parameter frame 'name)))
              (not (equal "WANDERLUST" (frame-parameter frame 'name)))
              (not (equal "MISCELLANEOUS" (frame-parameter frame 'name))) )
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (toggle-frame-maximized)
              (set-frame-name "MISCELLAENEOUS"))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (frame-exists "MISCELLAENEOUS"))
          (progn
            (make-frame)
            (toggle-frame-maximized)
            (set-frame-name "MISCELLAENEOUS"))) ))
    (find-file buffer-filename)
    (associate-current-buffer) )))

;;;;;;;;;;;;;;;;;;;;;;;;;;; LAUNCH FROM OSX FINDER TO EMACS ;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DISPLAY BUFFER NO FILE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar system-nofile-regexp nil
  "Regexps matching `buffer-name buffer` for frame name `SYSTEM`.")
(setq system-nofile-regexp '("\\(\\*Metahelp\\*\\|\\*Help\\*\\)"))

(defvar main-nofile-regexp nil
  "Regexps matching `buffer-name buffer` for frame name `MAIN`.")
(setq main-nofile-regexp '("\\.pdf"))

(defvar org-nofile-regexp nil
  "Regexps matching `buffer-name buffer` for frame name `ORG`.")
(setq org-nofile-regexp '("\\*Org Agenda\\*"))

(setq display-buffer-alist '((lawlist-p . (nofile-display-buffer-pop-up-frame))))


;;(customize-set-variable
;;  'display-buffer-alist '((".*" . (nofile-display-buffer-pop-up-frame)))) 

;; (add-to-list 'display-buffer-alist '( "\\(\\*Metahelp\\*\\|\\*Help\\*\\)"
;;   (nofile-display-buffer-pop-up-frame)) )

(defun lawlist-p (buffer action)
  (let ((buffer (get-buffer buffer)))
  (or
    (lawlist-regexps-match-p org-nofile-regexp (buffer-name buffer))
    (lawlist-regexps-match-p main-nofile-regexp (buffer-name buffer))
    (lawlist-regexps-match-p system-nofile-regexp (buffer-name buffer)) )))

(defun nofile-display-buffer-pop-up-frame (buffer alist)
    (when (lawlist-regexps-match-p org-nofile-regexp (buffer-name buffer))
      (if (frame-exists "ORG")
          (switch-to-frame "ORG")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (and
              (not (equal "MAIN" (frame-parameter frame 'name)))
              (not (equal "SYSTEM" (frame-parameter frame 'name)))
              (not (equal "ORG" (frame-parameter frame 'name)))
              (not (equal "WANDERLUST" (frame-parameter frame 'name)))
              (not (equal "MISCELLANEOUS" (frame-parameter frame 'name))) )
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "ORG")
              (toggle-frame-maximized))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (frame-exists "ORG"))
          (progn
            (make-frame)
            (set-frame-name "ORG")
            (toggle-frame-maximized))) ))
    (when (lawlist-regexps-match-p main-nofile-regexp (buffer-name buffer))
      (if (frame-exists "MAIN")
          (switch-to-frame "MAIN")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (and
              (not (equal "MAIN" (frame-parameter frame 'name)))
              (not (equal "SYSTEM" (frame-parameter frame 'name)))
              (not (equal "ORG" (frame-parameter frame 'name)))
              (not (equal "WANDERLUST" (frame-parameter frame 'name)))
              (not (equal "MISCELLANEOUS" (frame-parameter frame 'name))) )
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "MAIN")
              (toggle-frame-maximized))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (frame-exists "MAIN"))
          (progn
            (make-frame)
            (set-frame-name "MAIN")
            (toggle-frame-maximized))) ))
    (when (lawlist-regexps-match-p system-nofile-regexp (buffer-name buffer))
      (if (frame-exists "SYSTEM")
          (switch-to-frame "SYSTEM")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (and
              (not (equal "MAIN" (frame-parameter frame 'name)))
              (not (equal "SYSTEM" (frame-parameter frame 'name)))
              (not (equal "ORG" (frame-parameter frame 'name)))
              (not (equal "WANDERLUST" (frame-parameter frame 'name)))
              (not (equal "MISCELLANEOUS" (frame-parameter frame 'name))) )
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "SYSTEM")
              (toggle-frame-maximized))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (frame-exists "SYSTEM"))
          (progn
            (make-frame)
            (set-frame-name "SYSTEM")
            (toggle-frame-maximized))) ))
    (when (and (not (lawlist-regexps-match-p org-nofile-regexp (buffer-name buffer)))
            (not (lawlist-regexps-match-p main-nofile-regexp (buffer-name buffer)))
            (not (lawlist-regexps-match-p system-nofile-regexp (buffer-name buffer))) )
      (if (frame-exists "MISCELLAENOUS")
          (switch-to-frame "MISCELLAENOUS")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (and
              (not (equal "MAIN" (frame-parameter frame 'name)))
              (not (equal "SYSTEM" (frame-parameter frame 'name)))
              (not (equal "ORG" (frame-parameter frame 'name)))
              (not (equal "WANDERLUST" (frame-parameter frame 'name)))
              (not (equal "MISCELLANEOUS" (frame-parameter frame 'name))) )
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "MISCELLAENEOUS")
              (toggle-frame-maximized))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (frame-exists "MISCELLAENEOUS"))
          (progn
            (make-frame)
            (set-frame-name "MISCELLAENEOUS")
            (toggle-frame-maximized)))))
    (if (and (featurep 'init-frames) frame-bufs-mode)
      (frame-bufs-add-buffer buffer (selected-frame))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GENERIC REGEXP FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE:  (defvar system-nofile-regexp nil
;;             "Regexps matching `buffer-name buffer` for frame name `SYSTEM`.")
;;           (setq system-nofile-regexp '("\\(\\*Metahelp\\*\\|\\*Help\\*\\)"))
;;
;;           (lawlist-regexps-match-p org-nofile-regexp (buffer-name buffer))
;;
;;           (lawlist-regexps-match-p org-buffer-regexp buffer-filename)

(defun lawlist-regexps-match-p (regexps string)
  (catch 'matched
    (dolist (regexp regexps)
      (if (string-match regexp string)
        (throw 'matched t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GENERIC FRAME UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WANDERLUST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun wanderlust-display-buffer-pop-up-frame ()
  (if (frame-exists "WANDERLUST")
      (switch-to-frame "WANDERLUST")
    ;; If unnamed frame exists, then take control of it.
    (catch 'break (dolist (frame (frame-list))
      (if (and
          (not (equal "MAIN" (frame-parameter frame 'name)))
          (not (equal "SYSTEM" (frame-parameter frame 'name)))
          (not (equal "ORG" (frame-parameter frame 'name)))
          (not (equal "WANDERLUST" (frame-parameter frame 'name)))
          (not (equal "MISCELLANEOUS" (frame-parameter frame 'name))) )
        (throw 'break (progn
          (switch-to-frame (frame-parameter frame 'name))
          (toggle-frame-maximized)
          (set-frame-name "WANDERLUST")
          (lawlist-frame-bufs-reset))))))
    ;; If dolist found no unnamed frame, then create / name it.
    (if (not (frame-exists "WANDERLUST"))
      (progn
        (make-frame)
        (toggle-frame-maximized)
        (set-frame-name "WANDERLUST")
        (lawlist-frame-bufs-reset)))))

(defun associate-wl-summary-buffer-name ()
  (if (and (featurep 'init-frames) frame-bufs-mode)
    (let* (
      (frame (selected-frame))
      (buf (get-buffer wl-summary-buffer-name)))
    (frame-bufs-add-buffer buf frame))) )

(defun associate-wl-folder-buffer-name ()
(if (and (featurep 'init-frames) frame-bufs-mode)
    (let* (
      (frame (selected-frame))
      (buf (get-buffer wl-folder-buffer-name)) )
    (frame-bufs-add-buffer buf frame))) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DIAGNOSTIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (display-buffer "*Messages*")
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
;; with either `lawlist-tabbar-forward-group` or `lawlist-tabbar-backward-group`.
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
;; (setq tabbar-buffer-list-function 'buffer-lawlist-function)
(defun buffer-lawlist-function ()
  (let* ((hides (list ?\ ?\*))
  (re (regexp-opt tabbar+displayed-buffers))
  (cur-buf (current-buffer))
  (tabs (delq nil
  (mapcar (lambda (buf)
  (let ((name (buffer-name buf)))
  (when (or 
    (string-match "^*WL:Message*" name) ;; without the ^ will display the " Original . . ."
    (string-match "*~/.0.data/.** output*" name)
    (string-match re name)
    (not (memq (aref name 0) hides)))
  buf)))
  (buffer-list)))))
  (if (memq cur-buf tabs)
  tabs
  (cons cur-buf tabs))))

;;  (defun buffer-lawlist-function ()
;;    (let* ((hides (list ?\ ?\*))
;;    (re (regexp-opt tabbar+displayed-buffers))
;;    (cur-buf (current-buffer))
;;    (tabs (delq nil
;;    (mapcar (lambda (buf)
;;    (let ((name (buffer-name buf)))
;;    (when (and (not (string-match "*lawlist:.**" name)) ;; still not sure what this line does
;;    (or (string-match re name)
;;    (not (memq (aref name 0) hides))))
;;    buf)))
;;    (buffer-list)))))
;;    (if (memq cur-buf tabs)
;;    tabs
;;    (cons cur-buf tabs))))


(defvar wanderlust-insert nil)
(defvar system-insert nil)
(defvar main-insert nil)
(defvar org-insert nil)
(defun tabbar-choice ()
(interactive)
  "Different choices relating to tab groups, frame-bufs-mode, and the like."
  (message "Choose:  [F]rame-Bufs | [d]efault | [c]ourt | [a]ll | ido- [t/T]ab [f]rame [g]roup | [v/h] Tile | [G]roups" )
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
        (setq frame-bufs-mode nil)
        (setq tabbar-buffer-list-function 'buffer-lawlist-function)
        (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
        (global-set-key [?\s-\`] nil)
        (global-set-key [?\s-\`] (lambda () (interactive)
          (if (equal "MAIN" (frame-parameter nil 'name))
            (progn
              (switch-to-frame "ORG")
              (get-group "org"))
            (switch-to-frame "MAIN")
            (get-group "main"))))
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
      ((?T)
        ;; A modified version of frame-bufs by Al Parker is included in the lawlist repository.
        (tabbar-display-update)
        (sit-for 0)
        (ido-frame-bufs))
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
        ;; (frame-bufs-reset-all-frames) ;; Is this needed if frame-bufs-mode previously activated?
        (tabbar-display-update)
        (dolist (frame (frame-list))
          (switch-to-frame (frame-parameter frame 'name) )
          (switch-to-buffer (format "%s" (car (frame-bufs-buffer-list (selected-frame)))))
          ;;  NOTE:  The "nil" buffer is caused when there is no buffer assigned to
          ;;  the frame-bufs-buffer-list -- i.e., result when it is empty.
          ;;  I already have the `buffer-exists` function elsewhere:
          ;;  (defun buffer-exists (bufname) (not (eq nil (get-buffer bufname))))
          (if (buffer-exists "nil")
            (kill-buffer "nil")) )
        (restore-frame-buffer) )
      ((?G)
        (tabbar-buffer-show-groups-toggle-switch)
        (tabbar-display-update))
      (t (message "No changes have been made.")) )))))

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
  (unless (and (featurep 'tabbar) tabbar-mode)
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

(defvar tab-group nil)
(defun get-group (group-name)
  "Jump to a specific tabbar group."
  (unless (and (featurep 'tabbar) tabbar-mode)
    (error "Error: tabbar-mode not turned on."))
;;  (set tabbar-tabsets-tabset (tabbar-map-tabsets 'tabbar-selected-tab)) ;; refresh 1 of 3
;;  (tabbar-scroll tabbar-tabsets-tabset 0)  ;; refresh 2 of 3
;;  (tabbar-set-template tabbar-tabsets-tabset nil)  ;; refresh 3 of 3
  (let* ( (groups (mapcar #'(lambda (group) (format "%s" (cdr group))) (tabbar-tabs (tabbar-get-tabsets-tabset))))) ;; tabbar-tabsets-tabset
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
;;        (message "Switched to tab-group \"%s\", current buffer: %s" (cdr tab-group) (car tab-group))
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
            (message "The buffer named \"%s\" was moved to the front of the buffer-list." sibling))) )
    ) ;; end of if
  ) ;; end of let
 ) ;; end of defun

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


(defun ido-frame-bufs ()
  "Switch buffer, within buffers associated with current frame (`frame-bufs-buffer-list')
  Other buffers are excluded."
  (interactive)
  (if (and (featurep 'init-frames) frame-bufs-mode)
    (progn
      (let* ( (buffers (mapcar 'buffer-name (frame-bufs-buffer-list (selected-frame))))
              (buffers-rotated (append (cdr buffers) (cons (car buffers) nil)))
              (target (ido-completing-read "Buffer: " buffers-rotated)) )
        (switch-to-buffer target))
;;    (call-interactively 'ido-switch-buffer)
     )
    (error "\"frame-bufs-mode\" must first be enabled in order to use this function.")))
      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FRAMES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ido-frame ()
  (interactive)
  (setq frame-to (ido-completing-read "Select Frame:  " (mapcar (lambda (frame) (frame-parameter frame 'name)) (frame-list))))
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

(defun frame-exists-system ()
(interactive)
  (if (frame-exists "SYSTEM")
    ;; then
    (progn
      (switch-to-frame "SYSTEM")
      (get-group "system")
    )
    ;; else -- take control of an existing frame not yet specially named.
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
        (make-frame)
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
      (switch-to-frame "MAIN")
      (get-group "main")
    )
    ;; else -- take control of an existing frame not yet specially named.
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
        (make-frame)
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
      (switch-to-frame "ORG")
      (get-group "org")
    )
    ;; else -- take control of an existing frame not yet specially named.
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
        (make-frame)
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
      (switch-to-frame "WANDERLUST")
      (get-group "wanderlust")
    )
    ;; else -- take control of an existing frame not yet specially named.
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
        (make-frame)
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
        ;;  I already have the `buffer-exists` function elsewhere:
        ;;  (defun buffer-exists (bufname) (not (eq nil (get-buffer bufname))))
      (if (buffer-exists "nil")
        (kill-buffer "nil")) ))))

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
  ;;  I already have the `buffer-exists` function elsewhere:
  ;;  (defun buffer-exists (bufname) (not (eq nil (get-buffer bufname))))
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
  ;;  I already have the `buffer-exists` function elsewhere:
  ;;  (defun buffer-exists (bufname) (not (eq nil (get-buffer bufname))))
  (if (buffer-exists "nil")
    (kill-buffer "nil")) )

(defun delete-frame-if-empty ()
(interactive)
  (if (not (equal "SYSTEM" (frame-parameter nil 'name)))
  ;; THEN
  (progn
    (setq current-buffer-name (buffer-name))
    (tabbar-backward)
    (setq previous-buffer-name (buffer-name))
    (kill-buffer current-buffer-name)
    (if (equal current-buffer-name previous-buffer-name)
      (delete-frame)))
  ;; ELSE
  (if (not (equal (buffer-name) "*Messages*"))
    ;; then
    (kill-buffer nil)
    ;; else
    (message "Please leave the *Messages* buffer open, or use M-x kill-buffer or kill-this-buffer, or use C-x k.")) ))
  ;; (if (buffer-exists "*scratch*")
  ;; (switch-to-buffer "*scratch*")
  ;; (find-file "~/.0.data/.0.emacs/*scratch*")) 


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
                tab (car (if backward (last tabset) tabset)))) ;; this is just a setq, not an action.
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init-tabbar)
