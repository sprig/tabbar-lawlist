;;;;;;;;;;;;;;;;; DISPLAY-BUFFER-ALIST and DISPLAY-BUFFER ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see also:  https://github.com/lawlist/tabbar-lawlist/blob/master/init-tabbar.el

(defun example ()
  (interactive)
  ;; condition # 3 | file-visiting buffer
  (lawlist-find-file "*bar*")
  (set-frame-height (selected-frame) 20)
  (set-frame-width (selected-frame) 80)
  (set-frame-position (selected-frame) 0 0)
  (message "\*bar\* appears in frame name SYSTEM.")
  (sit-for 2)
  ;; condition # 4(a) | no-file-visiting buffer
  (display-buffer (get-buffer-create "*NO-FILE-special-buffer-regexp*"))
  (message "NO-FILE buffer existing frame.")
  (sit-for 2)
  ;; condition # 2(a) | file-visiting buffer
  (lawlist-find-file "foo.txt")
  (set-frame-height (selected-frame) 20)
  (set-frame-width (selected-frame) 80)
  (set-frame-position (selected-frame) 100 100)
  (message "\"foo.txt\" appears in frame name MAIN.")
  (sit-for 2)
  ;; condition # 1 | file-visiting buffer
  (lawlist-find-file "doe.org")
  (set-frame-height (selected-frame) 20)
  (set-frame-width (selected-frame) 80)
  (set-frame-position (selected-frame) 200 200)
  (message "\"doe.org\" appears in frame name ORG.")
  (sit-for 2)
  ;; condition # 4(b) | file-visiting buffer
  (lawlist-find-file "*FILE-special-buffer-regexp*")
  (message "FILE buffer existing frame.")
  (sit-for 2)
  ;; condition # 6 | no-file-visiting buffer default display
  (calendar)
  (message "Default for no-file-visiting-buffers.")
  (sit-for 2)
  ;; condition # 5 | file-visiting buffer with no pre-defined regexp.
  (lawlist-find-file "*FILE-undefined-regexp*")
  (set-frame-height (selected-frame) 20)
  (set-frame-width (selected-frame) 80)
  (set-frame-position (selected-frame) 300 300)
  (message "\*IS\* buffer-filename.  \*NOT\* defined by any particular regexp.")
  (sit-for 2)
  ;; condition # 2(b) | no-file-visiting buffer
  (display-buffer (get-buffer-create "*NO-FILE-main-buffer-regexp*"))
  (set-frame-height (selected-frame) 20)
  (set-frame-width (selected-frame) 80)
  (set-frame-position (selected-frame) 400 400)
  (message "\*NOT\* buffer-filename.  \*IS\* defined by main-buffer-regexp.")
  (sit-for 2)
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

(defvar regexp-frame-names "^\\(?:MAIN\\|SYSTEM\\|ORG\\|MISCELLANEOUS\\)$"
    "Regexp matching frames with specific names.")

(defvar system-buffer-regexp '("*scratch*" "*bbdb*" "*bar*")
  "Regexp of file / buffer names displayed in frame `SYSTEM`.")

(defvar main-buffer-regexp
  '("\\.txt" "\\.tex" "\\.el" "\\.yasnippet" "\\*NO-FILE-main-buffer-regexp\\*")
  "Regexp of file / buffer names displayed in frame `MAIN`.")

(defvar org-buffer-regexp
  '("[*]TODO[*]" "[*]Org Agenda[*]" "\\.org_archive" "\\.org")
  "Regexp of file / buffer names displayed in frame  `ORG`.")

(defvar special-buffer-regexp
  '("[*]NO-FILE-special-buffer-regexp[*]" "*FILE-special-buffer-regexp*")
  "Regexp of file / buffer names that will display in current frame.")

(defun lawlist-find-file (&optional lawlist-filename)
  "With assistance from the display-buffer-alist, locate or create a specific frame,
  and then open the file."
  (interactive)
  (display-buffer (find-file-noselect
    (if lawlist-filename
      lawlist-filename
      ;; (ns-read-file-name "Select File:" "/Users/HOME/.0.data/" t nil nil)
      (read-file-name "Select File: " "~/" nil nil nil nil) ))))

;; The following is a `pinpoint` alternative to using the `display-buffer-alist`.
;; (display-buffer (get-buffer-create "foo") '(lawlist-display-buffer-pop-up-frame))
(setq display-buffer-alist '((".*" . (lawlist-display-buffer-pop-up-frame))))
(defun lawlist-display-buffer-pop-up-frame (buffer alist)
  (cond
    ;; condition # 1 -- either file-visiting or no-file buffers
    ((regexp-match-p org-buffer-regexp (buffer-name buffer))
      (if (get-frame "ORG")
          (switch-to-frame "ORG")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "ORG"))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (get-frame "ORG"))
          (progn
            (make-frame)
            (set-frame-name "ORG"))) )
      (set-window-buffer (get-largest-window) (buffer-name buffer))
      (select-window (get-buffer-window (buffer-name buffer))) )
    ;; condition # 2 -- either file-visiting or no-file buffers
    ((regexp-match-p main-buffer-regexp (buffer-name buffer))
      (if (get-frame "MAIN")
          (switch-to-frame "MAIN")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "MAIN"))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (get-frame "MAIN"))
          (progn
            (make-frame)
            (set-frame-name "MAIN"))) )
      (set-window-buffer (get-largest-window) (buffer-name buffer))
      (select-window (get-buffer-window (buffer-name buffer))) )
    ;; condition # 3 -- either file-visiting or no-file buffers
    ((regexp-match-p system-buffer-regexp (buffer-name buffer))
      (if (get-frame "SYSTEM")
          (switch-to-frame "SYSTEM")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "SYSTEM"))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (get-frame "SYSTEM"))
          (progn
            (make-frame)
            (set-frame-name "SYSTEM"))) )
      (set-window-buffer (get-largest-window) (buffer-name buffer))
      (select-window (get-buffer-window (buffer-name buffer))) )
    ;; condition # 4
    ;; display buffer in the existing frame
    ((regexp-match-p special-buffer-regexp (buffer-name buffer))
      (set-window-buffer (get-largest-window) (buffer-name buffer))
      (select-window (get-buffer-window (buffer-name buffer))) )
    ;; condition # 5
    ;; file-visiting buffers that do NOT match any pre-defined regexp
    ((and (not (regexp-match-p org-buffer-regexp (buffer-name buffer)))
          (not (regexp-match-p main-buffer-regexp (buffer-name buffer)))
          (not (regexp-match-p system-buffer-regexp (buffer-name buffer)))
          (not (regexp-match-p special-buffer-regexp (buffer-name buffer)))
          (buffer-file-name (get-buffer (buffer-name buffer))))
      (if (get-frame "MISCELLANEOUS")
          (switch-to-frame "MISCELLANEOUS")
        ;; If unnamed frame exists, then take control of it.
        (catch 'break (dolist (frame (frame-list))
          (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
            (throw 'break (progn
              (switch-to-frame (frame-parameter frame 'name))
              (set-frame-name "MISCELLANEOUS"))))))
        ;; If dolist found no unnamed frame, then create / name it.
        (if (not (get-frame "MISCELLANEOUS"))
          (progn
            (make-frame)
            (set-frame-name "MISCELLANEOUS"))))
      (set-window-buffer (get-largest-window) (buffer-name buffer))
      (select-window (get-buffer-window (buffer-name buffer))) )
    ;; condition # 6
    ;; default display for no-file-visiting buffers
    (t nil )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/kentaro/auto-save-buffers-enhanced
;; `regexp-match-p` function modified by @sds on stackoverflow
;; http://stackoverflow.com/questions/20343048/distinguishing-files-with-extensions-from-hidden-files-and-no-extensions
(defun regexp-match-p (regexps string)
  (and string
       (catch 'matched
         (let ((inhibit-changing-match-data t)) ; small optimization
           (dolist (regexp regexps)
             (when (string-match regexp string)
               (throw 'matched t)))))))

;; http://www.emacswiki.org/emacs/frame-fns.el
;; author:  Drew Adams
(defun get-frame-name (&optional frame)
  "Return the string that names FRAME (a frame).  Default is selected frame."
  (unless frame (setq frame (selected-frame)))
  (if (framep frame)
      (cdr (assq 'name (frame-parameters frame)))
    (error "Function `get-frame-name': Argument not a frame: `%s'" frame)))

;; http://www.emacswiki.org/emacs/frame-fns.el
;; author:  Drew Adams
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
