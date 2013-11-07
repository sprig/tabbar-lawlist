TABBAR ENHANCEMENTS
=================

This is a work in progress (with ongoing changes) that is being developed with current versions of Emacs Trunk.  The tabbar-lawlist repository contains various configurations and functions for enhancing the use of Tabbar and Frame-Bufs with Emacs.

The most recent version of tabbar can be found here:  http://www.emacswiki.org/emacs/TabBarMode

The file `init-tabbar.el` has been configured to use five frames (SYSTEM; MAIN; WANDERLUST; ORG; MISCELLANEOUS).

There are two methods for adding and removing buffers from the `frame-bufs-buffer-list`.

* The first method involves activating the `*BUFFER LIST*` menu buffer with `M-x frame-bufs RET`.  Buffers are added with `frame-bufs-make-associated`, and buffers are removed with `frame-bufs-make-non-associated`.  Once the selections are made, the function `frame-bufs-menu-execute` is used to solidify those choices.
 
* The second option is used when *not* using the `frame-bufs` buffer menu window.  Buffers are added with `associate-current-buffer`, and buffers are removed with `disassociate-current-buffer`.  The function `lawlist-frame-bufs-reset` wipes the entire slate clean as to the selected frame.

I have modified `frame-bufs.el` (version 1.92) and `buff-menu.el` (from Emacs version 23.4) by renaming functions and variables so that the two files work in harmony, without conflicting with a current version of `buff-menu.el` that gets hard-coded into the Emacs executable during the build process.

The function `lawlist-find-file` and the functions for `display-buffer-alist`, along with a modified version of `ns-find-file`, create / locate frames with specific names based upon file type / file name / buffer name and associate them if `frame-bufs-mode` is active.

![screenshot](http://www.lawlist.com/images/frames_tabbar.png)
