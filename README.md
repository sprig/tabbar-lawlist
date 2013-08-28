TABBAR ENHANCEMENTS
=================

This is a work in progress (with ongoing changes) that is being developed with current versions of Emacs Trunk.  The tabbar-lawlist repository contains various configurations and functions for enhancing the use of Tabbar and Frame-Bufs with Emacs.

A slightly modified version of Tabbar 2.0 is included within this repository, except the *.tiff files in the original source.  The main changes are hard-coded fonts for the appearance of the tabs.  Alternate functions are set forth within init-tabbar.el, so as not to substantially alter tabbar.el itself.

The file `init-tabbar.el` has been configured to use four frames (SYSTEM; MAIN; WANDERLUST; ORG) with corresponding tab groups (system; main; wanderlust; org).  The function `tabbar-choice` has an option to enable frame-bufs-mode, and options to choose different tabbar groupings.  If `frame-bufs-mode` is enabled, Tabbar will group tabs on a per frame basis on-the-fly based upon the functions `frame-bufs-make-associated` and `frame-bufs-make-non-associated` that can be used once the function `frame-bufs` brings up the buffer menu window.  When not using the `frame-bufs` buffer menu window, the functions `associate-current-buffer` and a slightly modified version of `frame-bufs-dismiss-buffer` will do the same thing -- presently linked to defined keys in `init-tabbar.el`.  The function `lawlist-frame-bufs-reset` wipes the entire slate clean as to the selected frame.

I have modified frame-bufs.el (version 1.92) and buff-menu.el (from Emacs version 23.4) by renaming functions and variables so that the two files work in harmony, without conflicting with a current version of buff-menu.el that gets hard-coded into the Emacs executable during the build process.

The function `lawlist-find-file` and the functions for `display-buffer-alist`, along with a modified version of `ns-find-file`, create / locate frames with specific names based upon file type / file name / buffer name and associate them if `frame-bufs-mode` is active.

![screenshot](http://www.lawlist.com/images/frames_tabbar.png)
