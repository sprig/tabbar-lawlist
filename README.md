TABBAR ENHANCEMENTS
=================

This is a work in progress (with ongoing changes) that is being developed with current versions of Emacs Trunk.  The tabbar-lawlist repository contains various configurations and functions for enhancing the use of Tabbar and Frame-Bufs with Emacs.

A modified version of Tabbar 2.0 is included within this repository, except the *.tiff files in the original source.

The file `init-tabbar.el` has been configured to use four frames (SYSTEM; MAIN; WANDERLUST; ORG) with corresponding tab groups (system; main; wanderlust; org).  The function `tabbar-choice` has an option to enable frame-bufs-mode, and options to choose different tabbar groupings.  If `frame-bufs-mode` is enabled, Tabbar will group tabs on a per frame basis on-the-fly based upon the functions `frame-bufs-make-associated` and `frame-bufs-make-non-associated` that can be used once the function `frame-bufs` brings up the buffer menu window.

I have modified frame-bufs.el (version 1.92)and buff-menu.el (from Emacs version 23.4) by renaming functions and variables so that the two files work in harmony, without conflicting with a current version of buff-menu.el that gets hard-coded into the Emacs executable during the build process.

![screenshot](http://www.lawlist.com/images/frames-tabgroup.png)
