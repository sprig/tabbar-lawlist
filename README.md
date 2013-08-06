TABBAR ENHANCEMENTS
=================

This is a work in progress (with ongoing changes), which contains various configurations and functions for enhancing the use of Tabbar 2.0 with Emacs.

Tabbar 2.0 has been modified by hard-coding the colors and fonts of the tabs.

The file `init-tabbar.el` has been configured to use four frames (SYSTEM; MAIN; WANDERLUST; ORG) with corresponding tab groups (system; main; wanderlust; org).  The function `tabbar-choice` has an option to switch on/off frame-bufs-mode -- if frame-bufs-mode is enabled, Tabbar will group tabs on a per frame basis on-the-fly based upon the functions `frame-bufs-make-associated` and `frame-bufs-make-non-associated` that is used after the function `frame-bufs` brings up the buffer menu window.

I have modified frame-bufs.el (version 1.92)and buff-menu.el (from Emacs version 23.4) by renaming functions and variables so that the two files work in harmony, without conflicting with a current version of buff-menu.el that gets hard-coded into the Emacs executable during the build process.

![screenshot](http://www.lawlist.com/images/frames-tabgroup.png)
