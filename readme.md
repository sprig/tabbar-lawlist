TABBAR ENHANCEMENTS
=================

This is a work in progress (with ongoing changes) that is being developed with current versions of Emacs Trunk.

The most recent version of Tabbar can be found here:  http://www.emacswiki.org/emacs/TabBarMode

The file `init-tabbar.el` has been configured to use five frames (SYSTEM; MAIN; WANDERLUST; ORG; MISCELLANEOUS).  Buffers may be associated with frames through the `lawlist-buffer-list` -- buffers are added with  `associate-current-buffer`, and buffers are removed with `disassociate-current-buffer`.  The function `lawlist-buffer-list-reset` wipes the entire slate clean as to the selected frame.

The function `lawlist-find-file` and the functions for `display-buffer-alist`, along with a modified version of `ns-find-file`, create / locate frames with specific names based upon regexp of buffer name or file name.

![screenshot](http://www.lawlist.com/images/frames_tabbar.png)
