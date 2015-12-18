;;; Commentary:

;; This mode acts as a graphical user interface to GDB.  You can interact with
;; GDB through the GUD buffer in the usual way, but there are also further
;; buffers which control the execution and describe the state of your program.
;; It separates the input/output of your program from that of GDB and displays
;; expressions and their current values in their own buffers.  It also uses
;; features of Emacs 21 such as the fringe/display margin for breakpoints, and
;; the toolbar (see the GDB Graphical Interface section in the Emacs info
;; manual).

;; M-x gdb will start the debugger.

;; This file uses GDB/MI as the primary interface to GDB.  It is still under
;; development and is part of a process to migrate Emacs from annotations (as
;; used in gdb-ui.el) to GDB/MI.  It runs gdb with GDB/MI (-interp=mi) and
;; access CLI using "-interpreter-exec console cli-command".  This code works
;; without gdb-ui.el and uses MI tokens instead of queues. Eventually MI
;; should be asynchronous.

;; This mode will PARTLY WORK WITH RECENT GDB RELEASES (status in modeline
;; doesn't update properly when execution commands are issued from GUD buffer)
;; and WORKS BEST when GDB runs asynchronously: maint set linux-async on.
;;
;; You need the DEVELOPMENT VERSION of GDB 7.0 for this code to work.

;; This file replaces gdb-ui.el and is for development with GDB.  Use the
;; release branch of Emacs 22 for the latest version of gdb-ui.el.

;; Windows Platforms:

;; If you are using Emacs and GDB on Windows you will need to flush the buffer
;; explicitly in your program if you want timely display of I/O in Emacs.
;; Alternatively you can make the output stream unbuffered, for example, by
;; using a macro:

;;           #ifdef UNBUFFERED
;;	     setvbuf (stdout, (char *) NULL, _IONBF, 0);
;;	     #endif

;; and compiling with -DUNBUFFERED while debugging.

;; If you are using Cygwin GDB and find that the source is not being displayed
;; in Emacs when you step through it, possible solutions are to:

;;   1) Use Cygwin X Windows and Cygwin Emacs.
;;        (Since 22.1 Emacs builds under Cygwin.)
;;   2) Use MinGW GDB instead.
;;   3) Use cygwin-mount.el

