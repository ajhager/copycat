Copycat
=======

A translation of Melanie Mitchell's original Copycat project from Lisp to
Python.

To find the original information and source code for Copycat, see her
website: http://web.cecs.pdx.edu/~mm/

Status
------

11/29/09
The original copycat source code has been checked into the mmcat folder.
I have started a function by function accuracy audit marked by the commit
messages of the style "Acc: mmcat-function | copycat_function". mmcat code
is replaced by the commit message when that function is complete. Python
code should be more accurate and clean, with proper comments. 

07/01/09
The first phase is now complete.  It consisted of discovering the code
by doing a rough line by line conversion to Python.  The intent was to get
a better idea of the overall structure of the project and the dependencies
of each module.

Phase two is now starting. The focus will be on molding the rough cut into
a faithful and working version of the original, albeit without a graphical
user interface.

The focus of phase three will be on creating an intuitive interface for not
only viewing a running Copycat session, but also exploring the status and
underlying code of each active object. Students of the program will be able
to see and understand exactly what is going on behind the scenes that allows
Copycat to work.
