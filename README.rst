Copycat
=======

A translation of Melanie Mitchell's original Copycat project from Lisp to
Python.

To find the original information and source code for Copycat, see her
website: http://web.cecs.pdx.edu/~mm/

Status
------

image:: http://i.imgur.com/8Wgkt.png

1000 runs of abc -> abd => ijk -> ?

answer    totals    avgtmp
------    ------    ------
  ijl   847   86%     32
  jjk    94   10%     47
  ikl    18    2%     53
  ijd    11    1%     41
  jkk     5    1%     58
  djk     4    0%     52
  ijk     3    0%     52

12/13/09
An initial GUI using the pure python package 'pyglet' has been added.
For now, to use it you add --mode=opengl to the command line.

e.g., ./Copycat.py abc abd ijk 42 --mode=opengl

A more efficient and complete interface is being planned.

12/09/09
Copycat.py abc abd ijk 42 runs to completion with the answer ijl and a
final temperature of 30! This marks the first successful run. There is
still a lot of code to go through for accuracy, but this ensures that
we are at least on the right track.

12/07/09
Workspace strings have been completely revamped and enough random bugs
have been fixed to allow all types of codelets to be run now. There are
no longer any major hurdles left beyond continuing to go through method
by method to ensure accuracy.

12/04/09
Enough is working for bonds and groups to be successfully built with work
on Correspondences coming along. On some seeds, the program successfully
runs to completion (running an AnswerBuilder codelet), although the
answers aren't accurate yet. Last major update had at most 12 of 25
codelet types possibly running, and now it is more like 20 of 25. Work
on providing accuracy continues.

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

Issues
------

The entire project needs a revamp for consistency and style.
Having slipnodes in the global slipnet module level ruins enscapulation.
Similarly there is a problem with using the default global random instance.
Need to start thinking about multiplatform packaging.
Need to create both text and GUI tutorial/overview.
