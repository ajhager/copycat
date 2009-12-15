#Copycat

A translation of Melanie Mitchell's original Copycat project from Lisp to
Python.

To find the original information and source code for Copycat, see her
[website](http://web.cecs.pdx.edu/~mm/).

![Copycat GUI](http://imgur.com/REtAE.png)

##Status

###12/14/09
The GUI is able to accurately show the state and dynamic of the slipnet,
coderack, temperature and timestep. Only the letter strings are shown of
the workspace, and whether or not a letter has changed. You can also start
and pause the simulation using the mouse or keyboard. Work is underway to
create detailed screens for each module showing a deeper and more complete
view.

###12/13/09
An initial GUI using the pure python package 'pyglet' has been added.
For now, to use it you add --mode=opengl to the command line.

e.g., ./Copycat.py abc abd ijk 42 --mode=opengl

A more efficient and complete interface is being planned.

###12/09/09

Copycat.py abc abd ijk 42 runs to completion with the answer ijl and a
final temperature of 30! This marks the first successful run. There is
still a lot of code to go through for accuracy, but this ensures that
we are at least on the right track.

###12/07/09

Workspace strings have been completely revamped and enough random bugs
have been fixed to allow all types of codelets to be run now. There are
no longer any major hurdles left beyond continuing to go through method
by method to ensure accuracy.

###12/04/09

Enough is working for bonds and groups to be successfully built with work
on Correspondences coming along. On some seeds, the program successfully
runs to completion (running an AnswerBuilder codelet), although the
answers aren't accurate yet. Last major update had at most 12 of 25
codelet types possibly running, and now it is more like 20 of 25. Work
on providing accuracy continues.

###11/29/09

The original copycat source code has been checked into the mmcat folder.
I have started a function by function accuracy audit marked by the commit
messages of the style "Acc: mmcat-function | copycat_function". mmcat code
is replaced by the commit message when that function is complete. Python
code should be more accurate and clean, with proper comments. 

###07/01/09

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

##Issues

*  The entire project needs a revamp for consistency and style.
*  Having slipnodes in the global slipnet module level ruins enscapulation.
*  Similarly there is a problem with using the default global random instance.
*  Need to start thinking about multiplatform packaging.
*  Missing both text and GUI tutorial/overview.
