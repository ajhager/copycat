#Copycat

A translation of Melanie Mitchell's original Copycat project from Lisp to
Python.

To find the original information and source code for Copycat, see her
[website](http://web.cecs.pdx.edu/~mm/). Any questions, comments or
issues should be directed to [me at github](http://github.com/ajhager/).

![Copycat GUI](http://i.imgur.com/lHMwn.png)

##Status

###02/14/11
Project updated to prepare for a new release.

###12/18/09
Phase 2 (the accuracy phase) is now complete. There may still be some subtle
bugs around, but hopefully they will be caught on the next pass for style
and consistency. The next phase will consist of fleshing out the rest of the
basic gui while attempting to remove any barriers to running multiple runs
in parallel. A stats module is being worked on so that results can be verified
with the data from the orginal copycat.

##Issues

*  The entire project needs a revamp for consistency and style.
*  Add and cleanup comments.
*  Missing tutorial/overview.
*  Need an independent rng if we move to concurrent runs.
*  Module for batch runs for statistics.
