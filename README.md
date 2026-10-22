# Copycat

A translation of Melanie Mitchell's original Copycat project from Lisp to
Python. To find the original information and source code for Copycat, see her
[website](http://web.cecs.pdx.edu/~mm/).

The translation of the core logic is 100% complete. The engine needs only Python 3 (tested on 3.12); the GUI needs pyglet 2 (`pip install pyglet`). You can run a problem on the command line:

```
python3 Copycat.py --quiet abc abd ijk --seed 1     # headless; the same seed gives the same run
python3 Copycat.py abc abd xyz --seed 36             # the GUI (press Space to run)
```

![Copycat GUI](http://i.imgur.com/lHMwn.png)
