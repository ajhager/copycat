# Copyright (c) 2007-2009 Joseph Hager.
#
# Copycat is free software; you can redistribute it and/or modify
# it under the terms of version 2 of the GNU General Public License,
# as published by the Free Software Foundation.
# 
# Copycat is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Copycat; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
# 02110-1301, USA.

import curses

from copycat.run import Run

class CursesClient(object):
    def __init__(self, initial, modified, target, seed):
        curses.wrapper(self.main)

    def main(self, screen):
        # Turn off blinking cursor.
        curses.curs_set(0)

        # Set up colors.
        colors = [curses.COLOR_RED, curses.COLOR_GREEN, curses.COLOR_YELLOW,
                  curses.COLOR_BLUE, curses.COLOR_MAGENTA, curses.COLOR_CYAN]
        for pair, color in zip(range(1, 7), colors):
            curses.init_pair(pair, color, curses.COLOR_BLACK)
        RED = curses.color_pair(1)
        GREEN = curses.color_pair(2)
        YELLOW = curses.color_pair(3)
        BLUE = curses.color_pair(4)
        MAGENTA = curses.color_pair(5)
        CYAN = curses.color_pair(6)


        screen.addstr(15, 25, 'Copycat', BLUE)
        screen.refresh()

        while True:
            pass

        while not run.workspace.answer_string:
            run.step()
        print str(run.workspace.answer_string)
