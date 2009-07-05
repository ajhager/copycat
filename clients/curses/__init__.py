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
import time

from copycat.run import Run
from copycat.coderack.codelets import *

codelet_types = [AnswerBuilder, BondBottomUpScout, BondBuilder,
                 BondStrengthTester, BondTopDownCategoryScout,
                 BondTopDownDirectionScout, Breaker,
                 CorrespondenceBottomUpScout, CorrespondenceBuilder,
                 CorrespondenceImportantObjectScout,
                 CorrespondenceStrengthTester, DescriptionBottomUpScout,
                 DescriptionBuilder, DescriptionStrengthTester,
                 DescriptionTopDownScout, GroupBuilder, GroupStrengthTester,
                 GroupTopDownCategoryScout, GroupTopDownDirectionScout,
                 GroupWholeStringScout, ReplacementFinder, RuleBuilder,
                 RuleScout, RuleStrengthTester, RuleTranslator]

class CoderackStatistics:
    '''
    How many of each codelet each step.
    which bin they are in
    how many of each codelet for the run
    list of codelets that were run (by doing diffs)
    '''
    def __init__(self, coderack):
        self.coderack = coderack
        self.total_seen = self.empty_dict()
        self.last_step = self.empty_dict()

    def empty_dict(self):
        d = {}
        for codelet_type in codelet_types:
            d[codelet_type] = 0
        return d

    def update(self):
        this_step = self.empty_dict()
        for codelet in self.coderack.codelets():
            this_step[type(codelet)] += 1
        for codelet_type in codelet_types:
            change = this_step[codelet_type] - self.last_step[codelet_type]
            if change > 0:
                self.total_seen[codelet_type] += change
        self.last_step = this_step

class CursesClient(object):
    def __init__(self, initial, modified, target, seed):
        self.speed = 0
        self.run = Run(initial, modified, target, seed)

        self.coderack_stats = CoderackStatistics(self.run.coderack)

        curses.wrapper(self.main)

    def init_curses(self, screen):
        self.screen = screen
        # Turn off blinking cursor.
        curses.curs_set(0)

        # Setup colors.
        colors = [curses.COLOR_RED, curses.COLOR_GREEN, curses.COLOR_YELLOW,
                  curses.COLOR_BLUE, curses.COLOR_MAGENTA, curses.COLOR_CYAN]
        for pair, color in zip(range(1, 7), colors):
            curses.init_pair(pair, color, curses.COLOR_BLACK)
        self.RED = curses.color_pair(1)
        self.GREEN = curses.color_pair(2)
        self.YELLOW = curses.color_pair(3)
        self.BLUE = curses.color_pair(4)
        self.MAGENTA = curses.color_pair(5)
        self.CYAN = curses.color_pair(6)

    def show(self, string, position=None, color=0):
        if position:
            self.screen.addstr(position[0], position[1], string, color)
        else:
            self.screen.addstr(string, color)

    def display_coderack(self):
        totals = self.coderack_stats.total_seen.items()
        totals.sort(key=lambda x: x[1])
        total = sum(self.coderack_stats.total_seen.values())
        current = self.coderack_stats.last_step
        total_current = self.run.coderack.number_of_codelets()
        for (t, v) , y in zip(totals, range(len(totals))):
            now = current[t]
            self.show('%40s: %5s %5s' % (t.__name__, v, now), (y+2, 0))
        self.show('%40s: %5s %5s' % ('Totals', total, total_current), (28, 0),
                 self.GREEN)
        self.show('%40s: %5s' % ('Temperature', self.run.workspace.temperature),
                                 (29, 0), self.RED)

    def main(self, screen):
        self.init_curses(screen)

        while not self.run.workspace.answer_string:
            #self.screen.clear()
            self.run.step()
            self.coderack_stats.update()
            self.display_coderack()
            self.screen.refresh()
            time.sleep(self.speed)
        print self.run.workspace.answer_string
