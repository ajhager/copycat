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


import pyglet

class Window(pyglet.window.Window):
    def __init__(self, run, stats):
        super(Window, self).__init__(800, 600)
        self.run = run
        self.coderack_stats = stats
        pyglet.clock.schedule(self.update)
        pyglet.gl.glClearColor(40/255.,44/255.,49/255.,1)
        self.clock = pyglet.clock.ClockDisplay()

        self.coderack = pyglet.text.document.FormattedDocument(" ")
        self.coderack.set_style(0, 2, {'color':(92,141,152,255), 'font_name': "Monaco",
                                       'font_size': 10, 'bold': True})
        self.layout = pyglet.text.layout.TextLayout(self.coderack, 800, 600,multiline=True)
        self.layout.x = 100
        self.layout.y = -60

    def update(self, dt):
        self.run.step()
        self.coderack_stats.update()
        self.draw_coderack()

    def draw_coderack(self):
        text ='%s: %s' % ('Temperature', self.run.workspace.temperature)
        self.set_caption(text)
        text = ""
        totals = self.coderack_stats.total_seen.items()
        totals.sort(key=lambda x: x[1])
        total = sum(self.coderack_stats.total_seen.values())
        current = self.coderack_stats.last_step
        total_current = len(self.run.coderack.codelets())
        for (t, v) , y in zip(totals, range(len(totals))):
            now = current[t]
            text += "%40s: %5s %5s\n" % (t.__name__, v, now)
        text += '\n%40s: %5s %5s' % ('Totals', total, total_current)

        self.coderack.text = text
 
    def on_draw(self):
        self.clear()
        self.layout.draw()
        self.clock.draw()

class OpenglClient(pyglet.window.Window):
    def __init__(self, initial, modified, target, seed):
        run = Run(initial, modified, target, seed)
        coderack_stats = CoderackStatistics(run.coderack)
        window = Window(run, coderack_stats)
        pyglet.app.run()

