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

### NOTE: This is just a quickly thrown together mockup.
# IDEAS:
#     center workspace string vertically with respect to each other
#     Add drawing routines for workspace structures
#     use a caching mechanism to optimize color/size changes.
#     make size and shape of each module perfectly customizable
#     Add dedicated scenes for each module that adds full detail
#     Add the ability to type in the problem and seed in client
#     Add play/pause/stop/dump buttons on the top left
#     Add icons to switch between modules on the top right
#     Add a module for doing bulk runs showing a graph of stats
#     Abstract out theme and add one more amenable to being used in a paper
#     Consider using audio as cues to building/breaking/chaos/order/pause/end
#     Break each renderer off into its own separte module.
#     Add arrows to indicate the flow of the analogy
#     graphs in dedicated views showing activites over time.
#     Consider dynamic tooltips that show details of the underlying object.

import random

import pyglet
from pyglet.gl import *
pyglet.resource.path.append("clients/opengl/data")
pyglet.resource.reindex()
pyglet.resource.add_font("erasdust.ttf")
square = pyglet.resource.image("square.png")
square.anchor_x = square.width / 2.
square.anchor_y = square.height / 2.

from copycat.run import Run

class Slipnet(object):
    def __init__(self, slipnet, x, y, w, h, batch):
        self.slipnet = slipnet
        self.nodes = []
        self.labels = []
        self.scales = {}

        self.batch = batch
        self.x = x
        self.y = y
        self.w = w
        self.h = h

        node_w = w / 10.0
        node_h = h / 6.0

        index = 0
        for node_y in range(6, 0, -1):
            for node_x in range(10):
                if index >= len(slipnet.slipnodes):
                    break
                node = slipnet.slipnodes[index]
                self.scales[node] = 0
                x = node_x * node_w + node_w / 2.0
                y = node_y * node_h - 5
                sprite = pyglet.sprite.Sprite(square, x=x, y=y, batch=self.batch)
                sprite.opacity = 100
                sprite.rotation = 10
                self.nodes.append(sprite)
                label = pyglet.text.Label(node.name[:6], "EraserDust", 12, x=x,
                                          y=y-30, anchor_x="center", batch=self.batch)
                self.labels.append(label)
                index += 1
        
    def update(self, dt):

        for image, label, node in zip(self.nodes, self.labels, self.slipnet.slipnodes):
            target = node.activation * .01
            self.scales[node] += (target - self.scales[node]) * dt
            image.scale = self.scales[node]

            c = self.scales[node] * 100
            if node.is_active():
                label.color = (255, 255, 255, int(c * 2))
            else:
                label.color = (200, 200, 200, 130)

            if node.clamp:
                image.color = (c * 2, c * 2.55, c * 2.55)
            else:
                image.color = (20, c * 1.9, c * 2.4)

class Workspace(object):
    def __init__(self, workspace, x, y, w, h, batch):
        self.workspace = workspace
        self.batch = batch
        self.x = x
        self.y = y
        self.w = w
        self.h = h

        self.letters = []
        self.wletters = []
        x, y = self.w / 2.0 - 100.0, self.h + self.h / 3.0 * 2.0
        for letter in reversed(workspace.initial_string.get_letters()):
            label = pyglet.text.Label(letter.name, "EraserDust", 36, x=x, y=y,
                                      color=(255,255,255, 125), batch=self.batch)
            self.letters.append(label)
            self.wletters.append(letter)
            x -= 50
        x, y = self.w / 2.0 + 70.0, self.h + self.h / 3.0 * 2.0
        for letter in workspace.modified_string.get_letters():
            label = pyglet.text.Label(letter.name, "EraserDust", 36, x=x, y=y,
                                      color=(255,255,255, 125), batch=self.batch)
            self.letters.append(label)
            self.wletters.append(letter)
            x += 50
        x, y = self.w / 2.0 - 100.0, self.h + self.h / 4.0
        for letter in reversed(workspace.target_string.get_letters()):
            label = pyglet.text.Label(letter.name, "EraserDust", 36, x=x, y=y,
                                      color=(255,255,255, 125), batch=self.batch)
            self.letters.append(label)
            self.wletters.append(letter)
            x -= 50

    def update(self, dt):
        if self.workspace.answer_string:
            x, y = self.w / 2.0 + 70.0, self.h + self.h / 4.0
            for letter in self.workspace.answer_string.get_letters():
                label = pyglet.text.Label(letter.name, "EraserDust", 36, x=x, y=y,
                                          color=(255,255,255, 125), batch=self.batch)
                self.letters.append(label)
                self.wletters.append(letter)
                x += 50
                
        for label, letter in zip(self.letters, self.wletters):
            if letter.is_changed:
                label.color = (255, 100, 100, 130)
            else:
                label.color = (255, 255, 255, 130)

class Coderack(object):
    def __init__(self, coderack, x, y, w, h, batch):
        self.coderack = coderack
        self.x = x
        self.y = y
        self.w = w
        self.h = h

        self.batch = batch

        names = ['BondBottomUpScout', 'BondBuilder', 'BondStrengthTester',
                 'BondTopDownCategoryScout', 'BondTopDownDirectionScout',
                 'Breaker', 'CorrespondenceBottomUpScout',
                 'CorrespondenceBuilder', 'CorrespondenceImportantObjectScout',
                 'CorrespondenceStrengthTester', 'DescriptionBottomUpScout',
                 'DescriptionBuilder', 'DescriptionStrengthTester',
                 'DescriptionTopDownScout', 'GroupBuilder',
                 'GroupStrengthTester', 'GroupTopDownCategoryScout',
                 'GroupTopDownDirectionScout', 'GroupWholeStringScout',
                 'ReplacementFinder', 'RuleBuilder', 'RuleScout',
                 'RuleStrengthTester', 'RuleTranslator']

        vnames = ['Bond Scout', 'Bond Builder', 'Bond Strength Tester',
                  'Bond Category Scout', 'Bond Direction Scout',
                  'Breaker', 'Correspondence Scout',
                  'Correspondence Builder', 'Correspondence Importance Scout',
                  'Correspondence Strength Tester', 'Description Scout',
                  'Description Builder', 'Description Strength Tester',
                  'Description Top Down Scout', 'Group Builder',
                  'Group Strength Tester', 'Group Category Scout',
                  'Group Direction Scout', 'Group Whole String Scout',
                  'Replacement Finder', 'Rule Builder', 'Rule Scout',
                  'Rule Strenth Tester', 'Rule Translator']
        
        self.codelets = []
        self.counts = []
        x = self.x + 43
        y = self.h - 10
        z = 0
        for name, vname in zip(names, vnames):
            if z == 12:
                x += 270
                y = self.h - 10
            label = pyglet.text.Label(vname, "EraserDust", 12, x=x, y=y,
                                      color=(255,255,255, 130), batch=self.batch)
            label2 = pyglet.text.Label("00", "EraserDust", 12, x=x - 25, y=y,
                                       color=(255,255,255, 130), batch=self.batch)
            label.name = name
            self.codelets.append(label)
            self.counts.append(label2)
            y -= 25
            z += 1

    def update(self, dt):
        from collections import defaultdict as dd
        counts = dd(int)
        for codelet in self.coderack.codelets():
            counts[codelet.__class__.__name__] += 1

        for name, number in zip(self.codelets, self.counts):
            if self.coderack.last_chosen.__class__.__name__ == name.name:
                name.color = (200, 255, 180, 200)
            else:
                alpha = int(name.color[3] - (name.color[3] * dt))
                if alpha >= 80:
                    name.color = (200, 255, 180, int(alpha))
            num = counts[name.name]
            number.text = str(num)
            number.color = (255, 255, 255, min(200, int(num / 2.0 * 80)))

class Timer(object):
    def __init__(self, run, x, y, batch):
        self.run = run

        self.batch = batch
        self.numl = pyglet.text.Label("", "EraserDust", 12, x=x, y=y,
                                      color=(255,255,255, 125), batch=self.batch,
                                      halign="center", anchor_x="center")

    def update(self, dt):
        self.numl.text = str(self.run.coderack.time / self.run.timestep)

class Window(pyglet.window.Window):
    def __init__(self, run):
        super(Window, self).__init__(1024, 600, caption="Copycat", vsync=False)
        self.clock = pyglet.clock.ClockDisplay()
        self.done = False
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

        self.run = run
        
        background = pyglet.resource.image("blackboard.png")
        self.background = pyglet.sprite.Sprite(background)
        self.saved_temp = 0
        self.batch = pyglet.graphics.Batch()

        self.timer = Timer(self.run, 512, 580, self.batch)
        self.slipnet = Slipnet(self.run.slipnet, 0, 0, 512, 300, self.batch)
        self.coderack = Coderack(self.run.coderack, 512, 0, 512, 300, self.batch)
        self.workspace = Workspace(self.run.workspace, 0, 300, 1024, 300, self.batch)

        pyglet.clock.schedule(self.update)

    def update(self, dt):
        if self.done:
            return
        self.slipnet.update(dt)
        self.coderack.update(dt)
        self.workspace.update(dt)
        if self.run.workspace.answer_string:
            self.done = True
        self.timer.update(dt)
        target_temp = self.run.workspace.temperature * 2.55
        self.saved_temp += (target_temp - self.saved_temp) * dt
        self.background.color = (240,
                                 255 - self.saved_temp / 1.5,
                                 255 - self.saved_temp / 1.25)
        self.run.step()

    def on_draw(self):
        self.clear()
        self.background.draw()
        self.batch.draw()
        self.clock.draw()

class OpenglClient(pyglet.window.Window):
    def __init__(self, initial, modified, target, seed):
        run = Run(initial, modified, target, seed)
        window = Window(run)
        pyglet.app.run()

