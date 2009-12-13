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
#     Make the letters come from the center outward taking up the entire upper half.
#     Make the slipnet circles lerp from size to size.
#     Add coderack representation on the bottom right side.
#     Add drawing routines for workspace structures
#     use color to indicate age and urgency for codelets.

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
    def __init__(self, slipnet, x, y, w, h):
        self.slipnet = slipnet
        self.nodes = []
        self.labels = []

        self.batch = pyglet.graphics.Batch()
        self.x = x
        self.y = y
        self.w = w
        self.h = h

        node_w = w / 10.0
        node_h = h / 6.0

        index = 0
        for node_y in range(6):
            for node_x in range(10):
                if index >= len(slipnet.slipnodes):
                    break
                node = slipnet.slipnodes[index]
                x = node_x * node_w + node_w / 2.0
                y = node_y * node_h + node_h / 2.0 + 15
                sprite = pyglet.sprite.Sprite(square, x=x, y=y, batch=self.batch)
                sprite.opacity = 100
                self.nodes.append(sprite)
                label = pyglet.text.Label(node.name[:6], "EraserDust", 12, x=x,
                                          y=y-30, anchor_x="center", batch=self.batch)
                self.labels.append(label)
                index += 1
        
    def update(self, dt):

        for image, label, node in zip(self.nodes, self.labels, self.slipnet.slipnodes):
            image.scale = node.activation * .01 + .1

            if node.is_active():
                label.color = (255, 255, 255, 255)
            else:
                label.color = (200, 200, 200, 130)

            if node.clamp:
                image.color = (160, 200, 255)
            else:
                image.color = (255, 255, 255)

    def draw(self):
        self.batch.draw()

class Workspace(object):
    def __init__(self, workspace, x, y, w, h):
        self.workspace = workspace
        self.batch = pyglet.graphics.Batch()
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

    def draw(self):
        self.batch.draw()

class Window(pyglet.window.Window):
    def __init__(self, run):
        super(Window, self).__init__(1024, 600, caption="Copycat", vsync=False)
        self.clock = pyglet.clock.ClockDisplay()
        self.done = False
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

        self.run = run
        self.background = pyglet.resource.image("blackboard.png")

        self.slipnet = Slipnet(self.run.slipnet, 0, 0, 512, 300)
        self.workspace = Workspace(self.run.workspace, 0, 300, 1024, 300)

        pyglet.clock.schedule(self.update)

    def update(self, dt):
        if self.done:
            return
        self.workspace.update(dt)
        self.slipnet.update(dt)
        if self.run.workspace.answer_string:
            self.done = True
        self.run.step()

    def on_draw(self):
        self.clear()
        self.background.blit(0, 0)
        self.workspace.draw()
        self.slipnet.draw()
        self.clock.draw()

class OpenglClient(pyglet.window.Window):
    def __init__(self, initial, modified, target, seed):
        run = Run(initial, modified, target, seed)
        window = Window(run)
        pyglet.app.run()

