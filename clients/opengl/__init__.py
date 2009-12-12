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

class Slipnode(object):
    def __init__(self, node, x, y):
        self.node = node
        self.image = pyglet.sprite.Sprite(square, x=x, y=y)
        self.image.opacity = 100
        self.label = pyglet.text.Label(self.node.name[:6], "EraserDust", 12,
                                       x=x, y=y-30, anchor_x="center")
        
    def draw(self):
        self.image.scale = self.node.activation * .0045 + .1
        if self.node.is_active():
            self.label.color = (255, 255, 255, 255)
        else:
            self.label.color = (255, 255, 255, 130)
        
        self.image.draw()
        self.label.draw()

class Letter(object):
    def __init__(self, letter, x, y):
        self.letter = letter
        self.label = pyglet.text.Label(letter.name, "EraserDust", 36, x=x, y=y,
                                       color=(255,255,255, 125))

    def draw(self):
        if self.letter.is_changed:
            self.label.color = (255, 100, 100, 130)
        else:
            self.label.color = (255, 255, 255, 130)

        self.label.draw()

class Window(pyglet.window.Window):
    def __init__(self, run):
        super(Window, self).__init__(1024, 600, caption="Copycat")
        self.clock = pyglet.clock.ClockDisplay()
        self.done = False
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

        self.run = run
        self.background = pyglet.resource.image("blackboard.png")

        self.nodes = []
        x, y = 0, 0
        z = 0
        for y in range(10):
            for x in range(10):
                if z >= len(self.run.slipnet.slipnodes):
                    break
                node = self.run.slipnet.slipnodes[z]
                self.nodes.append(Slipnode(node, x * 50 + 30, y * 50 + 40))
                z += 1

        self.letters = []
        x, y = 110, 510
        for letter in self.run.workspace.initial_string.get_letters():
            self.letters.append(Letter(letter, x, y))
            x += 50
        x += 100
        for letter in self.run.workspace.modified_string.get_letters():
            self.letters.append(Letter(letter, x, y))
            x += 50
        x, y = 110, 375
        for letter in self.run.workspace.target_string.get_letters():
            self.letters.append(Letter(letter, x, y))
            x += 50

        pyglet.clock.schedule(self.update)

    def update(self, dt):
        if self.done:
            return
        if self.run.workspace.answer_string:
            self.done = True
            x, y = 350, 375
            for letter in self.run.workspace.answer_string.get_letters():
                self.letters.append(Letter(letter, x, y))
                x += 50
        self.run.step()

    def on_draw(self):
        self.clear()

        self.background.blit(0, 0)
        for letter in self.letters:
            letter.draw()
        for node in self.nodes:
            node.draw()

        self.clock.draw()

class OpenglClient(pyglet.window.Window):
    def __init__(self, initial, modified, target, seed):
        run = Run(initial, modified, target, seed)
        window = Window(run)
        pyglet.app.run()

