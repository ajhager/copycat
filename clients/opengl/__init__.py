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

import pyglet
from pyglet.gl import *
pyglet.resource.path.append("clients/opengl/data")
pyglet.resource.reindex()
pyglet.resource.add_font("erasdust.ttf")

from copycat.run import Run

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

        
        self.letters = []
        x, y = 80, 525
        for letter in self.run.workspace.initial_string.get_letters():
            self.letters.append(Letter(letter, x, y))
            x += 50
        x += 100
        for letter in self.run.workspace.modified_string.get_letters():
            self.letters.append(Letter(letter, x, y))
            x += 50
        x, y = 80, 400
        for letter in self.run.workspace.target_string.get_letters():
            self.letters.append(Letter(letter, x, y))
            x += 50

        pyglet.clock.schedule(self.update)

    def update(self, dt):
        if self.done:
            return
        if self.run.workspace.answer_string:
            self.done = True
            x, y = 330, 400
            for letter in self.run.workspace.answer_string.get_letters():
                self.letters.append(Letter(letter, x, y))
                x += 50
        self.run.step()

    def on_draw(self):
        self.clear()

        self.background.blit(0, 0)
        for letter in self.letters:
            letter.draw()

        self.clock.draw()

class OpenglClient(pyglet.window.Window):
    def __init__(self, initial, modified, target, seed):
        run = Run(initial, modified, target, seed)
        window = Window(run)
        pyglet.app.run()

