# Copyright (c) 2007-2017 Joseph Hager.
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

# IDEAS:
# Add drawing routines for workspace structures

import string

import pyglet

class Letter(object):
    def __init__(self, letter, x, y, batch):
        self.letter = letter
        self.label = pyglet.text.Label(letter.name, "EraserDust", 36, x=x, y=y,
                                       color=(255,255,255, 100), batch=batch)

    def update(self, dt):
        if self.letter.is_changed:
            self.label.color = (255, 100, 100, 130)

class String(object):
    def __init__(self, string, x, y, batch):
        self.letters = []
        t = len(string.get_letters()) * 19
        new_x = x - t
        for letter in string.get_letters():
            self.letters.append(Letter(letter, new_x, y, batch))
            new_x += 40

    def update(self, dt):
        for letter in self.letters:
            letter.update(dt)

class Workspace(object):
    def __init__(self, workspace, x, y, w, h, batch):
        self.workspace = workspace
        self.batch = batch

        arrow = pyglet.resource.image("arrow.png")
        arrow.anchor_x = arrow.width / 2.0
        arrow.anchor_y = arrow.height / 2.0
        self.arrow1 = pyglet.sprite.Sprite(arrow, 512, 505, batch=self.batch)
        self.arrow1.opacity = 100
        self.arrow2 = pyglet.sprite.Sprite(arrow, 512, 390, batch=self.batch)
        self.arrow2.opacity = 100

        left_x = w / 4.0
        right_x = left_x * 3.0
        bot_y = y + h / 4.0
        top_y = y + 3 * h / 4.0 - 35

        self.answer_x = right_x
        self.answer_y = bot_y

        self.strings = [String(workspace.initial_string, left_x, top_y, self.batch),
                        String(workspace.modified_string, right_x, top_y, self.batch),
                        String(workspace.target_string, left_x, bot_y, self.batch)]

    def update(self, dt):
        if self.workspace.answer_string:
            self.strings.append(String(self.workspace.answer_string,
                                       self.answer_x, self.answer_y, self.batch))

        for string in self.strings:
            string.update(dt)
