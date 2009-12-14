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
            
        self.update(0)

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
