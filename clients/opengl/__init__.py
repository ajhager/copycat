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
# Abstraction of check a value for change, if so modify gui (color, scale, etc) 
# Add dedicated scenes for each module that adds full detail
# graphs in dedicated views showing activites over time.
# Add icons to switch between modules on the top right
# Add a module for doing bulk runs showing a graph of stats
# Add the ability to type in the problem and seed in client

import pyglet
from pyglet.gl import *
pyglet.resource.path.append("clients/opengl/data")
pyglet.resource.reindex()
pyglet.resource.add_font("erasdust.ttf")

from copycat.run import Run

from coderack import Coderack
from slipnet import Slipnet
from workspace import Workspace

class Button(object):
    """Make this function as a normal os button would.
    
    highlight when hovered.
    
    make the keybinding settable
    """
    def __init__(self, image, x, y, callback, batch):
        self.sprite = pyglet.sprite.Sprite(image, x=x, y=y, batch=batch)
        self.sprite.opacity = 170
        self.pressed = False
        self.hovered = False
        self.callback = callback

    def update(self, dt):
        pass

    def contains(self, x, y):
        my_x = self.sprite.x - self.sprite.width / 2.0
        my_y = self.sprite.y - self.sprite.height / 2.0
        my_w = my_x + self.sprite.width
        my_h = my_y + self.sprite.height
        return all([x >= my_x, x <= my_w, y >= my_y, y <= my_h])

    def on_mouse_press(self, x, y, button, modifiers):
        if self.contains(x, y):
            self.sprite.opacity = 100
            self.pressed = True

    def on_mouse_release(self, x, y, button, modifiers):
        if self.contains(x, y) and self.pressed:
            self.callback()
        self.pressed = False
        self.sprite.opacity = 170

    def on_key_press(self, symbol, modifiers):
        if symbol == pyglet.window.key.SPACE:
            self.callback()
        
class Window(pyglet.window.Window):
    """The main window keeps track of what scene is currently being viewed,
    manages the gui elements that are always on screen, and takes care of
    updating the simulation."""

    def __init__(self, run):
        super(Window, self).__init__(1024, 600, caption="Copycat", vsync=False)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

        self.clock = pyglet.clock.ClockDisplay()
        self.show_fps = False

        self.done = False
        self.time = 0
        self.speed = 20
        self.playing = False

        self.run = run
        
        background = pyglet.resource.image("blackboard.png")
        self.background = pyglet.sprite.Sprite(background)
        self.saved_temp = 0
        self.batch = pyglet.graphics.Batch()

        self.play = pyglet.resource.image("play.png")
        self.pause = pyglet.resource.image("pause.png")
        self.play.anchor_x = self.play.width / 2.0
        self.play.anchor_y = self.play.height / 2.0
        self.pause.anchor_x = self.pause.width / 2.0
        self.pause.anchor_y = self.pause.height / 2.0
        self.button = Button(self.play, 30, 580,
                             self.on_play_button, self.batch)

        self.timer = pyglet.text.Label("0", "EraserDust", 14, x=512, y=575,
                                       color=(255,255,255, 125), batch=self.batch,
                                       halign="center", anchor_x="center")

        self.rule = pyglet.text.Label("", "EraserDust", 16, x=512, y=445,
                                      color=(255,255,255, 190), batch=self.batch,
                                      halign="center", anchor_x="center")

        self.slipnet = Slipnet(self.run.slipnet, 0, 0, 512, 300, self.batch)
        self.coderack = Coderack(self.run.coderack, 512, 0, 512, 300, self.batch)
        self.workspace = Workspace(self.run.workspace, 0, 300, 1024, 300, self.batch)

        pyglet.clock.schedule(self.update)

    def on_key_press(self, symbol, modifiers):
        if symbol == pyglet.window.key.ESCAPE:
            pyglet.app.exit()
        elif symbol == pyglet.window.key.F and modifiers == pyglet.window.key.MOD_CTRL:
            self.show_fps = not self.show_fps
        elif symbol == pyglet.window.key.SPACE:
            self.button.on_key_press(symbol, modifiers)
    
    def on_mouse_press(self, x, y, button, modifiers):
        self.button.on_mouse_press(x, y, button, modifiers)
        
    def on_mouse_release(self, x, y, button, modifiers):
        self.button.on_mouse_release(x, y, button, modifiers)

    def on_play_button(self):
        self.playing = not self.playing
        if self.playing:
            self.button.sprite.image = self.pause
        else:
            self.button.sprite.image = self.play

    def update(self, dt):
        self.button.update(dt) 

        if self.done or not self.playing:
            return

        # Update each graphical module.
        self.slipnet.update(dt)
        self.coderack.update(dt)
        self.workspace.update(dt)

        # Check for completion.
        if self.run.workspace.answer_string:
            self.rule.text = self.run.workspace.rule.to_string()
            self.done = True

        # Update the timestep display.
        self.timer.text = str(self.run.coderack.time / self.run.timestep)

        # Update the temperature display.
        target_temp = self.run.workspace.temperature * 2.55
        self.saved_temp += (target_temp - self.saved_temp) * dt
        self.background.color = (240,
                                 255 - self.saved_temp / 1.4,
                                 255 - self.saved_temp / 1.15)
        
        # Update the simulation at the given speed.
        self.time += dt
        if self.time >= 1.0 / self.speed:
            self.run.step()
            self.time = 0

    def on_draw(self):
        self.clear()
        self.background.draw()
        self.batch.draw()
        if self.show_fps:
            self.clock.draw()

class OpenglClient(pyglet.window.Window):
    def __init__(self, initial, modified, target, seed):
        run = Run(initial, modified, target, seed)
        window = Window(run)
        pyglet.app.run()
