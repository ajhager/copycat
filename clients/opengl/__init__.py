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
#     Add arrows to indicate the flow of the analogy
#     graphs in dedicated views showing activites over time.
#     Consider dynamic tooltips that show details of the underlying object.

import pyglet
from pyglet.gl import *
pyglet.resource.path.append("clients/opengl/data")
pyglet.resource.reindex()
pyglet.resource.add_font("erasdust.ttf")

from copycat.run import Run

from coderack import Coderack
from slipnet import Slipnet
from workspace import Workspace

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

        self.timer = pyglet.text.Label("", "EraserDust", 12, x=512, y=580,
                                       color=(255,255,255, 125), batch=self.batch,
                                       halign="center", anchor_x="center")
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
        self.timer.text = str(self.run.coderack.time / self.run.timestep)
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
