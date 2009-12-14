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

# Set up resources
square = pyglet.resource.image("square.png")
square.anchor_x = square.width / 2.
square.anchor_y = square.height / 2.

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
