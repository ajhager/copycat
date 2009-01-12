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

import random

from coderack import Coderack
from slipnet import Slipnet
from workspace import Workspace
from answer import AnswerBuilder

class Run(object):
    def __init__(self, initial, modified, target, seed):
        self.coderack = Coderack()
        self.slipnet = Slipnet()
        self.workspace = Workspace(initial, modified, target)
        random.seed(seed)
        self.timestep = 15

    def step(self):
        if self.coderack.time % self.timestep == 0:
            self.update()

        if self.coderack.is_empty():
            self.slipnet.clamp_initial_nodes()
            codelets = self.workspace.initial_codelets()
            for codelet in self.coderack.post(codelets):
                self.workspace.delete_proposed_structure(codelet.argument)

        codelet = self.coderack.choose()
        codelet.run(self.coderack, self.slipnet, self.workspace)

        if self.workspace.translated_rule:
            AnswerBuilder.run(self.coderack, self.slipnet, self.workspace)
            self.update()

    def update(self):
        self.workspace.update()
        
        if self.coderack.time == self.slipnet.clamp_time * self.timestep:
            self.slipnet.unclamp_initial_nodes()

        self.coderack.update(self.workspace.temperature)
        codelets = self.workspace.bottom_up_codelets() + \
                   self.slipnet.top_down_codelets()
        for codelet in self.coderack.post(codelets):
            self.workspace.delete_proposed_structure(codelet.argument)

        self.slipnet.update()
