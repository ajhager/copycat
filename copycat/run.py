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

from copycat.coderack import Coderack
from copycat.slipnet import Slipnet
from copycat.workspace import Workspace
from copycat.coderack.codelets import AnswerBuilder

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
            for codelet, urgency in codelets:
                deleted = self.coderack.post(codelet, urgency)
                self.workspace.delete_proposed_structure(deleted.arguments)

        codelet = self.coderack.choose()
        if codelet:
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
        for codelet, urgency in codelets:
            deleted = self.coderack.post(codelet, urgency)
            self.workspace.delete_proposed_structure(deleted.arguments)

        self.slipnet.update()

    def deal_with_snag(self):
        '''
        If there is a snag in building the answer, delete all proposed
        structures, empty the coderack, raise and clamp the temperature,
        and activate and clamp activation of all the descriptions of the
        object causing the snag.
        '''
        self.workspace.snag_count += 1
        self.workspace.last_snag_time = self.coderack.time
        self.workspace.snag_structures = self.workspace.structures
        for bond in self.workspace.proposed_bonds:
            bond.string.delete_proposed_bond(bond)
        for group in self.workspace.proposed_groups:
            group.string.delete_proposed_group(group)
        for correspondence in self.workspace.proposed_correspondences:
            self.workspace.delete_proposed_correspondence(correspondence)
        self.workspace.translated_rule = None
        self.workspace.answer_string = None
        self.workspace.snag_condition = True
        self.workspace.temperature = 100
        self.workspace.clamp_temperature = True
        for description in self.workspace.snag_object.descriptions:
            description.descriptor.clamped = True
        self.workspace.snag_object.clamp_salience = True
        self.coderack.clear()
        self.update()
