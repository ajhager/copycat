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

from coderack import Coderack, Codelet
from slipnet import Slipnet
from workspace import Workspace

class Run(object):
    '''
    A run is a way of grouping everything needed for copycat to execute.  
    '''
    def __init__(self, initial, modified, target, seed):
        '''
        Set up the initial state of the run.
        '''
        self.coderack = Coderack()
        self.slipnet = Slipnet()
        self.workspace = Workspace(initial, modified, target)
        random.seed(seed)

        # How many codelets to run before updating.
        self.time_step = 15

        # How many codelets to run before unclamping slipnet nodes.
        self.clamp_time = 50

    def step(self):
        '''
        The main driver of a copycat run, which loops until an answer is found.
        If the coderack runs out of codelets, certain nodes in the slipnet are
        clamped, and a group of bottom-up codelets are posted to the coderack.
        A codelet is then chosen from the coderack and run. If the rule has
        been translated, we build the answer string.
        '''
        if self.coderack.codelets_run % self.time_step == 0:
            self.update()

        if len(self.coderack) == 0:
            self.slipnet.clamp_initial_nodes()
            self.post(self.workspace.initial_codelets())

        self.run(self.coderack.choose())

        if self.workspace.translated_rule:
            self.run(Codelet('answer_builder'))
            self.update()

    def post(self, codelets):
        '''
        Post some codelets to the coderack. Any codelets that had to be removed
        should have their proposed structures removed from the workspace.
        '''
        removed = self.coderack.post(codelets)
        for codelet in removed:
            self.workspace.delete_proposed_structure(codelet.arguments)

    def update(self):
        '''
        Update all copycat subsystems.
        '''
        self.workspace.update()
        
        if self.coderack.codelets_run == self.clamp_time * self.time_step:
            self.slipnet.unclamp_initial_nodes()

        self.coderack.update(self.workspace.temperature)
        self.post(self.workspace.bottom_up_codelets())
        self.post(self.slipnet.top_down_codelets())
        self.slipnet.update()

    def run(self, codelet):
        '''
        Codelets in our copycat are very lightweight.  They only keep track
        of a couple of bookkeeping values and their name.  The name is really
        just the name of a workspace method.  When a codelet is sent here to
        be run, the codelet's name is looked up in the workspace's atrribute
        dictionary and run with the codelets argument attribute. The codelet
        method can return a flag to clear the coderack and a list of codelets
        to post.
        '''
        if codelet:
            method = getattr(self.workspace, codelet.name)
            result = method(*codelet.arguments)
            if result is None:
                return
            elif len(result) == 1:
                codelets = result
            elif len(result) == 2:
                replacing, codelets = result
                if replacing:
                    self.coderack.empty()
            else:
                return
            self.post(codelets)
