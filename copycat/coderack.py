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

import util

class Codelet(object):
    '''
    A codelet merely houses some data used to determine when and if it is run
    and a name which identifies to the workspace which codelet method to use
    when it is run.  The arguments to the method are sent in with the codelet
    so they can be used later.  The urgency sent in is used to determine the
    codelets urgency code.  The codelet's timestamp is set when it is added
    to the coderack.
    '''
    def __init__(self, name, arguments=(), urgency=0):
        self.name = name
        self.arguments = arguments
        self.timestamp = 0
        if urgency >= 100:
            self.urgency_code = 7
        else:
            self.urgency_code = int((urgency * 7) / 100.0) + 1

class Coderack(object):
    '''
    The coderack a simple data structure that houses codelets that have the
    potential to be run.  Urgency bins have been removed in favor of pushing
    that limited functionality into the codelet itself and using urgency codes
    to simulate urgency bins.  The end product is the same but with shorter,
    cleaner code.
    '''
    def __init__(self):
        self.temperature = 0
        self.codelets_run = 0
        self.codelets = []

    def __len__(self):
        '''
        The length or size of the coderack is equal to the number or codelets
        in it.
        '''
        return len(self.codelets)

    def _urgency_list(self):
        '''
        A helper function that creates a list of the urgencies of all codelets
        as a function of their urgency code and the temperature.
        '''
        urgency_list = []
        for codelet in self.codelets:
            urgency = round(codelet.urgency_code **\
                    ((110 - self.temperature) / 15.0))
            urgency_list.append(urgency)
        return urgency_list

    def _remove_one_codelet(self):
        '''
        This method removes one codelet from the coderack based on temperature,
        codelet urgency code, and codelet age.  A more in depth description is
        needed.
        '''
        if len(self) == 0:
            return

        highest_urgency = round(7 ** ((110 - self.temperature) / 15.0))
        urgency_list = self._urgency_list()
        remove_probability_list = []
        for index in range(len(self)):
            codelet = self.codelets[index]
            urgency = urgency_list[index]
            age = codelet.timestamp - self.codelets_run
            remove_probability = age * (1 + highest_urgency - urgency)
            remove_probability_list.append(remove_probability)
        index = util.select_list_position(remove_probability_list)
        codelet = self.codelets[index]
        self.codelets.remove(codelet)
        return codelet

    def update(self, temperature):
        '''
        The coderack update method merely updates the temperature.
        '''
        self.temperature = temperature

    def empty(self):
        '''
        Clears the coderack of all codelets.
        '''
        self.codelets = []

    def post(self, codelets):
        '''
        Posting a codelet to the coderack involves removing codelets to keep
        the size of the coderack under its max size, setting the codelet's
        timestamp to the number of codelets that have been run so far, and
        adding the codelet to the coderacks codelet list. Returns a list of
        deleted codelets so they can be processed.
        '''
        removed = []
        for codelet in list(codelets):
            if len(self) == 100:
                removed.append(self._remove_one_codelet())
            codelet.time_stamp = self.codelets_run
            self.codelets.append(codelet)
        return removed

    def choose(self):
        '''
        Choosing a codelet increments the codelets that have been run and
        returns a codelete chosen probabilistically based on the urgency list.
        '''
        if len(self) == 0:
            return

        self.codelets_run += 1
        index = util.select_list_position(self._urgency_list())
        codelet = self.codelets[index]
        self.codelets.remove(codelet)

        return codelet
