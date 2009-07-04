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

import copycat.toolbox as toolbox
from copycat.coderack.codelet import Codelet

class Bin(object):
    def __init__(self, urgency_code):
        self.urgency_code = urgency_code
        self.codelets = []

    def add(self, codelet):
        '''
        Add a codelet to the bin.
        '''
        self.codelets.append(codelet)
        codelet.bin = self

    def choose(self):
        '''
        Choose and remove a random codelet from the bin.
        '''
        codelet = random.choice(self.codelets)
        self.remove(codelet)
        return codelet

    def clear(self):
        '''
        Clear the bin of all its codelets.
        '''
        self.codelets = []

    def number_of_codelets(self):
        '''
        Return the total number of codelets currently in the bin.
        '''
        return len(self.codelets)

    def remove(self, codelet):
        '''
        Remove a codelet from the bin.
        '''
        if codelet in self.codelets:
            self.codelets.remove(codelet)

    def urgency(self, temperature):
        '''
        Return the value associated with this bin; a function of its urgency
        code and the temperature.
        '''
        return round((self.urgency_code + 1) ** ((110 - temperature) / 15))

    def urgency_sum(self, temperature):
        '''
        Return the sum of urgencies in this bin.
        '''
        return self.number_of_codelets() * self.urgency(temperature)

class Coderack(object):
    def __init__(self):
        self.max_codelets = 100
        self.temperature = 0
        self.time = 0

        self.extremely_low_bin = Bin(0)
        self.very_low_bin = Bin(1)
        self.low_bin = Bin(2)
        self.medium_bin = Bin(3)
        self.high_bin = Bin(4)
        self.very_high_bin = Bin(5)
        self.extremely_high_bin = Bin(6)
        self.bins = [self.extremely_low_bin, self.very_low_bin,
                     self.low_bin, self.medium_bin, self.high_bin,
                     self.very_high_bin, self.extremely_high_bin]

    def choose(self):
        '''
        Choose a codelet from the coderack.
        '''
        if self.is_empty():
            return None
        urgencies = [bin.urgency_sum() for bin in self.bins]
        bin = toolbox.weighted_select(urgencies, self.bins)
        return bin.choose()

    def clear(self):
        '''
        Clear the coderack of all codelets.
        '''
        for bin in self.bins:
            bin.clear()

    def codelets(self):
        '''
        Return a list of all the codelets in the coderack.
        '''
        return toolbox.flatten([bin.codelets for bin in self.bins])

    def is_empty(self):
        '''
        Return True if the coderack is empty.
        '''
        return self.urgency_sum() == 0

    def number_of_codelets(self):
        '''
        Return the total number of codelets currently in the coderack.
        '''
        return sum([bin.number_of_codelets() for bin in self.bins])

    def post(self, codelet, urgency):
        '''
        Post a codelet to the coderack.  If the coderack is already at its max
        size, remove a codelet to make room for the new one.
        '''
        removed_codelet = None
        if self.number_of_codelets == self.max_codelets:
            codelets = self.codelets()
            probabilities = [self.remove_probability(cl) for cl in codelets]
            removed_codelet = toolbox.weighted_select(probabilities, codelets)
            removed_codelet.bin.remove(removed_codelet)

        if urgency >= 100:
            bin = self.extremely_high_bin
        else:
            index = (len(self.bins) * value) / 100
            bin = self.bins[index]
        bin.add(codelet)
        codelet.timestamp = self.time

        return removed_codelet

    def remove_probability(self, codelet):
        '''
        Return the probability of removing this codelet from the coderack,
        which is a function of the codelet's urgency and age.  Even codelets
        with the highest urgency and lowest age have a chance to be removed.
        '''
        codelet_age = self.time - codelet.timestamp
        codelet_bin_urgney.bin.urgency(self.temperature)
        highest_bin_urgency = self.extremely_high_bin.urgency(self.temperature)
        return codelet_age * (1 + codelet_bin_urgency - highest_bin_urgency)

    def update(self, temperature):
        self.temperature = temperature

    def urgency_sum(self):
        '''
        Return the total amount of urgency in the coderack.
        '''
        return sum([bin.urgency_sum(self.temperature) for bin in self.bins])
