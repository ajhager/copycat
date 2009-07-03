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

import copycat.toolbox as toolbox
from copycat.coderack.codelet import Codelet

class Bin(object):
    def __init__(self, urgency_code, name):
        pass

class Coderack(object):
    def __init__(self):
        self.max_codelets = 100
        self.modifier = 0
        self.time = 0
        self.extremely_low_bin = Bin(0, 'extremely_low_bin')
        self.very_low_bin = Bin(1, 'very_low_bin')
        self.low_bin = Bin(2, 'low_bin')
        self.medium_bin = Bin(3, 'medium_bin')
        self.high_bin = Bin(4, 'high_bin')
        self.very_high_bin = Bin(5, 'very_high_bin')
        self.extremely_high_bin = Bin(6, 'extremely_high_bin')
        self.bins = [self.extremely_low_bin, self.very_low_bin,
                     self.low_bin, self.medium_bin, self.high_bin,
                     self.very_high_bin, self.extremely_high_bin]

    def update(self, temperature):
        self.modifier = (110.0 - temperature) / 15.0

    def urgency(self, urgency_bin):
        return round(urgency_bin ** self.modifier)

    def clear(self):
        self.codeletes = []

    def is_empty(self):
        return self.codelets == []

    def post(self, urgency, function, arguments):
        bin = min(self.max_bins, int((urgency * self.max_bins) / 100.0) + 1)
        codelet = dict(urgency_bin=bin, timestamp=self.time,
                       function=function, arguments=arguments)
        self.codelets.append(codelet)
        if len(self.codelets) > self.max_codelets:
            max_urgency = self.urgency(self.max_bins)
            weights = []
            for codelet in self.codelets:
                age = self.time - codelet['timestamp']
                codelet_urgency = self.urgency(codelet['urgency_bin'])
                weights.append(age * (1 + max_urgency - codelet_urgency))
            codelet = self.codelets.pop(util.weighted_index(weights))
            return codelet['function'], codelet['arguments']

    def choose(self):
        if len(self.codelets) == 0:
            return
        self.time += 1
        weights = [self.urgency(codelet['urgency_bin'])
                    for codelet in self.codelets]
        codelet = self.codelets.pop(util.weighted_index(weights))
        return codelet['function'], codelet['arguments']
