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

import util

class Codelet(object):
    def __init__(self, argument=None, urgency=0):
        self.argument = argument
        self.urgency_code = min(7, int((urgency * 7) / 100.0) + 1)
        self.timestamp = -1

    def run(self, coderack, slipnet, workspace):
        raise NotImplementedError, 'Must override the Codelet run method.'

class Coderack(object):
    def __init__(self):
        self.codelets = []
        self.time = 0
        self.temperature = 0

    def is_empty(self):
        return len(self.codelets) == 0

    def update(self, temperature):
        self.temperature = temperature

    def post(self, codelets):
        removed = []
        for codelet in list(codelets):
            if len(self.codelets) == 100:
                modifier = (110 - self.temperature) / 15.0
                max_urgency = round(7 ** modifier)
                probabilities = []
                for codelet in self.codelets:
                    urgency = round(codelet.urgency_code ** modifier)
                    age = self.time - codelet.timestamp
                    probability = age * (1 + max_urgency - urgency)
                    probabilities.append(probability)
                old_codelet = util.weighted_select(probabilities, self.codelets)
                self.codelets.remove(old_codelet)
                removed.append(old_codelet)
            codelet.timestamp = self.time
            self.codelets.append(codelet)
        return removed

    def choose(self):
        self.time += 1
        modifier = (110 - self.temperature) / 15.0
        urgencies = []
        for codelet in self.codelets:
            urgencies.append(round(codelet.urgency_code ** modifier))
        codelet = util.weighted_select(urgencies, self.codelets)
        self.codelets.remove(codelet)
        return codelet
