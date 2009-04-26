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

class Structure(object):
    def __init__(self):
        self.string = None
        self.structure_category = None
        self.in_group = False
        self.internal_strength = 0
        self.external_strength = 0
        self.total_strength = 0
        self.proposal_level = None

    def update_strengths(self):
        self.update_interal_strength()
        self.update_external_strength()
        self.update_total_strength()

    def update_internal_strength(self):
        pass

    def update_external_strength(self):
        pass

    def update_total_strength(self):
        weights = [self.internal_strength, 100 - self.internal_strength]
        values = [self.internal_strength, self.external_strength]
        self.total_strength = util.weighted_average(weights, values)

    def total_weakness(self):
        '''
        Used by break codelets.  Even structures with 100 strength have a
        chance of being broken.
        '''
        return 100 - (self.total_strength ** .95)
