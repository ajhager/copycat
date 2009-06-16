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

from workspace import Structure

class Description(Structure):
    def __init__(self, object1, description_type, descriptor):
        self.object = object1
        self.string = object1.string
        self.description_type = description_type
        self.descriptor = descriptor
        self.description_number = None

    def __eq__(self, other):
        return self.description_type == other.description_type and \
                self.descriptor == other.descriptor

    def is_relevant(self):
        '''
        Return True if the description type being described is active.
        '''
        return self.description_type.is_active()

    def conceptual_depth(self):
        return self.descriptor.conceptual_depth()

    def is_bond_description(self):
        '''
        Return True if the description refers to the bonds making up the
        group, either the bond category or the bond facet.
        '''
        return self.description_type == self.slipnet.plato_bond_category or \
                self.description_type == self.slipnet.plato_bond_facet

    def apply_slippages(self, object1, slippages):
        '''
        Return a new description with the slippages applied.
        '''
        new_description_type = self.description_type
        new_descriptor = self.descriptor
        for slippage in slippages:
            if slippage.descriptor1 == self.description_type:
                new_description_type = slippage.descriptor2
            if slippage.descriptor1 == self.descriptor:
                new_descriptor == slippage.descriptor2

        return Description(self.object, new_description_type, new_descriptor)

class ExtrinsicDescription(object):
    def __init__(self, relation, description_type_related, other_object):
        self.relation = relation
        self.description_type_related = description_type_related
        self.other_object = other_object

    def conceptual_depth(self):
        return self.relation.conceptual_depth()
