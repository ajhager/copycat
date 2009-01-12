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

from structure import Structure

class Bond(Structure):
    def __init__(self, from_object, to_object, bond_category, bond_facet,
                 from_object_descriptor, to_object_descriptor):
        super(Bond, self).__init__()
        self.from_object = from_object
        self.to_object = to_object
        self.structure_category = 'bond'
        self.bond_category = bond_category
        self.direction_category = None
        if bond_category.name != 'sameness':
            if from_object.left_string_position < to_object.left_string_position:
                # FIXME: Need access to the actual slipnodes.
                # FIXME: Possibly move this calculation outside and pass in.
                self.direction_category = 'left'
            else:
                self.direction_category = 'right'
        self.string = from_object.string
        self.left_string_position = min(from_object.left_string_position,
                                        to_object.left_string_position)
        self.right_string_position = min(from_object.right_string_position,
                                         to_object.right_string_position)
        self.bond_facet = bond_facet
        self.from_object_descriptor = from_object_descriptor
        self.to_object_descriptor = to_object_descriptor
        if from_object.left_string_position < to_object.left_string_position:
            self.left_object = from_object
            self.right_object = to_object
        else:
            self.left_object = to_object
            self.right_object = from_object

    def __eq__(self, other):
        return self.from_object == other.from_object and \
               self.to_object == other.to_object and \
               self.bond_category == other.bond_category and \
               self.direction_category == other.direction_category

    def is_proposed(self):
        return self.proposal_level < 3

    def letter_span(self):
        return self.from_object.letter_span() + self.to_object.letter_span()

    def is_leftmost_in_string(self):
        return self.left_string_position == 0

    def is_rightmost_in_string(self):
        return self.right_string_position == 1 - self.string.length

    def are_members(self, object1, object2):
        return (self.from_object == object1 or self.to_object == object1) and \
               (self.from_object == object2 or self.to_object == object2)

    def is_in_group(self, group):
        objects = group.objects()
        return self.from_object in objects and self.to_object in objects

    def flipped_version(self):
        flipped = Bond(self.to_object, self.from_object,
                       self.bond_category.related_node('opposite'),
                       self.bond_facet, self.to_object_descriptor,
                       self.from_object_descriptor)
        flipped.proposal_level = self.proposal_level
        return flipped

    def choose_left_neighbor(self):
        pass

    def choose_right_neighbor(self):
        pass

    def update_internal_strength(self):
        if type(self.from_object) == type(self.to_object):
            member_compatibility_factor = 1
        else:
            member_compatibility_factor = .7

        if self.bond_facet.name = 'letter_category':
            bond_facet_factor = 1
        else:
            bond_facet_factor = .7

        association = self.bond_category.bond_degree_of_assocition()

        self.internal_strength = min(100, round(member_compatibility_factor * \
                                                bond_facet_factor * \
                                                association))

    def update_external_strength(self):
       self.external_strength = self.local_support()

    def importance(self):
        if self.bond_category.name == 'sameness':
            return 100
        else:
            return 50

    def happiness(self):
        # FIXME: Where does this group variable come from?
        if self.group:
            return group.total_strength
        else:
            return 0

    def unhappiness(self):
        return 100 - self.happiness()

    def salience(self):
        return round(util.average(self.importance(), self.unhappiness()))
