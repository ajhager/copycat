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

import math

import copycat.toolbox as toolbox
from copycat.workspace import Object, Structure, Description, Mapping
import copycat.slipnet as nodes

class Group(Object, Structure):
    def __init__(self, workspace, string, group_category, direction_category,
                 left_object, right_object, objects, bonds):
        super(Group, self).__init__()
        self.workspace = workspace
        self.type_name = 'group'
        self.string = string
        self.structure_category = Group
        self.group_category = group_category
        self.direction_category = direction_category
        self.left_object = left_object
        self.right_object = right_object
        self.middle_object = None
        category = nodes.plato_string_position_category
        for obj in objects:
            if obj.get_descriptor(category) == nodes.plato_middle:
                self.middle_object = obj
                break
        self.left_object_position = left_object.left_string_position
        self.right_object_position = right_object.right_string_position
        self.left_string_position = self.left_object_position
        self.right_string_position = self.right_object_position
        self.objects = objects
        self.bonds = bonds
        category = nodes.plato_bond_category
        self.bond_category = nodes.get_related_node(group_category, category)
        self.bond_facet = None
        self.bond_descriptions = []

        if self.spans_whole_string():
            object_category = nodes.plato_object_category
            group = nodes.plato_group
            description = Description(self, object_category, group)
            self.add_description(description)

        category = nodes.plato_string_position_category
        if self.is_leftmost_in_string() and not self.spans_whole_string():
            leftmost = nodes.plato_leftmost
            description = Description(self, category, leftmost)
            self.add_description(description)
        elif self.is_middle_in_string():
            middle = nodes.plato_middle
            description = Description(self, category, middle)
            self.add_description(description)
        elif self.is_rightmost_in_string() and not self.spans_whole_string():
            rightmost = nodes.plato_rightmost
            description = Description(self, category, rightmost)
            self.add_description(description)

        if group_category == nodes.plato_sameness_group and \
           (bonds == [] or \
            bonds[0].bond_facet == nodes.plato_letter_category):
            category = nodes.plato_letter_category
            new_group_letter_category = left_object.get_descriptor(category)
            description = Description(self, category, new_group_letter_category)
            self.add_description(description)

        category = nodes.plato_group_category
        description = Description(self, category, group_category)
        self.add_description(description)
        if direction_category:
            category = nodes.plato_direction_category
            description = Description(self, category, direction_category)
            self.add_description(description)

        if bonds:
            new_bond_facet = bonds[0].bond_facet
            self.bond_facet = new_bond_facet
            category = nodes.plato_bond_facet
            description = Description(self, category, new_bond_facet)
            self.add_bond_description(description)

        category = nodes.plato_bond_category
        description = Description(self, category, self.bond_category)
        self.add_bond_description(description)

        length_description_probability = self.length_description_probability()
        if toolbox.flip_coin(length_description_probability):
            category = nodes.plato_length
            plato_number = nodes.get_plato_number(self.length())
            description = Description(self, category, plato_number)
            self.add_description(description)

    def __eq__(self, other):
        if other == None or not isinstance(other, Group):
            return False
        return self.left_object_position == other.left_object_position and \
                self.right_object_position == other.right_object_position and \
                self.group_category == other.group_category

    def is_recursive_member(self, obj):
        """Return True if the object is a member of the gruop or is a member
        of a member, etc."""
        if obj.type_name == 'letter':
            return False
        elif obj in self.objects:
            return True
        for g in self.objects:
            if obj.is_recursive_member(g):
                return True

    def calculate_internal_strength(self):
        '''
        For now, groups based on letter category are stronger than groups based
        on other facets. This should be fixed; a more general mechanism is
        needed.
        '''
        if self.bond_facet == nodes.plato_letter_category:
            bond_facet_factor = 1
        else:
            bond_facet_factor = .5

        bond_component = nodes.get_related_node(self.group_category,
                                                nodes.plato_bond_category).degree_of_association() * bond_facet_factor

        if self.length() == 1:
            length_component = 5
        elif self.length() == 2:
            length_component = 20
        elif self.length() == 3:
            length_component = 60
        else:
            length_component = 90

        bond_component_weight = bond_component ** .98
        length_component_weight = 100 - bond_component_weight
        return toolbox.weighted_average((bond_component_weight,length_component_weight),
                                        (bond_component, length_component))

    def calculate_external_strength(self):
        if self.spans_whole_string():
            return 100
        else:
            return self.local_support()

    def number_of_local_supporting_groups(self):
        '''
        Return the number of supporting groups in the given gruop's string.
        Looks at all the other groups in the string, counting groups of the
        same group category and direction category.  Does not take distance
        into acount; all qualifying groups in the string are counted the same.
        '''
        number_of_supporting_groups = 0
        groups = self.string.get_groups()
        if self in groups:
            groups.remove(self)
        for other_group in groups:
            if other_group == None:
                continue
            if (not (self.is_subgroup_of(other_group) or \
                         other_group.is_subgroup_of(self) or \
                         self.overlaps(other_group))) and \
                         other_group.group_category == self.group_category and \
                         other_group.direction_category == self.direction_category:
                number_of_supporting_groups += 1
        return number_of_supporting_groups
    
    def local_density(self):
        '''
        Return the rough measure of the density in the string of groups of the
        same group category and directin category as the given group. This
        method is used in calculating the external strength of a group.
        '''
        if self.is_string_spanning_group():
            return 100

        slot_sum = 0
        support_sum = 0

        next_object = self.left_object.choose_left_neighbor()
        if next_object and next_object.type_name == 'letter' and next_object.group:
            next_object = next_object.group
        while next_object:
            if next_object.type_name == 'letter':
                next_group = None
            else:
                next_group = next_object
            slot_sum += 1
            if next_group and not self.overlaps(next_group) and \
               next_group.group_category == self.group_category and \
               next_group.direction_category == self.direction_category:
                support_sum += 1
            next_object = next_object.choose_left_neighbor()

        next_object = self.right_object.choose_right_neighbor()
        if next_object.type_name == 'letter' and next_object.group:
            next_object = next_object.group
        while next_object:
            if next_object.type_name == 'letter':
                next_group = None
            else:
                next_group = next_object
            slot_sum += 1
            if next_group and not self.overlaps(next_group) and \
               next_group.group_category == self.group_category and \
               next_group.direction_category == self.direction_category:
                support_sum += 1
            next_object = next_object.choose_right_neighbor()

        if slot_sum == 0:
            return 100
        else:
            return round(100 * (support_sum / float(slot_sum)))

    def local_support(self):
        number = self.number_of_local_supporting_groups()
        if number == 0:
            return 0
        else:
            density = self.local_density()
            adjusted_density = 100 * (math.sqrt(density / 100.0))
            number_facet = min(1, .6 ** (1 / number ** 3))
            return round(adjusted_density * number_facet)

    def sharing_group(self, other):
        return self.group == other.group

    def is_subgroup_of(self, other):
        return other.left_object_position <= self.left_object_position and \
                other.right_object_position >= self.right_object_position

    def overlaps(self, other):
        return set(self.objects).issubset(set(other.objects))

    def incompatible_groups(self):
        '''
        Return a list of the groups that are incompatible with the group.
        '''
        groups = [obj.group for obj in self.objects if obj.group]
        groups = list(set(toolbox.flatten(groups)))
        if self in groups:
            groups.remove(self)
        return groups

    def incompatible_correspondences(self):
        '''
        Return a list of the correspondences that are incompatible.
        '''
        correspondences = []
        for obj in self.objects:
            if obj.correspondence and \
               self.is_incompatible_correspondence(obj.correspondence, obj):
                correspondences.append(obj.correspondence)
        return correspondences

    def is_incompatible_correspondence(self, correspondence, obj):
        category = nodes.plato_string_position_category
        for cm in correspondence.concept_mappings:
            if cm.description_type1 == category:
                concept_mapping = cm
                break

        if concept_mapping:
            other_object = correspondence.other_object(obj)
            other_bond = None
            if other_object.is_leftmost_in_string():
                other_bond = other_object.right_bond
            elif other_object.is_rightmost_in_string():
                other_bond = other_object.left_bond
            if other_bond:
                if other_bond.direction_category:
                    direction = nodes.plato_direction_category
                    group_mapping = Mapping(direction, direction,
                                            self.direction_category,
                                            other_bond.direction_category,
                                            None, None)
                    if group_mapping.is_incompatible_concept_mapping(concept_mapping):
                        return True


    def leftmost_letter(self):
        """Return the leftmost letter in the group or in the leftmost subgroup
        of the group."""
        if self.left_object.type_name == 'letter':
            return self.left_object
        else:
            return self.left_object.leftmost_letter()

    def rightmost_letter(self):
        """Return the rightmost letter in the group or the rightmost subgroup
        of the group."""
        if self.right_object.type_name == 'letter':
            return self.right_object
        else:
            return self.right_object.rightmost_letter()

    def is_leftmost_in_string(self):
        '''
        Return True if the group is leftmost in its string.
        '''
        return self.left_object_position == 0

    def is_rightmost_in_string(self):
        '''
        Return True if the group is righmost in its string.
        '''
        return self.right_object_position == self.string.length - 1

    def left_neighbor(self):
        if not self.leftmost_in_string():
            position = self.left_object_position - 1
            possible_left_neighbor = self.string.get_letter(position).group
            if possible_left_neighbor:
                if self not in possible_left_neighbor.objects and \
                   not self.is_subgroup(possible_left_neighbor):
                    return possible_left_neighbor

    def right_neighbor(self):
        if not self.rightmost_in_string():
            position = self.right_object_position + 1
            possible_right_neighbor = self.string.get_letter(position).group
            if possible_right_neighbor:
                if self not in possible_right_neighbor.objects and \
                   not self.is_subgroup(possible_right_neighbor):
                    return possible_right_neighbor


    def add_bond_description(self, description):
        """Add a bond description to the group's list of bond descriptions."""
        self.bond_descriptions.append(description)

    def length(self):
        """Return the number of objects in the group."""
        return len(self.objects)

    def flipped_version(self):
        '''
        Return the flipped version of this group.
        '''
        if not (self.group_category == nodes.plato_predecessor_group or\
                self.group_category == nodes.plato_successor_group):
            return self
        else:
            new_bonds = [bond.flipped_version() for bond in self.bonds]
            opposite = nodes.plato_opposite
            group_category = nodes.get_related_node(self.group_category, opposite)
            direction_category = nodes.get_related_node(self.direction_category,
                                                        opposite)
            flipped_group = Group(self.workspace, self.string, group_category,
                                  direction_category, self.left_object,
                                  self.right_object, self.objects, new_bonds)
            flipped_group.proposal_level = self.proposal_level
            return flipped_group

    def get_bonds_to_be_flipped(self):
        """Return a list of the bonds that need to be flipped in order for the
        group to be built."""
        bonds_to_be_flipped = []
        for b in self.bonds:
            to_be_flipped = self.string.get_bond(b.to_object, b.from_object)
            if to_be_flipped and b == to_be_flipped.flipped_version():
                bonds_to_be_flipped.append(to_be_flipped)
        return bonds_to_be_flipped

    def spans_whole_string(self):
        return self.letter_span() == self.string.length

    def is_proposed(self):
        return self.proposal_level < self.workspace.built

    def single_letter_group_probability(self):
        '''
        Return the probability to be used in deciding whether or not to propose
        the single letter group.
        '''
        n = self.number_of_local_supporting_groups()
        if n == 1:
            exponent = 4
        elif n == 2:
            exponent = 2
        else:
            exponent =  1

        a = self.local_support() / 100.
        b = nodes.plato_length.activation / 100.
        prob = (a * b) ** exponent
        return self.workspace.temperature_adjusted_probability(prob)

    def length_description_probability(self):
        if self.length() > 5:
            return 0
        a = self.length() ** 3
        b = (100 - nodes.plato_length.activation) / 100.
        prob = .5 ** (a * b)
        return self.workspace.temperature_adjusted_probability(prob)
