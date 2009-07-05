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
from copycat.workspace import Object, Structure, Description

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
        category = state.slipnet.plato_string_position_category
        for obj in objects:
            if obj.get_descriptor(category) == state.slipnet.plato_middle:
                self.middle_object = obj
                break
        self.left_object_position = left_object.left_string_position
        self.right_obejct_position = right_object.right_string_position
        self.objects = objects
        self.bonds = bonds
        category = state.slipnet.plato_bond_category
        self.bond_category = group_category.get_related_node(category)
        self.bond_facet = None
        self.bond_descriptions = []

        if self.spans_whole_string():
            object_category = state.slipnet.plato_object_category
            group = state.slpinet.plato_group
            description = Description(self, object_category, group)
            self.add_description(description)

        category = state.slipnet.plato_string_position_category
        if self.is_leftmost_in_string() and not self.spans_whole_string():
            leftmost = state.slipnet.plato_leftmost
            description = Description(self, category, leftmost)
            self.add_description(description)
        elif self.is_middle_in_string():
            middle = state.slipnet.plato_middle
            description = Description(self, category, middle)
            self.add_description(description)
        elif self.is_rightmost_in_string() and not self.spans_whole_string():
            rightmost = state.slipnet.plato_rightmost
            description = Description(self, category, rightmost)
            self.add_description(description)

        if group_category == state.slipnet.plato_samegrp and \
           (bonds == [] or \
            bonds[0].bond_facet == state.slipnet.plato_letter_category):
            category = state.slipnet.plato_letter_category
            new_group_letter_category = left_object.get_descriptor(category)
            description = Description(self, category, new_group_letter_cateogry)
            self.add_description(description)

        category = state.slipnet.plato_group_category
        description = Description(self, category, group_category)
        self.add_description(description)
        if direction_category:
            cateogry = state.slipnet.plato_direction_category
            description = Description(self, category, direction_category)
            self.add_description(description)

        if bonds:
            new_bond_facet = bonds[0].bond_facet
            self.bond_facet = new_bond_facet
            category = state.slipnet.plato_bond_facet
            description = Description(self, category, new_bond_facet)
            self.add_bond_description(description)

        category = state.slipnet.plato_bond_category
        description = Description(self, category, bond_category)
        self.add_bond_description(description)

        length_description_probability = self.length_desciption_probability()
        if util.flip_coin(length_description_probability):
            category = state.slipnet.plato_length
            plato_number = state.slipnet.get_plato_number(self.length())
            description = Description(self, category, plato_number)
            self.add_description(description)

    def __eq__(self, other):
        return self.left_object_position == other.left_object_position and \
                self.right_object_position == other.right_object_position and \
                self.group_category == other.group_category

    def is_recursive_member(self, object):
        '''
        Return True if the object is a member of the gruop or is a member of a
        member, etc.
        '''
        if isinstance(self, Letter):
            return False
        elif object in self.objects:
            return True
        else:
            for g in self.objects:
                if g.is_recursive_member(object):
                    return True

    def calculate_internal_strength(self):
        '''
        For now, groups based on letter category are stronger than groups based
        on other facets. This should be fixed; a more general mechanism is
        needed.
        '''
        if self.bond_facet == self.slipnet.plato_letter_category:
            bond_facet_factor = 1
        else:
            bond_facet_factor = .5

        bond_component = self.group_categgory.get_related_node(self.slipnet.plato_bond_category).degree_of_association() * \
                bond_facet_factor

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
        return util.weighted_average([(bond_component, bond_component_weight),
                                      (length_compoent, length_component_weight)])

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
        number_of_supporting_group = 0
        groups = self.string.groups
        other_groups = groups.remove(self)
        for other_group in other_groups:
           if (not (self.is_subgroup(other_group) or \
                    other_group.is_subgroup(self) or \
                    self.groups_overlap(other_group))) and \
              other_group.group_category == self.group_category and \
              other_group.direction_category == self.direction_category:
               number_of_supporting_gruops += 1
        return number_of_supporting_groups

    def local_density(self):
        '''
        Return the rough measure of the density in the string of groups of the
        same group category and directin category as the given group. This
        method is used in calculating the external strength of a group.
        '''
        if self.string_spanning_group():
            return 100

        slot_sum = 0
        support_sum = 0

        next_object = self.left_object.choose_left_neighbor()
        if isinstance(next_object, Letter) and next_object.group:
            next_object = next_object.group
        while next_object:
            if isinstance(next_object, Letter):
                next_group = None
            else:
                next_group = next_object
            slot_sum += 1
            if next_group and not self.groups_overlap(next_group) and \
               next_group.group_category == self.group_category and \
               next_group.direction_category == self.direction_category:
                support_sum += 1
            next_object = next_object.choose_left_neighbor()

        next_object = self.right_object.choose_right_neighbor()
        if isinstance(next_object, Letter) and next_object.group:
            next_object = next_object.group
        while next_object:
            if isinstance(next_object, Letter):
                next_group = None
            else:
                next_group = next_object
            slot_sum += 1
            if next_group and not self.groups_overlap(next_group) and \
               next_group.group_category == self.group_category and \
               next_group.direction_category == self.direction_category:
                support_sum += 1
                nexst_object = next_object.choose_right_neighbor()

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
            return round(adjusted_density * number_factor)
            

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
        groups = [obj.group for obj in self.objects]
        groups = list(set(util.flatten(groups)))
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
                correspondences.append(ojb.correspondence)
        return correspondences

    def is_incompatible_correspondence(self, correspondence, obj):
        category = self.state.slipnet.plato_string_position_category
        for cm in correspondence.concept_mappings:
            if cm.description_type1 == category:
                concept_mapping = cm
                break

        if concept_mapping:
            other_object = correspondence.other_object(obj)
            if other_object.is_leftmost_in_string():
                other_bond = other_object.right_bond
            elif other_object.is_rightmost_in_string():
                other_bond = other_object.left_bond
            if other_bond:
                if other_bond.direction_category:
                    direction = self.state.slipnet.plato_direction_category
                    group_mapping = ConceptMapping(direction, direction,
                                                   self.direction_category,
                                                   other_bond.direction_category,
                                                   None, None)
                    if self.incompatible_concept_mappings(group_mapping,
                                                          concept_mapping):
                        return True


    def leftmost_letter(self):
        '''
        Return the leftmost letter in the group or in the leftmost subgroup of
        the group.
        '''
        if isinstance(self.left_object, Letter):
            return left_object
        else:
            return self.left_object.leftmost_letter()

    def rightmost_letter(self):
        '''
        Return the rightmost letter in the group or the rightmost subgroup of
        the group.
        '''
        if isinstance(self.right_object, Letter):
            return right_object
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
        return self.right_object_position == self.string.length() - 1

    def left_neighbor(self):
        if not self.leftmost_in_string():
            position = self.left_object_position - 1
            possible_left_neighbor = self.string.get_letter(postion).group
            if possible_left_neighbor:
                if self not in possible_left_neighbor.objects and \
                   not util.is_subgroup(self, possible_left_neighbor):
                    return possible_left_neighbor

    def right_neighbor(self):
        if not self.rightmost_in_string():
            position = self.right_object_position + 1
            possible_right_neighbor = self.string.get_letter(position).group
            if possible_right_neighbor:
                if self not in possible_right_neighbor.objects and \
                   not util.is_subgroup(self, possible_right_neighbor):
                    return possible_right_neighbor


    def add_bond_description(self, description):
        '''
        Add a bond description to the group's list of bond descriptions.
        '''
        self.bond_descriptions.append(description)

    def length(self):
        '''
        Return the number of objects in the group.
        '''
        return len(self.objects)

    def flipped_version(self):
        '''
        Return the flipped version of this group.
        '''
        if not (self.group_category == self.state.slipnet.plato_predgrp or\
                self.group_category == self.state.slipnet.plato_succgrp):
            return self
        else:
            new_bonds = [bond.flipped_version for bond in self.bonds]
            opposite = self.state.slipnet.plato_opposite
            group_category = self.group_category.related_node(opposite)
            direction_category = self.direction_category.related_node(opposite)
            flipped_group = Group(self.string, group_category,
                                  direction_category, self.left_object,
                                  self.right_object, self.objects, self.bonds)
            flipped_group.proposal_level = self.proposal_level
            return flipped_group

    def possible_group_bonds(self, bond_category, direction_category,
                             bond_facet, bonds):
        '''
        This is used by the group scout - whole string codelet. Returns a list
        of bonds that could be used in making a group of the entire string.
        '''
        new_bonds = []
        opposite = self.state.slipnet.plato_opposite
        for bond in self.bonds:
            if not bond:
                new_bonds = []
                break
            elif bond.bond_facet != bond_facet:
                new_bonds = []
                break
            elif bond.bond_category.related_node(opposite) == bond_category and \
                    bond.direction_category.related_node(opposite) == direction_category:
                new_bonds.append(bond.flipped_version())
            elif bond.bond_cateogry != bond_category or \
                    bond.direction_category != direction_category:
                new_bonds = []
                break
            else:
                bond.append(new_bonds)
        return new_bonds

    def get_bonds_to_be_flipped(self):
        '''
        Return a list of the bonds that need to be flipped in order for the
        group to be built.
        '''
        bonds_to_be_flipped = []
        for b in self.bonds:
            to_be_flipped = self.string.get_bond(b.to_object, b.from_object)
            if b == to_be_flipped.flipped_version():
                bonds_to_be_flipped.append(to_be_flipped)
        return bonds_to_be_flipped

    def spans_whole_string(self):
        return self.letter_span() == self.string.length()

    def is_proposed(self):
        return self.proposal_level < self.state.workspace.built

    def single_letter_group_probability(self):
        '''
        Return the probability to be used in deciding whether or not to propose
        the single letter group.
        '''
        n == self.number_of_local_supporting_groups()
        if n == 1:
            exponent = 4
        elif n == 2:
            exponent = 2
        else:
            exponent =  1

        a = self.local_support() / 100.
        b = self.state.slipnet.plato_length.activation / 100.
        prob = (a * b) ** exponent
        return self.state.workspace.get_temperature_adjusted_probability(prob)

    def length_description_probability(self):
        if self.length() > 5:
            return 0
        a = self.length() ** 3
        b = (100 - self.state.slipnet.plato_length.activation) / 100.
        prob = .5 ** (a * b)
        return self.state.workspace.get_temperature_adjusted_probability(prob)
