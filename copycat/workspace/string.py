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

class String(object):
    def __init__(self, state, string):
        self.state = state
        self.highest_string_number = -1
        self.letters = []
        self.proposed_bonds = []
        self.left_right_bonds = []
        self.from_to_bonds = []
        self.proposed_groups = []
        self.built_groups = []
        self.length = len(string)
        self.name = string
        self.object_spaces = self.length
        self.intra_string_unhappiness = 0
        self.number_of_bonds_to_scan_distribution = range(self.length)

    def __repr__(self):
        return 'String(%s)' % self.name

    def random_object(self):
        '''
        Return a random object in the string.
        '''
        return random.choose(self.letters + self.groups)

    def random_letter(self):
        '''
        Return a random letter in the string.
        '''
        return random.choose(self.letters)

    def choose_object(self, method):
        '''
        Return an object in the string chosen probabilistically,
        adjusted for temperature, according to the given method.
        '''
        objects = self.letters + self.groups
        values = map(method, objects)
        values = self.state.workspace.temperature_adjusted_values(values)
        return objects[util.weighted_index(values)]

    def choose_leftmost_object(self):
        '''
        Return one of the leftmost objects in the string, chosen
        probabilistically according to the relative importance of the
        lefmost objects in the string.
        '''
        leftmost_objects = []
        category = self.state.slipnet.plato_string_position_category
        for workspace_object in self.objects:
            descriptor = workspace_object.descriptor(category)
            if descriptor == self.state.slipnet.plato_leftmost:
                leftmost_objects.append(workspace_object)
        if leftmost_objects:
            values = [obj.relative_importance() for obj in leftmost_objects]
            return util.weighted_select(values, leftmost_objects)

    def proposed_bonds(self):
        '''
        Return a list of all proposed bonds in the string.
        '''
        return list(set(self.proposed_bonds))

    def bonds(self):
        '''
        Return a list of all bonds in the string.
        '''
        return list(set(self.from_to_bonds))

    def proposed_groups(self):
        pass

    def non_string_spanning_objects(self):
        '''
        Return a list of all non string spanning objects in the string.
        '''
        non_string_spanning = []
        for workspace_object in self.objects:
            if workspace_object.spans_whole_string():
                non_string_spanning.append(workspace_object)
        return non_string_spanning

    def add_letter(self, letter):
        '''
        Add a letter to the string.
        '''
        self.highest_string_number += 1
        letter.string_number = self.highest_string_number
        position = letter.left_string_position
        self.letters.insert(position, letter)
        right_object = self.object_positions[position]
        self.object_positions.insert(position, [letter, right_object])

    def objects_by_category(self, category):
        '''
        Return a list of objects in the string of the given object category.
        '''
        if category == self.state.slipnet.plato_letter:
            return self.letters
        elif category == self.state.slipnet.plato_group:
            return self.groups

    def add_proposed_bond(self, bond):
        '''
        Add a proposed bond to the string.
        '''
        from_n = bond.from_object.string_number
        to_n = bond.to_object.string_number
        old_bond = self.proposed_bonds[from_n][to_n]
        self.propsed_bonds[from_n][to_n] = [bond, old_bond]

    def remove_proposed_bond(self, bond):
        '''
        Remove a proposed bond from the string.
        '''
        from_n = bond.from_object.string_number
        to_n = bond.to_object.string_number
        self.proposed_bonds[from_n][to_n].remove(bond)

    def add_bond(self, bond):
        '''
        Add a built bond to the string, storing sameness bonds twice, in both
        directions, since they have no direction.
        '''
        left_number = bond.left_object.string_number
        right_number = bond.right_object.string_number
        self.left_right_bonds[left_number][right_number] = bond

        from_number = bond.from_object.string_number
        to_number = bond.to_object.string_number
        self.from_to_bonds[from_number][to_number] = bond

        if bond.bond_category == self.state.slipnet.plato_sameness:
            self.left_right_bonds[right_number][left_number] = bond
            self.from_to_bonds[to_number][from_number] = bond

    def remove_bond(self, bond):
        '''
        Remove a built bond from the string, deleting both sameness bonds
        since they are stored twice in both directions.
        '''
        left_number = bond.left_object.string_number
        right_number = bond.right_object.string_number
        self.left_right_bonds[left_number][right_number] = None

        from_number = bond.from_object.string_number
        to_number = bond.to_object.string_number
        self.from_to_bonds[from_number][to_number] = None

        if bond.bond_category == self.state.slipnet.plato_sameness:
            self.left_right_bonds[right_number][left_number] = None
            self.from_to_bonds[to_number][from_number] = None

    def add_proposed_group(self, group):
        '''
        Add a proposed group to the string.
        '''
        left_number = group.left_object.string_number
        right_number = group.right_object.string_number
        self.proposed_groups[left_number][right_number].append(group)

    def remove_proposed_groupd(self, group):
        '''
        Remove a proposed group from the string.
        '''
        left_number = group.left_object.string_number
        right_number = group.right_object.string_number
        self.proposed_groups[left_number][right_number].remove(group)

    def add_group(self, group):
        '''
        Add a built group to the string.
        '''
        self.groups[group.left_object.string_number] = group
        self.highest_string_number += 1
        group.string_number = self.highest_string_number
        self.make_room_for_new_object()
        self.object_positions[group.left_string_position].append(group)
        self.object_positions[group.right_string_position].append(group)

    def remove_group(self, group):
        '''
        Remove a built group from the string.
        '''
        self.groups[group.left_object.string_number] = None
        self.object_positions[group.left_string_position].remove(group)
        self.object_positions[group.right_string_position].remove(group)

    def make_room_for_new_object(self):
        # TODO: Move the arrays to dicts (sparse matrices).
        pass

    def get_letter(self, position):
        '''
        Return the object at the given position in the letter list of the
        string.
        '''
        return self.letters[position]

    def get_group(self, position):
        '''
        Return the group at the given position in the letter list of the
        string.
        '''
        return self.get_letter[position].group

    def is_bond_present(self, bond):
        '''
        Return the existing bond if the bond exists in the string.
        '''
        from_number = bond.from_object.string_number
        to_number = bond.to_object.string_number
        existing_bond = self.from_to_bonds[from_number][to_number]
        if existing_bond and \
           existing_bond.bond_category == bond.bond_category and \
           existing_bond.direction_category == bond.directin_category:
            return existing_bond

    def get_bond(self, from_object, to_object):
        '''
        Return the bond, if any, from the from_object to the to_object.
        '''
        from_number = from_object.string_number
        to_number = to_object.string_number
        return self.from_to_bonds[from_number][to_number]

    def is_group_present(self, group):
        '''
        Return the existing group if the group exists in the string.
        '''
        existing_group = self.groups[group.left_object.string_number]
        if existing_group and \
           existing_group.length() == group.length() and \
           existing_group.group_category == group.group_category and \
           existing_group.direction_category == group.direction_category:
            return existing_group

    def local_bond_category_relevance(self, bond_category):
        '''
        A function of how many bonds in the string have the given bond
        category. This function is not perfect; it gives just a rough
        estimate of the relevance of this bond category.

        It is used in top down bond scout - category and top down group
        scout - category codelets as a way of probabilistically choosing
        a string to work in.
        '''
        objects = self.non_string_spanning_objects()
        if len(objects) == 1:
            return 0
        else:
            bond_count = 0
            for obj in objects:
                if obj.right_bond:
                    if obj.right_bond.bond_category == bond_category:
                        bond_count += 1
            return 100 * (float(bond_count) / (len(objects) - 1))

    def local_direction_category_relevance(self, direction_category):
        '''
        A function of how many bonds in the string have the given direction
        category. This function is not perfect; it gives just a rough
        estimate of the relevance of this direction category.

        It is used in the top down bond scout - direction and top down group
        scout - direction codelets as a way of probabilistically choosing
        a string to work in.
        '''
        objects = self.non_string_spanning_objects()
        if len(objects) == 1:
            return 0
        else:
            bond_count = 0
            for obj in objects:
                if obj.right_bond:
                    if obj.right_bond.direction_category == direction_category:
                        bond_count += 1
            return 100 * (float(bond_count) / (len(objects) - 1))

    def update_relative_importances(self):
        '''
        Update the relative, normalized importances of all the objects in
        the string.
        '''
        raw_importance = sum([obj.raw_importance for obj in self.objects])
        for obj in self.objects:
            if raw_importance == 0:
                importance = 0
            else:
                importance = round(100 * (obj.raw_importance / raw_importance))
            obj.relative_importance = importance

    def update_intra_string_unhappiness(self):
        '''
        Calculate the average of the intra-string unhappiness of all the
        objects in the string.
        '''
        unhappiness = [obj.intra_string_unhappiness() for obj in self.objects]
        length = len(unhappiness)
        self.intra_string_unhappiness = sum(unhappiness) / length
