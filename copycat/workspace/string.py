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
from collections import defaultdict

import copycat.toolbox as toolbox
import copycat.slipnet as slipnet

def array(width, height=0):
    if height == 0:
        return [None for i in range(width)]
    return [[None for i in range(width)] for j in range(height)]

def dd():
    def d():
        return defaultdict(list)
    return defaultdict(d)

class String(object):
    """String is a letter string in the workspace.

    This could be the initial string, modified string or target string.
    Each object in a string has a unique string number that identifies
    it from other objects in the string.

    Attributes:
        highest_string_number: The highest number of any objects.
        name: The literal string this corresponds to.
        length: The number of letters in the string.
    """

    def __init__(self, workspace, string):
        """Initializes String."""
        self.workspace = workspace
        self.highest_string_number = -1
        self.name = string
        self.length = len(string)
        self.object_spaces = self.length
        self.letters = array(self.length)
        self._groups = {}
        self.object_positions = array(self.length)
        self.proposed_bonds = dd()
        self.left_right_bonds = defaultdict(dict)
        self.from_to_bonds = defaultdict(dict)
        self.proposed_groups = array(100, 100)
        self.number_of_bonds_to_scan_distribution = range(self.length)
        self.intra_string_unhappiness = 0
        self.bonds_to_scan_distribution = range(self.length)

    def random_object(self):
        '''
        Return a random object in the string.
        '''
        return random.choose(self.letters + self.groups)

    def random_letter(self):
        '''
        Return a random letter in the string.
        '''
        return random.choice(self.letters)
    
    def objects(self):
        letters = filter(lambda x: x != None, self.letters)
        return letters + self.get_groups()

    def get_groups(self):
        return self._groups.values()

    def choose_object(self, method):
        '''
        Return an object in the string chosen probabilistically,
        adjusted for temperature, according to the given method.
        '''
        objects = [obj for obj in self.objects() if obj]
        values = [getattr(obj, method) for obj in objects]
        values = self.workspace.temperature_adjusted_values(values)
        return objects[toolbox.weighted_index(values)]

    def choose_leftmost_object(self):
        '''
        Return one of the leftmost objects in the string, chosen
        probabilistically according to the relative importance of the
        lefmost objects in the string.
        '''
        leftmost_objects = []
        category = slipnet.plato_string_position_category
        for workspace_object in self.objects():
            if not workspace_object:
                continue
            descriptor = workspace_object.get_descriptor(category)
            if descriptor == slipnet.plato_leftmost:
                leftmost_objects.append(workspace_object)
        if leftmost_objects:
            values = [obj.relative_importance for obj in leftmost_objects]
            return toolbox.weighted_select(values, leftmost_objects)

    def get_bonds(self):
        """Return a list of all bonds in the string."""
        bonds = []
        for d in self.from_to_bonds.values():
            bonds.extend(d.values())
        for d in self.left_right_bonds.values():
            bonds.extend(d.values())
        return list(set(bonds))

    def non_string_spanning_objects(self):
        '''
        Return a list of all non string spanning objects in the string.
        '''
        non_string_spanning = []
        for workspace_object in self.objects():
            if not workspace_object:
                continue
            if workspace_object.spans_whole_string():
                non_string_spanning.append(workspace_object)
        return non_string_spanning

    def add_letter(self, letter):
        """Add a letter to the string."""
        self.highest_string_number += 1
        letter.string_number = self.highest_string_number
        position = letter.left_string_position
        self.letters[position] = letter
        right_object = self.object_positions[position]
        self.object_positions[position] = [letter, right_object]

    def objects_by_category(self, category):
        '''
        Return a list of objects in the string of the given object category.
        '''
        if category == self.state.slipnet.plato_letter:
            return self.letters
        elif category == self.state.slipnet.plato_group:
            return self.groups

    def add_proposed_bond(self, bond):
        """Add a proposed bond to the string."""
        from_number = bond.from_object.string_number
        to_number = bond.to_object.string_number
        self.proposed_bonds[from_number][to_number].append(bond)

    def remove_proposed_bond(self, bond):
        """Remove a proposed bond from the string."""
        from_number = bond.from_object.string_number
        to_number = bond.to_object.string_number
        self.proposed_bonds[from_number][to_number].remove(bond)

    def add_bond(self, bond):
        """Add a built bond to the string, storing sameness bonds twice,
        in both directions, since they have no direction."""
        left_number = bond.left_object.string_number
        right_number = bond.right_object.string_number
        self.left_right_bonds[left_number][right_number] = bond

        from_number = bond.from_object.string_number
        to_number = bond.to_object.string_number
        self.from_to_bonds[from_number][to_number] = bond

        if bond.bond_category == slipnet.plato_sameness:
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

        if bond.bond_category == slipnet.plato_sameness:
            self.left_right_bonds[right_number][left_number] = None
            self.from_to_bonds[to_number][from_number] = None

    def add_proposed_group(self, group):
        '''
        Add a proposed group to the string.
        '''
        left_number = group.left_object.string_number
        right_number = group.right_object.string_number
        if not self.proposed_groups[left_number][right_number]:
            self.proposed_groups[left_number][right_number] = []
        self.proposed_groups[left_number][right_number].append(group)

    def remove_proposed_group(self, group):
        '''
        Remove a proposed group from the string.
        '''
        left_number = group.left_object.string_number
        right_number = group.right_object.string_number
        self.proposed_groups[left_number][right_number].remove(group)

    def add_group(self, group):
        """Add a built group to the string."""
        self._groups[group.left_object.string_number] = group
        self.highest_string_number += 1
        group.string_number = self.highest_string_number
        self.object_positions[group.left_string_position].append(group)
        self.object_positions[group.right_string_position].append(group)

    def remove_group(self, group):
        '''
        Remove a built group from the string.
        '''
        del(self._groups[group.left_object.string_number])
        self.object_positions[group.left_string_position].remove(group)
        self.object_positions[group.right_string_position].remove(group)

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
        for b in self.get_bonds():
            if all([b.length() == bond.length(),
                    b.group_category == bond.bond_category,
                    b.direction_category == bond.direction_category]):
                return b

    def get_bond(self, from_object, to_object):
        '''
        Return the bond, if any, from the from_object to the to_object.
        '''
        from_number = from_object.string_number
        to_number = to_object.string_number
        return self.from_to_bonds[from_number][to_number]

    def is_group_present(self, group):
        """Return the existing group if the group exists in the string."""
        for g in self.get_groups():
            if all([g.length() == group.length(),
                    g.group_category == group.group_category,
                    g.direction_category == group.direction_category]):
                return g

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
        raw_importance = sum([obj.raw_importance for obj in self.objects() if obj])
        for obj in self.objects():
            if not obj:
                continue
            if raw_importance == 0:
                importance = 0
            else:
                importance = round(100 * (obj.raw_importance / float(raw_importance)))
            obj.relative_importance = importance

    def update_intra_string_unhappiness(self):
        '''
        Calculate the average of the intra-string unhappiness of all the
        objects in the string.
        '''
        unhappiness = [obj.intra_string_unhappiness for obj in self.objects() if obj]
        length = len(unhappiness)
        self.intra_string_unhappiness = sum(unhappiness) / length
