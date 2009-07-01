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
    def __init__(self, state, from_object, to_object, bond_category, bond_facet,
                 from_object_descriptor, to_object_descriptor):
        super(Bond, self).__init__(state)
        self.from_object = from_object
        self.to_object = to_object
        self.structure_category = 'bond'
        self.bond_category = bond_category
        self.direction_category = None
        if bond_category.name != 'sameness':
            if from_object.left_string_position < to_object.left_string_position:
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

    def calculate_internal_strength(self):
        '''
        Bonds between objects of the same type are stronger than bonds between
        different types.
        '''
        if self.from_object.flavor_type == self.to_object.flavor_type:
            member_compatibility_factor = 1
        else:
            member_compatibility_factor = .7

        if self.bond_facet == self.slipnet.plato_letter_category:
            bond_facet_factor = 1
        else:
            bond_facet_factor = .7

        degree_of_associtation = self.bond_category.bond_degree_of_association()

        return min(100, round(member_compaitibility_factor * \
                              bond_facet_factor * \
                              degree_of_association))

    def calculate_external_strength(self):
        return self.local_support()

    def importance(self):
        '''
        Sameness bonds are more important than other bonds of other categories.
        '''
        if self.bond_category == self.slipnet.plato_sameness:
            return 100
        else:
            return 50

    def happiness(self):
        if self.group:
            return self.goup.total_strength()
        else:
            return 0

    def unhappiness(self):
        return round(util.average([self.importance(), self.happiness()]))

    def salience(self):
        return round(util.average([self.importance(), self.unhappiness()]))

    def number_of_local_supporting_bonds(self):
        '''
        Return the number of supporting bonds in the given bond's string.
        Looks at all the other bonds in the string, counting bonds of the same
        bond category and direction category.  Does not take distance into
        account; all qualifying bonds in the string are counted the same.
        '''
        number_of_supporting_bonds = 0
        bonds = self.string.bonds
        other_bonds = bonds.remove(self)
        for other_bond in other_bonds:
            if self.workspace.letter_distance(self.left_object,
                                              other_bond.left_object) != 0 and \
               self.workspace.letter_distance(right_object,
                                              other_bond.right_object) != 0 and \
               other_bond.bond_category == self.bond_category and \
               other_bond.direction_category == self.direction_category:
                number_of_supporting_bonds += 1
        return number_of_supporting_bonds

    def local_density(self):
        '''
        Return a rough measure of the density in the string of bonds of the
        same bond category and direction category as the given bond. This
        method is used in calculating the external strength of a bond.
        '''
        slot_sum = 0
        support_sum = 0

        last_object = self.left_object
        next_object = self.left_object.choose_left_neighbor()
        while next_object != None:
            slot_sum += 1
            x = next_object.string_number
            y = last_object.string_number
            next_bond = self.string.left_right_bonds[x][y]
            if next_bond:
                if next_bond.bond_category == self.bond_category and \
                   next_bond.direction_category == self.direction_category:
                    suppport_sum += 1
            last_object = next_object
            next_object = next_object.choose_left_neighbor()

        last_object = self.right_object
        next_object = right_object.choose_right_neighbor()
        while next_object != None:
            slot_sum += 1
            x = next_object.string_number
            y = last_object.string_number
            next_bond = self.string.left_right_bonds[x][y]
            if next_bond:
                if next_bond.bond_category == self.bond_category and \
                   next_bond.direction_category == self.direction_category:
                    support_sum += 1
            last_object = next_object
            next_object = next_object.choose_right_neighbor()

        if slot_sum == 0:
            return 100
        else:
            return round(100 * (support_sum / float(slot_sum)))

    def local_support(self):
        number = self.number_of_local_supporting_bonds()
        if number == 0:
            return 0
        density = self.local_density()
        adjusted_density = 100 * (math.sqrt(density / 100.0))
        number_factor = min(1, .6**(1 / number**3))
        return round(adjusted_density * number_factor)

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
        '''
        Return one of the left neighbors of the bond probabilistically by
        salience.
        '''
        if self.is_leftmost_in_string():
            return
        left_neighbors = []
        for obj in self.left_object.all_left_neighbors():
            possible_neighbor = self.string.left_right_bonds[obj.string_number][self.left_object.string_number]
            if possible_neighbor:
                left_neighbors.append(possible_neighbor)
        saliences = [obj.salience for obj in left_neighbors]
        return util.weight_select(saliences, left_neighbors)

    def choose_right_neighbor(self):
        '''
        Return one of the right neighbors of the bond probabilistically by
        salience.
        '''
        if self.is_rightmost_in_string():
            return
        right_neighbors = []
        for obj in self.right_object.all_right_neighbors():
            possible_neighbor = self.string.left_right_bonds[self.right_object.string_number][obj.string_number]
            if possible_neighbor:
                right_neighbors.append(possible_neighbor)
        saliences = [obj.salience for obj in right_neighbors]
        return util.weight_select(saliences, right_neighbors)

    def get_incompatible_bonds(self):
        '''
        Return the bonds that are incompatible with the give bond, i.e. any
        bonds involving one or both of the same two objects bonded by this
        bond.
        '''
        return list(set(util.flatten([self.left_object.right_bond,
                                      self.right_object.left_bond])))

    def get_incompatible_correspondences(self):
        '''
        Return the correspondences that are incompatible with this bond. This
        only applies to the directed bonds and to correspondences between
        objects at the edges of strings. E.g., in "abc -> abd, pqrs -> ?, if
        there is a correspondence between the "a" and the "p" (with concept
        mapping "leftmost -> leftmost"), and a right going succesor bond from
        the "a" to the "b" in "abc", then the correspondence will be
        incompatible with a left going predecessor bond from the "q" to the
        "p" in "pqrs", because the correspondence would then imply both
        "leftmost -> leftmost" (the letters) and "right -> left (the bonds.)
        '''
        incompatible_correspondences = []

        if self.is_leftmost_in_string():
            correspondence = self.left_object.correspondence
            if not correspondence:
                return
            for cm in correspondence.concept_mappings:
                if cm.description_type1 == self.slipnet.plato_string_position_category:
                    string_position_category_concept_mapping =  cm
            if not string_position_cateogry_conept_mapping:
                return
            other_object = correspondence.other_object(self.left_object)
            if other_object.is_leftmost_in_string():
                other_bond = other_object.right_bond
            elif other_object.is_rightmost_in_string():
                other_bond = other_object.left_bond
            else:
                return
            if not other_bond:
                return
            if not other_bond.direction_category:
                return
            bond_concept_mapping = Mapping(self.slipnet.plato_direction_category,
                                           self.slipnet.plato_direction_category,
                                           self.direction_category,
                                           other_bond.direction_category,
                                           None, None)
            if bond_concept_mapping.is_incompatible_concept_mapping(string_position_category_concept_mapping):
                incompatible_correspondences.append(correspondence)

        if self.is_rightmost_in_string():
            correspondence = self.right_object.correspondence
            if not correspondence:
                return
            for cm in correspondence.concept_mappings:
                if cm.description_type1 == self.slipnet.plato_string_position_category:
                    string_position_category_concept_mapping =  cm
            if not string_position_cateogry_conept_mapping:
                return
            other_object = correspondence.other_object(self.right_object)
            if other_object.is_leftmost_in_string():
                other_bond = other_object.right_bond
            elif other_object.is_rightmost_in_string():
                other_bond = other_object.left_bond
            else:
                return
            if not other_bond:
                return
            if not other_bond.direction_category:
                return
            bond_concept_mapping = Mapping(self.slipnet.plato_direction_category,
                                           self.slipnet.plato_direction_category,
                                           self.direction_category,
                                           other_bond.direction_category,
                                           None, None)
            if bond_concept_mapping.is_incompatible_concept_mapping(string_position_category_concept_mapping):
                incompatible_correspondences.append(correspondence)

        return incompatible_correspondences

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

    def choose_left_neighbor(self):
        '''
        Return one of the left neighbors of the bond, chosen probabilistically
        by salience.
        '''
        if self.is_leftmost_in_string():
            return
