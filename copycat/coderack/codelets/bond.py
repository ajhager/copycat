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
from copycat.coderack import Codelet
import copycat.slipnet as nodes

class BondBottomUpScout(Codelet):
    '''
    Choose an object and a neighbor of that object probabilistically by
    intra string salience. Choose a bond facet probabilistically by
    relevance in the string. Check if there is a bond between the two
    descriptors of this facet. Post a bond strength tester codelet with
    urgency a function of the degree of association of bonds of the bond
    category.
    '''
    structure_category = 'bond'
    def run(self, coderack, slipnet, workspace):
        from_object = workspace.choose_object('intra_string_salience')
        to_object = from_object.choose_neighbor()
        if to_object == None:
            return

        facet = workspace.choose_bond_facet(from_object, to_object)
        if facet == None:
            return

        from_descriptor = from_object.descriptor(facet)
        to_descriptor = to_object.descriptor(facet)
        if from_descriptor == None or to_descriptor == None:
            return

        category = from_descriptor.bond_category(to_descriptor)
        if category == None:
            return

        return workspace.propose_bond(from_object, to_object, category, facet,
                                      from_descriptor, to_descriptor)

class BondBuilder(Codelet):
    '''
    Attempts to build the proposed bond, fighting with competitiors if
    necessary.
    '''
    structure_category = 'bond'
    def run(self, coderack, slipnet, workspace):
        bond = self.args[0]
        string = bond.string
        from_object = bond.from_object
        to_object = bond.to_object

        objects = workspace.objects()
        if (from_object not in objects) or (to_object not in objects):
            return

        existing_bond = string.is_bond_present(bond)
        if existing_bond:
            existing_bond.bond_category.activation_buffer += self.activation
            direction_category = existing_bond.direction_category
            if direction_category:
                direction_category.activation_buffer += workspace.activation
            string.delete_proposed_bond(bond)
            return

        string.delete_proposed_bond(bond)

        incompatible_bonds = bond.incompatible_bonds()
        if incompatible_bonds:
            if not self.fight_it_out(bond, 1, incompatible_bonds, 1):
                return

        incompatible_groups = from_object.common_groups(to_object)
        if incompatible_groups:
            if  not self.fight_it_out(bond, 1, incompatible_groups,
                max([group.letter_span() for group in incompatible_groups])):
                return

        if bond.direction_category and (bond.is_leftmost_in_string() or \
                                        bond.is_rightmost_in_string()):
            incompatible_correspondences = bond.incompatible_correspondences()
            if incompatible_corresondences:
                if not self.fight_it_out(bond, 2,
                                         incompatible_correspondences, 3):
                    return

        if incompatible_bonds:
            for bond in incompatible_bonds:
                self.break_bond(bond)

        if incompatible_groups:
            for group in incompatible_groups:
                self.break_group(group)

        if incompatible_correspondences:
            for correrspondence in incompatible_correspondences:
                self.break_correspondence(correspondence)

        return workspace.build_bond(bond)

class BondStrengthTester(Codelet):
    '''
    Calculates the proposed bond's strength and decides probabilistically
    wheither or not to post a bond builder codelet with urgency as a
    function of the strength.
    '''
    structure_category = 'bond'
    def run(self, coderack, slipnet, workspace):
        # Update the strength values for the bond.
        bond.update_strength_values()
        strength = description.total_strength()

        # Decide whether or not to post the bond builder codelet.
        probability = strength / 100.0
        probability = self.temperature_adjusted_probability(probability)
        if not toolbox.flip_coin(probability):
            bond.string.delete_proposed_bond(bond)
            return

        # Add activation to some relevant descriptions.
        bond.from_object_descriptor.buffer += self.activation
        bond.to_object_descriptor.buffer += self.activation
        bond.bond_facet.buffer += self.activation

        # Change bond's proposal level.
        bond.proposal_level = 2

        return [Codelet('bond_builder', (bond,), strength)]

class BondTopDownCategoryScout(Codelet):
    '''
    Chooses a string probabilistically by the relevance of the category in
    the string and the string's unhappiness. Chooses an object and a
    neighbor of the object in the string probabilistically by instra
    string salience. Chooses a bond facet probabilistically by relevance
    in the string. Checks if there is a bond of the category between the
    two descriptors of the facet, posting a bond strength tester codelet
    with urgency a function of the degree of association of bonds of the
    category.
    '''
    structure_category = 'bond'
    def run(self, coderack, slipnet, workspace):
        category = self.arguments[0]
        # Choose a string.
        initial_string = workspace.initial_string
        target_string = workspace.target_string
        i_relevance = initial_string.local_bond_category_relevance(category)
        t_relevance = target_string.local_bond_category_relevance(category)
        i_unhappiness = initial_string.intra_string_unhappiness
        t_unhappiness = target_string.intra_string_unhappiness
        values = [round(toolbox.average(i_relevance, i_unhappiness)),
                  round(toolbox.average(t_relevance, t_unhappiness))]
        string = toolbox.weighted_select(values, [initial_string, target_string])

        # Choose an object and neighbor.
        obj = string.choose_object('intra_string_salience')
        neighbor = obj.choose_neighbor()
        if not neighbor:
            return

        # Choose bond facet.
        facet = workspace.choose_bond_facet(obj, neighbor)
        if not facet:
            return

        # Get the descriptors of the facet if they exist.
        object_descriptor = obj.descriptor(facet)
        neighbor_descriptor = neighbor.descriptor(facet)
        if (not object_descriptor) or (not neighbor_descriptor):
            return

        # Check for a possible bond.
        if object_descriptor.bond_category(neighbor_descriptor) == category:
            from_object = obj
            to_object = neighbor
            from_descriptor = object_descriptor
            to_descriptor = neighbor_descriptor
        elif neighbor_descriptor.bond_category(object_descriptor) == category:
            from_object = neighbor
            to_object = obj
            from_descriptor = neighbor_descriptor
            to_descriptor = object_descriptor
        else:
            return

        # Propose the bond.
        return self.propose_bond(from_object, to_object, category, facet,
                                 from_descriptor, to_descriptor)

class BondTopDownDirectionScout(Codelet):
    '''
    Chooses a string probabilistically by the relevance of the direction
    category in the string and the string's unhappiness. Chooses an object
    in the string probabilisitically by intra string salience. Chooses a
    neighbor of the object in the given direction. Chooses a bond facet
    probabilistically by relevance in the string. Checks if there is a
    bond of the given direction between the two descriptors of the facet,
    posting a bond strength tester codelet with urgency a function of the
    degree of association of bonds of the bond category.
    '''
    structure_category = 'bond'
    def run(self, coderack, slipnet, workspace):
        category = self.arguments[0]
        # Choose a string.
        initial_string = workspace.initial_string
        target_string = workspace.target_string
        i_relevance = initial_string.local_direction_category_relevance(category)
        t_relevance = target_string.local_direction_category_relevance(category)
        i_unhappiness = initial_string.intra_string_unhappiness
        t_unhappiness = target_string.intra_string_unhappiness
        values = [round(toolbox.average(i_relevance, i_unhappiness)),
                  round(toolbox.average(t_relevance, t_unhappiness))]
        string = toolbox.weighted_select(values, [initial_string, target_string])

        # Choose an object and neighbor.
        obj = string.choose_object('intra_string_salience')
        if category == nodes.plato_left:
            neighbor = obj.choose_left_neighbor()
        elif category == nodes.plato_right:
            neighbor = obj.choose_right_neighbor()
        if not neighbor:
            return

        # Choose bond facet.
        facet = workspace.choose_bond_facet(obj, neighbor)
        if not facet:
            return
        
        # Get the descriptors of the facet if they exist.
        object_descriptor = object.descriptor(facet)
        neighbor_descriptor = neighbor.descriptor(facet)
        if (not object_descriptor) or (not neighbor_descriptor):
            return

        # Check for a possible bond.
        bond_category = from_descriptor.bond_category(to_descriptor)
        if (not bond_category) or (not bond_category.directed):
            return

        # Propose the bond.
        return workspace.propose_bond(from_object, to_object,
                                      bond_category, facet,
                                 from_descriptor, to_descriptor)
