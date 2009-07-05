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

from copycat.coderack import Codelet

class GroupBuilder(Codelet):
    '''
    Tries to build the proposed group, fighting with any competitors.
    '''
    def run(self, coderack, slipnet, workspace):
        string = group.string

        # Make sure this group doesn't already exist.
        existing_group = string.group_present(group)
        if existing_group:
            for description in existing_group.descriptions:
                description.desriptor.buffer += self.activation
            for description in group.descriptions:
                if not existing_group.description_present(description):
                    new_description = Description(existing_group,
                                                  description.description_type,
                                                  description.descriptor)
                    self.build_description(new_description)
            string.delete_proposed_group(group)
            return

        # Make sure all bonds or their flipped versions are still there.
        all_bonds_exist = True
        for bond in group.bonds():
            flipped = bond.flipped_version()
            if not (string.bond_present(bond) or string.bond_present(flipped)):
                all_bonds_exist = False
                break
        if not all_bonds_exist:
            string.delete_proposed_group(group)
            return

        # Take the proposed group off the list of proposed groups.
        string.delete_proposed_group(group)

        # Check if any bonds need to be flipped and fight if so.
        bonds_to_flip = group.bonds_to_be_flipped()
        if bonds_to_flip:
            result = self.fight_it_out(group, group.letter_span,
                                       bonds_to_flip, 1)
            if not result:
                return

        # Fight any incompatible groups.
        incompatible_groups = group.incompatible_groups()
        for incompatible_group in incompatible_groups:
            if (group.group_category == incompatible_group.group_category) and\
            (group.direction_category == incompatible_group.direction_category):
                group_weight = group.length
                incompatible_weight = incompatible_group.length
            else:
                group_weight = 1
                incompatible_weight = 1
            result = self.fight_it_out(group, group_weight,
                                       incompatible_group, incompatible_weight)
            if not result:
                return

        # Fight any incompatible correspondences.
        incompatible_correspondences = group.incompatible_correspondences()
        if group.direction_category and incompatible_correspondences:
            result = self.fight_it_out(group, 1,
                                       incompatible_correspondences, 1)
            if not result:
                return

        # Break incompatible groups.
        for incompatible_group in incompatible_groups:
            self.break_group(incompatible_group)

        # Flip any bonds that need it and replace any bonds that were rebuilt.
        new_bonds = []
        if bonds_to_flip:
            for bond in group.bonds():
                flipped_bond = string.bond_present(bond.flipped_version())
                if flipped_bond:
                    self.break_bond(flipped_bond)
                    self.build_bond(bond)
                    new_bonds.append(bond)
                else:
                    existing_bond = string.bond_present(bond)
                    if existing_bond != bond:
                        new_bonds.append(existing_bond)
                    else:
                        new_bonds.append(bond)
        # FIXME: Actually replacing the bonds is not in the copycat source.
        #group.bonds = new_bonds

        # Break incompatible correspondences.
        for incompatible_correspondence in incompatible_correspondences:
            self.break_correspondence(incompatible_correspondence)

        # Build the group.
        self.build_group(group)

class GroupStrengthTester(Codelet):
    '''
    Calculates the proposed group's strength and probabilistically decides
    whether or not to post a group builder codelet with urgency a function
    of the strength.
    '''
    def run(self, coderack, slipnet, workspace):
        # Calculate the group's stength.
        group.update_strength_values()
        stength = group.total_stength()

        # Decide whether or not to post the group builder codelet.
        probability = strength / 100.0
        probability = self.temperature_adjusted_probability(probability)
        if not util.flip_coin(probability):
            group.string.delete_proposed_group(group)
            return

        # Add some activation to descriptions.
        group.bond_category.buffer += self.activation
        if group.direction_category:
            group.direction_category.buffer += self.activation

        # Set group's proposal level.
        group.proposal_level = 2

        return Codelet('group_builder', (group,), strength)

class GroupTopDownCategoryScout(Codelet):
    '''
    Chooses an object, a direction to scan in, and a number of bonds to
    scan in that direction. The direction category of the group is the
    direction of the first bond scanned. If there are no bonds and the
    group category is not plato-same-group, the chooses a direction
    category probabilistically as a function of global and local relevance.
    Scans until no more bonds of the necessary type and direction are
    found. If possible, makes a proposed group of the given type of the
    objects scanned and posts a group length tester codelet with urgency
    a function of the degree of association of bonds of the given bond
    category.
    '''
    def run(self, coderack, slipnet, workspace):
        bond_category = category.related_node('plato_bond_category')

        # Choose a string based on local bond category relevance.
        i_string = self.initial_string
        i_relevance = i_string.local_bond_category_relevance(bond_category)
        i_unhappiness = i_string.intra_string_unhappiness()
        t_string = self.target_string
        t_relevance = t_string.local_bond_category_relevance(bond_category)
        t_unhappiness = t_string.intra_string_unhappiness()
        choices = [i_string, t_string]
        weights = [round(util.average(i_relevance, i_unhappiness)),
                   round(util.average(t_relevance, t_unhappniess))]
        string = choices[util.select_list_position(weights)]

        # Choose an object by intra string salience.
        object = string.choose_object('intra_string_salience')
        if object.spans_whole_string():
            return

        # Choose a direction in which to scan.
        # FIXME: no access to slipnodes.
        if object.leftmost_in_string():
            direction = plato_right
        elif object.rightmost_in_string():
            direction = plato_left
        else:
            activations = [plato_left.activation, plato_right.activation]
            index = util.select_list_position(activations)
            direction = [plato_left, plato_right][index]

        # Choose the number of bonds to scan.
        number = util.select_list_position(string.bonds_to_scan_distribution())

        # Get the first bond in that direction.
        if direction == plato_left:
            bond = object.left_bond
        else:
            bond = object.right_bond

        if (not bond) or (bond.bond_category != bond_category):
            if isinstance(object, Group):
                return
            objects = [object]
            bonds = []
            if category == plato_samegroup:
                possible_single_letter_group_direction = None
            else:
                choices = [plato_left, plato_right]
                weights = [node.local_descriptor_support(string, plato_group) \
                            for node in choices]
                index = util.select_list_position(weights)
                possible_single_letter_group_direction = choices[index]

            possible_single_letter_group = Group(category,
                                                 possible_single_letter_direction,
                                                 object, object, objects, bonds)

            probility = possible_possible_single_letter_group.single_letter_group_probability()
            if util.flip_coin(probability):
                return self.propose_group(objects, bonds, category,
                                          possible_single_letter_group_direction)
        
        direction_category = bond.direction_category
        facet = bond.bond_facet
        opposite_bond_category = bond_category.get_related_node(plato_opposite)
        if direction_category:
            opposite_direction_category = direction_category.get_related_node(plato_opposite)

        # Get objects and bonds.
        objects = [bond.left_object, bond.right_object]
        bonds = [bond]
        next_bond = bond
        for i in range(2, number):
            bond_to_add = None
            if direction == plato_left:
                next_bond = next_bond.choose_left_neighbor()
                if not next_bond:
                    break
                else:
                    next_object = next_bond.left_object
            else:
                next_bond = next_bond.choose_right_neighbor()
                if not next_bond:
                    break
                else:
                    next_object = next_bond.right_object

            # Decide whether or not to add bond.
            if not next_bond:
                bond_to_add = None
            elif (next_bond.bond_category == bond_category) and \
                 (next_bond.direction_category == direction_category) and\
                 (next_bond.bond_facet == facet):
                bond_to_add = next_bond
            elif (next_bond.bond_category == opposite_bond_category) and \
                 (next_bond.direction_category == opposite_direction_category) and \
                 (next_bond.bond_facet == facet):
                bond_to_add = next_bond.flipped_version()

            if bond_to_add:
                objects.append(next_object)
                bonds.append(bond_to_add)
            else:
                break

        # Propose the group.
        return self.propose_group(objects, bonds, category, direction_category)

class GroupTopDownDirectionScout(Codelet):
    '''
    Chooses an object, a direction to scan in, and a number of bonds to
    scan in that direction. The category of the group is the associated
    group category of the first bond scanned. Scans until no more bonds of
    the necessary type and direction are found. If possible, makes a
    proposed group of the given direction out of the objects scanned and
    posts a group strength tester codelet with urgency a function of the
    degree of association of bonds of the given bond category.
    '''
    def run(self, coderack, slipnet, workspace):
        # Choose a string based on local direction category relevance.
        i_string = self.initial_string
        i_relevance = i_string.local_direction_category_relevance(category)
        i_unhappiness = i_string.intra_string_unhappiness()
        t_string = self.target_string
        t_relevance = t_string.local_direction_category_relevance(category)
        t_unhappiness = t_string.intra_string_unhappiness()
        choices = [i_string, t_string]
        weights = [round(util.average(i_relevance, i_unhappiness)),
                   round(util.average(t_relevance, t_unhappniess))]
        string = choices[util.select_list_position(weights)]

        # Choose an object by intra string salience.
        object = string.choose_object('intra_string_salience')
        if object.spans_whole_string():
            return

        # Choose a direction in which to scan.
        # FIXME: no access to slipnodes.
        if object.leftmost_in_string():
            direction = plato_right
        elif object.rightmost_in_string():
            direction = plato_left
        else:
            activations = [plato_left.activation, plato_right.activation]
            index = util.select_list_position(activations)
            direction = [plato_left, plato_right][index]

        # Choose the number of bonds to scan.
        number = util.select_list_position(string.bonds_to_scan_distribution())

        # Get the first bond in that direction.
        if direction == plato_left:
            bond = object.left_bond
        else:
            bond = object.right_bond

        if (not bond) or (bond.direction_category != category):
            return

        bond_category = bond.bond_category
        facet = bond.bond_facet

        opposite_bond_category = bond_category.get_related_node(plato_opposite)
        opposite_category = category.get_related_node(plato_opposite)

        # Get objects and bonds.
        objects = [bond.left_object, bond.right_object]
        bonds = [bond]
        next_bond = bond
        for i in range(2, number):
            bond_to_add = None
            if direction == plato_left:
                next_bond = next_bond.choose_left_neighbor()
                if not next_bond:
                    break
                else:
                    next_object = next_bond.left_object
            else:
                next_bond = next_bond.choose_right_neighbor()
                if not next_bond:
                    break
                else:
                    next_object = next_bond.right_object

            # Decide whether or not to add bond.
            if not next_bond:
                bond_to_add = None
            elif (next_bond.bond_category == bond_category) and \
                 (next_bond.direction_category == direction_category) and \
                 (next_bond.bond_facet == facet):
                bond_to_add = next_bond
            elif (next_bond.bond_category == opposite_bond_category) and \
                 (next_bond.direction_category == opposite_direction_category) and \
                 (next_bond.bond_facet == facet):
                bond_to_add = next_bond.flipped_version()

            if bond_to_add:
                objects.append(next_object)
                bonds.append(bond_to_add)
            else:
                break

        group_category = bond_category.get_related_node(plato_group_category)

        return self.propose_group(objects, bonds, group_category, category)

class GroupWholeStringScout(Codelet):
    '''
    Tries to make a group out of the entire string. If possible, makes a
    proposed string spanning group and posts a group strength tester
    codelet with urgency a function of the degree of association of bonds
    of the given bond category.
    '''
    def run(self, coderack, slipnet, workspace):
        string = self.random_string()

        # Choose a salient leftmost object and get objects and bonds.
        if not string.bonds():
            return
        left_object = string.choose_leftmost_object()
        next_bond = left_object.right_bond
        objects = [left_object]
        bonds = []
        while next_bond:
            bonds.append(next_bond)
            next_object = next_bond.right_object
            objects.append(next_object)
            next_bond = next_object.right_bond
        right_object = next_object
        if (not bonds) or (not right_object.rightmost_in_string()):
            return

        # Choose a random bond and try making a group based on it.
        bond = random.choice(bonds)
        bond_category = bond.bond_category
        direction_category = bond.direction_category
        facet = bond.bond_facet
        bonds = self.possible_group_bonds(bond_category, direction_category,
                                          facet, bonds)
        if not bonds:
            return

        group_category = bond_category.related_node(plato_group_category)

        return self.propose_group(objects, bonds, group_category,
                                  direction_category)
