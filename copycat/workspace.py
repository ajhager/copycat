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
import random

import util
from string import String
from coderack import Codelet

class Workspace(object):
    '''
    First we construct workspace strings out of the puzzle strings passed in.
    The temperature is initialized and unclamped.  The workspace contains a
    list of replacements (mappings from the initial string to the modified
    string), correspondences (mappings from the inital string to the target
    string), and proposed correspondences.  The workspace also keeps track
    of whether we have a rule and translated rule and what they are if so.
    '''
    def __init__(self, initial, modified, target):
        self.initial_string = String(initial)
        self.modified_string = String(modified)
        self.target_string = String(target)
        self.answer_string = None

        self.activation = 0
        self.temperature = 0
        self.clamp_temperature = False

        self._replacements = []
        self._correspondences = []
        self._proposed_correspondences = []

        self.rule = None
        self.translated_rule = None

        self.snag_object = None
        self.snag_condition = None

    def update(self):
        '''
        Update various values of the structures, objects, and strings in the
        workspace. Check to see if the snag conditions have been met so that
        everything can go back to normal.  Finally, update the temperature.
        '''
        for structure in self.structures():
            structure.update_strength_values()
        for object_ in self.objects():
            object_.update_object_values()

        self.initial_string.update_relative_importances()
        self.target_string.update_relative_importances()
        self.initial_string.update_intra_string_unhappiness()
        self.target_string.update_intra_string_unhappiness()

        self.test_snag_condition()

        self.update_temperature()

    def set_snag_condition(self):
        '''
        Called when dealing with a snag.  This is most definitely going to
        need some work as we get a better idea of snags.
        '''
        self.snag_count += 1
        self.last_snag_time = self.coderack.codelets_run
        self.snag_structures = self.workspace.structures()
        self.translated_rule = None
        self.answer_string = None
        self.snag_conition = True
        self.temperature = 100
        self.clamp_temperature = True
        for description in self.snag_object.descriptions():
            description.descriptor.set_clamp(True)
        self.snag_object.set_clamp_salience(True)

    def test_snag_condition(self):
        '''
        If the program is dealing with a snag, then see if any new structures
        have been made.  If so, see if the snag condition should be ended.
        This will also need work as we learn more about handling snags.
        '''
        return # need to revisit this
        if self.snag_object and self.snag_condition:
            new_structures = []
            for structure in self.structures:
                if snag_structures(structure):
                    new_structures.append(structure)
            
            unclamp_probability = 0
            if new_structures:
                unclamp_probability = max([structure.total_strength() \
                    for structure in new_structures]) / 100.

            if util.flip_coin(unclamp_probability):
                self.snag_condition = None
                self.clamp_temperature = False
                for description in self.snag_object.descriptions:
                    description.set_clamp(False)
                self.snag_object.set_clamp_salience(False)

    def choose_object(self, method):
        '''
        Returns an object chosen by temperature adjusted probability according
        to the given method.
        '''
        objects = self.objects()
        values = [getattr(object, method)() for object in objects]
        values = self.temperature_adjusted_values(values)
        index = util.select_list_position(values)]
        return objects[index]

    def delete_proposed_structure(self, structure):
        pass

    def update_temperature(self):
        '''
        Updates the temperature, which is a function of the average total
        unhappiness of objects on the blackboard (weighted by importance)
        and the weakness of the rule.
        '''
        if not self.clamp_temperature:
            rule_weakness = 100
            if self.rule:
                rule_weakness = 100 - self.rule.total_strength()

            self.temperature = util.weighted_average(
                    (self.total_unhappiness(), 8),
                    (rule_weakness, 2))

    def post_codelet_probability(self, category):
        '''
        Return a probability to use when deciding to post codelets searching
        for this type of structure.
        '''
        probability = 0
        if category == 'description':
            probability = math.sqrt(self.temperature) / 100.
        elif category in ['bond', 'group']:
            probability = self.intra_string_unhappiness()
        elif category == 'replacement' and self.unreplaced_objects():
            probability = 100
        elif category == 'correspondence':
            probability = self.inter_string_unhappiness()
        elif category == 'rule' and self.rule:
            probability = self.rule.total_weakness()
        elif category == 'rule':
            probability = 100
        elif category == 'translated_rule' and self.rule:
            probability = 100
        
        return probability / 100.

    def post_codelet_number(self, category):
        '''
        Return the number of codelets looking for the structure given by
        category that should be posted.
        '''
        number = 0
        case = {'few': 1, 'medium': 2, 'many': 3}
        if category == 'description':
            number = 1
        elif category == 'bond':
            number = case[self.rough_number_of_unrelated_objects()]
        elif category == 'group' and self.bonds:
            number = case[self.rough_number_of_ungrouped_objects()]
        elif category == 'replacement' and self.rule:
            number = case[self.rough_number_of_unreplaced_objects()]
        elif category == 'correspondence':
            number = case[self.rough_number_of_uncorresponding_objects()]
        elif category == 'rule':
            number = 2
        elif category == 'translated_rule' and self.rule:
            number = 1
        
        return number

    def bottom_up_codelets(self):
        '''
        Returns various bottom up codelets, with urgency and number based on
        how many of each type of codelet is needed.
        '''
        def test(category, name, urgency):
            '''
            Based on the category sent in, test for the probability for the
            codelet related to that category to be posted.  If the test does
            succeed, the number of each codelet to return is determined based
            on category.
            '''
            codelets = []
            probability = self.post_codelet_probability(category)
            number = self.post_codelet_number(category)
            if util.flip_coin(probability):
                for i in range(number):
                    codelets.append(Codelet(name, urgency=urgency))
            return codelets

        return \
        test('description', 'bottom_up_description_scout', 30) +\
        test('bond', 'bottom_up_bond_scout', 30) +\
        test('group', 'group_scout__whole_string', 30) +\
        test('replacement', 'replacement_finder', 30) +\
        test('correspondence', 'bottom_up_correspondence_scout', 30) +\
        test('correspondence', 'important_object_correspondence_scout', 30) +\
        test('rule', 'rule_scout', 30) +\
        test('translator_rule', 'rule_translator',
                30 if self.temperature > 25 else 60) +\
        [Codelet('breaker', urgency=0)]

    def objects(self):
        return self.initial_string.objects() + self.target_string.objects()

    def unreplaced_objects(self):
        unreplaced_objects = []
        for letter in self.initial_string.letters:
            if letter.replacement == None:
                unreplaced_objects.append(letter)
        return unreplaced_objects

    def structures(self):
        structures = self.bonds() + self.groups() + self.correspondences()
        if self.rule:
            return structures + [self.rule]
        else:
            return structures

    def bonds(self):
        return self.initial_string.bonds() + self.target_string.bonds()

    def groups(self):
        return self.initial_string.groups() + self.target_string.groups()

    def correspondences(self):
        return util.flatten(self._correspondences)

    def unrelated_objects(self):
        unrelated_objects = []
        for object in self.objects():
            if not object.spans_whole_string() and object.group == None:
                number_of_bonds = len(object.incoming_bonds) + len(object.outgoing_bonds)
                if object.is_leftmost_in_string() or object.is_rightmost_in_string():
                    if number_of_bonds == 0: unrelated_objects.append(object)
                else:
                    if number_of_bonds < 2: unrelated_objects.append(object)
        return unrelated_objects

    def ungrouped_objects(self):
        ungrouped_objects = []
        for object in self.objects():
            if not object.spans_whole_string() and object.group == None:
                ungrouped_objects.append(object)
        return ungrouped_objects

    def ungrouped_bonds(self):
        ungrouped_bonds = []
        for bond in self.bonds():
            if bond.from_object.group == None or bond.to_object.group == None:
                ungrouped_bonds.append(bond)
        return ungrouped_bonds

    def unreplaced_objects(self):
        unreplaced_objects = []
        for letter in self.initial_string.letters:
            if letter.replacement == None:
                unreplaced_objects.append(letter)
        return unreplaced_objects

    def uncorresponding_objects(self):
        uncorresponding_objects = []
        for object in self.objects():
            if object.correspondence == None:
                uncorresponding_objects.append(object)
        return uncorresponding_objects

    def rough_number_of_unrelated_objects(self):
        number_of_unrelated_objects = len(self.unrelated_objects())
        if number_of_unrelated_objects < util.blur(2):
            return 'few'
        elif number_of_unrelated_objects < until.blur(4):
            return 'medium'
        else:
            return 'many'

    def rough_number_of_ungrouped_objects(self):
        number_of_ungrouped_objects = len(self.ungrouped_objects())
        if number_of_ungrouped_objects < util.blur(2):
            return 'few'
        elif number_of_ungrouped_objects < until.blur(4):
            return 'medium'
        else:
            return 'many'

    def rough_number_of_unreplaced_objects(self):
        number_of_unreplaced_objects = len(self.unreplaced_objects())
        if number_of_unreplaced_objects < util.blur(2):
            return 'few'
        elif number_of_unreplaced_objects < until.blur(4):
            return 'medium'
        else:
            return 'many'

    def rough_number_of_uncorresponding_objects(self):
        number_of_uncorresponding_objects = len(self.uncorresponding_objects())
        if number_of_uncorresponding_objects < util.blur(2):
            return 'few'
        elif number_of_uncorresponding_objects < until.blur(4):
            return 'medium'
        else:
            return 'many'

    def total_unhappiness(self):
        # TODO: implement.
        return 30

    def intra_string_unhappiness(self):
        # TODO: implement.
        return 30

    def inter_string_unhappiness(self):
        # TODO: implement.
        return 30

    # Codelet methods.
    def answer_builder(self):
        print 'Answer Builder'

    def bond_builder(self, bond):
        '''
        Attempts to build the proposed bond, fighting with competitiors if
        necessary.
        '''
        string = bond.string
        from_object = bond.from_object
        to_object = bond.to_object

        # Make sure these objects still exist.
        objects = self.objects()
        if (from_object not in objects) or (to_object not in objects):
            return

        # Make sure the bond does not exist.
        existing_bond = string.is_bond_present(bond)
        if existing_bond:
            existing_bond.bond_category.buffer += self.activation
            if existing_bond.direction_category:
                existing_bond.direction_category.buffer += self.activation
            string.delete_proposed_bond(bond)
            return

        # Remove the proposed bond from the list of proposed bonds.
        string.delete_proposed_bond(bond)

        # Fight any incompatible bonds.
        incompatible_bonds = bond.incompatible_bonds()
        if incompatible_bonds:
            if not self.fight_it_out(bond, 1, incompatible_bonds, 1):

        # Try to break any groups shared by from_object and to_object.
        incompatible_groups = from_object.common_groups(to_object)
        if incompatible_groups:
            if  not self.fight_it_out(bond, 1, incompatible_groups,
                max([group.letter_span() for group in incompatible_groups])):
                return

        # Try to break any incompatible correspondences.
        if bond.direction_category and (bond.is_leftmost_in_string() or \
                                        bond.is_rightmost_in_string()):
            incompatible_correspondences = bond.incompatible_correspondences()
            if incompatible_corresondences:
                if not self.fight_it_out(bond, 2,
                                         incompatible_correspondences, 3):
                    return

        # Break incompatible bonds, if any.
        if incompatible_bonds:
            for bond in incompatible_bonds:
                self.break_bond(bond)

        # Break incompatible groups, if any.
        if incompatible_groups:
            for group in incompatible_groups:
                self.break_group(group)

        # Break incompatible correspondences, if any.
        if incompatible_correspondences:
            for correrspondence in incompatible_correspondences:
                self.break_correspondence(correspondence)

        # Build the new bond.
        self.build_bond(bond)

    def bond_strength_tester(self, bond):
        '''
        Calculates the proposed bond's strength and decides probabilistically
        wheither or not to post a bond builder codelet with urgency as a
        function of the strength.
        '''
        # Update the strength values for the bond.
        bond.update_strength_values()
        strength = description.total_strength()

        # Decide whether or not to post the bond builder codelet.
        probability = strength / 100.0
        probability = self.temperature_adjusted_probability(probability)
        if not util.flip_coin(probability):
            bond.string.delete_proposed_bond(bond)
            return

        # Add activation to some relevant descriptions.
        bond.from_object_descriptor.buffer += self.activation
        bond.to_object_descriptor.buffer += self.activation
        bond.bond_facet.buffer += self.activation

        # Change bond's proposal level.
        bond.proposal_level = 2

        return [Codelet('bond_builder', (bond,), strength)]
        
    def bottom_up_bond_scout(self):
        '''
        Chooses an object and a neighbor of that object probabilistically by
        intra string salience. Chooses a bond facet probabilistically by
        relevance in the string. Checks if there is a bond between the two
        descriptors of this facet, posting a bond strength tester codelet
        with urgency a function of the degree of association of bonds of the
        bond category.
        '''
        # Choose an object.
        from_object = self.choose_object('instra_string_salience')

        # Choose neighbor.
        to_object = from_object.choose_neighbor()
        if not to_object:
            return

        # Choose bond facet.
        facet = from_object.choose_bond_facet(to_object)
        if not facet:
            return

        # Get the two descriptors of the facet if they exist.
        from_descriptor = from_object.descriptor(facet)
        to_descriptor = to_object.descriptor(facet)
        if (not from_descriptor) or (not to_descriptor):
            return

        # Check for possible bond.
        category = from_description.bond_category(to_descriptor)
        if not category:
            return

        # Propose the bond.
        return self.propose_bond(from_object, to_object, facet,
                                 from_descriptor, to_descriptor)

    def bottom_up_correspondence_scout(self):
        '''
        Chooses two objects, one from the initial string and one from the
        target string, probabilistically by inter string salience. Finds all
        concept mappings between nodes at most one link away. If any concept
        mappings can be made between distinguishing descriptors, propoes a
        correspondence between the two objects, including all the concept
        mappings. Posts a correspondence strength tester codelet with urgency
        a funcion of the average strength of the distinguishing concept
        mappings.
        '''
        # Choose two objects.
        object1 = self.initial_string.choose_object('inter_string_salience')
        object2 = self.target_string.choose_object('inter_string_salience')

        # If one object spans the whole string and the other does not, fizzle.
        if object1.spans_whole_string() != object2.spans_whole_string():
            return

        # Get the possible concept mappings.
        mappings = self.concept_mappings(object1, object2,
                                         object1.relevant_descriptions(),
                                         object2.relevant_descriptions())
        if not mappings:
            return

        # Decide whether or not to continue based on slippability.
        possible = False
        for mapping in mappings:
            probability = mapping.slippablity() / 100.0
            probability = self.temperature_adjusted_probability(probability)
            if util.flip_coin(probability):
                possible = True
        if not possible:
            return

        # Check if there are any distinguishing mappings.
        distinguished_mappings = [m.distinguishing() for m in mappings]
        if not distinguished_mappings:
            return

        # If both objects span the strings, check if description needs flipped.
        possible_opposite_mappings = []
        for mapping in distinguishing_mappings:
            description_type = mapping.description_type1
            if description_type != 'plato_string_position_category' and \
               description_type != 'plato_bond_facet':
                   possible_opposite_mappings.append(mapping)

        opposite_descriptions = [m.description_type1 for m in mappings]
        if all([object1.string_spanning_group(),
                object2.string_spanning_group(),
                # FIXME: not plato_opposite.is_active(),
                self.all_opposite_concept_mappings(possible_opposite_mappings),
                'plato_direction_category' in opposite_descriptions]):
            old_object2_string_number = object2.string_number
            object2 = object2.flipped_version()
            object2.string_number = old_object2_string_number
            mappings = self.concept_mappings(object1, object2,
                                             object1.relevant_descriptions(),
                                             object2.relevant_descriptions())

        return self.propose_correspondence(object1, object2, mappings, True)

    def bottom_up_description_scout(self):
        '''
        Chooses an object probabilistically by total salience and chooses a
        relevant description of the object probabilistically by activation.
        If the description has any "has property" links that are short enough,
        chooses one of the properties probabilistically based on degree of
        association and activation. Then proposes a description based on the
        property and posts a description strength tester codelet with urgency
        a function of the activation of the property.
        '''
        # Choose an object.
        object = self.workspace.choose_object('total_salience')

        # Choose a relevant description of the object.
        description = object.choose_relevant_description_by_activation()
        if not description:
            return
        descriptor = description.descriptor

        # Check for short enough "has property" links.
        links = descriptor.similar_has_property_links()
        if not links:
            return

        # Choose a property by degree of association and activation.
        associations = [link.degree_of_association() for link in links]
        activations = [link.to_node().activation for link in links]
        choices = map(lambda a, b: a * b, associations, activations)
        index = util.select_list_position(choices)
        property = links[index].to_node()
        
        # Propose the description.
        return self.propose_description(object, property.category(), property)

    def breaker(self):
        '''
        Chooses a structure at random and decides whether or not to break it
        as a function of its total weakness.
        '''
        # Probabilistically fizzle based on temperature.
        if util.flip_coin((100.0 - self.temperature) / 100.0):
            return

        # Choose a structure at random.
        structure = random.choice(self.structures())
        if not structure:
            return

        # If the structure is a bond in a group, have to break the group first.
        if isinstance(structure, Bond) and structure.group:
            structures = [structure, structure.group]
        else:
            structures = [structure]

        # See if the structures can be broken.
        for structure in structures:
            probability = structure.total_weakness() / 100.0
            probability = self.temperature_adjusted_probability(probability)
            if not util.flip_coin(probability):
                return

        # Break the structures.
        for structure in structues:
            if isinstance(structure, Bond):
                self.break_bond(structure)
            elif isinstance(structure, Group):
                self.break_group(structure)
            elif isinstance(structure, Correspondence):
                self.break_correspondence(structure)

    def correspondence_builder(self, correspondence, flip_object2):
        print 'Correspondence Builder'

    def correspondence_strength_tester(self, correspondence, flip_object2):
        print 'Correspondence Strength Tester'

    def description_builder(self, description):
        '''
        Attempts to build the proposed description. If it already exists, its
        activations are boosted.
        '''
        # Make sure the object still exists.
        if description.object not in self.objects():
            return

        # Make sure the description does not exist.
        if description in description.object.descriptions():
            description.description_type.buffer += self.activation
            description.descriptor.buffer += self.activation
            return

        # Build the description.
        self.build_description(description)

    def description_strength_tester(self, description):
        '''
        Calculates the proposed descriptions's strength and probabilistically
        decides whether or not to post a description builder codelet with
        urgency as a function of the strength.
        '''
        # Activate the descriptor.
        description.descriptor.buffer += self.activation

        # Update the strength values for the description.
        description.update_strength_values()
        strength = description.total_strength()

        # Decide whether or not to post the description builder codelet.
        probability = strength / 100.0
        probability = self.temperature_adjusted_probability(probability)
        if not util.flip_coin(probability):
            return
        
        return [Codelet('description_builder', (description,), strength)]

    def group_builder(self, group):
        print 'Group Builder'

    def group_scout__whole_string(self):
        print 'Group Scout - Whole String'

    def group_strength_tester(self, group):
        print 'Group Strength Tester'

    def important_object_correspondence_scout(self):
        '''
        Chooses an object from the initial string probabilistically based on
        importance. Picks a description of the object probabilistically and
        looks for an object in the target string with the same description,
        modulo the appropriate slippage, if any of the slippages currently in
        the workspace apply. Then finds all concept mappings between nodes at
        most one link away. Makes a proposed correspondence between the two
        objects, including all the concept mappings. Posts a correspondence
        strength tester codelet with urgency a function of the average
        strength of the distinguishing concept mappings.
        '''
        # Choose an object.
        object1 = self.initial_string.choose_object('relative_importance')

        # Choose a description by conceptual depth.
        object1_description = object1.choose_relative_distinguishing_description_by_conceptual_depth()
        if not object1_description:
            return
        object1_descriptor = object1_description.descriptor

        # Find the corresponding object2_descriptor.
        object2_descriptor = object1_descriptor
        for slippage in self.slippages:
            if slippage.descriptor1 == object1_descriptor:
                object2_descriptor = slippage.descriptor2

        # Find an object with that descriptor in the target string.
        object2_candidates = []
        for object in self.target_string.objects():
            for description in object.relevant_descriptions():
                if description.descriptor == object2_descriptor:
                    object2_candidates.append(object)
        if not object2_candidates:
            return
        values = [obj.inter_string_salience() for obj in object2_candidates]
        index = util.select_list_position(values)
        object2 = object2_candidates[index]

        # If one object spans the whole string and the other does not, fizzle.
        if object1.spans_whole_string() != object2.spans_whole_string():
            return

        # Get the possible concept mappings.
        mappings = self.concept_mappings(object1, object2,
                                         object1.relevant_descriptions(),
                                         object2.relevant_descriptions())
        if not mappings:
            return

        # Decide whether or not to continue based on slippability.
        possible = False
        for mapping in mappings:
            probability = mapping.slippablity() / 100.0
            probability = self.temperature_adjusted_probability(probability)
            if util.flip_coin(probability):
                possible = True
        if not possible:
            return

        # Check if there are any distinguishing mappings.
        distinguished_mappings = [m.distinguishing() for m in mappings]
        if not distinguished_mappings:
            return

        # If both objects span the strings, check if description needs flipped.
        possible_opposite_mappings = []
        for mapping in distinguishing_mappings:
            description_type = mapping.description_type1
            if description_type != 'plato_string_position_category' and \
               description_type != 'plato_bond_facet':
                   possible_opposite_mappings.append(mapping)

        opposite_descriptions = [m.description_type1 for m in mappings]
        if all([object1.string_spanning_group(),
                object2.string_spanning_group(),
                # FIXME: not plato_opposite.is_active(),
                self.all_opposite_concept_mappings(possible_opposite_mappings),
                'plato_direction_category' in opposite_descriptions]):
            old_object2_string_number = object2.string_number
            object2 = object2.flipped_version()
            object2.string_number = old_object2_string_number
            mappings = self.concept_mappings(object1, object2,
                                             object1.relevant_descriptions(),
                                             object2.relevant_descriptions())

        return self.propose_correspondence(object1, object2, mappings, True)

    def replacement_finder(self):
        print 'Replacement Finder'

    def rule_builder(self, rule):
        print 'Rule Builder'

    def rule_scout(self):
        print 'Rule Scout'

    def rule_strength_tester(self, rule):
        print 'Rule Strength Test'

    def rule_translator(self):
        print 'Rule Translator'

    def top_down_bond_scout__category(self, category):
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
        # Choose a string.
        initial_string = self.initial_string
        target_string = self.target_string
        i_relevance = initial_string.local_bond_category_relevance(category)
        t_relevance = target_string.local_bond_category_relevance(category)
        i_unhappiness = initial_string.intra_string_unhappiness()
        t_unhappiness = target_string.intra_string_unhappiness()
        values = [round(util.average(i_relevance, i_unhappiness)),
                  round(util.average(t_relevance, t_unhappiness))]
        index = util.select_list_position(values)
        string = [initial_string, target_string][index]

        # Choose an object and neighbor.
        object = string.choose_object('intra_string_salience')
        neighbor = object.choose_neighbor()
        if not neighbor:
            return

        # Choose bond facet.
        facet = object.choose_bond_facet(neighbor)
        if not facet:
            return

        # Get the descriptors of the facet if they exist.
        object_descriptor = object.descriptor(facet)
        neighbor_descriptor = neighbor.descriptor(facet)
        if (not object_descriptor) or (not neighbor_descriptor):
            return

        # Check for a possible bond.
        if object_descriptor.bond_category(neighbor_descriptor) == category:
            from_object = object
            to_object = neighbor
            from_descriptor = object_descriptor
            to_descriptor = neighbor_descriptor
        elif neighbor_descriptor.bond_category(object_descriptor) == category:
            from_object = neighbor
            to_object = object
            from_descriptor = neighbor_descriptor
            to_descriptor = object_descriptor
        else:
            return

        # Propose the bond.
        return self.propose_bond(from_object, to_object, category, facet,
                                 from_descriptor, to_descriptor)

    def top_down_bond_scout__direction(self, category):
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
        # Choose a string.
        initial_string = self.initial_string
        target_string = self.target_string
        i_relevance = initial_string.local_direction_category_relevance(category)
        t_relevance = target_string.local_direction_category_relevance(category)
        i_unhappiness = initial_string.intra_string_unhappiness()
        t_unhappiness = target_string.intra_string_unhappiness()
        values = [round(util.average(i_relevance, i_unhappiness)),
                  round(util.average(t_relevance, t_unhappiness))]
        index = util.select_list_position(values)
        string = [initial_string, target_string][index]

        # Choose an object and neighbor.
        object = string.choose_object('intra_string_salience')
        if category.name = 'plato_left':
            neighbor = object.choose_left_neighbor()
        elif category.name = 'plato_right':
            neighbor = object.choose_right_neighbor()
        if not neighbor:
            return

        # Choose bond facet.
        facet = object.choose_bond_facet(neighbor)
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
        return self.propose_bond(from_object, to_object, bond_category, facet,
                                 from_descriptor, to_descriptor)

    def top_down_description_scout(self, description_type):
        '''
        Chooses an object probabilistically by total salience, checking if it
        fits any of the descriptions in the description_type's "has instance"
        list. If so, proposes a description based on the property and posts a
        description strength tester codelet with urgency a funtion of the
        activation of the proposed descriptor.
        '''
        # Choose an object.
        object = self.choose_object('total_salience')

        # Choose a relevant descriptor.
        descriptors = description_type.possible_descriptors(object)
        if not descriptors:
            return
        activations = [descriptor.activation for descriptor in descriptors]
        index = util.select_list_position(activations)
        descriptor = descriptors[index]

        # Propose the description.
        return self.propose_description(object, description_type, descriptor)

    def top_down_group_scout__category(self, category):
        print 'Top Down Group Scout - Category'

    def top_down_group_scout__direction(self, direction):
        print 'Top Down Group Scout - Direction'
