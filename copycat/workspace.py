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
        print 'Bond Builder'

    def bond_strength_tester(self, bond):
        print 'Bond Strength Tester'
        
    def bottom_up_bond_scout(self):
        print 'Bottom Up Bond Scout'

    def bottom_up_correspondence_scout(self):
        print 'Bottom Up Correspondence Scout'

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
        activations = [link.to_node().activation() for link in links]
        choices = map(lambda a, b: a * b, associations, activations)
        index = util.select_list_position(choices)
        property = links[index].to_node()
        
        # Propose the description.
        self.propose_description(object, property.category(), property)

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
        print 'Description Builder'

    def description_strength_tester(self, description):
        '''
        Calculates the proposed descriptions's strength and probabilistically
        decides whether or not to post a description builder codelet with
        urgnency as a function of the strength.
        '''
        # Activate the descriptor.
        description.descriptor.activation += self.activation

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
        print 'Important Object Correspondence Scout'

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
        print 'Top Down Bond Scout - Category'

    def top_down_bond_scout__direction(self, direction):
        print 'Top Down Bond Scout - Direction'

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
        self.propose_description(object, description_type, descriptor)

    def top_down_group_scout__category(self, category):
        print 'Top Down Group Scout - Category'

    def top_down_group_scout__direction(self, direction):
        print 'Top Down Group Scout - Direction'
