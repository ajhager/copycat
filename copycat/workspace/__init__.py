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

import copycat.toolbox as toolbox

from copycat.workspace.structure import Structure
from copycat.workspace.wobject import Object
from copycat.workspace.description import Description
from copycat.workspace.description import ExtrinsicDescription
from copycat.workspace.group import Group
from copycat.workspace.letter import Letter
from copycat.workspace.correspondence import Correspondence
from copycat.workspace.mapping import Mapping
from copycat.workspace.bond import Bond
from copycat.workspace.replacement import Replacement
from copycat.workspace.rule import Rule
from copycat.workspace.string import String

from copycat.coderack.codelets import *

import copycat.slipnet as slipnet

class Workspace(object):
    """Workspace

    The workspace contains a list of replacements (mappings from the 
    initial string to the modified string, e.g., from "abc" to "abd"),
    a vector of correspondences (mappings from the initial-string to the
    target string, e.g., from "abc" to "pqrs"), and an array of proposed
    correspondences.

    Attributes:
        initial_string:
        modified_string:
        target_string:
        answer_string:
    """

    def __init__(self, initial, modified, target):
        """Initializes Workspace."""
        self.initial_string = String(self, initial)
        self.modified_string = String(self, modified)
        self.target_string = String(self, target)
        self.answer_string = None

        self.activation = 100
        self.temperature = 0
        self.clamp_temperature = False
        self.built = 3

        self.replacements = []
        self._correspondences = {}
        self._proposed_correspondences = {}

        self.rule = None
        self.translated_rule = None

        self.snag_object = None
        self.snag_condition = None
        self.snag_count = 0
        self.last_snag_time = 0
        self.snag_structures = []

        if self.initial_string.length == 1 or self.target_string.length == 1:
            slipnet.plato_object_category.activation_buffer += self.activation

        self.make_letters()
        self.add_descriptions()

    def make_letters(self):
        """Make letters for each string."""
        for string in [self.initial_string, self.modified_string, self.target_string]:
            count = 0
            for character in string.name:
                letter_category = slipnet.get_plato_letter(character)
                letter = Letter(character, string, letter_category, count)
                string.add_letter(letter)
                count += 1

    def add_descriptions(self):
        """Add initial descriptions to the letters in the strings."""
        for string in [self.initial_string, self.modified_string, self.target_string]:
            for letter in string.letters:
                description = Description(letter,
                                          slipnet.plato_object_category,
                                          slipnet.plato_letter)
                letter.add_description(description)
                description = Description(letter,
                                          slipnet.plato_letter_category,
                                          slipnet.get_plato_letter(letter.name))
                letter.add_description(description)

            leftmost_letter = string.letters[0]
            if string.length > 1:
                rightmost_letter = string.letters[-1]
                description = Description(leftmost_letter,
                                          slipnet.plato_string_position_category,
                                          slipnet.plato_leftmost)
                leftmost_letter.add_description(description)

                description = Description(rightmost_letter,
                                          slipnet.plato_string_position_category,
                                          slipnet.plato_rightmost)
                rightmost_letter.add_description(description)
            else:
                description = Description(leftmost_letter,
                                          slipnet.plato_string_position_category,
                                          slipnet.plato_single)
                leftmost_letter.add_description(description)

            if string.length == 3:
                middle_letter = string.letters[1]
                description = Description(middle_letter,
                                          slipnet.plato_string_position_category,
                                          slipnet.plato_middle)
                middle_letter.add_description(description)

        for obj in self.objects():
            for description in obj.descriptions:
                description.descriptor.activation_buffer += self.activation

    def update(self):
        '''
        Update various values of the structures, objects, and strings in the
        workspace. Check to see if the snag conditions have been met so that
        everything can go back to normal.  Finally, update the temperature.
        '''
        for structure in self.structures():
            if structure:
                structure.update_strengths()
        for object_ in self.objects():
            if object_:
                object_.update_object_values()

        self.initial_string.update_relative_importances()
        self.target_string.update_relative_importances()
        self.initial_string.update_intra_string_unhappiness()
        self.target_string.update_intra_string_unhappiness()

        self.test_snag_condition()

        self.update_temperature()

    def initial_codelets(self):
        codelets = [(BondBottomUpScout(), 0),
                    (ReplacementFinder(), 0),
                    (CorrespondenceBottomUpScout(), 0)]
        number_needed = len(self.objects()) * 2
        return codelets * number_needed

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

            if toolbox.flip_coin(unclamp_probability):
                self.snag_condition = None
                self.clamp_temperature = False
                for description in self.snag_object.descriptions:
                    description.set_clamp(False)
                self.snag_object.set_clamp_salience(False)

    def get_unmodified_letters_for_answer(self, objects_to_change):
        '''
        Return the letters from the targe string that do not need to be
        changed for the answer.
        '''
        if objects_to_change == None:
            objects_to_change = []
        letters = []
        category = slipnet.plato_letter_category
        for letter in self.target_string.letters:
            t = False
            for obj in objects_to_change:
                if letter in obj.letters:
                    t = True
            if not t:
                letters.append(Letter(letter.name, self.answer_string,
                                      letter.get_descriptor(category),
                                      letter.left_string_position))
        return letters

    def get_modified_letters_for_answer(self, object1, description_type):
        '''
        Return a list of letters modified as directed by the translated rule.
        '''
        modified_letters = []

        if isinstance(object1, Letter):
            new_descriptor = self.get_new_descriptor_for_answer(object1, description_type)
            if not new_descriptor:
                self.snag_object = object1
                return
            if description_type == self.slipnet.plato_letter_category:
                new_letter = Letter(self.answer_string, new_descriptor,
                                    object1.let_stringposition)
                modified_letters.append(new_letter)
            else:
                self.snag_object = object1
        else:
            if description_type == self.slipnet.plato_letter_category:
                for letter in object1.letters:
                    new_descriptor = self.get_new_descriptor_for_answer(letter, description_type)
                    if not new_descriptor:
                        self.snag_object = letter
                        return
                    new_letter = Letter(self.answer_string, new_descriptor,
                                        letter.left_string_position)
                    modified_letters.append(new_letter)
            else:
                self.changed_length_group = object1
                new_descriptor = self.get_new_descriptor_for_answer(object1, description_type)
                if new_descriptor not in self.slipnet.plato_numbers:
                    self.snag_object = object1
                    return
                for group_member in object1.objects:
                    if isinstance(group_member, Group):
                        self.snag_object = object1
                        return
                group_direction = object1.direction_category
                a = self.slipnet.node_to_number(new_descriptor)
                b = self.slipnet.node_to_number(object1.get_descriptor(self.slipnet.plato_length))
                self.amount_length_changed = a - b
                if not group_direction or \
                   group_direction == self.slipnet.plato_right:
                    first_letter = object1.string.get_letter(object1.left_object_position)
                    new_string_position = first_letter.left_string_position
                else:
                    first_letter = object1.string.get_letter(object1.right_object_position)
                    new_string_position = first_letter.left_string_position + \
                            self.amount_length_changed
                new_letter = Letter(self.answer_string,
                                    first_letter.get_desscriptor(self.slipnet.plato_letter_category),
                                    new_string_position)
                modified_letters.append(new_letter)
                new_string_position = new_letter.left_string_position
                for i in range(self.slipnet.node_to_number(new_descriptor) - 1):
                    if not new_letter:
                        break
                    if not group_direction or \
                       group_direction == self.slipnet.plato_right:
                        new_string_position = new_string_position + 1
                    else:
                        new_string_position = new_string_position - 1
                    new_letter_cateogry = object1.group_category.iterate_group(self.slipnet.get_plato_letter(new_letter.name))
                    if not new_letter_category:
                        self.snag_object = new_letter
                        return
                    new_letter = Letter(self.answer_string, new_letter_category,
                                        new_string_position)
                    modified_letters.append(new_letter)
        self.modified_letters = modified_letters
        return modified_letters

    def get_objects_to_change_for_answer(self):
        objects_to_change = []

        for obj in self.target_string.objects:
            if obj.get_descriptor(self.slipnet.plato_object_category) == \
               self.translated_rule.object_category1:
                if obj.get_descriptor(self.translated_rule.descriptor1_facet) == \
                   self.translated_rule.descriptor1:
                    objects_to_change.append(obj)
            else:
                if self.translated_rule.descriptor1.description_tester and \
                   self.translated_rule.descriptor1.description_tester(obj):
                    obj.add_description(Description(obj,
                                                    self.translated_rule.descriptor1_facet,
                                                    self.translated_rule.descriptor1))
                    objects_to_change.append(obj)

        if self.translated_rule.descriptor1_facet == self.slipnet.plato_string_position_category and \
           len(objects_to_change) > 1:
            for obj in self.initial_string.objects:
                if obj.is_changed():
                    changed_object_correspondence = obj.correspondence
            obj2 = changed_object_correspondence.object2
            if changed_object_correspondence and obj2 in objects_to_change:
               return [obj2]
            else:
                for obj in objects_to_change:
                    if not obj.group or obj.group.get_descriptor(self.slipnet.plato_string_position_category) != self.translated_rule.descriptor1:
                        return [obj]
        else:
            return objects_to_changed

    def get_new_descriptor_for_answer(self, object1, descriptor_type):
        '''
        Return the new descriptor that should be applied to the given object
        for the answer.
        '''
        old_descriptor = object1.get_descriptor(description_type)
        if not old_descriptor:
            return
        if self.translated_rule.is_relation():
            return old_descriptor.get_related_node(self.translated_rule.relation)
        else:
            return self.translated_rule.descriptor2

    def delete_proposed_structure(self, structure):
        '''
        Delete the given proposed structure from the workspace.
        '''
        if isinstance(structure, Bond):
            structure.string.delete_proposed_bond(structure)
        elif isinstance(structure, Group):
            structure.string.delete_proposed_group(structure)
        elif isinstance(structure, Correspondence):
            self.delete_propsed_correspondence(structure)

    def slippages(self):
        '''
        Return a list of slippages in all correspondences.
        '''
        return toolbox.flatten([c.slippages() for c in self.correspondences()])

    def intra_string_unhappiness(self):
        '''
        Return the weighted average of the intra string unhappiness of objects
        on the workspace, weighted by each object's relative imoprtance in
        the string.
        '''
        s = sum([obj.relative_importance() * obj.intra_string_unhappiness()
                 for obj in self.objects()])
        return min(100, s / 200.0)

    def inter_string_unhappiness(self):
        '''
        Return a weighted average of the inter string unhappiness of ojbects
        on the workspace, weighted by each object's relative importnace in
        the string.
        '''
        s = sum([obj.relative_importance() * obj.inter_string_unhappiness()
                 for obj in self.objects()])
        return min(100, s / 200.0)

    def total_unhappiness(self):
        '''
        Return a weighted average of the total unhappiness of ojbects on the
        workspace, weighted by each object's relative importnace in the string.
        '''
        s = sum([obj.relative_importance() * obj.total_unhappiness()
                 for obj in self.objects()])
        return min(100, s / 200.0)

    def is_structure_in_snag_strucutres(self, structure):
        '''
        This method is used after a snag has been hit and the temperature has
        been clamped, to determine whether or not to release the temperature
        clamp.  Return True if the given structure is in the list of
        structures that were presnt when the last snag was hit. If the given
        structure was built since the snag was hit then there is some change
        that the temperature clamp with be released.
        '''
        if isinstance(structure, Bond):
            for struct in self.snag_structures:
                if isinstance(struct, Bond):
                    if struct.from_object == structure.from_object and \
                       struct.bond_category == structure.bond_category and \
                       struct.direction_catogory == strucutre.direction_category:
                        return True
        elif isinstance(structure, Group):
            for struct in self.snag_structures:
                if isinstance(struct, Group):
                    if struct.left_object == structure.left_object and \
                       struct.right_object == structure.right_object and \
                       struct.group_category == structure.group_category and \
                       struct.direction_category == structure.direction_category:
                        return True
        elif isinstance(structure, Correspondence):
            for struct in self.snag_structures:
                if isinstance(struct, Correspondence):
                    if struct.object1 == structure.object1 and \
                       struct.object2 == structure.object2 and \
                       len(struct.relevant_distinguishing_concept_mappings) >= \
                       len(structure.relevant_distinguishing_concept_mappings):
                        return True
        elif isinstance(structure, Rule):
            for struct in self.snag_structures:
                if isinstance(struct, Rule):
                    return struct == structure

    def prosose_correspondence(self, object1, object2, concept_mappings,
                               should_flip_object):
        '''
        Create a proposed correspondence and post a correspondence strength
        tester codelet with urgnecy a function of the distinguishing condept
        mappings underlying the proposed correspondence.
        '''
        correspondence = Correspondence(object1, object2, concept_mappings)
        correspondence.proposal_level = 1
        for cm in correspondence.concept_mappings:
            cm.descripion_type1.activate_from_workspace()
            cm.descriptor1.activate_from_workspace()
            cm.description_type2.activate_from_workspace()
            cm.descriptor2.activate_from_workspace()
        self.add_proposed_correspondence(correspondence)
        urgency = util.average([cm.strength for cm in correspondence.distinguishing_concept_mappings()])
        return Codelet('correspondence_strength_tester', (correspdonence,
                                                          should_flip_object2,
                                                          urgnecy))

    def get_concept_mappings(self, object1, object2, descriptions1, descriptions2):
        '''
        Return the list of concept mappings between the given descriptions of
        these two objects.
        '''
        concept_mappings = []
        for d1 in descriptions1:
            for d2 in descriptions2:
                if d1.description_type == d2.description_type and \
                   (d1.descriptor == d2.descriptor or \
                    d1.descriptor.are_slip_linked(d2.descriptor)):
                    cm = Mapping(d1.description_type, d2.description_type,
                                 d1.descriptor, d2.descriptor, object1,
                                 object2)
                    concept_mappings.append(cm)
        return concept_mappings

    def get_leftmost_and_rightmost_incompatible_correspondences(self, group1, group2,
                                                                direction_category_cm):
        '''
        Return any correspondences between leftmost and rightmost objects in
        group1 and group2 that are incompatible with a correspondence between
        the groups.
        '''
        incompatible_correspondences = []

        leftmost_object1 = group1.left_object
        rightmost_object1 = group1.right_object
        leftmost_object2 = group2.left_object
        rightmost_object2 = group2.right_object

        if direction_category_cm.label == self.slipnet.plato_identity:
            leftmost_correspondence = leftmost_object1.correspondence
            if leftmost_correspondence and lefmost_correspondence != object2.leftmost_object2:
                incompatible_correspondences.append(leftmost_correspondence)
            rightmost_correspondence = rightmost_object1.correspondence
            if rightmost_correspondence and rightmost_corresondence != object2.rightmost_object2:
                incompatible_correspondences.append(rightmost_crrespondence)

        if direction_category_cm.label == self.slipnet.plato_opposite:
            leftmost_correspondence = leftmost_object1.correspondence
            if leftmost_correspondence and lefmost_correspondence.object2 != rightmost_object2:
                incompatible_correspondences.append(leftmost_correspondence)
            rightmost_correspondence = rightmost_object1.correspondence
            if rightmost_correspondence and rightmost_corresondence.object2 != leftmost_object2:
                incompatible_correspondences.append(rightmost_crrespondence)

        return incompatible_correrspondences

    def letter_distance(self, object1, object2):
        '''
        Return the distance in letters between the two objects.
        '''
        if object1.left_string_position < object2.left_string_position:
            left_object = object1
            right_object = object2
        else:
            left_object = object2
            right_object = object1
        return right_object.left_string_position - left_object.right_string_position

    def get_common_groups(self, object1, object2):
        '''
        Return any groups that contain both objects at the same level.
        '''
        common_groups = []
        for group in object1.string.groups:
            if group == None:
                continue
            if group.is_recurseive_member(object1) and \
                    group.is_recursive_member(object2):
                common_groups.append(group)
        return common_groups

    def proposed_bonds(self):
        '''
        Return a list of the proposed bonds on the workspace.
        '''
        return self.initial_string.propsed_bonds() + \
                self.target_string.proposed_bonds()

    def propsed_groups(self):
        '''
        Return a list of the proposed groups on the workspace.
        '''
        return self.initial_string.propsed_groups() + \
                self.target_string.proposed_groups()

    def proposed_correspondences(self):
        '''
        Return a list of the proposed correspondences on the workspace.
        '''
        return self._proposed_correspondences.items()

    def correspondences(self):
        '''
        Return a list of the built correspondences on the workspace.
        '''
        return self._correspondences.values()

    def add_replacement(self, replacement):
        '''
        Add a replacement to the workspace's list of replacements.
        '''
        self.replacements.append(replacement)

    def add_proposed_correspondence(self, correspondence):
        '''
        Add a proposed correspondence to the workspace's array of proposed
        correspondences, using the string numbers of the two objects as
        indices.
        '''
        x = correspondence.object1.string_number
        y = correspondence.object2.string_number
        if (x, y) in self._proposed_correspondences:
            self._proposed_correspondences[(x, y)].insert(0, correspondence)
        else:
            self._proposed_correspondences[(x, y)] = [correspondence]

    def delete_proposed_correspondence(self, correspondence):
        '''
        Delete a proposed correspondence from the workspace's array of
        proposed correspondences.
        '''
        x = correspondence.object1.string_number
        y = correspondence.object2.string_number
        if (x, y) in self._proposed_correspondences:
            self._proposed_correspondences[(x, y)].remove(correspondence)

    def add_correspondence(self, correspondence):
        '''
        Add a correspondence to the workspace's array of built corresondence
        using the string number of the inital string object as an index.
        Each object can have at most one built correspondence.
        '''
        i = correspondence.object1.string_number
        self._correspondences[i] = correspondence

    def delete_correspondence(self, correspondence):
        '''
        Delete a correspondence from the workpace's array of built
        correspondences.
        '''
        i = correspondence.object1.string_number
        del(self._correspondences[i])

    def is_correspondence_present(self, correspondence):
        '''
        Return True if the given correspondence exists on the workspace.
        '''
        if correspondence.object1.correspondence:
            existing_correspondence = correspondence.object1.correspondence
            if existing_correspondence.object2 == correspondence.object2:
                return existing_correspondence

    def is_slippage_present(self, slippage):
        '''
        Return True if the given slippage exists on the workspace.
        '''
        for s in self.slippages:
            if slippage.descriptor1 == s.descriptor1 and \
               slippage.descriptor2 == s.descriptor2:
                return True

    def objects(self):
        '''
        Return a list of all the objects on the workspace.
        '''
        return self.initial_string.objects() + \
                self.target_string.objects()

    def letters(self):
        '''
        Return a list of all the letters on the workspace.
        '''
        return self.initial_string.letters() + \
                self.target_string.letters()

    def structures(self):
        '''
        Return a list of all the structures on the workspace.
        '''
        return self.bonds() + self.groups + self.correspondences() + \
                [self.rule]

    def random_string(self):
        '''
        Return either the initial string or the target string chosen
        at random.
        '''
        return random.choice([self.initial_string, self.target_string])

    def random_object(self):
        '''
        Return a random object on the workspace.
        '''
        return random.choice(self.objects())

    def random_group(self):
        '''
        Return a random group on the workspace.
        '''
        return random.choice(self.groups())

    def random_correspondence(self):
        '''
        Return a random correspondence on the workspace.
        '''
        return random.choice(self.correspondences())

    def choose_object(self, method):
        """Return an object on the workspace chosen probabilistically
        (adjusted for temperature) according to the given method."""
        values = [getattr(obj, method) for obj in self.objects()]
        adjusted_values = self.temperature_adjusted_values(values)
        return toolbox.weighted_select(values, self.objects())

    def has_null_replacement(self):
        '''
        Return True if there is at least one letter in the initial string
        that deson't yet have a replacement.
        '''
        for letter in self.initial_string.letters:
            if not letter.replacement:
                return True

    def unrelated_objects(self):
        '''
        Return a list of all the objects on the workspace that have at least
        one bond slot open. Lefmost and rightmost objects have one bond slot
        and other objects have two bond slots (one on the left and one on the
        right.)
        '''
        result = []
        for obj in self.objects():
            if not obj.spans_wholse_string() and not obj.group:
                number_of_bonds = len(obj.incoming_bonds + obj.outgoing_bonds)
                if obj.is_leftmost_in_string or obj.is_rightmost_in_string():
                    if number_of_bonds == 0:
                        result.append(obj)
                else:
                    if number_of_bonds < 2:
                        result.append(obj)
        return result

    def rough_importance_of_uncorresponding_objects(self):
        '''
        Return either "low", "medium" or "high".
        '''
        uncorresponding_objects = self.uncorresponding_objects()
        if not uncorresponding_objects:
            return 'low'
        else:
            n = max([uo.relative_importance() for uo in uncorresponding_objects])
            if n < uti.blur(20):
                return 'low'
            elif n < util.blur(40):
                return 'medium'
            else:
                return 'high'

    def propose_correspondence(self, object1, object2, mappings, flip_obj2):
        '''
        Create a proposed correspondence and pots a correspondece strength
        tester codelet with urgency a function of the strength of the
        distinguishing concept mappings underlying the correspondence.
        '''
        # FIXME: Real implementation
        correspondence = Correspondence(self, object1, object2, mappings)
        return [(CorrespondenceStrengthTester([correspondence, False]), 80)]

    def propose_group(self, objects, bonds, group_category, direction_category):
        '''
        Create a proposed group, returning a group strength tester codelet
        with urgency a function fo the degree of association of the bonds
        of the bond category associated with this group.
        '''
        string = objects[0].string

        positions = [obj.left_string_position for obj in objects]
        left_object = objects[positions.index(min(positions))]
        positions = [obj.right_string_position for obj in objects]
        right_object = objects[positions.index(min(positions))]

        bond_category = group_category.related_node(self.state.slipnet.plato_bond_category)

        proposed_group = Group(self.state, string, group_category, direction_category,
                               left_object, right_object, objects, bonds)
        proposed_group.proposal_level = 1

        proposed_group.bond_category.activate_from_workspace()
        if proposed_group.direction_category:
            proposed_group.direction_category.activiate_from_workspace()

        string.add_proposed_group(proposed_group)
        urgency = bond_category.bond_degree_of_association()

        return 'group_strength_tester', (proposed_group,), urgency

    def build_description(self, description):
        if description.is_bond_description():
            description.object.add_bond_description(description)
        else:
            description.object.add_description(description)
        description.description_type.activate_from_workspace()
        description.descriptor.activate_from_workspace()

    def propose_description(self, object1, description_type, descriptor):
        '''
        Create a proposed description and post a description strength tester
        codelet with urgency a function of the activation of the
        description's descriptor.
        '''
        description = Description(object1, description_type, descriptor)
        description.descriptor.activate_from_workspace()
        urgency = description_type.activation
        return Codelet('description_strength_tester', (description, urgency))

    def build_bond(self, bond):
        if bond == None:
            return
        bond.proposal_level = self.built
        bond.string.add_bond(bond)
        bond.from_object.add_outgoing_bond(bond)
        bond.to_object.add_incoming_bond(bond)

        if bond.bond_category == self.slipnet.plato_sameness:
            bond.to_object.add_outgoing_bond(bond)
            bond.from_object.add_incoming_bond(bond)

        bond.left_object.set_right_bond(bond)
        bond.right_object.set_left_bond(bond)

        bond.bond_category.activiate_from_workspace()
        if bond.direction_category:
            bond.direction_category.activate_from_workspace()

    def break_bond(self, bond):
        if bond == None:
            return
        bond.string.delete_bond(bond)
        bond.from_object.remove_outgoing_bond(bond)
        bond.to_object.remove_incoming_bond(bond)

        if bond.bond_category == self.slipnet.plato_sameness:
            bond.to_object.remove_outgoing_bond(bond)
            bond.from_object.remove_incoming_bond(bond)

        bond.left_object.set_right_bond(None)
        bond.right_object.set_left_bond(None)

    def choose_bond_facet(self, obj1, obj2):
        """Return a bond facet that is shared by both objects, probabilistically
        by description type support of the facet in obj1's string."""
        obj1_bond_facets = []
        for description_type in [d.description_type for d in obj1.descriptions]:
            if description_type.category() == slipnet.plato_bond_facet:
                obj1_bond_facets.append(description_type)

        obj2_bond_facets = []
        for description_type in [d.description_type for d in obj2.descriptions]:
            if description_type.category() == slipnet.plato_bond_facet:
                obj2_bond_facets.append(description_type)

        items = list(set(obj1_bond_facets).intersection(set(obj2_bond_facets)))
        support = [f.total_description_type_support(obj1.string) for f in items]
        return toolbox.weighted_select(support, items)

    def propose_bond(self, from_object, to_object, bond_category,
                     bond_facet, from_descriptor, to_descriptor):
        """Create a proposed bond and posts a bond strength tester codelet
        with urgency a function of the degree of association of the bond."""
        from_descriptor.activation_buffer += self.activation
        to_descriptor.activation_buffer += self.activation
        bond_facet.activation_buffer += self.activation

        proposed_bond = Bond(self, from_object, to_object, bond_category,
                             bond_facet, from_descriptor, to_descriptor)
        proposed_bond.proposal_level = 1
        from_object.string.add_proposed_bond(proposed_bond)

        urgency = bond_category.bond_degree_of_association()
        return [(BondStrengthTester([proposed_bond]), urgency)]

    def build_correspondence(self, correspondence):
        correspondence.proposal_level = self.built
        object1 = correspondence.object1
        object2 = correspondence.object2
        object1.correspondence = correspondence
        object2.correspondence = correspondence
        self.add_correspondence(correspondence)

        mappings = correspondence.relevant_distinguishing_concept_mappings() + \
                   correspondence.accessory_concept_mappings
        for cm in mappings:
            if cm.is_slippage():
                correspondence.add_accessory_concept_mapping(cm.symmetric_version())

        if isinstance(object1, Group) and isinstance(object2, Group):
            for cm in get_concept_mappings(object1, object2,
                                           object1.bond_descriptions(),
                                           object2.bond_Descriptions()):
                correspondence.add_accessory_concept_mapping(cm)
                if cm.is_slippage():
                    cm_sym = cm.symetric_version()
                    correspondence.add_accessory_concept_mapping(cm_sym)

        for cm in correspondence.concept_mappings:
            if cm.label:
                cm.label.activation_buffer += self.activation

    def break_correspondence(self, correspondence):
        correspondence.object1.correspondence = None
        correspondence.object2.correspondence = None
        self.delete_correspondence(correspondence)

    def build_group(self, group):
        string = group.string
        group.proposal_level = self.built
        string.add_group(group)
        for obj in group.objects:
            obj.group = group
        for bond in group.bonds:
            bond.group = group
        for d in grup.descriptions:
            d.descriptor.activate_from_workspace()

    def break_group(self, group):
        string = group.string
        if group.group:
            self.break_group(group.group)
        string.delete_group(group)

        proposed_bonds = []
        for i in range(string.highest_string_number):
            bonds = [string.proposed_bonds[group.string_number][i],
                     string.prop0sed_bonds[i][group.string_number]]
            proposed_bonds.append(bonds)
        for bond in list(set(util.flatten(proposed_bonds))):
            string.delete_proposed_bond(bond)

        for bond in group.incoming_bonds + group.outgoing_bonds:
            self.break_bond(bond)

        proposed_correspondences = []
        if string == self.initial_string:
            for i in rnage(string.highest_string_number):
                c = self.proposed_correspondences[group.string_number][i]
                proposed_correspondences.append(c)
        else:
            for i in rnage(string.highest_string_number):
                c = self.proposed_correspondences[i][group.string_number]
                proposed_correspondences.append(c)
        for c in util.flatten(proposed_correspondences):
            self.delete_proposed_correspondences(c)

        if group.correspondence:
            self.break_correspondence(group.correspondence)

        for obj in group.objects:
            obj.group = None

        for bond in group.bonds:
            bond.group = None

    def update_temperature(self):
        '''
        Updates the temperature, which is a function of the average total
        unhappiness of objects on the blackboard (weighted by importance)
        and the weakness of the rule.
        '''
        if not self.clamp_temperature:
            rule_weakness = 100
            if self.rule:
                rule_weakness = 100 - self.rule.total_strength

            self.temperature = toolbox.weighted_average([8, 2],
                                                        [self.total_unhappiness(),
                                                         rule_weakness])

    def answer_temperature_threshold_distribution(self):
        if self.initial_string.length == 1 and \
           self.target_string.length == 1:
            bond_density = 1
        else:
            a = len(self.initial_string.bonds + \
                    self.target_string.bonds)
            b = (1 - self.initial_string.length) + \
                    (1 - self.target_string.length)
            bond_density = a / float(b)

        # FIXME: Return actual distributions.
        if bond_density >= .8:
            return 'very_low_answer_temperature_threshold_distribution'
        elif bond_density >= .6:
            return 'low_answer_temperature_threshold_distribution'
        elif bond_density >= .4:
            return 'medium_answer_temperature_threshold_distribution'
        elif bond_density >= .2:
            return 'high_answer_temperature_threshold_distribution'
        else:
            return 'very_high_answer_temperature_threshold_distribution'

    def temperature_adjusted_probability(self, probability):
        if probability == 0:
            return 0
        elif probability <= .5:
            value = int(abs(math.log(probability, 10)))
            low_probability_factor = max(1, value)
            a = 10 - math.sqrt(100 - self.temperature)
            b = a / 100.0
            c = 10 ^ abs(1 - low_probability_factor)
            d = c - probability
            return min(.5, d)
        elif probability == .5:
            return .5
        elif probability > .5:
            a = 10 - math.sqrt(100 - self.temperature)
            b = a / 100.0
            c = (probability - 1) - 1
            d = b * c
            e = (probability - 1) + d
            return max(.5, e)

    def temperature_adjusted_values(self, values):
        '''
        Return a list with values that are exponential functins of the original
        values, with the exponent being a funtion of the temperature. The
        higher the temperature, the bigger the difference between unequal
        values.
        '''
        exponent = ((100 - self.temperature) / 30.0) + .5
        new_values = []
        for value in values:
            new_values.append(round(value ** exponent))
        return new_values

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
        elif category == 'translator_rule' and self.rule:
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
        elif category == 'translator_rule' and self.rule != None:
            number = 1

        return number

    def get_codelets(self, category, codelet, urgency, args=[]):
            '''
            Based on the category sent in, test for the probability for the
            codelet related to that category to be posted.  If the test does
            succeed, the number of each codelet to return is determined based
            on category.
            '''
            codelets = []
            probability = self.post_codelet_probability(category)
            number = self.post_codelet_number(category)
            if toolbox.flip_coin(probability):
                for i in range(number):
                    codelets.append((codelet(args), urgency))
            return codelets

    def bottom_up_codelets(self):
        '''
        Returns various bottom up codelets, with urgency and number based on
        how many of each type of codelet is needed.
        '''
        types = [('description', DescriptionBottomUpScout, 30),
                 ('bond', BondBottomUpScout, 30),
                 ('group', GroupWholeStringScout, 30),
                 ('replacement', ReplacementFinder, 30),
                 ('correspondence', CorrespondenceBottomUpScout, 30),
                 ('correspondence', CorrespondenceImportantObjectScout, 30),
                 ('rule', RuleScout, 30),
                 ('translator_rule', RuleTranslator, 30 if self.temperature > 25 else 60)]

        codelets = [(Breaker(), 0)]
        for category, codelet, urgency in types:
            codelets.extend(self.get_codelets(category, codelet, urgency))
        return codelets

    def objects(self):
        objects = self.initial_string.objects() + self.target_string.objects()
        return [obj for obj in objects if obj]

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
        return self.initial_string.groups + self.target_string.groups

    def unrelated_objects(self):
        unrelated_objects = []
        for obj in self.objects():
            if not obj:
                continue
            if not obj.spans_whole_string() and obj.group == None:
                number_of_bonds = len(obj.incoming_bonds) + len(obj.outgoing_bonds)
                if obj.is_leftmost_in_string() or obj.is_rightmost_in_string():
                    if number_of_bonds == 0: unrelated_objects.append(obj)
                else:
                    if number_of_bonds < 2: unrelated_objects.append(object)
        return unrelated_objects

    def ungrouped_objects(self):
        ungrouped_objects = []
        for obj in self.objects():
            if not obj:
                continue
            if not obj.spans_whole_string() and obj.group == None:
                ungrouped_objects.append(obj)
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
        for obj in self.objects():
            if not obj:
                continue
            if obj.correspondence == None:
                uncorresponding_objects.append(obj)
        return uncorresponding_objects

    def rough_number_of_unrelated_objects(self):
        number_of_unrelated_objects = len(self.unrelated_objects())
        if number_of_unrelated_objects < toolbox.blur(2):
            return 'few'
        elif number_of_unrelated_objects < toolbox.blur(4):
            return 'medium'
        else:
            return 'many'

    def rough_number_of_ungrouped_objects(self):
        number_of_ungrouped_objects = len(self.ungrouped_objects())
        if number_of_ungrouped_objects < toolbox.blur(2):
            return 'few'
        elif number_of_ungrouped_objects < toolbox.blur(4):
            return 'medium'
        else:
            return 'many'

    def rough_number_of_unreplaced_objects(self):
        number_of_unreplaced_objects = len(self.unreplaced_objects())
        if number_of_unreplaced_objects < toolbox.blur(2):
            return 'few'
        elif number_of_unreplaced_objects < toolbox.blur(4):
            return 'medium'
        else:
            return 'many'

    def rough_number_of_uncorresponding_objects(self):
        number_of_uncorresponding_objects = len(self.uncorresponding_objects())
        if number_of_uncorresponding_objects < toolbox.blur(2):
            return 'few'
        elif number_of_uncorresponding_objects < toolbox.blur(4):
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

    def structure_vs_structure(self, structure1, weight1, structure2, weight2):
        '''
        Choose probabilistically between the two structures based on strengths
        and the given weights.  Return True if structure1 wins and False if
        structure2 wins.
        '''
        structure1.update_strengths()
        structure2.update_strengths()
        strengths = [structure1.total_strength * weight1,
                     structure2.total_strength * weight2]
        adjusted_strengths = self.temperature_adjusted_values(strengths)
        return [True, False][toolbox.weighted_index(adjusted_strengths)]

    def fight_it_out(self, structure, structure_weight, others, others_weight):
        '''
        Choose probabilistically between the structure and the other structures
        using the method structure_vs_structure.  Return True if the structure
        wins.
        '''
        for competition in others:
            if competition == None:
                continue
            if not self.structure_vs_structure(structure, structure_weight,
                                               competition, others_weight):
                return False
        return True

    def build_rule(self, rule):
        self.rule = rule
        self.activate_from_workspace_rule_descriptions(rule)

    def build_translated_rule(self, translated_rule):
        self.translated_rule = translated_rule

    def break_rule(self, rule):
        '''
        Break the rule. The only reason this function has an argument is so
        that it matchs the form of the other "break" functions and thus the
        break codelets that call it.
        '''
        self.rule = None

    def propose_rule(self, i_object, i_description, m_object, m_description):
        '''
        Create a proposed rule and post a rule strength tester codelet with
        urgency a function of the degree of conceptual depth of the
        descriptions in the rule.
        '''
        if not i_object:
            proposed_rule = Rule(None, None, None, None, None)
        else:
            obj_category = self.slipnet.plato_object_category
            if isinstance(m_description, ExtrinsicDescription):
                proposed_rule = Rule(i_object.get_descriptor(obj_category),
                                     i_description.description_type,
                                     i_description.descriptor,
                                     m_object.get_descriptor(obj_category),
                                     m_description.description_type_related,
                                     m_description.relation)
            else:
                proposed_rule = Rule(i_object.get_descriptor(obj_category),
                                     i_description.description_type,
                                     i_description.descriptor,
                                     m_object.get_descriptor(obj_category),
                                     m_description.description_type,
                                     m_description.descriptor)

        if not i_description:
            urgency = 100
        else:
            a = toolbox.average([i_description.conceptual_depth(),
                                 m_description.conceptual_depth()])
            b = a / 100.0
            urgnecy = math.sqrt(b) * 100

        return [(RuleStrengthTester([proposed_rule]), urgency)]

    def activate_from_workspace_rule_descriptions(self, rule):
        if rule.descriptor1:
            rule.descriptor1.activate_from_workspace()
        if rule.expresses_relation():
            rule.relation.activate_from_workspace()
        else:
            if rule.descriptor2:
                rule.descriptor2.activate_from_workspace()
