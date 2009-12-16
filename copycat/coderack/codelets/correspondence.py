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

class CorrespondenceBottomUpScout(Codelet):
    """Choose two objects, one from the initial string and one from the
    target string, probabilistically by inter string salience. Finds all
    concept mappings between nodes at most one link away. If any concept
    mappings can be made between distinguishing descriptors, propoes a
    correspondence between the two objects, including all the concept
    mappings. Posts a correspondence strength tester codelet with urgency
    a funcion of the average strength of the distinguishing concept
    mappings."""
    def run(self, coderack, slipnet, workspace):
        object1 = workspace.initial_string.get_random_object('inter_string_salience')
        object2 = workspace.target_string.get_random_object('inter_string_salience')

        if object1.spans_whole_string() != object2.spans_whole_string():
            return # Fizzle

        mappings = workspace.get_concept_mappings(object1, object2,
                                                  object1.relevant_descriptions(),
                                                  object2.relevant_descriptions())

        possible = False
        for mapping in mappings:
            probability = mapping.slippability() / 100.0
            probability = workspace.temperature_adjusted_probability(probability)
            if toolbox.flip_coin(probability):
                possible = True

        if not possible:
            return # Fizzle

        distinguished_mappings = [m for m in mappings if m.is_distinguishing()]
        if not distinguished_mappings:
            return # Fizzle

        opposite_mappings = []
        for mapping in distinguished_mappings:
            description_type = mapping.description_type1
            if description_type != nodes.plato_string_position_category and \
               description_type != nodes.plato_bond_facet:
                   opposite_mappings.append(mapping)

        opposite_descriptions = [m.description_type1 for m in opposite_mappings]
        if all([object1.is_string_spanning_group(),
                object2.is_string_spanning_group(),
                not nodes.plato_opposite.is_active(),
                nodes.are_all_opposite_concept_mappings(opposite_mappings),
                nodes.plato_direction_category in opposite_descriptions]):
            old_object2_string_number = object2.string_number
            object2 = object2.flipped_version()
            object2.string_number = old_object2_string_number
            mappings = workspace.get_concept_mappings(object1, object2,
                                                      object1.relevant_descriptions(),
                                                      object2.relevant_descriptions())

        return workspace.propose_correspondence(object1, object2, mappings, True)

class CorrespondenceBuilder(Codelet):
    """Attempt to build the proposed correspondence, fighting it out with
    competitors if necessary."""
    def run(self, coderack, slipnet, workspace):
        correspondence = self.arguments[0]
        flip_obj2 = self.arguments[1]

        object1 = correspondence.object1
        object2 = correspondence.object2

        # LEFT OFF HERE
        objects = workspace.objects()
        if object1 not in objects:
            return # Fizzle
        obj2_present = object2 in objects
        flipped = False
        if flip_obj2:
            flipv = object2.flipped_version()
            if flipv.type_name == 'group':
                flipped = workspace.target_string.get_existing_group(flipv)

        if not obj2_present and not flipped:
            return # Fizzle

        existing_correspondence = workspace.is_correspondence_present(correspondence)
        if existing_correspondence:
            workspace.remove_proposed_correspondence(correspondence)
            labels = [m.label for m in correspondence.get_concept_mappings()]
            for label in labels:
                if label:
                    label.activation_buffer += workspace.activation
            mappings_to_add = []
            for mapping in correspondence.get_concept_mappings():
                if not existing_correspondence.is_concept_mapping_present(mapping):
                    mappings_to_add.append(mapping)
            existing_correspondence.add_concept_mappings(mappings_to_add)
            return # Fizzle

        for mapping in correspondence.get_concept_mappings():
            if not mapping.is_relevant():
                workspace.remove_proposed_correspondence(correspondence)
                return # Fizzle

        workspace.remove_proposed_correspondence(correspondence)

        incompatible_correspondences = correspondence.incompatible_correspondences()
        for incompatible_correspondence in incompatible_correspondences:
            if not workspace.fight_it_out(correspondence,
                                          correspondence.letter_span(),
                                          [incompatible_correspondence],
                                          incompatible_correspondence.letter_span()):
                return # Fizzle
        
        incompatible_bond = None
        incompatible_group = None
        if (object1.is_leftmost_in_string() or object1.is_rightmost_in_string()) and \
               (object2.is_leftmost_in_string() or object2.is_rightmost_in_string()):
            incompatible_bond = correspondence.incompatible_bond()
            if incompatible_bond:
                if not workspace.fight_it_out(correspondence, 3,
                                              [incompatible_bond], 2):
                    return # Fizzle
                incompatible_group = incompatible_bond.group
                if incompatible_group:
                    if not workspace.fight_it_out(correspondence, 1,
                                                  [incompatible_group], 1):
                        return # Fizzle

        if flip_obj2:
            if not workspace.fight_it_out(correspondence, 1, [flipped], 1):
                return # Fizzle

        incompatible_rule = correspondence.is_incompatible_rule()
        if incompatible_rule:
            if not workspace.fight_it_out(correspondence, 1, [workspace.rule], 1):
                return # Fizzle

        if incompatible_correspondences:
            for incompatible_correspondence in incompatible_correspondences:
                workspace.break_correspondence(incompatible_correspondence)

        if incompatible_bond:
            workspace.break_bond(incompatible_bond)

        if incompatible_group:
            workspace.break_group(incompatible_group)
        
        existing_object2_group = flipped
        if existing_object2_group:
            workspace.break_group(existing_object2_group)
            for bond in existing_object2_group.bonds:
                workspace.break_bond(bond)
            for bond in object2.bonds:
                workspace.build_bond(bond)
            workspace.build_group(object2)

        if incompatible_rule:
            workspace.break_rule(workspace.rule)

        workspace.build_correspondence(correspondence)

class CorrespondenceImportantObjectScout(Codelet):
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
    def run(self, coderack, slipnet, workspace):
        # Choose an object.
        object1 = workspace.initial_string.get_random_object('relative_importance')

        # Choose a description by conceptual depth.
        object1_description = object1.choose_relevant_distinguishing_description_by_conceptual_depth()
        if not object1_description:
            return
        object1_descriptor = object1_description.descriptor

        # Find the corresponding object2_descriptor.
        object2_descriptor = object1_descriptor
        for slippage in workspace.slippages():
            if slippage.descriptor1 == object1_descriptor:
                object2_descriptor = slippage.descriptor2

        # Find an object with that descriptor in the target string.
        object2_candidates = []
        for object in workspace.target_string.get_objects():
            for description in object.relevant_descriptions():
                if description.descriptor == object2_descriptor:
                    object2_candidates.append(object)
        if not object2_candidates:
            return
        values = [obj.inter_string_salience for obj in object2_candidates]
        index = toolbox.weighted_index(values)
        object2 = object2_candidates[index]

        # If one object spans the whole string and the other does not, fizzle.
        if object1.spans_whole_string() != object2.spans_whole_string():
            return

        # Get the possible concept mappings.
        mappings = workspace.get_concept_mappings(object1, object2,
                                                  object1.relevant_descriptions(),
                                                  object2.relevant_descriptions())
        if not mappings:
            return

        # Decide whether or not to continue based on slippability.
        possible = False
        for mapping in mappings:
            probability = mapping.slippability() / 100.0
            probability = workspace.temperature_adjusted_probability(probability)
            if toolbox.flip_coin(probability):
                possible = True
        if not possible:
            return

        # Check if there are any distinguishing mappings.
        distinguished_mappings = [m for m in mappings if m.is_distinguishing()]
        if not distinguished_mappings:
            return

        # If both objects span the strings, check if description needs flipped.
        possible_opposite_mappings = []
        for mapping in distinguished_mappings:
            description_type = mapping.description_type1
            if description_type != 'plato_string_position_category' and \
               description_type != 'plato_bond_facet':
                   possible_opposite_mappings.append(mapping)

        opposite_descriptions = [m.description_type1 for m in mappings]
        if all([object1.is_string_spanning_group(),
                object2.is_string_spanning_group(),
                not nodes.plato_opposite.is_active(),
                nodes.are_all_opposite_concept_mappings(possible_opposite_mappings),
                'plato_direction_category' in opposite_descriptions]):
            old_object2_string_number = object2.string_number
            object2 = object2.flipped_version()
            object2.string_number = old_object2_string_number
            mappings = self.concept_mappings(object1, object2,
                                             object1.relevant_descriptions(),
                                             object2.relevant_descriptions())

        return workspace.propose_correspondence(object1, object2, mappings, True)

class CorrespondenceStrengthTester(Codelet):
    '''
    Calculate the proposed correspondence's strength and probabilistically
    decides whether or not to post a correspondence builder codelt with
    urgency a function of the strength.
    '''
    def run(self, coderack, slipnet, workspace):
        correspondence = self.arguments[0]
        flip_object2 = self.arguments[1]

        object1 = correspondence.object1
        object2 = correspondence.object2
        flipped = object2.flipped_version()

        # If the objects do not exist anymore, then fizzle.
        objects = workspace.objects()
        if (object1 not in objects) or \
            ((object2 not in objects) and \
            (not (flip_object2 and workspace.target_string.get_existing_group(flipped)))):
            return

        # Calculate the proposed correspondence's strength.
        correspondence.update_strengths()
        strength = correspondence.total_strength

        # Decide whether to post a corresondpondence builder codelet or not.
        probability = strength / 100.0
        probability = workspace.temperature_adjusted_probability(probability)
        if not toolbox.flip_coin(probability):
            workspace.remove_proposed_correspondence(correspondence)
            return

        # Add some activation to some descriptions.
        for mapping in correspondence.concept_mappings:
            mapping.description_type1.activation_buffer += workspace.activation
            mapping.descriptor1.activation_buffer += workspace.activation
            mapping.description_type2.activation_buffer += workspace.activation
            mapping.descriptor2.activation_buffer += workspace.activation

        # Set correspondence proposal level.
        correspondence.proposal_level = 2

        # Post the correspondence builder codelet.
        return [(CorrespondenceBuilder((correspondence, flip_object2)), strength)]
