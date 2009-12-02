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
from copycat.workspace import Structure, Letter, Group
import copycat.slipnet as slipnet

class Correspondence(Structure):
    def __init__(self, workspace, object1, object2, concept_mappings):
        super(Correspondence, self).__init__()
        self.workspace = workspace
        self.object1 = object1
        self.object2 = object2
        self.concept_mappings = concept_mappings
        self.accessory_concept_mappings= []
        self.structure_category = Correspondence

    def is_supporting_correspondence(self, other):
        '''
        Return True if this correspondence supports the given correspondence.
        '''
        if self.object1 == other.object1 or \
           self.object2 == other.object2 or \
           self.is_incompatible_correspondence(other):
            return
        else:
            for cm1 in self.distinguishing_concept_mappings():
                for cm2 in other.distinguishing_concept_mappings():
                    if cm1.is_supporting_concept_mapping(cm2):
                        return True

    def is_incompatible_correspondence(self, other):
        '''
        Return True if this correspondence is incompatible with the given
        correspondence.
        '''
        if self.object1 == other.object1 or \
           self.object2 == other.object2:
            return True
        else:
            for cm1 in self.concept_mappings():
                for cm2 in other.concept_mappings():
                    if cm1.is_incompatible_concept_mapping(cm2):
                        return True

    def support(self):
        """Return the summed strength of other supporting correspondences.

        For now there are three levels of compatibility:
            1. Supporting
            2. Not incompatible but not supporting
            3. Incompatible
        If one of the objects is the single letter in its string, then the
        support is 100.
        """
        if (isinstance(self.object1, Letter) and self.object1.spans_whole_string()) or \
           (isinstance(self.object2, Letter) and self.object2.spans_whole_string()):
            return 100
        else:
            support_sum = 0
            other_correspondences = self.workspace.correspondences()
            if self in other_correspondences:
                other_correspondences.remove(self)
            for c in other_correspondences:
                if self.is_supporting_correspondence(c):
                    support_sum += c.total_strength
            return min(100, support_sum)

    def calculate_external_strength(self):
        return self.support()

    def calculate_internal_strength(self):
        '''
        A function of how many concept mappings there are, how strong they
        are and how much internal coherence there is among concept mappings.
        '''
        relevant_distinguishing_cms = self.relevant_distinguishing_concept_mappings()
        if not relevant_distinguishing_cms:
            return 0
        average_strength = toolbox.average(*[cm.strength() for cm in relevant_distinguishing_cms])
        number_of_cms = len(relevant_distinguishing_cms)
        if number_of_cms == 1:
            number_of_cms_factor = .8
        elif number_of_cms == 2:
            number_of_cms_factor = 1.2
        else:
            number_of_cms_factor = 1.6

        if self.is_internally_coherent():
            internal_coherence_factor = 2.5
        else:
            internal_coherence_factor = 1

        return min(100, round(average_strength * internal_coherence_factor * \
                              number_of_cms_factor))

    def is_internally_coherent(self):
        '''
        Returns True if there is any pair of relevant distinguishing
        concept mappings that support each other.
        '''
        cms = self.relevant_distinguishing_concept_mappings()
        if len(cms) > 1:
            for cm in cms:
                for other_cm in cms:
                    if other_cm != cm:
                        if cm.is_supporting_concept_mapping(other_cm):
                            return True

    def other_object(self, obj):
        '''
        Return the object that the given object corresponds to.
        '''
        if obj == self.object1:
            return self.object2
        else:
            return self.object1

    def other_group(self, group):
        '''
        Return the group on the "other side" of the correspondence from the
        given group.
        '''
        if self.object1 in group.objects:
            return self.object2.group
        else:
            return self.object1.group

    def letter_span(self):
        return self.object1.letter_span() + self.object2.letter_span()

    def add_accessory_concept_mapping(self, mapping):
        self.accessory_concept_mappings.append(mapping)

    def is_concept_mapping_present(self, mapping):
        for cm in self.concept_mappings:
            if cm.descriptor1 == mapping.descriptor1 and \
               cm.descriptor2 == mapping.descriptor2:
                return True

    def add_concept_mappings(new_mappings):
        self.concept_mappings.extend(new_mappings)
        for cm in new_mappings:
            if cm.label:
                cm.label.activate_from_workspace()

    def slippages(self):
        '''
        Return the list of slippages in this correspondence.
        '''
        mappings = self.concept_mappings + self.accessory_concept_mappings
        return [cm for cm in mappings if cm.is_slippage()]

    def relevant_concept_mappings(self):
        return [cm for cm in self.concept_mappings if cm.is_relevant()]

    def distinguishing_concept_mappings(self):
        return [cm for cm in self.concept_mappings if cm.is_distinguishing()]

    def relevant_distinguishing_concept_mappings(self):
        mps = self.concept_mappings
        return [cm for cm in mps if cm.is_relevant() and cm.is_distinguishing]

    def incompatible_correspondences(self):
        """Return a list of all the already existing correspondences that are
        incompatible with the correspondence"""
        cors = self.workspace.correspondences()
        incomp = []
        for c in self.workspace.correspondences():
            if c and are_incompatible_correspondences(self, c):
                incomp.append(c)

        if isinstance(self.object1, Group):
            for obj in self.object1.objects():
                if (obj.correspondence and (isinstance(self.object2, Letter)) or \
                      obj.correspondence not in self.object2.objects()):
                    incomp.append(obj.correspondence)
        if isinstance(self.object2, Group):
            for obj in self.object2.objects():
                if (obj.correspondence and (isinstance(self.object1, Letter)) or \
                      obj.correspondence not in self.object1.objects()):
                    incomp.append(obj.correspondence)

        if self.object1.group:
            group_correspondence = self.object1.group.correspondence
            if group_correspondence:
                if not self.object2.group or \
                        self.object2.group != group_correspondence.object2:
                    incomp.append(group_correspondence)

        if self.object2.group:
            group_correspondence = self.object2.group.correspondence
            if group_correspondence:
                if not self.object1.group or \
                        self.object1.group != group_correspondence.object1:
                    incomp.append(group_correspondence)

        direction_category = slipnet.plato_direction_category
        direction_category_cm = None
        for cm in self.concept_mappings:
            if cm.description_type1 == direction_category and \
               cm.description_type2 == direction_category:
                direction_cateogry_cm = cm
                break
        if self.object1.is_string_spanning_group() and \
           self.object2.is_string_spanning_group() and \
           direction_category_cm:
            incomp.extend(leftmost_and_rightmost_incompatible_correspondences(
                self.object1, self.object2, direction_category_cm))

        return list(set(incomp))

    def incompatible_bond(self):
        '''
        Return the bond that is incompatible with this correspondence.
        '''
        if self.object1.is_leftmost_in_string():
            bond1 = self.object1.right_bond
        else:
            bond1 = self.object1.left_bond

        if self.object2.is_leftmost_in_string():
            bond2 = self.object2.right_bond
        else:
            bond2 = self.object2.left_bond

        if bond1 and bond2 and \
           bond1.direction_cateogry and bond2.direction_category:
            plato_direction_category = self.slipnet.plato_direction_category
            bond_concept_mappings = [ConceptMapping(plato_direction_category,
                                                    plato_direction_category,
                                                    bond1.direction_category,
                                                    bond2.direction_category,
                                                    None, None)]
            if are_incompatible_concept_mapping_lists(bond_concept_mappings,
                                                      self.concept_mappings):
                return bond2

    def is_incompatible_rule(self):
        '''
        A rule is incompatible with this correspondence if object1 is the
        changed object and object2 doesn't have the rule's descriptor1 in
        its relevant_descriptions.
        '''
        rule = self.workspace.rule
        descriptor1 = rule.descriptor1
        descriptors = [cm.descriptor1 for cm in self.concept_mappings]
        slippages = self.workspace.slippages
        relevant_descriptors = [d.descriptor for d in self.object2.relevant_descriptions]
        slipped = [d.apply_slippages() for d in relevant_descriptors]
        return self.object1.is_changed() and rule and \
                descritpr1 not in descriptors and \
                descriptor1 not in slipped + slippages

    def is_proposed(self):
        return self.proposal_level < self.workspace.built

                
