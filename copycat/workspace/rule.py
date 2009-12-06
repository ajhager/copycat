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

from copycat.workspace import Structure
import copycat.toolbox as toolbox

class Rule(Structure):
    def __init__(self, object_category1, descriptor1_facet, descriptor1,
                 object_category2, replaced_description_type, relation=None):
        super(Rule, self).__init__()
        self.object_category1 = object_category1
        self.descriptor1_facet = descriptor1_facet
        self.descriptor1 = descriptor1
        self.descriptor2 = None
        self.object_category2 = object_category2
        self.replaced_description_type = replaced_description_type
        self.relation = relation
        self.structure_category = 'rule'

    def __eq__(self, other):
        return self.object_category1 == other.object_category1 and \
                self.descriptor1_facet == other.descriptor1_facet and \
                self.descriptor1 == other.descriptor1 and \
                self.object_category2 == other.object_category2 and \
                self.descriptor2 == other.descriptor2 and \
                self.relation == other.relation and \
                self.replaced_description_type == other.replaced_description_type

    def expresses_relation(self):
        return self.relation

    def has_no_change(self):
        return not self.descriptor1

    def calculate_internal_strength(self):
        if self.has_no_change():
            return 100

        conceptual_depth1 = self.description1.conceptual_depth()
        if self.relation:
            conceptual_depth2 = self.relation.conceptual_depth()
        else:
            conceptual_depth2 = self.description2.conceptual_depth()

        conceptual_difference = abs(conceptual_depth1 - conceptual_depth2)

        for obj in self.workspace.initial_string.objects:
            if obj.has_changed():
                i_object = obj
                break

        if i_object.correspondence:
            i_object_corresponding_object = i_object.correspondence.object2
        else:
            i_object_corresponding_object = None

        if not i_object_corresponding_object:
            shared_descriptor_term = 0
        else:
            slipped_descriptors = []
            for d in i_object_corresponding_object.relevant_descriptions():
                ds = d.apply_slippages(i_object_corresponding_object,
                                       self.workspace.slippages)
                slipped_descriptors.append(ds.descriptor)
            if self.descriptor1 in slipped_descriptors:
                shared_descriptor_term = 100
            else:
                return 0

        shared_descriptor_weight = round(((100 - self.descriptor1.conceptual_depth()) / \
                                         10) ** 1.4)

        rule_strength = round(toolbox.weighted_average([(toolbox.average([conceptual_depth1, conceptual_depth2]), 10)
                                                     (100 - conceptual_difference, 12),
                                                     (shared_descriptor_term, shared_descriptor_weight)]))

        return min(rule_strength, 100)

    def calculate_external_strength(self):
        return self.internal_strength
