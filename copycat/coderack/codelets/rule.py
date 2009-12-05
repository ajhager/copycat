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
from copycat.workspace import Rule, ExtrinsicDescription
import copycat.slipnet as nodes

class RuleBuilder(Codelet):
    '''
    Tries to build the proposed rule, fighting with competitors as needed.
    '''
    def run(self, coderack, slipnet, workspace):
        # Make sure this rule doesn't already exist.
        rule = self.arguments[0]
        workspace_rule = workspace.rule
        if workspace_rule:
            if workspace_rule == rule:
                workspace.activate_from_workspace_rule_descriptions(rule)
                return

        # Fight an existing rule.
        if workspace_rule:
            result = workspace.fight_it_out(rule, 1, workspace_rule, 1)
            if not result:
                return

        # Build the rule.
        if workspace_rule:
            workspace.break_rule(workspace_rule)
        workspace.build_rule(rule)

class RuleScout(Codelet):
    """Fills in the rule template, "Replace _____ by _____".

    To do this, it chooses descriptions of the changed object in the initial
    string and the object in the modified string that replaces it. If a rule
    can be made, it is proposed, and a rule strength tester codelet is posted
    with urgency a function of the degree of conceptual depth of the chosen
    descriptions.
    """
    def run(self, coderack, slipnet, workspace):
        if workspace.has_null_replacement():
            return # Fizzle

        changed_objects = []
        for obj in workspace.initial_string.objects():
            if obj.is_changed:
                changed_objects.append(obj)

        if len(changed_objects) > 1:
            print "Can't solve problems with more than one letter changed."
            # FIXME: Too harsh.
            sys.exit()

        if not changed_objects:
            return workspace.propose_rule(None, None, None, None)

        i_object = changed_objects[0]
        m_object = i_object.replacement.object2

        # Get all relevant distinguishing descriptions.
        if not i_object.correspondence:
            i_descriptions = i_object.rule_initial_string_descriptions()
        else:
            correspondence_slippages = i_object.correspondence.slippages()
            i_descriptions = []
            for description in i_object.rule_initial_string_descriptions():
                applied = description.apply_slippages(i_object,
                                                      correspondence_slippages)
                relevant = i_object.correspondence.object2.relevant_descriptions()
                if applied in relevant:
                    i_descriptions.append(description)
        if not i_descriptions:
            return # Fizzle

        depths = [d.conceptual_depth() for d in i_descriptions]
        i_probabilities = workspace.temperature_adjusted_values(depths)
        i_description = toolbox.weighted_select(i_probabilities, i_descriptions)

        m_descriptions = m_object.extrinsic_descriptions + \
                         m_object.rule_modified_string_descriptions()
        if not m_descriptions:
            return # Fizzle
        depths = [d.conceptual_depth() for d in m_descriptions]
        m_probabilities = workspace.temperature_adjusted_values(depths)
        m_description = toolbox.weighted_select(m_probabilities, m_descriptions)

        related_descriptor = nodes.get_related_node(i_description.descriptor,
                                                    m_description.relation)
        if isinstance(m_description, ExtrinsicDescription) and related_descriptor:
            for description in m_object.descriptions:
                if description.descriptor == related_descriptor:
                    m_description = description
                    break

        workspace.propose_rule(i_object, i_description, m_object, m_description)

class RuleStrengthTester(Codelet):
    '''
    Calculates the proposed rule's strength and probabilistically decides
    whether or not to post a rule builder codelet with urgency a fucntion
    for its strength.
    '''
    def run(self, coderack, slipnet, workspace):
        # Calculate strength.
        rule = self.arguments[0]
        rule.update_strengths()
        strength = rule.total_strength

        # Decide whether or not to post a rule builder codelet.
        probability = strength / 100.0
        probability = workspace.temperature_adjusted_probability(probability)
        if not toolbox.flip_coin(probability):
            return
        return [(RuleBuilder([rule]), strength)]

class RuleTranslator(Codelet):
    '''
    Translate the rule according to the translation rules given in the
    slippages on the workspace.
    '''
    def run(self, coderack, slipnet, workspace):
        # Make sure there is a rule.
        workspace_rule = workspace.rule
        if not workspace_rule:
            return
        if workspace_rule.has_no_change:
            workspace.translated_rule = Rule(None, None, None, None, None, None)
            return

        # If the temperature is too high, fizzle.
        threshold = workspace.answer_temperature_threshold_distribution.choose()
        if workspace.temperature > threshold:
            return

        # Build the translation of the rule.
        changed_object = None
        for obj in self.initial_string.objects():
            if not obj:
                continue
            if obj.changed:
                changed_object = obj
                break
        if not changed_object:
            return

        changed_object_correspondence = changed_object.correspondence

        # Get the slippages to use.
        slippages = self.slippages
        if changed_object_correspondence:
            for slippage in workspace.slippages:
                for mapping in changed_object_correspondence.concept_mapptings:
                    if self.contradictory_concept_mappings(mapping, slippage):
                        slippages.remove(slippage)

        rule = workspace_rule
        if rule.relation():
            args = []
            for arg in [rule.object_category1, rule.descriptor1_face,
                        rule.descriptor1, rule.object_category2,
                        rule.replaced_description_type, rule.relation]:
                args.append(arg.apply_slippages(slippages))
                translated_rule = RelationRule(*args)
        else:
            args = []
            for arg in [rule.object_category1, rule.descriptor1_facet,
                        rule.descriptor1, rule.object_category2,
                        rule.replaced_description_type, rule.descriptor2]:
                args.append(arg.apply_slippages(slippages))
                translated_rule = NonRelationRule(*args)

        self.build_translated_rule(translated_rule)
