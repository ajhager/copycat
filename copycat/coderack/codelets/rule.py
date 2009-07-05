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

class RuleBuilder(Codelet):
    '''
    Tries to build the proposed rule, fighting with competitors as needed.
    '''
    def run(self, coderack, slipnet, workspace):
        # Make sure this rule doesn't already exist.
        if self.rule == rule:
            rule.activate_descriptions_from_workspace(rule, self.activation)
            return

        # Fight an existing rule.
        if self.rule:
            result = self.fight_it_out(rule, 1, self.rule, 1)
            if not result:
                return

        # Build the rule.
        if self.rule:
            self.break_rule(self.rule)
        self.build_rule(rule)

class RuleScout(Codelet):
    '''
    Fills in the rule template, "Replace _____ by _____". To do this, it
    chooses descriptions of the changed object in the initial string and
    the object in the modified string that replaces it. If a rule can be
    made, it is proposed, and a rule strength tester codelet is posted with
    urgency a function of the degree of conceptual depth of the chosen
    descriptions.
    '''
    def run(self, coderack, slipnet, workspace):
        # If not all replacements have been found, then fizzle.
        if self.null_replacement():
            return

        # Find changed object.
        changed_objects = []
        for object in self.initial_string.objects():
            if object.changed():
                changed_objects.append(object)

        # If there is more than one changed object signal an error and quit.
        if len(changed_objects) > 1:
            print "Can't solve problems with more than one letter changed."
            sys.exit()

        # If not changed object, propose rule specifying no changes.
        if not changed_objects:
            self.propose_rule(None, None, None, None)
            return

        i_object = changed_objects[0]
        m_object = i_object.replacement.object2

        # Get all relevant distinguishing descriptions.
        if not i_object.correspondences:
            i_descriptions = i_object.rule_initial_string_descriptions
        else:
            correspondence_slippages = i_object.correspondence.slippages
            i_descriptions = []
            for description in i_object.rule_initial_string_descriptions:
                # FIXME: Where is this defined?
                applied_slippages = description.apply_slippages(i_object,
                        correspondence_slippages)
                relevant_descriptions = i_object.correspondence.object2.relevant_descriptions()
                if description_member(applied_slippages, relevant_descriptions):
                    i_descriptions.append(description)
        if not i_descriptions:
            return

        # Choose the descriptior for the initial string object.
        depths = [desription.conceptual_depth for description in i_descriptions]
        i_probabilities = self.temperature_adjusted_values(depths)
        index = util.select_list_position(i_probabilities)
        i_description = i_descriptions[index]

        # Choose the descriptor for the modified string object.
        m_descriptions = m_object.extrinsic_descriptions() + \
                         m_object.rule_modified_string_descriptions()
        if not m_descriptions:
            return
        depths = [desription.conceptual_depth for description in m_descriptions]
        m_probabilities = self.temperature_adjusted_values(depths)
        index = util.select_list_position(m_probabilities)
        m_description = m_descriptions[index]

        # Kludge to avoid rules like "Replace C by succesor of C".
        relation = m_description.relatino
        related_descriptor = i_description.descriptor.related_node(relation)
        if isinstance(m_description, ExtrinsicDescription) and \
                related_descriptor:
            for description in m_object.descriptions:
                if description.descriptor == related_descriptor:
                    m_desription = description
                    break

        # Propose the rule.
        self.propose_rule(i_object, i_description, m_object, m_description)

class RuleStrengthTester(Codelet):
    '''
    Calculates the proposed rule's strength and probabilistically decides
    whether or not to post a rule builder codelet with urgency a fucntion
    for its strength.
    '''
    def run(self, coderack, slipnet, workspace):
        # Calculate strength.
        rule.update_strength_values()
        strength = rule.total_strength()

        # Decide whether or not to post a rule builder codelet.
        probability = strength / 100.0
        probability = self.temperature_adjusted_probability(probability)
        if not util.flip_coin(probability):
            return

        return Codelet('rule_builder', (rule,), strength)

class RuleTranslator(Codelet):
    '''
    Translate the rule according to the translation rules given in the
    slippages on the workspace.
    '''
    def run(self, coderack, slipnet, workspace):
        # Make sure there is a rule.
        if not self.rule:
            return
        if self.rule.no_change:
            self.translated_rule = NonRelationRule(None, None, None,
                                                   None, None, None)
            return

        # If the temperature is too high, fizzle.
        threshold = self.answer_temperature_threshold_distribution.choose()
        if self.temperature > threshold:
            return

        # Build the translation of the rule.
        changed_object = None
        for object in self.initial_string.objects():
            if object.changed:
                changed_object = object
                break
        if not changed_object:
            return

        changed_object_correspondence = changed_object.correspondence

        # Get the slippages to use.
        slippages = self.slippages
        if changed_object_correspondence:
            for slippage in self.slippages:
                for mapping in changed_object_correspondence.concept_mapptings:
                    if self.contradictory_concept_mappings(mapping, slippage):
                        slippages.remove(slippage)

        rule = self.rule
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
