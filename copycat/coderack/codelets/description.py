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
import copycat.toolbox as toolbox

class DescriptionBottomUpScout(Codelet):
    '''
    Chooses an object probabilistically by total salience and chooses a
    relevant description of the object probabilistically by activation.
    If the description has any "has property" links that are short enough,
    chooses one of the properties probabilistically based on degree of
    association and activation. Then proposes a description based on the
    property and posts a description strength tester codelet with urgency
    a function of the activation of the property.
    '''
    structure_category = 'description'
    def run(self, coderack, slipnet, workspace):
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
        index = toolbox.select_list_position(choices)
        property = links[index].to_node()
        
        # Propose the description.
        return self.propose_description(object, property.category(), property)

class DescriptionBuilder(Codelet):
    '''
    Attempts to build the proposed description. If it already exists, its
    activations are boosted.
    '''
    structure_category = 'description'
    def run(self, coderack, slipnet, workspace):
        description = self.arguments[0]

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

class DescriptionStrengthTester(Codelet):
    '''
    Calculates the proposed descriptions's strength and probabilistically
    decides whether or not to post a description builder codelet with
    urgency as a function of the strength.
    '''
    structure_category = 'description'
    def run(self, coderack, slipnet, workspace):
        description = self.arguments[0]

        # Activate the descriptor.
        description.descriptor.buffer += self.activation

        # Update the strength values for the description.
        description.update_strength_values()
        strength = description.total_strength()

        # Decide whether or not to post the description builder codelet.
        probability = strength / 100.0
        probability = self.temperature_adjusted_probability(probability)
        if not toolbox.flip_coin(probability):
            return
        
        return [Codelet('description_builder', (description,), strength)]

class DescriptionTopDownScout(Codelet):
    '''
    Chooses an object probabilistically by total salience, checking if it
    fits any of the descriptions in the description_type's "has instance"
    list. If so, proposes a description based on the property and posts a
    description strength tester codelet with urgency a funtion of the
    activation of the proposed descriptor.
    '''
    structure_category = 'description'
    def run(self, coderack, slipnet, workspace):
        description_type = self.arguments[0]

        # Choose an object.
        object = self.choose_object('total_salience')

        # Choose a relevant descriptor.
        descriptors = description_type.possible_descriptors(object)
        if not descriptors:
            return
        activations = [descriptor.activation for descriptor in descriptors]
        index = toolbox.select_list_position(activations)
        descriptor = descriptors[index]

        # Propose the description.
        return self.propose_description(object, description_type, descriptor)
