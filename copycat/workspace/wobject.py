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

from copycat import toolbox as toolbox
from copycat import slipnet as slipnet

class Object(object):
    """Object
    
    Attributes:
        string:
    """

    def __init__(self):
        """Initializes Object."""
        self.string = None
        self.string_number = None
        self.left_string_position = None
        self.right_string_position = None
        self.raw_importance = 0
        self.relative_importance = 0
        self.intra_string_happiness = 0
        self.intra_string_unhappiness = 0
        self.inter_string_happiness = 0
        self.inter_string_unhappiness = 0
        self.total_happiness = 0
        self.total_unhappiness = 0
        self.intra_string_salience = 0
        self.inter_string_salience = 0
        self.total_salience = 0
        self.descriptions = []
        self.extrinsic_descriptions = []
        self.outgoing_bonds = []
        self.incoming_bonds = []
        self.left_bond = None
        self.right_bond = None
        self.group = None
        self.replacement = None
        self.correspondence = None
        self.is_changed = False
        self.is_new_answer_letter = False
        self.clamp_salience = False

    def flipped_version(self):
        return self

    def calculate_raw_importance(self):
        """Return the raw importance of the object.
        
        This is a function of the number and activation of the object's relevant
        descriptions. In addition, the importance of the changed object is
        enhanced, and the importance of objects in groups is dimished. Sum the
        activation of the descriptors of relevant descriptions up to 300.
        """
        descriptions = self.relevant_descriptions()
        result = min(300, sum([d.descriptor.activation for d in descriptions]))
        if self.is_changed:
            result *= 2
        if self.group:
            result *= 2/3.0
        return result

    def calculate_intra_string_happiness(self):
        """Return the intra string happiness of the object.

        This value represents how well the object is fitting into a structuring
        of its string and is a function of the strength of bonds or a group
        involving the object.
        """
        if self.spans_whole_string():
            return 100
        if self.group:
            return self.group.total_strength()
        if not self.all_bonds():
            return 0
        if self.is_leftmost_in_string() or self.is_rightmost_in_string():
            return round(self.bonds[0].total_stength() / 3.0)
        return round(sum([b.total_strength() for b in self.bonds]) / 6.0)

    def calculate_intra_string_unhappiness(self):
        """Return the intra string unhappiness."""
        return 100 - self.intra_string_happiness

    def calculate_inter_string_happiness(self):
        """Return the inter string happiness.

        This value represents how well the object is fitting into a mapping
        from the initial string to the target string.
        """
        if self.correspondence:
            return self.corresondence.total_strength()
        else:
            return 0

    def calculate_inter_string_unhappiness(self):
        """Return the inter string unhappiness."""
        return 100 - self.inter_string_happiness

    def calculate_total_happiness(self):
        """Return the total string happiness."""
        return round(toolbox.average(self.intra_string_happiness,
                                      self.inter_string_happiness))

    def calculate_total_unhappiness(self):
        """Return the total string unhappiness."""
        return 100 - self.total_happiness

    def calculate_intra_string_salience(self):
        '''
        This value represents how much the object is crying out for attention
        from codelets that build structures inside a single string.
        '''
        if self.clamp_salience:
            return 100
        else:
            return round(toolbox.weighted_average([2, 8],
                                                  [self.relative_importance,
                                                   self.intra_string_unhappiness]))

    def calculate_inter_string_salience(self):
        '''
        This value represents how much the object is crying out for attention
        from codelets that build structures between strings.
        '''
        if self.clamp_salience:
            return 100
        else:
            a = round(toolbox.weighted_average([8, 2],
                                               [self.relative_importance,
                                               self.inter_string_unhappiness]))
            return a

    def calculate_total_salience(self):
        return round(toolbox.average(self.intra_string_salience,
                                     self.inter_string_salience))

    def update_object_values(self):
        self.raw_importance = self.calculate_raw_importance()
        self.intra_sring_happiness = self.calculate_intra_string_happiness()
        self.intra_string_unhappiness = self.calculate_intra_string_unhappiness()
        self.inter_string_happiness = self.calculate_inter_string_happiness()
        self.inter_string_unhappiness = self.calculate_inter_string_unhappiness()
        self.total_happiness = self.calculate_total_happiness()
        self.total_unhappiness = self.calculate_total_unhappiness()
        self.intra_string_salience = self.calculate_intra_string_salience()
        self.inter_string_salience = self.calculate_inter_string_salience()
        self.total_string_salience = self.calculate_total_salience()

    def letter_span(self):
        '''
        Return the number of letters spanned by the object.
        '''
        if self.group == None:
            return 1
        return sum([obj.letter_span() for obj in self.objects])

    def letters(self):
        '''
        Return a list of the letters at the lowest level of the object.
        '''
        if isinstance(self, Letter):
            return [self]
        return toolbox.flatten([obj.letters() for obj in self.objects])

    def is_leftmost_in_string(self):
        return self.left_string_position == 0

    def is_rightmost_in_string(self):
        right_position = len(self.string.letters) - 1
        return self.right_string_position == right_position

    def is_middle_in_string(self):
        left_neighbor = self.ungrouped_left_neighbor()
        right_neighbor = self.ungrouped_right_neighbor()
        return left_neighbor and right_neighbor and \
                left_neighbor.is_leftmost_in_string() and \
                right_neighbor.is_rightmost_in_string()

    def spans_whole_string(self):
        '''
        Return True if the object is the single letter in its string or a
        group that spans the string.
        '''
        return self.letter_span() == self.string.length

    def is_string_spanning_group(self):
        return self.type_name == 'group'  and self.spans_whole_string()

    def ungrouped_left_neighbor(self):
        '''
        Return the left neighbor of the group that either isn't in a group
        or is in the same group as the given object.
        '''
        if not self.is_leftmost_in_string():
            for left_neighbor in self.all_left_neighbors():
                group = left_neighbor.group
                if not group or self.is_recursive_group_member(group):
                    return left_neighbor

    def ungrouped_right_neighbor(self):
        '''
        Return the right neighbor of the group that either isn't in a group
        or is in the same group as the given object.
        '''
        if not self.is_rightmost_in_string():
            for right_neighbor in self.all_right_neighbors():
                group = right_neighbor.group
                if not group or self.is_recursive_group_member(group):
                    return right_neighbor

    def all_left_neighbors(self):
        '''
        Return a list of all the object's immediate left neighbors, both
        letters and groups.
        '''
        objects = []
        if not self.is_leftmost_in_string():
            position = self.left_string_position - 1
            for obj in self.string.object_positions[position]:
                if not obj:
                    continue
                if not (obj.type_name == 'group' and \
                        self.is_recursive_group_member(obj)):
                    objects.append(obj)
        return objects

    def all_right_neighbors(self):
        '''
        Return a list of all the objects's left neighbors.
        '''
        objects = []
        if not self.is_rightmost_in_string():
            position = self.right_string_position + 1
            for obj in self.string.object_positions[position]:
                if not obj:
                    continue
                if not (obj.type_name == 'group' and \
                        self.is_recursive_group_member(obj)):
                    objects.append(obj)
        return objects

    def all_neighbors(self):
        '''
        Return a list of all the immediate neighbors of the objects.
        '''
        return self.all_left_neighbors() + self.all_right_neighbors()

    def random_left_neighbor(self):
        '''
        Return a randomly selected left neighbor.
        '''
        return random.choose(self.all_left_neighbors())

    def random_right_neighbor(self):
        '''
        Return a randomly selected right neighbor.
        '''
        return random.choose(self.all_right_neighbors())

    def random_neighbor(self):
        '''
        Return a randomly selected neighbor.
        '''
        return random.choose(self.all_neighbors())

    def choose_left_neighbor(self):
        '''
        Choose a left neighbor probabilistically based on intra string
        salience.
        '''
        neighbors = self.all_left_neighbors()
        values = [obj.intra_string_salience for obj in neighbors]
        return toolbox.weighted_select(values, neighbors)

    def choose_right_neighbor(self):
        '''
        Choose a right neighbor probabilistically based on intra string
        salience.
        '''
        neighbors = self.all_right_neighbors()
        values = [obj.intra_string_salience for obj in neighbors]
        return toolbox.weighted_select(values, neighbors)

    def choose_neighbor(self):
        '''
        Choose a neighbor probabilistically based on intra string salience.
        '''
        neighbors = self.all_neighbors()
        saliences = [o.intra_string_salience for o in neighbors]
        return toolbox.weighted_select(saliences, neighbors)

    def all_bonds(self):
        '''
        Return all bonds connected to this ojbect, either incoming or
        outgoing.
        '''
        return self.incoming_bonds + self.outgoing_bonds

    def add_description(self, description):
        '''
        Add the given description to the object's description list.
        '''
        description.descriptor_number = len(self.descriptions)
        self.descriptions.append(description)

    def add_extrinsic_description(self, description):
        self.extrinsic_descriptions.append(description)

    def get_descriptor(self, description_type):
        '''
        Return the descriptor of the object corresponding to the given
        desicription type.
        '''
        for description in self.descriptions:
            if description.description_type == description_type:
                return description.descriptor

    def relevant_descriptions(self):
        '''
        Return a list of the object's relevant desriptions; those whose
        description type is fully active.
        '''
        return [d for d in self.descriptions if d.description_type.is_active()]

    def distinguishing_descriptions(self):
        '''
        Return a list of the object's distinguishing descriptions; those which
        distinguish it in a string.
        '''
        f = self.is_distinguishing_descriptor
        return [d for d in self.descriptions if f(d.descriptor)]

    def non_distinguishing_descriptions(self):
        '''
        Return a list of the object's non distinguishing descriptions.
        '''
        f = self.is_distinguishing_descriptor
        return [d for d in self.descriptions if not f(d.descriptor)]

    def is_distinguishing_descriptor(self, descriptor):
        '''
        Return True if no other object of the same type has the same descriptor.
        Object category and length descriptions are not distinguishing.
        '''
        if descriptor == slipnet.plato_letter or \
           descriptor == slipnet.plato_group or \
           descriptor in slipnet.slipnet_numbers:
            return
        else:
            if self.type_name == 'letter':
                other_objects = self.string.letters[:]
                other_objects.remove(self)
            else:
                other_objects = self.string.groups[:]
                other_objects.remove(self)
                if self.group:
                    other_objects.remove(self.group)
                for obj in self.objects:
                    if isinstance(obj, Group):
                        other_objects.remove(obj)
            descriptions = toolbox.flatten([o.descriptions for o in other_objects])
            other_descriptors = [d.descriptor for d in descriptions]
            return not descriptor in other_descriptors

    def relevant_distinguishing_descriptions(self):
        descriptions = self.distinguishing_descriptions()
        [d for d in descriptions if d.description_type.is_active()]

    def relevant_non_distinguishing_descriptions(self):
        descriptions = self.non_distinguishing_descriptions()
        [d for d in descriptions if d.description_type.is_active()]

    def choose_relevant_description_by_activation(self):
        '''
        Choose a relevant description probabilistically based on the
        descriptor's activation.
        '''
        relevant_descriptions = self.relevant_descriptions()
        if relevant_descriptions:
            descriptors = [d.descriptor for d in relevant_descriptions]
            activations = [d.activation for d in descriptors]
            return util.weighted_select(activations, relevant_descriptions)

    def choose_relevant_distinguishing_description_by_conceptual_depth(self):
        '''
        Chooses a relevant, distinguishing description probabilistically based
        on the descriptor's conceptual depth.
        '''
        relevant_descriptions = self.relevant_distinguishing_descriptions()
        if relevant_descriptions:
            depths = [d.conceptual_depth for d in relevant_descriptions]
            return util.weighted_select(depths, relevant_descriptions)

    def is_description_present(self, description):
        '''
        Return True if this object already has this description.
        '''
        for d in self.descriptions:
            if d.description_type == description.description_type and \
               d.descriptor == description.descriptor:
                return True

    def rule_initial_string_descriptions(self):
        '''
        Return all the descriptions that can be used in making the initial
        string part of the rule, with this object as the changed object in the
        initial string.
        '''
        descriptions = []
        for d in self.descriptions:
            if d.description_Type.is_active() and \
               self.is_distinguishing_descriptor(d.descriptor) and \
               not d.descriptor == self.state.slipnet.plato_object_category:
                descriptions.append(d)
        return descriptions

    def rule_modified_string_descriptions(self):
        '''
        Return all the non extrinsic descriptions that can be used in make the
        modified string part of the rule with this object as the object in the
        modified string corresponding to the initial string changed object.
        '''
        descriptions = []
        categories = [self.state.slipnet.plato_string_position_category,
                      self.state.slipnet.plato_object_category]
        for d in self.descriptions:
            if d.description_type.is_active() and \
               self.is_distinguishing_descriptor(d.descriptor) and \
               d.description_type not in categories:
                descriptions.append(d)
        return descriptions

    def add_incoming_bond(self, bond):
        '''
        Add a new incoming bond to the object.
        '''
        self.incoming_bonds.append(bond)

    def add_outgoing_bond(self, bond):
        '''
        Add a new outgoing bond to the object.
        '''
        self.outgoing_bonds.append(bond)

    def structures(self):
        '''
        Return a list of the structures attached to the object.
        '''
        return self.descriptions + [self.left_bond, self.right_bond,
                                    self.group, self.correspondence]

    def is_descriptor_present(self, descriptor):
        '''
        Return True if this object has a description with this descriptor.
        '''
        for d in self.descriptions:
            if d.descriptor == descriptor:
                return True

    def is_description_type_present(self, description_type):
        '''
        Return True if this object has a description with the given
        description type.
        '''
        for d in self.descriptions:
            if d.description_type == description_type:
                return True
