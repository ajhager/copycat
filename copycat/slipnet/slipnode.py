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

class Slipnode(object):
    def __init__(self, name, depth):
        self.name = name
        self.conceptual_depth = depth
        self.activation = 0
        self.buffer = 0
        self.initially_clamped = False
        self.clamp = False
        self.directed = False
        self.codelets = []

        self.intrinsic_link_length = None
        self.shrunk_link_length = None

        self.category_links = []
        self.instance_links = []
        self.has_property_links = []
        self.lateral_slip_links = []
        self.lateral_nonslip_links = []
        self.incoming_links = []

        self.description_tester = None
        self.iterate_group = None

    def are_related(self, other_node):
        '''
        Return True if the two nodes are equal or are linked in the slipnet.
        '''
        return self == other_node or self.are_linked(other_node)

    def are_linked(self, other_node):
        '''
        Return True if the two nodes are linked in the slipnet.
        '''
        return other_node in [ol.to_node for ol in self.outgoing_links]

    def are_slip_linked(self, other_node):
        '''
        Return True if the two nodes are linked by a slip link in the slipnet.
        '''
        return other_node in [lsl.to_node for lsl in self.later_slip_links]

    def local_descriptor_support(self, string, object_category):
        if object_category == self.slipnet.plato_letter:
            objects = string.letters
        else:
            objects = string.groups
        
        if not objects:
            return 0
        
        descriptor_count = 0
        for obj in objects:
            if obj.is_descriptor_present(self):
                descriptor_count += 1

        return round(100 * (descriptor_count / float(len(objects))))

    def local_description_type_support(self, string):
        description_type_count = 0
        for ojb in string.objects:
            if obj.has_description_type_present(self):
                description_type_count += 1

        return round(100 * (description_type_count / float(len(string.objects))))


    def outgoing_links(self):
        return self.has_property_links + \
               self.lateral_slip_links + \
               self.lateral_nonslip_links + \
               self.category_links + \
               self.instance_links

    def category(self):
        if self.category_links:
            return self.category_links[0].to_node()
        else:
            return None

    def local_descriptor_support(self, string, object_category):
        '''
        The percentage of objects of the given cateogry in the string that
        have this descriptor.
        '''
        pass

    def local_descriptor_type_support(self, string):
        '''
        The percentage of objects in the string that have descriptions with
        this description type.
        '''
        pass

    def total_description_type_support(self, string):
        '''
        A function of the local description type support and the node's
        activation.
        '''
        support = self.local_description_type_support(string)
        return round((suppport + self.activation) / 2.0)

    def intrinsic_degree_of_association(self):
        return 100 - self.intrinsic_link_length

    def degree_of_association(self):
        if self.is_active():
            return 100 - self.shrunk_link_length
        else:
            return self.intrinsic_link_length

    def bond_degree_of_association(self):
        '''
        Return the degree of association bonds of the given category are
        considered to have.
        '''
        return min(100, round(11 * self.degree_of_association()))

    def is_active(self):
        return self.activation == 100

    def get_possible_descriptors(self, obj):
        descriptors = []
        for link in self.instance_links:
            instance = link.to_node
            if instance.description_tester(obj):
                descriptors.append(instance)
        return descriptors

    def apply_slippages(self, slippage_list):
        for slippage in slippage_list:
            if slippage.descriptor1 == self:
                return slippage.descriptor2
        return self

    def decay(self):
        amount = int(((100 - self.conceptual_depth) / 100.0) * self.activation)
        self.buffer -= amount

    def get_label_node(self, to_node):
        if self == to_node:
            return None
        else:
            for link in self.outgoing_links():
                if link.to_node == to_node:
                    return link.label

    def get_related_node(self, relation):
        if relation.name == 'identity':
            return self
        for link in self.outgoing_links():
            if link.label == relation:
                return link.to_node

    def similar_has_property_links(self):
        similar_links = []
        for link in self.has_property_links:
            prob = link.degree_of_association() / 100.0
            #prob = get_temperature_adjusted_probability(prob)
            if util.flip_coin(prob):
                similar_links.append(link)
        return similar_links
