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
    def __init__(self):
        self.name = ""
        self.conceptual_depth = 0
        self.activation = 0
        self.activation_buffer = 0
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

    def outgoing_links(self):
        return self.has_property_links +\
               self.lateral_slip_links +\
               self.lateral_nonslip_links +\
               self.category_links +\
               self.instance_links

    def category(self):
        if self.category_links:
            return self.category_links[0].to_node()
        else:
            return None

    def intrinsic_degree_of_association(self):
        return 100 - self.intrinsic_link_length

    def degree_of_association(self):
        if self.is_active:
            return 100 - self.shrunk_link_length
        else:
            return self.intrinsic_link_length

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
        self.activation_buffer -= amount

    def get_label_node(self, to_node):
        if self == to_node:
            return None # plato_identity
        else:
            for link in self.outgoing_links():
                if link.to_node == to_node:
                    return link.label

    def get_related_node(self, relation):
        # if relation == plato_identity: return self
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

class Sliplink(object):
    # TODO: Add docs.
    def __init__(self, from_node, to_node, label=None, fixed_length=None):
        self.from_node = from_node
        self.to_node = to_node
        self.label = label
        self.fixed_length = fixed_length

    def intrinsic_degree_of_association(self):
        if self.fixed_length:
            return 100 - self.fixed_length
        else:
            return self.label.intrinsic_degree_of_association()

    def degree_of_association(self):
        if self.fixed_length:
            return 100 - self.fixed_length
        else:
            return self.label.degree_of_association()

class Slipnet(object):
    '''
    The slipnet is copycat's log term memory and consists of a network of
    conceptual nodes called slipnodes.
    '''
    def __init__(self):
        '''
        So far we are just setting up an empty list of slipnodes.  We need to
        decide what the best data structure should be for the slipnet.
        '''
        self.slipnodes = []

    def update(self):
        '''
        Update activations and link lengths.
        '''
        for node in self.slipnodes:
            node.decay()
            if node.activation == 100:
                for link in node.outgoing_links():
                    amount_to_spread = int(node.activation * \
                            (link.intrinsic_degree_of_association() / 100.0))
                    link.to_node.activation_buffer += amount_to_spread
            
        for node in self.slipnodes:
            node.activation = min(100, node.activation + node.activation_buffer)
            if node.clamp:
                node.activation = 100
            else:
                if node.activation >= 50:
                    full_activation_probability = (node.activation / 100.0) ** 3
                    if util.flip_coin(full_activation_probability):
                        node.activation = 100
            node.activation_buffer = 0

    def clear(self):
        '''
        Zero out the acitivations of all slipnodes.
        '''
        for node in self.slipnodes:
            node.activation_buffer = 0
            node.activation = 0

    def clamp_initial_nodes(self):
        '''
        Clamp those slipnodes that were marked to be initially clamped.
        '''
        for node in self.slipnodes:
            if node.initially_clamped:
                node.clamp = True

    def unclamp_initial_nodes(self):
        '''
        Unclamp those slipnodes that were marked to be initially clamped.
        '''
        for node in self.slipnodes:
            if node.initially_clamped:
                node.clamp = False

    def top_down_codelets(self):
        '''
        Ask each node at or above the activation threshold for any codelets
        attached to them and return them all.
        '''
        # TODO: Make sure all these exist.
        codelets = []
        for node in self.slipnodes:
            if node.activation >= 50:
                for codelet in node.codelets():
                    codelets.append(codelet)
        return codelets
