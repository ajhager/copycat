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

import string

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

class Sliplink(object):
    def __init__(self, from_node, to_node, label, fixed_length):
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
    def __init__(self):
        self.slipnodes = []
        self.sliplinks = []
        self.clamp_time = 50

        # Letter nodes
        slipnet_letters = []
        for letter in string.ascii_lowercase:
            slipnet_letters.append(self.add_slipnode(letter, 10))

        # Number nodes
        slipnet_numbers = []
        for number in range(1, 6):
            slipnet_numbers.append(self.add_slipnode(str(number), 30))

        # String position nodes
        leftmost = self.add_slipnode('leftmost', 40)
        rightmost = self.add_slipnode('rightmost', 40)
        middle = self.add_slipnode('middle', 40)
        single = self.add_slipnode('single', 40)
        whole = self.add_slipnode('whole', 40)

        # Alphabetic position nodes
        first = self.add_slipnode('first', 60)
        last = self.add_slipnode('last', 60)

        # Direction nodes
        left = self.add_slipnode('left', 40)
        left.codelets.append('top_down_bond_scout__direction')
        left.codelets.append('top_down_group_scout__direction')
        right = self.add_slipnode('right', 40)
        right.codelets.append('top_down_bond_scout__direction')
        right.codelets.append('top_down_group_scout__direction')

        # Bond nodes
        predecessor = self.add_slipnode('predecessor', 50, 60)
        predecessor.directed = True
        predecessor.codelets.append('top_down_bond_scout__category')
        successor = self.add_slipnode('successor', 50, 60)
        successor.directed = True
        successor.codelets.append('top_down_bond_scout__category')
        sameness = self.add_slipnode('sameness', 80, 0)
        sameness.codelets.append('top_down_bond_scout_category')

        # Group nodes
        predecessor_group = self.add_slipnode('predecessor group', 50)
        predecessor_group.directed = True
        predecessor_group.codelets.append('top_down_group_scout__category')
        successor_group = self.add_slipnode('successor group', 50)
        successor_group.directed = True
        successor_group.codelets.append('top_down_group_scout__category')
        sameness_group = self.add_slipnode('sameness group', 80)
        sameness_group.codelets.append('top_down_group_scout__category')

        # Other relation nodes
        identity = self.add_slipnode('identity', 90, 0)
        opposite = self.add_slipnode('opposite', 90, 80)

        # Object nodes
        letter = self.add_slipnode('letter', 20)
        group = self.add_slipnode('group', 80)

        # Category nodes
        letter_category = self.add_slipnode('letter category', 30)
        letter_category.initially_clamped = True
        string_position_category = self.add_slipnode('string position category', 70)
        string_position_category.initially_clamped = True
        string_position_category.codelets.append('top_down_description_scout')
        alphabetic_position_category = self.add_slipnode('alphabetic position category', 80)
        alphabetic_position_category.codelets.append('top_down_description_scout')
        direction_category = self.add_slipnode('direction category', 70)
        bond_category = self.add_slipnode('bond category', 80)
        group_category = self.add_slipnode('group category', 80)
        length = self.add_slipnode('length', 60)
        object_category = self.add_slipnode('object category', 90)
        bond_facet = self.add_slipnode('bond facet', 90)

        # Letter links
        for i in range(25):
            before = slipnet_letters[i]
            after = slipnet_letters[i + 1]
            self.add_sliplink('nonslip', before, after, successor, None)
            self.add_sliplink('nonslip', after, before, predecessor, None)

        # Number links
        for i in range(4):
            before = slipnet_numbers[i]
            after = slipnet_numbers[i + 1]
            self.add_sliplink('nonslip', before, after, successor, None)
            self.add_sliplink('nonslip', after, before, predecessor, None)

        # Letter category links
        for i in range(26):
            l = slipnet_letters[i]
            fixed_length = letter_category.conceptual_depth - l.conceptual_depth
            self.add_sliplink('category', l, letter_category, None, fixed_length)
            self.add_sliplink('instance', letter_category, l, None, 97)
        self.add_sliplink('category', sameness_group, letter_category, None, 50)

        # Length links
        for i in range(5):
            n = slipnet_numbers[i]
            fixed_length = length.conceptual_depth - n.conceptual_depth
            self.add_sliplink('category', n, length, None, fixed_length)
            self.add_sliplink('instance', length, n, None, 100)
        self.add_sliplink('nonslip', predecessor_group, length, None, 95)
        self.add_sliplink('nonslip', successor_group, length, None, 95)
        self.add_sliplink('nonslip', sameness_group, length, None, 95)

        # Opposite links
        self.add_sliplink('slip', first, last, opposite, None)
        self.add_sliplink('slip', last, first, opposite, None)
        self.add_sliplink('slip', leftmost, rightmost, opposite, None)
        self.add_sliplink('slip', rightmost, leftmost, opposite, None)
        self.add_sliplink('slip', left, right, opposite, None)
        self.add_sliplink('slip', right, left, opposite, None)
        self.add_sliplink('slip', successor, predecessor, opposite, None)
        self.add_sliplink('slip', predecessor, successor, opposite, None)
        self.add_sliplink('slip', successor_group, predecessor_group,
                          opposite, None)
        self.add_sliplink('slip', predecessor_group, successor_group,
                          opposite, None)

        # Has property links
        self.add_sliplink('property', slipnet_letters[0], first, None,  75)
        self.add_sliplink('property', slipnet_letters[25], last, None, 75)

        # Object category links
        self.add_sliplink('category', letter, object_category, None,
                   object_category.conceptual_depth - letter.conceptual_depth)
        self.add_sliplink('instance', object_category, letter, None, 100)
        self.add_sliplink('category', group, object_category, None,
                   object_category.conceptual_depth - group.conceptual_depth)
        self.add_sliplink('instance', object_category, group, None, 100)

        # String position inks
        self.add_sliplink('category', leftmost, string_position_category, None,
          string_position_category.conceptual_depth - leftmost.conceptual_depth)
        self.add_sliplink('instance', string_position_category, leftmost,
                          None, 100)
        self.add_sliplink('category', rightmost, string_position_category, None,
          string_position_category.conceptual_depth - rightmost.conceptual_depth)
        self.add_sliplink('instance', string_position_category, rightmost,
                          None,  100)
        self.add_sliplink('category', middle, string_position_category, None,
          string_position_category.conceptual_depth - middle.conceptual_depth)
        self.add_sliplink('instance', string_position_category, middle,
                          None, 100)
        self.add_sliplink('category', single, string_position_category, None,
          string_position_category.conceptual_depth - single.conceptual_depth)
        self.add_sliplink('instance', string_position_category, single,
                          None, 100)
        self.add_sliplink('category', whole, string_position_category, None,
          string_position_category.conceptual_depth - whole.conceptual_depth)
        self.add_sliplink('instance', string_position_category, whole,
                          None, 100)

        # Alphabetic position category links
        self.add_sliplink('category', first, alphabetic_position_category, None,
          alphabetic_position_category.conceptual_depth - first.conceptual_depth)
        self.add_sliplink('instance', alphabetic_position_category, first,
                          None, 100)
        self.add_sliplink('category', last, alphabetic_position_category, None,
          alphabetic_position_category.conceptual_depth - last.conceptual_depth)
        self.add_sliplink('instance', alphabetic_position_category, last,
                          None, 100)

        # Direction category links
        self.add_sliplink('category', left, direction_category, None,
          direction_category.conceptual_depth - left.conceptual_depth)
        self.add_sliplink('instance', direction_category, left, None,  100)
        self.add_sliplink('category', right, direction_category, None,
          direction_category.conceptual_depth - right.conceptual_depth)
        self.add_sliplink('instance', direction_category, right, None, 100)

        # Bond category links
        self.add_sliplink('category', predecessor, bond_category, None,
          bond_category.conceptual_depth - predecessor.conceptual_depth)
        self.add_sliplink('instance', bond_category, predecessor, None, 100)
        self.add_sliplink('category', successor, bond_category, None,
          bond_category.conceptual_depth - successor.conceptual_depth)
        self.add_sliplink('instance', bond_category, successor, None, 100)
        self.add_sliplink('category', sameness, bond_category, None,
          bond_category.conceptual_depth - sameness.conceptual_depth)
        self.add_sliplink('instance', bond_category, sameness, None, 100)

        # Group category links
        self.add_sliplink('category', predecessor_group, group_category, None,
          group_category.conceptual_depth - predecessor_group.conceptual_depth)
        self.add_sliplink('instance', group_category, predecessor_group,
                          None, 100)
        self.add_sliplink('category', successor_group, group_category, None,
          group_category.conceptual_depth - successor_group.conceptual_depth)
        self.add_sliplink('instance', group_category, successor_group,
                          None, 100)
        self.add_sliplink('category', sameness_group, group_category, None,
          group_category.conceptual_depth - sameness_group.conceptual_depth)
        self.add_sliplink('instance', group_category, sameness_group,
                          None, 100)

        # Associated group links
        self.add_sliplink('nonslip', sameness, sameness_group,
                          group_category, 30)
        self.add_sliplink('nonslip', successor, successor_group,
                          group_category, 60)
        self.add_sliplink('nonslip', predecessor, predecessor_group,
                          group_category, 60)

        # Associated bond links
        self.add_sliplink('nonslip', sameness_group, sameness,
                          bond_category, 90)
        self.add_sliplink('nonslip', successor_group, successor,
                          bond_category, 90)
        self.add_sliplink('nonslip', predecessor_group, predecessor,
                          bond_category, 90)

        # Bond facet links
        self.add_sliplink('category', letter_category, bond_facet, None,
          bond_facet.conceptual_depth - letter_category.conceptual_depth)
        self.add_sliplink('instance', bond_facet, letter_category, None, 100)
        self.add_sliplink('category', length, bond_facet, None,
          bond_facet.conceptual_depth - length.conceptual_depth)
        self.add_sliplink('instance', bond_facet, length, None, 100)

        # Letter category links
        self.add_sliplink('slip', letter_category, length, None, 95)
        self.add_sliplink('slip', length, letter_category, None, 95)

        # Letter group links
        self.add_sliplink('slip', letter, group, None, 90)
        self.add_sliplink('slip', group, letter, None, 90)

        # Direction position, direction neighbor, position neighbor links
        self.add_sliplink('nonslip', left, leftmost, None, 90)
        self.add_sliplink('nonslip', leftmost, left, None, 90)
        self.add_sliplink('nonslip', right, leftmost, None, 100)
        self.add_sliplink('nonslip', leftmost, right, None, 100)
        self.add_sliplink('nonslip', right, rightmost, None, 90)
        self.add_sliplink('nonslip', rightmost, right, None, 90)
        self.add_sliplink('nonslip', left, rightmost, None, 100)
        self.add_sliplink('nonslip', rightmost, left, None, 100)
        self.add_sliplink('nonslip', leftmost, first, None, 100)
        self.add_sliplink('nonslip', first, leftmost, None, 100)
        self.add_sliplink('nonslip', rightmost, first, None, 100)
        self.add_sliplink('nonslip', first, rightmost, None, 100)
        self.add_sliplink('nonslip', leftmost, last, None, 100)
        self.add_sliplink('nonslip', last, leftmost, None, 100)
        self.add_sliplink('nonslip', rightmost, last, None, 100)
        self.add_sliplink('nonslip', last, rightmost, None, 100)

        # Other links
        self.add_sliplink('slip', single, whole, None, 90)
        self.add_sliplink('slip', whole, single, None, 90)

    def add_sliplink(self, kind, from_node, to_node, label, fixed_length):
        sliplink = Sliplink(from_node, to_node, label, fixed_length)
        self.sliplinks.append(sliplink)
        to_node.incoming_links.append(sliplink)
        if kind == 'slip':
            from_node.lateral_slip_links.append(sliplink)
        elif kind == 'nonslip':
            from_node.lateral_nonslip_links.append(sliplink)
        elif kind == 'property':
            from_node.has_property_links.append(sliplink)
        elif kind == 'instance':
            from_node.instance_links.append(sliplink)
        elif kind == 'category':
            from_node.category_links.append(sliplink)

    def add_slipnode(self, name, conceptual_depth, intrinsic_link_length=None):
        slipnode = Slipnode(name, conceptual_depth)
        if intrinsic_link_length != None:
            slipnode.intrinsic_link_length = intrinsic_link_length
            slipnode.shrunk_link_length = round(intrinsic_link_length * .4)
        self.slipnodes.append(slipnode)
        return slipnode

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
                    link.to_node.buffer += amount_to_spread
            
        for node in self.slipnodes:
            node.activation = min(100, node.activation + node.buffer)
            if node.clamp:
                node.activation = 100
            else:
                if node.activation >= 50:
                    full_activation_probability = (node.activation / 100.0) ** 3
                    if util.flip_coin(full_activation_probability):
                        node.activation = 100
            node.buffer = 0

    def clear(self):
        '''
        Zero out the acitivations of all slipnodes.
        '''
        for node in self.slipnodes:
            node.buffer = 0
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
        codelets = []
        for node in self.slipnodes:
            if node.activation >= 50:
                for codelet in node.codelets():
                    codelets.append(codelet)
        return codelets
