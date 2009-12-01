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

import copycat.toolbox as toolbox
from slipnode import Slipnode
from sliplink import Sliplink


slipnodes = []
def add_node(name, depth, codelets=[], intrinsic_link_length=None,
             initially_clamped=False, directed=False):
    
    slipnode = Slipnode(name, depth, codelets, intrinsic_link_length,
                        initially_clamped, directed)
    slipnodes.append(slipnode)
    return slipnode

sliplinks = []
def add_link(kind, from_node, to_node, label, fixed_length):
    sliplink = Sliplink(from_node, to_node, label, fixed_length)
    sliplinks.append(sliplink)
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

# Letter nodes
slipnet_letters = []
for letter in string.ascii_lowercase:
    slipnet_letters.append(add_node(str(letter), 10))

# Number nodes
slipnet_numbers = []
for number in range(1, 6):
    slipnet_numbers.append(add_node(str(number), 30))

# String position nodes.
plato_leftmost = add_node('leftmost', 40)
plato_rightmost = add_node('rightmost', 40)
plato_middle = add_node('middle', 40)
plato_single = add_node('single', 40)
plato_whole = add_node('whole', 40)


# Alphabetic position nodes
plato_first = add_node('first', 60)
plato_last = add_node('last', 60)

# Direction nodes
plato_left = add_node('left', 40, ['BondTopDownDirectionScout',
                                  'GroupTopDownDirectionScout'])
plato_right = add_node('right', 40, ['BondTopDownDirectionScout',
                                     'GroupTopDownDirectionScout'])

# Bond nodes
plato_predecessor = add_node('predecessor', 50, ['BondTopDownCategoryScout'],
                             60, directed=True)
plato_successor = add_node('successor', 50, ['BondTopDownCategoryScout'],
                           60, directed=True)
plato_sameness = add_node('sameness', 80, ['BondTopDownCategoryScout'], 0)

# Group nodes
plato_predecessor_group = add_node('predgroup', 50,
                                   ['GroupTopDownCategoryScout'], directed=True)
plato_successor_group = add_node('succgrp', 50, ['GroupTopDownCategoryScout'],
                                directed=True)
plato_sameness_group = add_node('samegrp', 80, ['GroupTopDownCategoryScout'])

# Other relation nodes
plato_identity = add_node('identity', 90, [], 0)
plato_opposite = add_node('opposite', 90, [], 80)

# Object nodes
plato_letter = add_node('letter', 20)
plato_group = add_node('group', 80)

# Category nodes
plato_letter_category = add_node('letter-category', 30, initially_clamped=True)
plato_string_position_category = add_node('string-position-category', 70,
                                          ['DescriptionTopDownScout'],
                                          initially_clamped=True)
plato_alphabetic_position_category = add_node('alphabetic-position-category', 80,
                                              ['DescriptionTopDownScout'])
plato_direction_category = add_node('direction-category', 70)
plato_bond_category = add_node('bond-category', 80)
plato_group_category = add_node('group-category', 80)
plato_length = add_node('length', 60)
plato_object_category = add_node('object-category', 90)
plato_bond_facet = add_node('bond-facet', 90)

# Letter links
for i in range(25):
    before = slipnet_letters[i]
    after = slipnet_letters[i + 1]
    add_link('nonslip', before, after, plato_successor, None)
    add_link('nonslip', after, before, plato_predecessor, None)

# Number links
for i in range(4):
    before = slipnet_numbers[i]
    after = slipnet_numbers[i + 1]
    add_link('nonslip', before, after, plato_successor, None)
    add_link('nonslip', after, before, plato_predecessor, None)

# Letter category links
for i in range(26):
    l = slipnet_letters[i]
    fixed_length = plato_letter_category.conceptual_depth - l.conceptual_depth
    add_link('category', l, plato_letter_category, None, fixed_length)
    add_link('instance', plato_letter_category, l, None, 97)
add_link('category', plato_sameness_group, plato_letter_category, None, 50)

# Length links
for i in range(5):
    n = slipnet_numbers[i]
    fixed_length = plato_length.conceptual_depth - n.conceptual_depth
    add_link('category', n, plato_length, None, fixed_length)
    add_link('instance', plato_length, n, None, 100)
add_link('nonslip', plato_predecessor_group, plato_length, None, 95)
add_link('nonslip', plato_successor_group, plato_length, None, 95)
add_link('nonslip', plato_sameness_group, plato_length, None, 95)

# Opposite links
add_link('slip', plato_first, plato_last, plato_opposite, None)
add_link('slip', plato_last, plato_first, plato_opposite, None)
add_link('slip', plato_leftmost, plato_rightmost, plato_opposite, None)
add_link('slip', plato_rightmost, plato_leftmost, plato_opposite, None)
add_link('slip', plato_left, plato_right, plato_opposite, None)
add_link('slip', plato_right, plato_left, plato_opposite, None)
add_link('slip', plato_successor, plato_predecessor, plato_opposite, None)
add_link('slip', plato_predecessor, plato_successor, plato_opposite, None)
add_link('slip', plato_successor_group, plato_predecessor_group,
                  plato_opposite, None)
add_link('slip', plato_predecessor_group, plato_successor_group,
                  plato_opposite, None)

# Has property links
add_link('property', slipnet_letters[0], plato_first, None,  75)
add_link('property', slipnet_letters[25], plato_last, None, 75)

# Object category links
add_link('category', plato_letter, plato_object_category, None,
           plato_object_category.conceptual_depth - plato_letter.conceptual_depth)
add_link('instance', plato_object_category, plato_letter, None, 100)
add_link('category', plato_group, plato_object_category, None,
           plato_object_category.conceptual_depth - plato_group.conceptual_depth)
add_link('instance', plato_object_category, plato_group, None, 100)

# String position inks
add_link('category', plato_leftmost, plato_string_position_category, None,
  plato_string_position_category.conceptual_depth - plato_leftmost.conceptual_depth)
add_link('instance', plato_string_position_category, plato_leftmost,
                  None, 100)
add_link('category', plato_rightmost, plato_string_position_category, None,
  plato_string_position_category.conceptual_depth - plato_rightmost.conceptual_depth)
add_link('instance', plato_string_position_category, plato_rightmost,
                  None,  100)
add_link('category', plato_middle, plato_string_position_category, None,
  plato_string_position_category.conceptual_depth - plato_middle.conceptual_depth)
add_link('instance', plato_string_position_category, plato_middle,
                  None, 100)
add_link('category', plato_single, plato_string_position_category, None,
  plato_string_position_category.conceptual_depth - plato_single.conceptual_depth)
add_link('instance', plato_string_position_category, plato_single,
                  None, 100)
add_link('category', plato_whole, plato_string_position_category, None,
  plato_string_position_category.conceptual_depth - plato_whole.conceptual_depth)
add_link('instance', plato_string_position_category, plato_whole,
                  None, 100)

# Alphabetic position category links
add_link('category', plato_first, plato_alphabetic_position_category, None,
  plato_alphabetic_position_category.conceptual_depth - plato_first.conceptual_depth)
add_link('instance', plato_alphabetic_position_category, plato_first,
                  None, 100)
add_link('category', plato_last, plato_alphabetic_position_category, None,
  plato_alphabetic_position_category.conceptual_depth - plato_last.conceptual_depth)
add_link('instance', plato_alphabetic_position_category, plato_last,
                  None, 100)

# Direction category links
add_link('category', plato_left, plato_direction_category, None,
  plato_direction_category.conceptual_depth - plato_left.conceptual_depth)
add_link('instance', plato_direction_category, plato_left, None,  100)
add_link('category', plato_right, plato_direction_category, None,
  plato_direction_category.conceptual_depth - plato_right.conceptual_depth)
add_link('instance', plato_direction_category, plato_right, None, 100)

# Bond category links
add_link('category', plato_predecessor, plato_bond_category, None,
  plato_bond_category.conceptual_depth - plato_predecessor.conceptual_depth)
add_link('instance', plato_bond_category, plato_predecessor, None, 100)
add_link('category', plato_successor, plato_bond_category, None,
  plato_bond_category.conceptual_depth - plato_successor.conceptual_depth)
add_link('instance', plato_bond_category, plato_successor, None, 100)
add_link('category', plato_sameness, plato_bond_category, None,
  plato_bond_category.conceptual_depth - plato_sameness.conceptual_depth)
add_link('instance', plato_bond_category, plato_sameness, None, 100)

# Group category links
add_link('category', plato_predecessor_group, plato_group_category, None,
  plato_group_category.conceptual_depth - plato_predecessor_group.conceptual_depth)
add_link('instance', plato_group_category, plato_predecessor_group,
                  None, 100)
add_link('category', plato_successor_group, plato_group_category, None,
  plato_group_category.conceptual_depth - plato_successor_group.conceptual_depth)
add_link('instance', plato_group_category, plato_successor_group,
                  None, 100)
add_link('category', plato_sameness_group, plato_group_category, None,
  plato_group_category.conceptual_depth - plato_sameness_group.conceptual_depth)
add_link('instance', plato_group_category, plato_sameness_group,
                  None, 100)

# Associated group links
add_link('nonslip', plato_sameness, plato_sameness_group,
                  plato_group_category, 30)
add_link('nonslip', plato_successor, plato_successor_group,
                  plato_group_category, 60)
add_link('nonslip', plato_predecessor, plato_predecessor_group,
                  plato_group_category, 60)

# Associated bond links
add_link('nonslip', plato_sameness_group, plato_sameness,
                  plato_bond_category, 90)
add_link('nonslip', plato_successor_group, plato_successor,
                  plato_bond_category, 90)
add_link('nonslip', plato_predecessor_group, plato_predecessor,
                  plato_bond_category, 90)

# Bond facet links
add_link('category', plato_letter_category, plato_bond_facet, None,
  plato_bond_facet.conceptual_depth - plato_letter_category.conceptual_depth)
add_link('instance', plato_bond_facet, plato_letter_category, None, 100)
add_link('category', plato_length, plato_bond_facet, None,
  plato_bond_facet.conceptual_depth - plato_length.conceptual_depth)
add_link('instance', plato_bond_facet, plato_length, None, 100)

# Letter category links
add_link('slip', plato_letter_category, plato_length, None, 95)
add_link('slip', plato_length, plato_letter_category, None, 95)

# Letter group links
add_link('slip', plato_letter, plato_group, None, 90)
add_link('slip', plato_group, plato_letter, None, 90)

# Direction position, direction neighbor, position neighbor links
add_link('nonslip', plato_left, plato_leftmost, None, 90)
add_link('nonslip', plato_leftmost, plato_left, None, 90)
add_link('nonslip', plato_right, plato_leftmost, None, 100)
add_link('nonslip', plato_leftmost, plato_right, None, 100)
add_link('nonslip', plato_right, plato_rightmost, None, 90)
add_link('nonslip', plato_rightmost, plato_right, None, 90)
add_link('nonslip', plato_left, plato_rightmost, None, 100)
add_link('nonslip', plato_rightmost, plato_left, None, 100)
add_link('nonslip', plato_leftmost, plato_first, None, 100)
add_link('nonslip', plato_first, plato_leftmost, None, 100)
add_link('nonslip', plato_rightmost, plato_first, None, 100)
add_link('nonslip', plato_first, plato_rightmost, None, 100)
add_link('nonslip', plato_leftmost, plato_last, None, 100)
add_link('nonslip', plato_last, plato_leftmost, None, 100)
add_link('nonslip', plato_rightmost, plato_last, None, 100)
add_link('nonslip', plato_last, plato_rightmost, None, 100)

# Other links
add_link('slip', plato_single, plato_whole, None, 90)
add_link('slip', plato_whole, plato_single, None, 90)

def get_bond_category(from_node, to_node):
    """Return the node representing the label of the link between the nodes.

    There is either zero or one link between the two nodes.
    """
    if from_node == to_node:
        return plato_sameness
    else:
        for link in from_node.outgoing_links():
            if link.to_node == to_node:
                return link.label

def get_plato_letter(character):
    """Given a character, return the corresponding slipnet letter node."""
    for node in slipnet_letters:
        if node.name == str(character):
            return node

def get_plato_number(number):
    for node in slipnet_numbers:
        if node.name == str(number):
            return node

def are_all_opposite_concept_mappings(concept_mappings):
    '''
    Return True if all the concept mappings in the list have the label
    "opposite".
    '''
    for mapping in concept_mappings:
        if mapping.label != plato_opposite:
            return False
    return True

class Slipnet(object):
    """Slipnet contains nodes and the links between them.
    
    The Slipnet manages the activation and decay of nodes and the conceptual
    distance between them.

    Attributes:
        slipnodes: The nodes in the slipnet.
        sliplinks: The links between the nodes.
        clamp_time: The amount of steps to clamp activation in the slipnet.
    """

    def __init__(self):
        """Initializes Slipnet."""
        self.slipnodes = slipnodes
        self.sliplinks = sliplinks
        self.clamp_time = 50

    def get_label_node(self, from_node, to_node):
        if from_node == to_node:
            return plato_identity
        for link in from_node.outgoing_links():
            if link.to_node == to_node:
                return link.label

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
                    if toolbox.flip_coin(full_activation_probability):
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
        codelets = []
        for node in self.slipnodes:
            if node.activation >= 50:
                for codelet in node.codelets:
                    codelets.append((codelet, [node], node.conceptual_depth / 100.))
        return codelets
