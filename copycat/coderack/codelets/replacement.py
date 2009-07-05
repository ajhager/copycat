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
import copycat.slipnet as nodes
from copycat.workspace import Replacement

class ReplacementFinder(Codelet):
    '''
    Chooses a letter at random in the initial string. Checks if it is the
    changed leter and marks it as changed. Adds a description of the 
    relation describing the change if there is one. Can only deal with
    letters changing into letters, not letters changing into groups or
    vice versa.
    '''
    def run(self, coderack, slipnet, workspace):
        i_letter = workspace.initial_string.random_letter()
        if i_letter.replacement:
            return

        index = i_letter.left_string_position
        m_letter = workspace.modified_string.letters[index]

        # Check if m_letter's letter_category is different form i_letter's.
        i_letter_category = i_letter.get_descriptor(nodes.plato_letter_category)
        m_letter_category = m_letter.get_descriptor(nodes.plato_letter_category)
        if i_letter_category != m_letter_category:
            i_letter.changed = True
            change_relation = slipnet.get_label_node(i_letter_category,
                                                     m_letter_category)
            if change_relation:
                description = ExtrinsicDescription(change_relation,
                                                   nodes.plato_letter_category,
                                                   i_letter)
                m_letter.add_extrinsic_description(description)

        replacement = Replacement(i_letter, m_letter)
        workspace.add_replacement(replacement)
        i_letter.replacement = replacement
