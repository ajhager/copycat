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
from copycat.workspace import String

class AnswerBuilder(Codelet):
    def run(self, coderack, slipnet, workspace):
        workspace.changed_length_group = None
        workspace.snag_object = None

        if workspace.translated_rule.has_no_change():
            objects_to_change = None
        else:
            objects_to_change = workspace.objects_to_change_for_answer()

        desc_type = workspace.translated_rule.replaced_description_type

        # Change the objects needed in the target string.
        answer_string_letters = []
        if objects_to_change:
            for obj in workspace.target_string.objects():
                if obj in objects_to_change:
                    letters = workspace.modified_letters_for_answer(obj, desc_type)
                    answer_string_letters.extend(letters)

        if workspace.snag_object:
            workspace.snag_count += 1
            workspace.last_snag_time = coderack.time
            workspace.snag_structures = workspace.structures()

            for bond in workspace.proposed_bonds:
                bond.string.delete_proposed_bond(bond)
            for group in workspace.proposed_groups:
                group.string.delete_proposed_group(g)
            for correspondence in workspace.proposed_correspondences:
                workspace.delete_proposed_correspondence(correspondence)
        
            workspace.translated_rule = None
            workspace.answer_string = None
            workspace.snag_condition = True
            workspace.temperature = 100
            workspace.clamp_temperature = True
            for description in workspace.snag_object.descriptions():
                description.descriptor.clamp = True
            workspace.snag_object.clamp_salience = True
            
            coderack.clear()

            return workspace.initial_codelets()

        letters = workspace.get_unmodified_letters_for_answer(objects_to_change)
        answer_string_letters.extend(letters)
        if workspace.changed_length_group:
            for letter in answer_string_letters:
                left_position = letter.left_string_position
                right_position = letter.right_string_position
                group_position = self.changed_length_group.right_string_position
                if (letter not in self.modified_letters) and \
                   (left_position > group_position):
                    letter.left_string_position += self.amount_length_changed
                    letter.right_string_position = letter.left_string_position+\
                            self.amount_length_changed

        name = ""
        for letter in answer_string_letters:
            name += letter.name
        workspace.answer_string = String(workspace, name)
        for letter in answer_string_letters:
            workspace.answer_string.add_letter(letter)

        workspace.answer_found = True

