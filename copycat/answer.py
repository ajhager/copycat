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

from coderack import Codelet

class AnswerBuilder(Codelet):
    @staticmethod
    def run(coderack, slipnet, workspace):
        workspace.answer_string = String(-1, "answer string")
        workspace.changed_length_group = None
        workspace.snag_object = None

        if workspace.translated_rule.has_no_change():
            objects_to_change = None
        else:
            objects_to_change = workspace.get_objects_to_change_for_answer()

        desscription_type = workspace.translated_rule.replaced_description_type()

        answer_string_letters = []
        for obj in workspace.target_string.objects:
            if obj in objects_to_changed:
                answer_string_letters.extend(workspace.get_modified_letters_for_answer(obj, description_type))

        if workspace.snag_object:
            workspace.deal_with_snag()
        else:
            answer_string_lettes.extend(workspace.get_unmodified_letters_for_answer(objects_to_change))
            if workspace.changed_length_group:
                for letter in answer_string_letters:
                    if letter not in workspace.modified_letters and \
                       letter.left_string_position > workspace.changed_length_group.right_string_position:
                        letter.set_left_string_position(letter.left_string_position + workspace.amount_length_changed)
                        letter.set_right_string_position(letter.right_string_position + workspace_amount_length_changed)

            for letter in answer_string_letters:
                workspace.answer_string.add_letter(letter)
            workspace.found_answer = True
