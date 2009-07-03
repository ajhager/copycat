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

class AnswerBuilder(Codelet):
    def run(self):
        # Runs the translated rule on the target string to build the answer.
        self.answer_string = String('')
        self.answer_string.highest_string_number = -1

        # Used when the answer involves changing the length of a group.
        self.changed_length_group = None

        # Used in case there is a snag while trying to build the answer.
        self.snag_object = None

        # Get objects in the target string that need changing.
        if self.translated_rule.no_change():
            objects_to_change = None
        else:
            objects_to_change = self.objects_to_change_for_answer()

        # Get the description type to change.
        description_type = self.translated_rule.replaced_description_type

        # Change the objects needed in the target string.
        answer_string_letters = []
        for object in self.target_string.objects():
            if object in objects_to_change:
                letters = self.modified_letters_for_answer(object,
                                                           description_type)
                answer_string_letters.extend(letters)

        # If there was snag building the answer, deal with it and fizzle.
        if self.snag_object:
            self.snag_count += 1
            self.snag_structures = self.structures()

            # Remove proposed structures.
            for bond in self.proposed_bonds:
                bond.string.delete_proposed_bond(bond)
            for group in self.proposed_groups:
                group.string.delete_proposed_group(g)
            for correspondence in self.proposed_correspondences:
                self.delete_proposed_correspondence(correspondence)
        
            # Reset answer variables, clamp temperature, and clamp snag nodes.
            self.translated = None
            self.answer_string = None
            self.snag_condition = True
            self.temperature = 100
            self.clamp_temperature = True
            for description in self.snag_object.descriptions():
                description.clamp = True
            self.snag_object.clamp_salience = True

            # Set flag to empty the coderack and post initial codelets.
            return (True, self.initial_codelets())

        # Set up the answer string.
        # Add unmodified letters.
        letters = self.unmodified_letters_for_answer(objects_to_change)
        answer_string_letters.extend(letters)

        # If the rule directed a length change, fix the letter positions.
        if self.changed_length_group:
            for letter in answer_string_letters:
                left_position = letter.left_string_position
                right_position = letter.right_string_position
                group_position = self.changed_length_group.right_string_position
                if (letter not in self.modified_letters) and \
                   (left_position > group_position):
                    letter.left_string_position += self.amount_length_chnaged
                    letter.right_string_position = letter.left_string_position+\
                            self.amount_length_changed

        # Set up the answer string.
        for letter in answer_string_letters:
            self.answer_string.add_letter(letter)
