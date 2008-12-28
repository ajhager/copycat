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

class String(object):
    def __init__(self, string):
        self.highest_string_number = 0
        self.letters = []
        self.proposed_bonds = []
        self.left_right_bonds = []
        self.from_to_bonds = []
        self.proposed_groups = []
        self.object_positions = []
        self.length = 0
        self.name = ''
        self.objert_spaces = 0
        self.number_of_bonds_to_scan_distribution = []
        self.intra_string_unhappiness = 0

    def __str__(self):
        return ''

    def random_object(self):
        pass

    def random_letter(self):
        pass

    def choose_object(self, method):
        pass

    def choose_leftmost_object(self):
        pass

    def proposed_bonds(self):
        pass

    def bonds(self):
        return []

    def proposed_groups(self):
        pass

    def groups(self):
        return []

    def objects(self):
        return []

    def non_string_spanning_objects(self):
        pass

    def add_letter(self, letter):
        pass

    def objects_by_category(self, category):
        pass

    def add_proposed_bond(self, bond):
        pass

    def remove_proposed_bond(self, bond):
        pass

    def add_bond(self, bond):
        pass

    def remove_bond(self, bond):
        pass

    def add_proposed_group(self, group):
        pass

    def remove_proposed_groupd(self, group):
        pass

    def add_group(self, group):
        pass

    def remove_group(self, group):
        pass

    def make_room_for_new_object(self):
        pass

    def get_letter(self, position):
        pass

    def get_group(self, position):
        pass

    def is_bond_present(self, bond):
        pass

    def get_bond(self, from_object, to_object):
        pass

    def is_group_present(self, group):
        pass

    def local_bond_category_relevance(self, bond_category):
        pass

    def local_direction_category_relevance(self, direction_category):
        pass

    def update_relative_importances(self):
        pass

    def calculate_intra_string_unhappiness(self):
        pass

    def update_intra_string_unhappiness(self):
        pass
