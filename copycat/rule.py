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

from workspace import Structure

class Rule(Structure):
    def __init__(self, object_category1, descriptor1_facet, descriptor1,
                 object_category2, replaced_description_type, relation=None):
        self.object_category1 = object_category1
        self.descriptor1_facet = descriptor1_facet
        self.descriptor1 = descriptor1
        self.object_category2 = object_category2
        self.replaced_description_type = replaced_description_type
        self.relation = relation
        self.structure_category = 'rule'

    def __eq__(self, other):
        return self.object_category1 == other.object_category1 and \
                self.descriptor1_facet == other.descriptor1_facet and \
                self.descriptor1 == other.descriptor1 and \
                self.object_category2 == other.object_category2 and \
                self.descriptor2 == other.descriptor2 and \
                self.relation == other.relation and \
                self.replaced_description_type == other.replaced_description_type

    def expresses_relation(self):
        return self.relation

    def is_no_change_made(self):
        return not self.descriptor1

