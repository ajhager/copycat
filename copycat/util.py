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

import math
import random

def select_list_position(int_list):
    ''' 
    Probabilistically chooses one of the integers in the list according to
    value.  Returns the position of that integer.
    '''
    if int_list == []:
        return

    int_list_sum = sum(int_list)
    if int_list_sum <= 0:
        return random.randint(0, len(int_list) - 1)

    value = random.randint(0, int_list_sum - 1)
    new_int_sum = 0
    counter = 0
    for item in int_list:
        new_int_sum += item
        counter += 1
        if new_int_sum > value:
            return counter - 1

def select_assoc(assoc_list):
    '''
    assoc_list is of the form:
        [(item, probability), (item, probability) . . .]
    Returns one of the items, chosen probabilistically.
    '''
    if assoc_list == []:
        return

    probability_sum = 0
    for item, probability in assoc_list:
        probability_sum += probability
    if probability_sum <= 0:
        return

    value = random.uniform(0, probability_sum)
    new_probability_sum = 0
    for item, probability in assoc_list:
        new_probability_sum += probability
        if new_probability_sum > value:
            return item

def flatten(list_):
    '''
    Flattens a list so that it has no nested structure.
    '''
    if isinstance(list_, list):
        return sum(map(flatten, list_), [])
    else:
        return [list_]

def flip_coin(prob_of_true = .5):
    '''
    Returns either True or False based on the probabity of true sent as an
    argument.
    '''
    if prob_of_true >= 1:
        return True

    return select_assoc([[True, int(prob_of_true * 1000)]
                        ,[False, int((1 - prob_of_true) * 1000)]])

def weighted_average(*value_weight_pairs):
    '''
    Return the weighted arithmetic mean of arguments: (value, weight), . . .
    '''
    value_sum = 0
    weight_sum = 0
    for value, weight in value_weight_pairs:
        value_sum += value * weight
        weight_sum += weight
    return int(value_sum / weight_sum)

def blur(number):
    '''
    A normal distribution around number within square_root(number).
    '''
    return random.normalvariate(number, math.sqrt(number))
