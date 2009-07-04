#!/usr/bin/env python

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

import optparse

from copycat.run import Run
from clients import CursesClient, OpenglClient

usage = '%prog [OPTIONS] [INITIAL MODIFIED TARGET SEED]'
version = '%prog 0.1 - (c) 2007-2009 Joseph Hager.\nReleased under the GPLv2'
parser = optparse.OptionParser(usage=usage, version=version)
parser.add_option('-m', '--mode', dest='mode', default='headless',
                  help='interaction mode: curses, opengl')
(options, args) = parser.parse_args()

if len(args) == 0:
    initial = None
    modified = None
    target = None
    seed = None
elif len(args) == 4:
    initial = str(args[0])
    modified = str(args[1])
    target = str(args[2])
    seed = int(args[3])
else:
    parser.error('must supply all three strings and a seed')

if options.mode == 'headless':
    if not initial:
        initial = raw_input('Initial: ')
        modified = raw_input('Modified: ')
        target = raw_input('Target: ')
        seed = int(raw_input('Seed: '))
    run = Run(initial, modified, target, seed)
    while not run.workspace.answer_string:
        run.step()
    print str(run.workspace.answer_string)
elif options.mode == 'curses':
    CursesClient(initial, modified, target, seed)
elif options.mode == 'opengl':
    OpenglClient(initial, modified, target, seed)
