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

import sys
sys.path.insert(0, "lib")
import optparse

from copycat.run import Run

# TRACE: http://www.dalkescientific.com/
import linecache
import inspect

def traceit(frame, event, arg):
    if event == 'line':
        lineno = frame.f_lineno
        if '__file__' in frame.f_globals:
            filename = frame.f_globals['__file__']
            if (filename.endswith('.pyc') or
                filename.endswith('.pyo')):
                filename = filename[:-1]
            name = frame.f_globals['__name__']
            line = linecache.getline(filename, lineno)
        else:
            name = '[unknown]'
            try:
                src = inspect.getsourcelines(frame)
                line = src[lineno]
            except IOError:
                line = 'Unknown code named [%s].  VM instruction #%d' % \
                    (frame.f_code.co_name, frame.f_lasti)
        print '%s:%s: %s' % (name, lineno, line.rstrip())
    return traceit
# TRACE END

usage = '%prog [OPTIONS] [INITIAL MODIFIED TARGET SEED]'
version = '%prog 0.1 - (c) 2007-2009 Joseph Hager.\nReleased under the GPLv2'
parser = optparse.OptionParser(usage=usage, version=version)
parser.add_option('-m', '--mode', dest='mode', default='headless',
                  help='interaction mode: curses, opengl')
parser.add_option("-t", "--trace",
                  action="store_true", dest="trace", default=False,
                  help="generate a execution trace")
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

if options.trace:
    sys.settrace(traceit)

if options.mode == 'headless':
    if not initial:
        initial = raw_input('Initial: ')
        modified = raw_input('Modified: ')
        target = raw_input('Target: ')
        seed = int(raw_input('Seed: '))
    run = Run(initial, modified, target, seed)
    while not run.workspace.answer_string:
        run.step()
    print "%5s %s %s %s" % (seed,
                            str(run.workspace.answer_string.name),
                            run.workspace.temperature,
                            run.coderack.time)
elif options.mode == 'curses':
    from clients import CursesClient
    CursesClient(initial, modified, target, seed)
elif options.mode == 'opengl':
    from clients import OpenglClient
    OpenglClient(initial, modified, target, seed)
