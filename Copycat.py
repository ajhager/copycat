# Copyright (c) 2007-2017 Joseph Hager.
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

usage = '%prog [OPTIONS] [INITIAL MODIFIED TARGET SEED]'
version = '%prog 0.1 - (c) 2007-2017 Joseph Hager.\nReleased under the GPLv2'
parser = optparse.OptionParser(usage=usage, version=version)
parser.add_option("-q", "--quiet",
                  action="store_true", dest="quiet", default=False,
                  help="run in headless mode")
(options, args) = parser.parse_args()

if len(args) == 0:
    initial = "abc"
    modified = "abd"
    target = "ijk"
    seed = 42
elif len(args) == 4:
    initial = str(args[0])
    modified = str(args[1])
    target = str(args[2])
    seed = int(args[3])
else:
    parser.error("Please supply all three strings and a seed.")
    
if options.quiet:
    run = Run(initial, modified, target, seed)
    while not run.workspace.answer_string:
        run.step()
    print run.workspace.rule.to_string()
    print "Answer: " + run.workspace.answer_string.name
    print "Temperature: " + str(run.workspace.temperature)
    print "Steps: " + str(run.coderack.time)
else:
    from clients import OpenglClient
    OpenglClient(initial, modified, target, seed)
