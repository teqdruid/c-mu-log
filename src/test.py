#!/usr/bin/python

import os.path
import os
import glob
import sys

def run_and_compare(cmd, testOut):
    pipe = os.popen(cmd)
    to = open(testOut)
    for line in pipe.readlines():
        toLine = to.readline()
        if toLine != line:
            to.close()
            pipe.close()
            return False

    pipe.close()
    if to.readline() != '':
        to.close()
        return False
    to.close()
    return True

for test in glob.glob("tests/*.ul"):
    testOut = test.replace(".ul", ".out")
    if not os.path.exists(testOut):
        print "Warning, no comparison output exists for ", test
        continue
    sys.stdout.write("Running %-25s..." % test)
    sys.stdout.flush()
    if (test.find("/pr") != -1):
        prog = "print <"
    else:
        if (test.find("/sim") != -1):
            prog = "cat "
        else:
            prog = "culog"
    if run_and_compare("./%s %s 2>&1" % (prog, test), testOut):
        print "\tOK"
    else:
        print "\tFAIL!"
