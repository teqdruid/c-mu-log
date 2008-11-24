#!/usr/bin/python

import os.path
import os
import glob

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
    print "Running %s..." % test
    if run_and_compare("./culog %s 2>&1" % test, testOut):
        print "\tok."
    else:
        print "\tFAIL!"
