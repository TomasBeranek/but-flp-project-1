#!/usr/bin/env python3

#  File    tester.py
#  Author  Tomas Beranek <xberan46@stud.fit.vutbr.cz>
#  Brief   Automated script for running tests
#  Date    1.3.2022
#  Up2date sources can be found at: https://github.com/TomasBeranek/but-flp-project-1

import os
import sys
import subprocess

class bcolors:
    PASSED = '\033[92m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'

script_name = sys.argv[0]
tested_binary = sys.argv[1]
tests_dir = sys.argv[2]

if tests_dir[-1] != "/":
    tests_dir += "/"

tests = os.listdir(tests_dir)

tests_args      = [ file for file in tests if file.endswith(".args") ]
tests_input     = [ file for file in tests if file.endswith(".in") ]
tests_output    = [ file for file in tests if file.endswith(".out") ]
tests_rc        = [ file for file in tests if file.endswith(".rc") ]

tests_args.sort()
tests_input.sort()
tests_output.sort()
tests_rc.sort()

passed_cnt = 0
failed_cnt = 0
# requires every file (args, in, out, rc) for every test case
# even if some won't be used
for args, input, output, rc in zip(tests_args, tests_input, tests_output, tests_rc):
    test_name = args[:-5]
    input_filename = tests_dir + input

    with open(tests_dir + args, 'r') as file:
        args = file.read().replace('\n', '')

    with open(tests_dir + input, 'r') as file:
        input = file.read()

    with open(tests_dir + output, 'r') as file:
        output = file.read()

    with open(tests_dir + rc, 'r') as file:
        rc = int(file.read().replace('\n', ''))

    for inpute_type in ["stdin", "file"]:
        cmd = ["./" + tested_binary, args]
        result = None

        if inpute_type == "file":
            cmd += [input_filename]
            result = subprocess.run(cmd, stdout=subprocess.PIPE)
        else:
            input = input.encode("utf-8")
            result = subprocess.run(cmd, stdout=subprocess.PIPE, input = input)

        output_actual = result.stdout.decode("utf-8")
        rc_actual = result.returncode

        if inpute_type == "file":
            print((test_name + " (file)").ljust(20), end = "")
        else:
            print((test_name + " (stdin)").ljust(20), end = "")

        if rc == rc_actual:
            if output == output_actual:
                print(f"{bcolors.PASSED}PASSED{bcolors.ENDC}")
                passed_cnt += 1
            else:
                print(f"{bcolors.FAIL}FAILED{bcolors.ENDC}  Outputs differ!")
                failed_cnt += 1

                print("#"*20 + "  Expected  " + "#"*20)
                print(output)
                print("#"*21 + "  Actual  " + "#"*21)
                print(output_actual)
                print("#"*52)

        else:
            print(f"{bcolors.FAIL}FAILED{bcolors.ENDC}  Return codes differ!")
            failed_cnt += 1

            print(f"Expected:  {rc}")
            print(f"Actual:    {rc_actual}")

# final stats
print()
print(  f"{bcolors.PASSED}PASSED{bcolors.ENDC}: {passed_cnt}     " +
        f"{bcolors.FAIL}FAILED{bcolors.ENDC}: {failed_cnt}")
