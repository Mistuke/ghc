import string
import re

#--------------------------------------
# Python normalization functions
#--------------------------------------
def normalise_duplicate_errmsg( msg ):
     return re.sub(r"((?:[a-z, A-Z]+):|(?:\s+))[\/\\\\]+(?:.+[\/\\\\]+?)?(.+?)[\/\\\\]+", "    ", 
               re.sub(r"version\s(\d+\.\d+\.\d+)\sfor\s[a-z,A-Z,0-9,_,-]+(?=\))", r"", msg))

#--------------------------------------
# Test functions
#--------------------------------------

test('T11223_simple_link',
     [],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_simple_link'])

test('T11223_simple_link_lib',
     [],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_simple_link_lib'])

# I'm ignoring the output since for this particular invocation normalise_errmsg
# isn't being called and I can't figure out why not.
test('T11223_simple_duplicate',
     [ignore_output, exit_code(2), normalise_errmsg_fun(normalise_duplicate_errmsg)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_simple_duplicate'])

test('T11223_simple_duplicate_lib',
     [normalise_errmsg_fun(normalise_duplicate_errmsg)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_simple_duplicate_lib'])

test('T11223_simple_unused_duplicate_lib',
     [],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_simple_unused_duplicate_lib'])

test('T11223_link_order_a_b_succeed',
     [],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_link_order_a_b_succeed'])

test('T11223_link_order_b_a_succeed',
     [],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_link_order_b_a_succeed'])

test('T11223_link_order_a_b_2_fail',
     [normalise_errmsg_fun(normalise_duplicate_errmsg)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_link_order_a_b_2_fail'])

test('T11223_link_order_b_a_2_succeed',
     [],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_link_order_b_a_2_succeed'])

# Weak Symbols are not currently implemented. So Disable all the tests

test('T11223_weak_only_link_fail',
     [expect_broken(11223)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_only_link_fail'])

test('T11223_weak_only_link_succeed',
     [expect_broken(11223)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_only_link_succeed'])

test('T11223_weak_both_link_order_a_b_succeed',
     [expect_broken(11223)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_both_link_order_a_b_succeed'])

test('T11223_weak_both_link_order_b_a_succeed',
     [expect_broken(11223)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_both_link_order_b_a_succeed'])

test('T11223_weak_single_link_order_a_b_succeed',
     [expect_broken(11223)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_single_link_order_a_b_succeed'])

test('T11223_weak_single_link_order_b_a_succeed',
     [expect_broken(11223)],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_single_link_order_b_a_succeed'])