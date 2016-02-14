import string
import re

#--------------------------------------
# Python normalization functions
#--------------------------------------
def normalise_duplicate_errmsg( msg ):
     return re.sub(r"version\s(\d+\.\d+\.\d+)\sfor\s[a-z,A-Z,0-9,_,-]+\)", r")", "GHC version 8.1.20160205 for x86_64-unknown-mingw32")

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

test('T11223_simple_duplicate',
     normalise_errmsg_fun(normalise_duplicate_errmsg),
     run_command,
     ['$MAKE -s --no-print-directory t_11223_simple_duplicate'])

test('T11223_simple_duplicate_lib',
     [],
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
     [],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_link_order_a_b_2_fail'])

test('T11223_link_order_b_a_2_succeed',
     [],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_link_order_b_a_2_succeed'])

test('T11223_weak_only_link_fail',
     [],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_only_link_fail'])

test('T11223_weak_only_link_succeed',
     [],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_only_link_succeed'])

test('T11223_weak_both_link_order_a_b_succeed',
     [],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_both_link_order_a_b_succeed'])

test('T11223_weak_both_link_order_b_a_succeed',
     [],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_both_link_order_b_a_succeed'])

test('T11223_weak_single_link_order_a_b_succeed',
     [],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_single_link_order_a_b_succeed'])

test('T11223_weak_single_link_order_b_a_succeed',
     [],
     run_command,
     ['$MAKE -s --no-print-directory t_11223_weak_single_link_order_b_a_succeed'])