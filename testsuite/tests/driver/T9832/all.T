test('T9832',
     [clean_cmd('$MAKE -s clean')],
     run_command,
     ['$MAKE -s --no-print-directory T9832'])
     
test('T9832a',
     [clean_cmd('$MAKE -s clean')],
     run_command,
     ['$MAKE -s --no-print-directory T9832a'])

