#!/bin/bash Batch file to generate test files

readexcel "c:\opt\code\trunk\tests\excel_files\Win Excel 2007 (as 97)\c_basic_functions_tests_a_e.xls" B3:B1130

generatetest "c_basic_functions_tests_a_e_test.yaml"

move "c_*SUITE.erl" "c:\opt\code\trunk\tests\excel_import_1c_test\"

rm *.yaml
