readexcel "c:\opt\code\trunk\tests\excel_files\Win Excel 2007 (as 97)\a_alignment.xls" A1:H12 A1:H12
readexcel "c:\opt\code\trunk\tests\excel_files\Win Excel 2007 (as 97)\a_should_be_10_tests.xls" A1:B5 

gen_rev_comp_test "a_alignment_test.yaml"
gen_rev_comp_test "a_should_be_10_tests_test.yaml"

move "a_*SUITE.erl" "c:\opt\code\trunk\tests\excel_import_1a_test\"
