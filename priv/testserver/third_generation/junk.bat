del *.erl

readexcel "c:\opt\code\trunk\tests\excel_files\Win Excel 2007 (as 97)\a_alignment.xls" A1:H12 A1:H12

gen_rev_comp_test "a_alignment_test.yaml"

move "a_*SUITE.erl" "c:\opt\code\trunk\tests\excel_import_1a_test\"

gen_full_test "a_alignment_test.yaml"                   "a_alignment_load.yaml"

move "a_*SUITE.erl" "c:\opt\code\trunk\tests\excel_import_2a_test\"

del *.yaml