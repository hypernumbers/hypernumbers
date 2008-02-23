del *.erl

readexcel "c:\opt\code\trunk\tests\excel_files\Win Excel 2007 (as 97)\b_array_formulae.xls" A2:C7

gen_rev_comp_test "b_array_formulae_test.yaml"

move "b_*SUITE.erl" "c:\opt\code\trunk\tests\excel_import_1b_test\"

gen_full_test "b_array_formulae_test.yaml"             "b_array_formulae_load.yaml"

move "b_*SUITE.erl" "c:\opt\code\trunk\tests\excel_import_2b_test\"
