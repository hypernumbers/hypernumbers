del *.erl

readexcel "c:\opt\code\trunk\tests\excel_files\Win Excel 2007 (as 97)\b_shared_formulae.xls" A2:F11

gen_rev_comp_test "b_shared_formulae_test.yaml"

move "b_*SUITE.erl" "c:\opt\code\trunk\tests\excel_import_1b_test\"

gen_full_test "b_shared_formulae_test.yaml"             "b_shared_formulae_load.yaml"

move "b_*SUITE.erl" "c:\opt\code\trunk\tests\excel_import_2b_test\"
