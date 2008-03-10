del *.erl

readexcel "c:\opt\code\trunk\tests\excel_files\Win Excel 2007 (as 97)\e_bitwise.xls" A1:E7 A1:V64 A1:V64 A1:V64 A1:V64 A1:V64
readexcel "c:\opt\code\trunk\tests\excel_files\Win Excel 2007 (as 97)\e_trig.xls" A1:E6 A1:AQ49 A1:AQ49
#readexcel "c:\opt\code\trunk\tests\excel_files\Win Excel 2007 (as 97)\e_operator.xls" A1:C16  A1:V66 A1:V66 A1:V66 A1:V66 A1:V66 A1:V66 A1:V66 A1:V66 A1:V66 A1:V66 A1:V66 A1:V66

gen_rev_comp_test "e_bitwise_test.yaml"
gen_rev_comp_test "e_operator_test.yaml"
#gen_rev_comp_test "e_operator_test.yaml"

move "e_*SUITE.erl" "c:\opt\code\trunk\tests\excel_import_1e_test\"

gen_full_test "e_bitwise_test.yaml"             "e_bitwise_load.yaml"
gen_full_test "e_trig_test.yaml"             "e_trig_load.yaml"
#gen_full_test "e_operator_test.yaml"             "e_operator_load.yaml"

move "e_*SUITE.erl" "c:\opt\code\trunk\tests\excel_import_2e_test\"
