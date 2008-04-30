#!/usr/bin/env ruby -wKU

PATH_TO_DATA_FILES = "../../tests/excel_files/Win_Excel07_As_97/DATA/"
#PATH_TO_DATA_FILES = "C:\\\\cygwin\\\\home\\\\hasse\\\\proj\\\\hn\\\\tests\\\\excel_files\\\\Win_Excel07_As_97\\\\DATA\\\\"

PATH_TO_TESTS = "../../tests/"
#PATH_TO_TESTS = "C:\\\\cygwin\\\\home\\\\hasse\\\\proj\\\\hn\\\\tests\\\\"

IO.readlines("load_ranges_list.txt").map { |l| l.strip }.each { |l|
  a = l.split(/\s+/)
  `ruby gen_rev_comp_test.rb #{PATH_TO_DATA_FILES + a[0]}.dat #{a.slice(1..-1).join(" ")}`
  puts "OK: #{l.split(/\s+/)[0]}"
}

puts "DONE GENERATING."

["a", "b", "c", "d", "e", "x"].each do |l|
  `mv #{l}_*erl #{PATH_TO_TESTS + "excel_import_1" + l + "_test"}`
end

puts "ALL DONE."


