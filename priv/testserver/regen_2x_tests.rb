#!/usr/bin/env ruby -wKU

# To regenerate only 2c, run as regen_2x_test.rb c etc.

PATH_TO_DATA_FILES = "../../tests/excel_files/Win_Excel07_As_97/DATA/"
#PATH_TO_DATA_FILES = "C:\\\\cygwin\\\\home\\\\hasse\\\\proj\\\\hn\\\\tests\\\\excel_files\\\\Win_Excel07_As_97\\\\DATA\\\\"

PATH_TO_TESTS = "../../tests/"
#PATH_TO_TESTS = "C:\\\\cygwin\\\\home\\\\hasse\\\\proj\\\\hn\\\\tests\\\\"

ls = IO.readlines("load_ranges_list.txt").map { |l| l.strip }
ls = ARGV[0] != nil ? ls.select { |l| l[0..1] == "#{ARGV[0]}_" } : ls

ls.each { |l|
  a = l.split(/\s+/)
  print "-> #{a[0]} ....."; $stdout.flush
  `ruby gen_full_test.rb #{PATH_TO_DATA_FILES + a[0]}.dat #{a.slice(1..-1).join(" ")}`
  puts " OK"
}

puts "DONE GENERATING."

mv = ARGV[0] == nil ? ["a", "b", "c", "d", "e", "x"] : [ARGV[0]]

mv.each do |l|
  `mv #{l}_*erl #{PATH_TO_TESTS + "excel_import_2" + l + "_test"}`
end

puts "ALL DONE."


