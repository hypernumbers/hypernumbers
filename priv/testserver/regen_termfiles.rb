#!/usr/bin/env ruby -wKU

# Regenerates data files for Excel spreadsheets used for testing. The list is
# read from list.txt
# USAGE: Amend the path below, and run. Make sure to separate directories with
# 4 slashes.

PATH_TO_XLS_FILES = "C:\\\\cygwin\\\\home\\\\hasse\\\\proj\\\\hn\\\\tests\\\\excel_files\\\\Win_Excel07_As_97\\\\"

IO.readlines("list.txt").each do |s|
  x = PATH_TO_XLS_FILES + s.strip
  `ruby readexcel.rb #{x}`
  puts "OK: #{s.strip}"
end

puts "ALL DONE"
