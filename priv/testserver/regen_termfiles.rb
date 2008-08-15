#!/usr/bin/env ruby -wKU

# Generates data files for test generators from Excel spreadsheets.
# Usage: ruby regen_termfiles.rb C:\some_folder_with_xls_files
# (NO TRAILING BACKSLASHES!)

# TODO: Load read function from readexcel.rb & invoke them in here
#       rather than creating a new process to do it.

path = ARGV[0].gsub('\\', '\\\\\\\\') + '\\\\'

xlsfiles = Dir[path + "*.xls"]

xlsfiles.each do |fn|
  `ruby readexcel.rb #{fn}`
  puts "OK: #{File.basename(fn, ".xls")}"
end

puts "ALL DONE"
