#!/usr/bin/env ruby -wKU

# Generates data files for test generators from Excel spreadsheets.
# Usage: ruby regen_termfiles.rb C:\some_folder_with_xls_files
# (NO TRAILING BACKSLASHES!)

# TODO: Load read function from readexcel.rb & invoke them in here
#       rather than creating a new process to do it.

ARGVX = ARGV[0]

puts "ARGVX is #{ARGVX}"

path = ARGV[0].gsub('\\', '\\\\\\\\') + '\\\\'

puts "Path is #{path}"

xlsfiles = Dir[path + "*.xls"]

puts "XL files are #{xlsfiles}"

xlsfiles.each do |fn|
  `ruby readexcel.rb #{fn}`
  puts "OK: #{File.basename(fn, ".xls")}"
end

puts "ALL DONE"
