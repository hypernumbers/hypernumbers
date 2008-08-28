#!/usr/bin/env ruby -wKU

# Generates data files for test generators from Excel spreadsheets.
# Usage: ruby regen_termfiles.rb C:\\some_folder_with_xls_files
# (NO TRAILING BACKSLASHES!)

# TODO: Load read function from readexcel.rb & invoke them in here
#       rather than creating a new process to do it.

load "readexcel.rb"

path = ARGV[0] + "\\"
xlsfiles = IO.readlines("list.txt").map { |x| x.strip }

xlsfiles.each do |x|
  fn = path + x
  puts fn
  #`ruby readexcel.rb #{fn}`
  do_file(fn)
  puts "OK: #{File.basename(fn, ".xls")}"
end

puts "ALL DONE"
