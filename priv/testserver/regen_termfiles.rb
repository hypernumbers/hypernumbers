#!/usr/bin/env ruby -wKU

# Regenerates data files for Excel spreadsheets used for testing. The list is
# read from list.txt
# USAGE: Amend the path below, and run. Make sure to separate directories with
# 4 slashes.

# TODO: Use readlist.txt and simply discard range information.
# TODO: Get path to XLS files on the command line.
# TODO: Load read function from readexcel.rb & invoke them in here rather than creating
#       a new process to do it.

path = ARGV[0].gsub('\\', '\\\\\\\\') + '\\\\'

IO.readlines("list.txt").each do |s|
  x = path + s.strip
  `ruby readexcel.rb #{x}`
  puts "OK: #{s.strip}"
end

puts "ALL DONE"
