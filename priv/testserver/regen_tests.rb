#!/usr/bin/env ruby -wKU

# Run as:
#   ruby regen_tests.rb 1x -- to regenerate all 1x suites (or 2x).
#   ruby regen_tests.rb 1x a -- to regenerate the a suite only.
#   ruby regen_tests.rb 2x abc -- to regeneratee suites a, b, and c.

PATH_TO_DATA_FILES = "../../tests/excel_files/Win_Excel07_As_97/DATA/"

generator = (ARGV[0].downcase == "1x" ? "gen_rev_comp_test.rb" : "gen_full_test.rb")
series = ARGV[0].downcase[0].chr

rawlines = IO.readlines("readlist.txt").map { |line| line.strip }
lines = (if ARGV[1] # Given test suite id, e.g. a?
           rawlines.select { |line| # Select lines which start with one of those letters
             ARGV[1].include?(line[0].chr)
           }
         else
           rawlines
         end)

lines.map do |line|
  toks = line.split(/\s+/)
  print "-> #{toks[0]} ... "; $stdout.flush
  filename = PATH_TO_DATA_FILES + toks[0] + ".dat" # Don't use File#join here!
  ranges = toks.slice(1..-1).join(" ")

  `ruby #{generator} #{filename} #{ranges}`
  puts"OK"
end

mvs = if ARGV[1]
        chars = []
        ARGV[1].each_byte { |b| chars << b.chr }
        chars
      else
        ["a", "b", "c", "d", "e"]
      end

mvs.each do |char|
  dir = "excel_import_#{series}#{char}_test"
  `mkdir ../../tests/#{dir}`
   `mv #{PATH_TO_DATA_FILES}/#{char}*.erl ../../tests/#{dir}`
end

puts "ALL DONE."
