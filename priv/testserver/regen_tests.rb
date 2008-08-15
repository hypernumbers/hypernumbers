#!/usr/bin/env ruby -wKU

# Run as:
#   ruby regen_tests.rb 1x -- to regenerate all 1x suites (or 2x).
#   ruby regen_tests.rb 1x a -- to regenerate the a suite only.
#   ruby regen_tests.rb 2x abc -- to regeneratee suites a, b, and c.

require "fileutils"

PATH_TO_DATA_FILES = "../../tests/excel_files/Win_Excel07_As_97/DATA/"
fullpath = __FILE__ + PATH_TO_DATA_FILES

generator = (ARGV[0].downcase == "1x" ?
             "reader_test_generator.rb" : "gen_full_test.rb")
datfiles = Dir[fullpath + "*.dat"]

datfiles.select { |fn| ARGV[1].include?(File.basename(fn)[0].chr) }.each do |fn|
  print "-> #{File.basename(fn, ".dat")}"; $stdout.flush
  `ruby #{generator} #{fn}`
  puts "OK"
end

mvs = if ARGV[1]
        chars = []
        ARGV[1].each_byte { |b| chars << b.chr }
        chars
      else
        ["a", "b", "c", "d", "e", "x"]
      end

series = ARGV[0].downcase[0].chr
mvs.each do |char|
  dir = "excel_import_#{series}#{char}_test"
  FileUtils.mkdir("../../tests/#{dir}") rescue nil
  FileUtils.mv("#{char}*.erl" "../../tests/#{dir}") rescue nil
end

puts "ALL DONE."
