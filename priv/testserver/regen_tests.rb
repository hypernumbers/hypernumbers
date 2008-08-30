#!/usr/bin/env ruby -wKU

# Run as:
#   ruby regen_tests.rb 1x -- to regenerate all 1x suites (or 2x).
#   ruby regen_tests.rb 1x a -- to regenerate the a suite only.
#   ruby regen_tests.rb 2x abc -- to regenerate suites a, b, and c.

require "fileutils"

series = ARGV[0].downcase[0].chr
PATH_TO_DATA_FILES = "../../tests/excel_files/Win_Excel07_As_97/DATA/"
fullpath = File.join(Dir.pwd, PATH_TO_DATA_FILES)
generator = (series == "1" ?
             "reader_test_generator.rb" : "gen_full_test.rb")
datfiles = Dir[fullpath + "*.dat"]

suites = ARGV[1] || "abcdef"

datfiles.select { |fn| suites.include?(File.basename(fn)[0].chr) }.each do |fn|
  print "-> #{File.basename(fn, ".dat")}... "; $stdout.flush
  `ruby #{generator} #{fn}`
  puts "OK"
end

mvs = if ARGV[1]
        chars = []
        ARGV[1].each_byte { |b| chars << b.chr }
        chars
      else
        ["a", "b", "c", "d", "e", "f", "x"]
      end

mvs.each do |char|
  dir = "excel_import_#{series}#{char}_test"
  FileUtils.mkdir("../../tests/#{dir}") rescue nil

  Dir["#{char}*.erl"].each do |fn|
    FileUtils.mv(fn, "../../tests/#{dir}/") rescue nil
  end
end

puts "ALL DONE."
