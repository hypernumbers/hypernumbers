#!/usr/bin/env ruby -wKU

# Generates a test suite for Muin (from our reader's output).
# <hasan@hypernumbers.com>

# USAGE: gen_full_test.rb /path/to/data_file [RANGE:ONE, RANGE:TWO etc]

# If a range is given, test cases will be generated for cells in that
# range only. If more than one range is given, they are taken to mean
# ranges on sheets.

# TODO: Sort test cases nicely (row/column or column/row).

$KCODE = 'u'

require "erb"
require "ssdoc"

datafile = ARGV[0]
ranges = ARGV.slice(1..-1)

ssdoc = Ssdoc.new(datafile)

@basename = File.basename(datafile, ".dat").downcase


# Utility functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

def preptype(val)
  t = {
    -2146826288 => "'#NULL!'",
    -2146826281 => "'#DIV/0!'",
    -2146826273 => "'#VALUE!'",
    -2146826265 => "'#REF!'",
    -2146826259 => "'#NAME?'",
    -2146826252 => "'#NUM!'",
    -2146826246 => "'#N/A'",
  }

  if val.kind_of?(String)
    literal_or_utf8list(val)
  else
    t[val] or val
  end
end

# Main generation code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# populate test case data, which will be a list of
# [case name, path, A1-style cell ref, expected value]
@testcasedata = []
ssdoc.sheets.each_with_index do |sheet, idx|
  currange = ranges[idx] == nil ? [] : expand_range(ranges[idx])
  sheet.cells.each do |cell|
    if currange == [] || currange.include?([cell.col, cell.row])
      casename = "sheet#{idx + 1}_#{cell.a1ref}"
      path = "/#{sheet.name}/"
      expval = preptype(cell.value)
      @testcasedata << [casename, path, cell.a1ref, expval]
    end
  end
end

templ = ERB.new(IO.readlines("test_suite_template.erb").flatten.join, 0, "%<>")
testcode = templ.result
File.open("#{@basename}_SUITE.erl", "w") { |f| f << testcode }
