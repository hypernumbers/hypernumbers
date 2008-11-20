#!/usr/bin/env ruby -wKU

# Generates a test suite for Muin (from our reader's output).
# <hasan@hypernumbers.com>

# USAGE: gen_full_test.rb /path/to/data_file

# Will generate test cases for all cells with formulas on all sheets.

$KCODE = 'u'

require "erb"
require "ssdoc"

datafile = ARGV[0]

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
  sheet.cells.each do |cell|
    if cell.type == :formula
      casename = "sheet#{idx + 1}_#{cell.a1ref}"
      path = "/#{sheet.name}/".gsub(/[ ]/,"_")
      expval = preptype(cell.value)
      @testcasedata << [casename, path, cell.a1ref, expval]
    end
  end
end

templ = ERB.new(IO.readlines("test_suite_template.erb").flatten.join, 0, "%<>")
testcode = templ.result
File.open("#{@basename}_SUITE.erl", "w") { |f| f << testcode }
