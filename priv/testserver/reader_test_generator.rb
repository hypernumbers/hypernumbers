#!/usr/bin/env ruby -wKU

# Hasan Veldstra <hasan@hypernumbers.com>

# Utility script to generate a test suite for read_excel library.
#
# Usage:
#   ruby reader_test_generator.rb /path/to/file.dat
#
# Data files are generated by readexcel.rb.

# TODO: Test that format strings.
# TODO: Test names.

$KCODE = 'u'

require "erb"
require "Ssdoc"

def typeof(cell)
  if cell.formula.length > 1 && cell.formula[0].chr == "="
    :formula
  elsif cell.value.kind_of?(Numeric)
    :number
  elsif cell.value.kind_of?(String)
    :string
  elsif cell.value.kind_of?(TrueClass) || cell.value.kind_of?(FalseClass)
    :boolean
  end
end

datafile = ARGV[0]
ssdoc = Ssdoc.new(datafile)

@basename = File.basename(datafile, ".dat").downcase

# populate test case data, which will be a list of
# [case name, key, expected value]
@testcasedata = []
ssdoc.sheets.each_with_index do |sheet, idx|
  sheet.cells.each do |cell|
    casename = "sheet#{idx + 1}_#{cell.a1ref}"
    type = typeof(cell)
    got = (if type == :formula
             literal_or_utf8list(cell.formula.to_s)
           elsif type == :string
             literal_or_utf8list(cell.value)
           elsif type == :number
             cell.value
           else # boolean
             cell.value.to_s
           end)

    expval = "{#{type.to_s}, #{got}}"
    key="{\"#{sheet.name}\", #{cell.row - 1}, #{cell.col - 1}}"
    @testcasedata << [casename, key, expval]
  end
end

@testcasedata.sort!

templ = ERB.new(IO.readlines("revcomp_test_template.erb").flatten.join, 0, "%<>")
testcode = templ.result
File.open("#{@basename}_SUITE.erl", "w") { |f| f << testcode }
