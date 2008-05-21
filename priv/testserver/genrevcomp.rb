#!/usr/bin/env ruby -wKU

# Generates a test suite for our Excel reader.
# <hasan@hypernumbers.com>

# USAGE: gen_full_test.rb /path/to/data_file [RANGE:ONE, RANGE:TWO etc]

# If a range is given, test cases will be generated for cells in that
# range only. If more than one range is given, they are taken to mean
# ranges on sheets.

$KCODE = 'u'

require "erb"
load "gen_util.rb"

datafile = ARGV[0]
ranges = ARGV.slice(1..-1)

data = eval(IO.readlines(datafile).join)

@basename = File.basename(datafile, ".dat").downcase

def literal_or_utf8list(str)
  if str.unpack("U*").any? { |x| x > 127 } # any non ASCII characters?
    # write out as list of UTF-8 codepoints
    bytes = []
    str.each_byte { |b| bytes << b }
    "[#{bytes.join(", ")}]"
  else
    # write as literal string
    str.inspect
  end
end


# Main generation code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# populate test case data, which will be a list of
# [case name, key, expected value]
@testcasedata = []
data.each_with_index { |sheetdata, sheetidx|
  sheetname = sheetdata[0]
  currange = ranges[sheetidx] == nil ? [] : expand_range(ranges[sheetidx])
  sheetdata.slice(1..-1).map { |rowdata|
    if rowdata.length > 1
      rowdata.map { |celldata|
        if celldata.kind_of?(Array)
          if currange == [] || currange.include?([celldata[0], rowdata[0]])
            cellname = itob26(celldata[0]) + rowdata[0].to_s # make A1-style name
            casename = "sheet#{sheetidx + 1}_#{cellname}"

            value = celldata[1][:value]
            formula = celldata[1][:formula]

            type = (if formula.length > 1 && formula[0].chr == "="
                      :formula
                    elsif value.kind_of?(Numeric)
                      :number
                    elsif value.kind_of?(String)
                      :string
                    elsif value.kind_of?(TrueClass)
                      :boolean
                    elsif value.kind_of?(FalseClass)
                      :boolean
                    end)

            got = (if type == :formula
                     literal_or_utf8list(formula.to_s)
                   elsif type == :string
                     literal_or_utf8list(value)
                   elsif type == :number
                     value
                   else # boolean
                     value.to_s
                   end)

            expval = "{#{type.to_s}, #{got}}"
            key="{\"#{sheetdata[0]}\", #{rowdata[0] - 1}, #{celldata[0] - 1}}"

            @testcasedata << [casename, key, expval]
          end
        end
      }
    end
  }
}

templ = ERB.new(IO.readlines("revcomp_test_template.erb").flatten.join, 0, "%<>")
testcode = templ.result
File.open("#{@basename}_SUITE.erl", "w") { |f| f << testcode }
