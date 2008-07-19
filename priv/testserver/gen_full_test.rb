#!/usr/bin/env ruby -wKU

# Generates a test suite for Muin (from our reader's output).
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
    "\"" + val.gsub("\\", "\\\\\\").gsub("\"", "\\\"") + "\""
  else
    t[val] or val
  end
end

# Main generation code ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# populate test case data, which will be a list of
# [case name, path, A1-style cell ref, expected value]
@testcasedata = []
data.each_with_index { |sheetdata, sheetidx|
  sheetname = sheetdata[0]
  currange = ranges[sheetidx] == nil ? [] : expand_range(ranges[sheetidx])
  sheetdata.slice(1..-1).map { |rowdata|
    if rowdata.length > 1
      rowdata.map { |celldata|
        if celldata.kind_of?(Array)
          puts currange.inspect
          puts [celldata[0], rowdata[0]].inspect
          if currange == [] || currange.include?([celldata[0], rowdata[0]])
            cellname = itob26(celldata[0]) + rowdata[0].to_s # make A1-style name
            casename = "sheet#{sheetidx + 1}_#{cellname}"
            path = "/#{sheetname}/"
            expval = preptype(celldata[1][:value])
            @testcasedata << [casename, path, cellname, expval]
          end
        end
      }
    end
  }
}

templ = ERB.new(IO.readlines("test_suite_template.erb").flatten.join, 0, "%<>")
testcode = templ.result
File.open("#{@basename}_SUITE.erl", "w") { |f| f << testcode }
