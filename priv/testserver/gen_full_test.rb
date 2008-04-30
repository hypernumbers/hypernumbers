#!/usr/bin/env ruby -wKU

# <hasan@hypernumbers.com>

# USAGE: gen_full_test.rb /path/to/data_file [RANGE:ONE, RANGE:TWO etc]

# If a range is given, test cases will be generated for cells in that
# range only. If more than one range is given, they are taken to mean
# ranges on sheets.

require "erb"
load "gen_util.rb"

datafile = ARGV[0]
ranges = ARGV.slice(1..-1)

data = eval(IO.readlines(datafile).join)

@basename = File.basename(datafile, ".dat").downcase


# Utility functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

def preptype(val)
  t = {
    -2146826288 => "'NULL!'",
    -2146826281 => "'#DIV/0!'",
    -2146826273 => "'#VALUE!'",
    -2146826265 => "'#REF!'",
    -2146826259 => "'#NAME?'",
    -2146826252 => "'#NUM!'",
    -2146826246 => "'#N/A'",
  }

  if val.kind_of?(String)
    "\"" + val + "\""
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
  sheetdata.slice(1..-1).map { |rowdata|
    if rowdata.length > 1
      rowdata.map { |celldata|
        if celldata.kind_of?(Array)
          cellname = itob26(celldata[0]) + rowdata[0].to_s # make A1-style name
          casename = "sheet#{sheetidx + 1}_#{cellname}"
          path = "/#{sheetname}/"
          expval = preptype(celldata[1][:value])
          @testcasedata << [casename, path, cellname, expval]
        end
      }
    end
  }
}

templ = ERB.new(IO.readlines("test_suite_template.erb").flatten.join, 0, "%<>")
testcode = templ.result
File.open("#{@basename}_SUITE.erl", "w") { |f| f << testcode }
