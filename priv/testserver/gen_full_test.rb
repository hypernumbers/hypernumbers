#!/usr/bin/env ruby -wKU

# <hasan@hypernumbers.com>

# USAGE: gen_full_test.rb /path/to/data_file [RANGE:ONE, RANGE:TWO etc]

# If a range is given, test cases will be generated for cells in that
# range only. If more than one range is given, they are taken to mean
# ranges on sheets.

require "erb"

datafile = ARGV[0]
ranges = ARGV.slice(1..-1)

data = eval(IO.readlines(datafile).join)

@basename = File.basename(datafile, ".dat").downcase


# Utility functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Column name -> index, e.g. AZ -> 52.
def b26toi(str)
  str.downcase.unpack('C*').map { |x| # make a list of ASCII codes
    x - 96 # convert to list of numbers in range [1..26]
  }.inject { |acc, x|
    x + acc * 26
  }
end

# Index -> column name, e.g. 26 -> Z, 52 -> AZ, 64 -> BL, 78 -> BZ, 256 -> IV
def itob26(n)
  n = n - 1
  res = ""
  while(n >= 26)
    res = (65 + n % 26).chr + res
    n = n / 26 - 1
  end
  (65 + n).chr + res
end

# A1-style range -> list of [col, row] pairs, e.g.
# A1:B2 -> [ [1, 1], [1, 2], [2, 1], [2, 2] ]
def expand_range(a1range)
  cell1, cell2 = a1range.downcase.split(":")
  col1 = b26toi(cell1[/[a-z]+/])
  col2 = b26toi(cell2[/[a-z]+/])
  row1 = cell1[/[0-9]+/].to_i
  row2 = cell2[/[0-9]+/].to_i

  (col1..col2).map { |colidx|
    (row1..row2).map { |rowidx|
      [colidx, rowidx]
    }
  }
end

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
