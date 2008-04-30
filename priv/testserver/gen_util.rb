# Utility functions used by test generators
# <hasan@hypernumbers.com>

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
