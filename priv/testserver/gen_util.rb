# Utility functions used by test generators
# <hasan@hypernumbers.com>

module GenUtil
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

    res = []
  
    (col1..col2).each { |colidx|
      (row1..row2).each { |rowidx|
        res << [colidx, rowidx]
      }
    }

    res
  end

  # If a Ruby string contains any non-ASCII characters or any non-printable characters, 
  # return its Erlang list representation, otherwise return it as Erlang string.
  # Not doing Latin-1 because it's not fully compatible with UTF-8.
  def literal_or_utf8list(str)
    if str.unpack("U*").any? { |x| x < 33 || x > 127 } # any offenders?
      # write out as list of UTF-8 codepoints
      bytes = []
      str.each_byte { |b| bytes << b }
      "[#{bytes.join(", ")}]"
    else # write as literal string
      str.inspect
    end
  end
end