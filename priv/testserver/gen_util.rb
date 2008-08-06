# Utility functions used by test generators.
# Hasan Veldstra <hasan@hypernumbers.com>

INT_TO_B26_MAP = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "AA", "AB", "AC", "AD", "AE", "AF", "AG", "AH", "AI", "AJ", "AK", "AL", "AM", "AN", "AO", "AP", "AQ", "AR", "AS", "AT", "AU", "AV", "AW", "AX", "AY", "AZ", "BA", "BB", "BC", "BD", "BE", "BF", "BG", "BH", "BI", "BJ", "BK", "BL", "BM", "BN", "BO", "BP", "BQ", "BR", "BS", "BT", "BU", "BV", "BW", "BX", "BY", "BZ", "CA", "CB", "CC", "CD", "CE", "CF", "CG", "CH", "CI", "CJ", "CK", "CL", "CM", "CN", "CO", "CP", "CQ", "CR", "CS", "CT", "CU", "CV", "CW", "CX", "CY", "CZ", "DA", "DB", "DC", "DD", "DE", "DF", "DG", "DH", "DI", "DJ", "DK", "DL", "DM", "DN", "DO", "DP", "DQ", "DR", "DS", "DT", "DU", "DV", "DW", "DX", "DY", "DZ", "EA", "EB", "EC", "ED", "EE", "EF", "EG", "EH", "EI", "EJ", "EK", "EL", "EM", "EN", "EO", "EP", "EQ", "ER", "ES", "ET", "EU", "EV", "EW", "EX", "EY", "EZ", "FA", "FB", "FC", "FD", "FE", "FF", "FG", "FH", "FI", "FJ", "FK", "FL", "FM", "FN", "FO", "FP", "FQ", "FR", "FS", "FT", "FU", "FV", "FW", "FX", "FY", "FZ", "GA", "GB", "GC", "GD", "GE", "GF", "GG", "GH", "GI", "GJ", "GK", "GL", "GM", "GN", "GO", "GP", "GQ", "GR", "GS", "GT", "GU", "GV", "GW", "GX", "GY", "GZ", "HA", "HB", "HC", "HD", "HE", "HF", "HG", "HH", "HI", "HJ", "HK", "HL", "HM", "HN", "HO", "HP", "HQ", "HR", "HS", "HT", "HU", "HV", "HW", "HX", "HY", "HZ", "IA", "IB", "IC", "ID", "IE", "IF", "IG", "IH", "II", "IJ", "IK", "IL", "IM", "IN", "IO", "IP", "IQ", "IR", "IS", "IT", "IU", "IV"]

module GenUtil

  # Column name -> index, e.g. AZ -> 52.
  def b26toi(str)
    str.downcase.unpack('C*').map { |x| # make a list of ASCII codes
      x - 96 # convert to list of numbers in range [1..26]
    }.inject { |acc, x|
      x + acc * 26
    }
  end

  # Index -> column name, e.g. 26 -> Z, 52 -> AZ, 64 -> BL, 78 -> BZ etc.
  def itob26(n)
    if false # n <= 255
      INT_TO_B26_MAP[n - 1]
    else
      n = n - 1
      res = ""
      while(n >= 26)
        res = (65 + n % 26).chr + res
        n = n / 26 - 1
      end
      (65 + n).chr + res
    end
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

  # If a Ruby string contains any non-ASCII characters or any non-printable
  # characters, return its Erlang list representation, otherwise return it
  # as Erlang string.
  # Not doing Latin-1 because it's not fully compatible with UTF-8.
  def literal_or_utf8list(str)
    if str.unpack("U*").any? { |x| x < 32 || x > 127 } # any offenders?
      # write out as list of UTF-8 codepoints
      bytes = []
      str.each_byte { |b| bytes << b }
      "[#{bytes.join(", ")}]"
    else # write as literal string
      str.inspect
    end
  end

end
