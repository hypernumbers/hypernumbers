# A class to represent a spreadsheet document.
# Hasan Veldstra <hasan@hypernumbers.com>

# Sample usage:
# $ irb
# > load "ssdoc.rb"
# > s = Ssdoc.new("path/to/some_file.dat")
# > s.sheets.length
# > s.sheets[0].name
# > s.sheets[1].cell_at(1, 1).value
# > s.sheets[1].cell_at(2, 3).format

require "gen_util.rb"
include GenUtil

class Ssdoc
  attr_reader :sheets
  
  # Create an Ssdoc object from a DAT file created by regen_termfiles.rb
  def initialize(datfile)
    data = eval(IO.readlines(datfile).join)

    cells = []
    @sheets = []
    data.each_with_index { |sheetdata, sheetidx|
      sheetname = sheetdata[0].gsub(/\s+/, "_") # No  whitespace in URLs eh.
      sheetdata.slice(1..-1).map { |rowdata|
        if rowdata.length > 1
          rowdata.map { |celldata|
            if celldata.kind_of?(Array)
              h = celldata[1]
              cells << Cell.new(h[:value], h[:formula], h[:format],
                                h[:text], rowdata[0], celldata[0])                
            end
          }
        end
      }
      
      @sheets << Sheet.new(sheetname, cells)
    }
  end
end

class Sheet
  def initialize(name, cells)
    @name = name
    @cells = cells
  end
  
  def cell_at(row, col)
    cells = @cells.select { |cell| cell.row == row && cell.col == col }
    cells[0]
  end
  
  attr_reader :name
  attr_reader :cells
end

class Cell
  def initialize(value, formula, format, text, row, col)
    @value = value
    @formula = formula
    @format = format
    @text = text
    @row = row
    @col = col
    @a1ref = itob26(@col) + @row.to_s # make A1-style name
  end
  
  attr_reader :value
  attr_reader :formula
  attr_reader :format
  attr_reader :text
  attr_reader :row
  attr_reader :col
  attr_reader :a1ref
end