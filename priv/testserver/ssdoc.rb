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
  
  def initialize(datfile)
    data = eval(IO.readlines(datfile).join)

    @sheets = []
    data.each_with_index { |sheetdata, sheetidx|
      sheetname = sheetdata[0]
      cells = []
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
  attr_reader :name
  attr_reader :cells

  def initialize(name, cells)
    @name = name
    @cells = cells
  end
  
  def cell_at(row, col)
    cells = @cells.select { |cell| cell.row == row && cell.col == col }
    cells[0]
  end
end

class Cell
  attr_reader :value
  attr_reader :formula
  attr_reader :format
  attr_reader :text
  attr_reader :row
  attr_reader :col
  attr_reader :a1ref

  def initialize(value, formula, format, text, row, col)
    @value = value
    @formula = formula
    @format = format
    @text = text
    @row = row
    @col = col
    @a1ref = itob26(@col) + @row.to_s # make A1-style name
  end
  
  def type
    if @formula.length > 1 && @formula[0].chr == "="
      :formula
    elsif @value.kind_of?(Numeric)
      :number
    elsif @value.kind_of?(String)
      :string
    elsif @value.kind_of?(TrueClass) || cell.value.kind_of?(FalseClass)
      :boolean
    else
      throw "Unknown type in cell #{@a1ref}"
    end    
  end
end
