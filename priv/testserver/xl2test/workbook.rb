# Platform-independent Excel reader.
# Uses the OLE bridge on Windows, and the Applescript interface on Mac.
#
# Returns result as a hash.
# Hasan Veldstra <hasan@hypernumbers.com>

require "rubygems"

# first define a function to generate column names from numbers
def int_to_B26(n)
  #puts "into int_to_B26 with n of #{n}"
  # use the fact that excel columns are limited to 256
  if n < 26
    #puts "character of #{n} is #{(n+97).chr}"
    (n+97).chr
  else
    m = n/26
    mm = m*26
    nn = n-mm
    #puts "m is #{m}"
    #puts "nn is #{nn}"
    val=(m+97).chr+(nn+97).chr
    #puts "val is #{val}"
  end
end

class WIN32OLE
  def list_ole_methods
    method_names = ole_methods.collect {|m| m.name}
    puts method_names.sort.uniq
  end
end

class Array
  # Computes a number of the specified base using the array's elements
  # as digits.
  def to_number(base = 10)
    inject{ |result, variable| variable + result * base }
  end
end

# The base we use when converting words to and from numbers.
BASE = ('a'..'z').to_a.size
# The offset of characters compared to digits in word-numbers.
OFFSET = 'a'[0]
# Converts a string to a number of base BASE (inverse of #i_to_s ).
def B26_to_int(string)
  string.downcase.unpack('C*').map{ |x| x - OFFSET }.to_number(BASE)
end


class Workbook

  attr_reader :os
  attr_reader :filename

  
  def initialize(filename)
    @os = :osx if File.exist?("/Users")
    @os = :win if File.exist?("c:\\")

    case @os
    when :osx
      require "appscript"
      @excel = Appscript.app("Microsoft Excel")
    when :win
      require "win32ole"
      @excel = WIN32OLE.new("Excel.Application")
      @excel.visible=TRUE

    else
      raise "Unknown OS."
    end

    open(filename)
  end
  
  def open(filename)
    self.method("open_#{@os.to_s}".to_sym).call(filename)
    @filename = filename
  end

  
  # Range is fully-qualified range name like A1:B10.
  def range(range, sheet = 0)
    self.method("range_#{@os.to_s}".to_sym).call(range, sheet)
  end

  
  # ------------- #
  # Class methods #
  # ------------- #

  # --------------- #
  # Private methods #
  # --------------- #
  
  private

  def quote(myString)
    if !(myString[0] == "\"" && myString[myString.length-1] == "\"")
      string2="\""
      string2 << myString
      string2 <<"\""
      #puts "Wasn't a string is now #{string2}"
    else
      string2=myString
      #puts "Was a string all along #{string2}"
    end
  string2
  end

  # --- Mac-specific

  def open_osx(filename)
    # Convert Unix path into format Excel expects.
    filename.gsub!("/", ":").slice!(0..0)
    @excel.open(filename)
  end

  
  def range_osx(range, sheet_idx)
    first, last = range.split(":")
    #first << "1" if !(first =~ /[0-9]+/)
    #last << "65536" if !(last =~ /[0-9]+/)
    fcol = first.match(/[a-zA-z]+/)[0]
    frow = first.match(/[0-9]+/)[0]
    lcol = last.match(/[a-zA-Z]+/)[0]
    lrow = last.match(/[0-9]+/)[0]

    # This does not work. :(
    #formulas = @excel.sheets[sheet_idx].ranges[range].formula.get
    #values   = @excel.sheets[sheet_idx].ranges[range].value.get
  end

  # --- Windows-specific

  def open_win(filename)
    @workbook_win = @excel.Workbooks.open(filename)
  end

  def range_win(range, sheet_idx)
    #puts "sheet_idx is #{sheet_idx}"
    worksheet = @workbook_win.Worksheets(sheet_idx+1)
    name=worksheet.name
    #puts "name is #{name}"
    #worksheet.cells.ole_methods.each {|method|
    #    puts "Method is #{method}"
    #  }
    #puts "range is #{range}"
    first, last = range.split(":")
    fcol = first.match(/[a-zA-z]+/)[0]
    frow = first.match(/[0-9]+/)[0]
    lcol = last.match(/[a-zA-Z]+/)[0]
    lrow = last.match(/[0-9]+/)[0]
    fcol = B26_to_int(fcol)+1
    lcol = B26_to_int(lcol)+1
    frow=frow.to_i
    lrow=lrow.to_i
    #puts "fcol is #{fcol} frow is #{frow}"
    #puts "lcol is #{lcol} lrow is #{lrow}"
    data={}
    data["#{name}"]={}
      #cycle over every row
      for j in (frow .. lrow)
        for k in (fcol .. lcol)
          #puts "k is #{k} and j is #{j}"
          cell=worksheet.Cells(j,k)
          #cell.ole_methods.each {|method|
          #  puts "Method is #{method}"
          #}
          #puts "cell is #{cell}"
          #Get the contents of the cell as a string
          cell_value = cell.Value2
          cell_formula = cell.Formula
          if cell_value.instance_of? String
            if cell_value.include? '"'
              # Escape double quotes in strings with a backslash so that Erlang can recognise them...
              cell_value= cell_value.gsub!(/"/,'\"')
            end
          end
        if cell_formula.include? '"'
          # Escape double quotes in strings with a backslash so that Erlang can recognise them...
          cell_formula= cell_formula.gsub!(/"/,'\"')
        end
        if cell_value != nil
          #puts "Row: #{k} Col: #{j} value is > #{cell_value} formula is > #{cell_formula}"
          cell_desc=int_to_B26(k)+j.to_s
          #puts "cell_desc is #{cell_desc}"
          data["#{name}"][cell_desc]={}
          data["#{name}"][cell_desc]["formula"] = quote(cell_formula)
          data["#{name}"][cell_desc]["value"] = cell_value
        end
      end
    end
    #puts "inspected data is #{data.inspect()}"
    #puts "data is #{data}"
    data

  end
  
end
