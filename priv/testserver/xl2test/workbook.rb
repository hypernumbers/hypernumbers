# Platform-independent Excel reader.
# Uses the OLE bridge on Windows, and the Applescript interface on Mac.
#
# Returns result as a hash.
# Hasan Veldstra <hasan@hypernumbers.com>

require "rubygems"

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
      @excel.visible = true
    else
      raise "Unknown OS."
    end

    open(filename)
  end

  
  def open(filename)
    self.method("range_#{@os.to_s}".to_sym).call(range)
    @filename = filename
  end

  
  # Range is fully-qualified range name like A1:B10.
  def range(range, sheet = 0)
    self.method("range_#{@os.to_s}".to_sym).call(range, sheet)
  end

  
  # ------------- #
  # Class methods #
  # ------------- #

  
  # number -> column name
  def self.int_to_col(n)
    return (n + 64).chr if n < 26

    whole = n.div(26)
    rem = n % 26

    return "Z" * whole if rem == 0

    (whole + 64).chr + (rem + 64).chr
  end

  
  # --------------- #
  # Private methods #
  # --------------- #
  
  private


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
    formulas = @excel.sheets[sheet_idx].ranges[range].formula.get
    values   = @excel.sheets[sheet_idx]ranges[range].value.get
  end

  # --- Windows-specific

  def open_win(filename)
    @workbook_win = @excel.Workbooks.open(filename)
  end

  def range_win(range, sheet_idx)
    # TODO:
  end
  
end
