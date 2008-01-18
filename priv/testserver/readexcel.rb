#!/usr/bin/env ruby -wKU
#-------------------------------------------------------------------------------
# Reads an Excel file and dumps the result to a text file (in YAML).
# Platform-independent.
#
# Hasan Veldstra <hasan@hypernumbers.com>
# Gordon Guthrie <gordon@hypernumbers.com>
#-------------------------------------------------------------------------------

# TODO: 
#  * Batch processing to speed things up (will need to modify Rake task too).

require "rubygems"
require "yaml"

class Excel
  attr_reader :os
  attr_reader :filename

  # Generate column name from a number.
  def self.int_to_col(n)
    if n < 26
      (n+64).chr
    else
      m = n/26
      mm = m*26
      nn = n-mm
      val=(m+64).chr+(nn+64).chr
    end
  end

  def initialize
    if File.exist?("/Users")
      @os = :osx
      require "appscript"
      @excel = Appscript.app("Microsoft Excel")
    elsif File.exist("c:\\")
      @os = :win
      require "win32ole"
    else
      @os = :lin
      # TODO: Implement.
    end
  end

  # Open an Excel file.
  def open(file)
    self.method(("open_" + @os.to_s).to_sym).call(file)
  end

  # Return the [formula, value] list for a given cell.
  # col is a string, row is an integer.
  def cell(col, row)
    self.method(("cell_" + @os.to_s).to_sym).call(col, row)
  end

  #-----------------#
  # private methods #
  #-----------------#

  private

  #--------------#
  # Mac-specific #
  #--------------#

  def open_osx(filename)
    @filename = filename

    # Convert Unix path into format Excel expects.
    filename.gsub!("/", ":").slice!(0..0)
    @excel.open(filename)
  end

  def cell_osx(col, row)
    formula = @excel.ranges["#{col}#{row}:#{col}#{row}"].formula.get
    value   = @excel.ranges["#{col}#{row}:#{col}#{row}"].value.get
    [formula, value]
  end
end

# This *MUST* be a fully qualified name.
xlsfile = ARGV[0].dup

excel = Excel.new
excel.open(xlsfile)

# Read the Excel file into a hash.
data = {}
(1..256).to_a.each do |x|
  col = Excel.int_to_col(x)
  data[col] = {}
  (1..65536).to_a.each do |row|
    formula, value = excel.cell(col, row)
    
    if formula != "" && value != ""
      data[col][row] = {}
      data[col][row]["formula"] = formula
      data[col][row]["value"] = value
    end
  end
end

# Append some metadata.
data["source-file"] = File.basename(ARGV[0])
data["generated-on"] = Time.now.to_s

# And write hash to file as YAML.
File.open("#{File.basename(ARGV[0])}.yaml", "w") { |f| f << data.to_yaml }
