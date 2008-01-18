#!/usr/bin/env ruby debugger -wKU
#-------------------------------------------------------------------------------
# Description: Reads an Excel file and dumps the result to a text file (in YAML).
#
# Author:      Hasan Veldstra <hasan@hypernumbers.com>
#                 ported to Windows by Gordon Guthrie <gordon@hypernumbers.com>
# Created on:  17 Oct 2007
#-------------------------------------------------------------------------------

# TODO: batch processing to speed things up (will need to modify Rake task too).

#require "rubygems"
#require "appscript"
require "yaml"
require "win32ole"

# first define a function to generate column names from numbers
def int_to_B26(n)
  #puts "into int_to_B26 with n of #{n}"
  # use the fact that excel columns are limited to 256
  if n < 26
    #puts "character of #{n} is #{(n+97).chr}"
    (n+64).chr
  else
    m = n/26
    mm = m*26
    nn = n-mm
    #puts "m is #{m}"
    #puts "nn is #{nn}"
    val=(m+64).chr+(nn+64).chr
    #puts "val is #{val}"
  end
end

# This *MUST* be a fully qualified name.
xlsfile = ARGV[0].dup
#xlsfile="C:/Users/Gordon Guthrie/Documents/SVN/trunk/priv/testserver/test.xls"
puts "xlsfile is #{xlsfile}"

application= WIN32OLE.new('Excel.Application')
application.visible=TRUE

#Open the excel file passed in from the commandline
#workbook = application.Workbooks.open("C:/Users/Gordon Guthrie/Documents/SVN/trunk/priv/testserver/test.xls")
workbook = application.Workbooks.open(xlsfile)

#Get the first worksheet
worksheet = workbook.Worksheets(1)
#puts "worksheet methods are #{worksheet.ole_methods}"
#worksheet.cells.ole_methods.each {|method|
#    puts "Method is #{method}"
#  }
maxI=worksheet.UsedRange.rows.count
maxJ=worksheet.UsedRange.columns.count
#puts "maxI is #{maxI} maxJ is #{maxJ}"

#cycle over every row
data = {}
for j in (1 .. maxJ)
  data[int_to_B26(j)]={}
 #row=worksheet.rows(j)
  for i in (1 .. maxI)
    #puts "i is #{i} and j is #{j}"
    cell=worksheet.cells(i,j)
    #Get the contents of the cell as a string
    cell_value = cell.value
    cell_formula = cell.formula
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
      #puts "Row: #{i} Col: #{j} value is > #{value} formula is > #{formula}"
      data[int_to_B26(j)][i]={}
      data[int_to_B26(j)][i]["formula"] = cell_formula
      data[int_to_B26(j)][i]["value"] = cell_value
    end
  end
end
#puts "inspected data is #{data.inspect()}"
#puts "data is #{data}"

# Append some metadata
data["source-file"] = File.basename(ARGV[0])
data["generated-on"] = Time.now.to_s

# And write hash to file as YAML.
File.open("#{File.basename(ARGV[0])}.yaml", "w") { |f| f << data.to_yaml }