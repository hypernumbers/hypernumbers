#!/usr/bin/env ruby debugger -wKU
#-------------------------------------------------------------------------------
# Description: Reads an Excel file and dumps the result to a text file (in YAML).
#
# Author:      Hasan Veldstra <hasan@hypernumbers.com>
#                 ported to Windows by Gordon Guthrie <gordon@hypernumbers.com>
# Created on:  17 Oct 2007
#-------------------------------------------------------------------------------

# Reads an Excel file and dumps the result in two sets of Yaml.
# The arguments are a filename and a list of ranges
# These ranges refer to cell ranges on a series of sheets reading left to right in
# an Excel workbook
#
# This function first iterates over the ranges for the contiguous set of sheets and generates a Yaml
# output describing them - this Yaml ouput is called filename.test.yaml and will be used to generate
# the test suite
#
# Then this function reads *all* the sheets - and iterates over *all* the cells on each of them and
# generates a Yaml file - this Yaml file is used to setup the tests.
#
# The purpose of this is that it allows us to seperate to do test setup in a spreadsheet and then to
# remove the setup from the actual list of tests.
#
# Example - a file simple.xls has three sheets "basic", "advanced" and "data". This function is invoked
# with read simple.xls A1:B10 A1:G17
#
# The test suite will be set up with all cells on all three sheets, but there will only be an actual
# for populated cells in range A1:B10 on sheet "basic" and range A1:G17 on sheet "advanced"

require "yaml"
require "win32ole"

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

# first define a function to generate column names from numbers
def int_to_B26(n)
  # puts "into int_to_B26 with n of #{n}"
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

def read_range(outputfile,workbook,sheet_no,mincol,maxcol,minrow,maxrow)
  #puts "outputfile is #{outputfile} workbook is #{workbook} sheet_no is #{sheet_no}"
  #puts "mincol is #{mincol} maxcol is #{maxcol} minrow is #{minrow} maxrow is #{maxrow}"
  worksheet=workbook.Worksheets(sheet_no)
  #cycle over every row
  data = {}
  for j in (mincol .. maxcol)
    data[int_to_B26(j)]={}
    #row=worksheet.rows(j)
    for i in (minrow .. maxrow)
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
        #puts "Row: #{i} Col: #{j} cell_value is > #{cell_value} cell_formula is > #{cell_formula}"
        data[int_to_B26(j)][i]={}
        data[int_to_B26(j)][i]["formula"] = cell_formula
        data[int_to_B26(j)][i]["value"] = cell_value
      end
    end
  end
  #puts "inspected data is #{data.inspect()}"
  #puts "data is #{data}"
  data
end

##############################################################
#                                                            #
# This is the main routine                                   #
#                                                            #
##############################################################

# This *MUST* be a fully qualified name.
xlsfile = ARGV[0].dup
ranges = ARGV[1..ARGV.length - 1]

filename,ext =File.basename(ARGV[0]).split(".")
#puts "filename is #{filename}"

application= WIN32OLE.new('Excel.Application')
application.visible=TRUE

#Open the excel file passed in from the commandline
workbook = application.Workbooks.open(xlsfile)

no_worksheets = workbook.Worksheets.count.to_i
#worksheet.cells.ole_methods.each {|method|
#    puts "Method is #{method}"
#  }

##############################################################
#                                                            #
# First thing we do is generate the load yaml                #
#                                                            #
##############################################################

outputfile="#{filename}_load.yaml"
# Start with some metadata
# first file open is a 'w' to clear any old files
# And write hash to file as YAML.
data={}
for i in (1 .. no_worksheets)
  worksheet=workbook.Worksheets(i)
  name=worksheet.name
  maxrow=worksheet.UsedRange.rows.count
  maxcol=worksheet.UsedRange.columns.count
  #puts "maxI is #{maxI} maxJ is #{maxJ}"
  data[name]=read_range(outputfile,workbook,i,1,maxcol,1,maxrow)
end
data["source-file"] = File.basename(ARGV[0])
data["generated-on"] = Time.now.to_s
File.open(outputfile, "w") { |f| f << data.to_yaml }

##############################################################
#                                                            #
# Now we generate the complete test yaml                     #
#                                                            #
##############################################################
ranges = ARGV[1..ARGV.length - 1]
#puts "ranges are #{ranges}"
outputfile="#{filename}_test.yaml"
data={}
for i in (1 .. ARGV.length - 1)
  worksheet=workbook.Worksheets(i)
  current_range=ranges[i-1]
  #puts "current_range is #{current_range} for i of #{i}"
  first, last = current_range.split(":")
  fcol = first.match(/[a-zA-z]+/)[0]
  frow = first.match(/[0-9]+/)[0]
  lcol = last.match(/[a-zA-Z]+/)[0]
  lrow = last.match(/[0-9]+/)[0]
  fcol = B26_to_int(fcol)+1
  lcol = B26_to_int(lcol)+1
  frow=frow.to_i
  lrow=lrow.to_i
  name=worksheet.name
  #puts "frow is #{frow} lrow is #{lrow} fcol is #{fcol} and lcol is #{lcol}"
  data[name]=read_range(outputfile,workbook,i,fcol,lcol,frow,lrow)
end
# Start with some metadata
# first file open is a 'w' to clear any old files
data["source-file"] = File.basename(ARGV[0])
data["generated-on"] = Time.now.to_s
File.open(outputfile, "w") { |f| f << data.to_yaml }
