#!/usr/bin/env ruby -wKU

# Hasan Veldstra <hasan@hypernumbers.com>
# Utility script to read an XLS file with Excel through OLE bridge,
# and dump the results in a file (consisting of plain Ruby terms).
# Usage: ruby readexcel.rb "C:\dir\somefile.xls"

require "win32ole"
load "gen_util.rb"
include GenUtil

def is_date?(s)
    (s =~ /\d\d\d\d\/\d\d\/\d\d\s\d\d:\d\d:\d\d/) == 0
end

def do_file(xlsfile)

  WIN32OLE.codepage = WIN32OLE::CP_UTF8
  
  xl = WIN32OLE.new("Excel.Application")
  wb = xl.Workbooks.open(xlsfile)

  xlsdata = []
  (1..wb.Worksheets.Count).each do |sheetidx|
    sheet = wb.Worksheets(sheetidx)
    sheetdata = [sheet.Name]
    range=sheet.UsedRange.Address.split(":")
    topleft=range[0].split("$")
    firstrow=topleft[2].to_i
    firstcol=topleft[1].to_i+1
    # puts "firstrow is #{firstrow} and firstcol is #{firstcol}"
    # puts "no of rows is #{sheet.UsedRange.Rows.Count}" 
    # puts "no of cols is #{sheet.UsedRange.Columns.Count}"
   (firstrow..sheet.UsedRange.Rows.Count+1).each do |rowidx|
   rowdata = [rowidx]
      (firstcol..sheet.UsedRange.Columns.Count+1).each do |colidx|
        cell = sheet.Cells(rowidx, colidx)
          if cell.Value != nil && cell.Formula != ""
          celldata = [colidx, {}]
          celldata[1][:text] = cell.Text # value as it is displayed
          if (is_date?(cell.Value)) 
             celldata[1][:value] = cell.Value
             # puts "this is the date #{cell.Value} #{is_date?(cell.Value)}"
          else   
             celldata[1][:value] = cell.Value2
          end
          celldata[1][:formula] = cell.Formula
          celldata[1][:format] = cell.NumberFormat
          rowdata << celldata
        end
      end
      sheetdata << rowdata
    end
    xlsdata << sheetdata
  end

  File.open(File.basename(xlsfile, ".xls") + ".dat", "w") do |f|
    f << xlsdata.inspect
  end
  xl.ActiveWorkbook.Close(0);
  xl.Quit();
end

#do_file(ARGV[0])
