#!/usr/bin/env ruby -wKU

# Hasan Veldstra <hasan@hypernumbers.com>
# Utility script to read an XLS file with Excel through OLE bridge,
# and dump the results in a file (consisting of plain Ruby terms).
# Usage: ruby readexcel.rb "C:\dir\somefile.xls"

require "win32ole"

def do_file(xlsfile)

  WIN32OLE.codepage = WIN32OLE::CP_UTF8
  
  xl = WIN32OLE.new("Excel.Application")
  wb = xl.Workbooks.open(xlsfile)

  xlsdata = []
  (1..wb.Worksheets.Count).each do |sheetidx|
    sheet = wb.Worksheets(sheetidx)
    sheetdata = [sheet.Name]
    puts "Range is #{sheet.UsedRange.Address}"
    (1..sheet.UsedRange.Rows.Count).each do |rowidx|
      rowdata = [rowidx]
      (1..sheet.UsedRange.Columns.Count).each do |colidx|
        cell = sheet.Cells(rowidx, colidx)
        if cell.Value != nil && cell.Formula != ""
          celldata = [colidx, {}]
          celldata[1][:text] = cell.Text # value as it is displayed
          celldata[1][:value] = cell.Value # real value underneath
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
end

#do_file(ARGV[0])
