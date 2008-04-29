#!/usr/bin/env ruby -wKU

# USAGE: ruby readexcel.rb C:\\dir\\somefile.xls

require "win32ole"

xlsfile = ARGV[0]

WIN32OLE.codepage = WIN32OLE::CP_UTF8

xl = WIN32OLE.new("Excel.Application")
wb = xl.Workbooks.Open(xlsfile)

xlsdata = []
(1..wb.Worksheets.Count).each do |sheetidx|
  sheet = wb.Worksheets(sheetidx)
  sheetdata = [sheet.Name]
  (1..sheet.UsedRange.Rows.Count).each do |rowidx|
    rowdata = [rowidx]
    (1..sheet.UsedRange.Columns.Count).each do |colidx|
      cell = sheet.Cells(rowidx, colidx)
      if cell.Value != nil && cell.Formula != ""
        celldata = [colidx, {}]
        celldata[1][:text] = cell.Text
        celldata[1][:value] = cell.Value
        celldata[1][:formula] = cell.Formula
        celldata[1][:format] = cell.NumberFormat
        rowdata << celldata
      end
    end
    sheetdata << rowdata
  end
  xlsdata << sheetdata
end

File.open(File.basename(xlsfile, ".xls") + ".dat", "w") { |f| f << xlsdata.inspect }
