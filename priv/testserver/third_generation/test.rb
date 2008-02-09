#!/usr/bin/env ruby debugger -wKU
require "win32ole"
xlsfile="C:/opt/code/trunk/priv/testserver/third_generation/test.xls"
application= WIN32OLE.new('Excel.Application')
application.visible=TRUE
workbook = application.Workbooks.open(xlsfile)
puts "No of sheets is #{workbook.worksheets.count}"
WIN32OLE.ole_methods.each {|method|
    puts "Method is #{method}"
  }
#puts "ole methods are #{WIN32OLE.ole_methods}"
WIN32OLE.quit('Excel.application')