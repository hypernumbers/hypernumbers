#!/usr/bin/env ruby -wKU

# Posts CPU & RAM load to Spriki every 2 minutes.
# <hasan@hypernumbers.com>

require "hypernumbers"

cells = [:a1, :b1]
hn = Hypernumbers::Connection.new("127.0.0.1", 9000, "/load_data/")

loop do
  if File.exist?("/Users") # OSX
    o = `top -l 1 -n 0`
    cpu_load = o[/(CPU usage:)\s*\d*(.)\d*/][/\d+(.)\d+/]
    ram_load = o[/\d+(M)\s+(active)/][/\d+/].to_f / 10
  else # Assume Ubuntu
    o = `top -b -n 1`
    cpu_load = o[/(Cpu)\S*\s*\d*(.)\d*/][/\d+(.)\d+/]
    ram_load = o[/(Mem).+/][/\d+k\s+(used)/].to_f / 1024 / 10
  end

  hn.post(cells[0], cpu_load)
  hn.post(cells[1], ram_load)

  puts "[#{Time.now.hour}:#{Time.now.min}] posted CPU load (#{cpu_load}) and RAM load #{ram_load} to Spriki."
  
  cells = [(cells[0].to_s[0] + (cells[0].to_s[1].to_i + 1).to_s).to_sym,
           (cells[1].to_s[0] + (cells[1].to_s[1].to_i + 1).to_s).to_sym]
  
  sleep 120
end
