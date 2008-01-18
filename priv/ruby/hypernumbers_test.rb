require "hypernumbers"

site = "127.0.0.1"
port = 9000

hn = Hypernumbers::Connection.new(site, port, "/")

hn.post(:a1, 1)
hn.post(:a2, "=a1 + 1")
puts "a3 is: #{hn.get(:a2)}, should be: 2"
