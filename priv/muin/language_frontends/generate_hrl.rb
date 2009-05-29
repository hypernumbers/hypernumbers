#!/usr/bin/env ruby -wKU

# turns the langauge maps into tuple term files to be used in generating
# warnings and help files
# Run as:
# ./generate_terms russian
# This will look for a UTF-8 encoded file russian.txt under ./maps/
# The result will be written to russian_terms.txt. The lexer will expect
# input to be encoded in UTF-8.

# Gordon Guthrie <gordon@hypernumbers.com>
# based on generate_lexer.rb by
# Hasan Veldstra <hasan@hypernumbers.com>

require "erb"

if ARGV[0].nil?
  puts "You need to specify a map."
  Kernel.exit
end

mapfile = "./maps/#{ARGV[0]}.txt"

if !File.exist?(mapfile)
  puts "#{mapfile} does not exist."
  Kernel.exit
end

# Read the map file, create English => Other Language map.
map = IO.readlines(mapfile).inject({}) { |acc, line|
  en, tr = line.split("=").map { |s| s.strip }
  puts "en is #{en} and tr is #{tr}"
  acc[en] = tr 
  acc
}

@lang = ARGV[0].downcase
@terms = ""
@bits = ""
map.each do |k, v|
  bytes = []
  v.each_byte { |b| bytes << b }
  octal_seq =
    bytes.inject([]) { |acc, b| acc << b.to_s; acc }.join(",")

  @terms << "{'#{@lang}', \"#{k}\", [#{octal_seq}]},\n"
  
end

template = ERB.new(IO.readlines("hrl_template.erb").flatten.join, 0, "%<>")
lexer = template.result
# TODO: Move the file to SVNROOT/lib/hypernumbers-1.0/include/
File.open("#{@lang.downcase}_fns.hrl", "w") { |f| f << lexer }
