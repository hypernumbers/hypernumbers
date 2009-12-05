#!/usr/bin/env ruby -wKU

# Generates Leex lexers for language frontends for Muin.
# Run as:
# ./generate_lexer russian
# This will look for a UTF-8 encoded file russian.txt under ./maps/
# The result will be written to russian_lexer.xrl. The lexer will expect
# input to be encoded in UTF-8.

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
  acc[en] = tr # ic.iconv(tr)
  acc
}

@lang = ARGV[0].capitalize
@function_defs = ""
@function_rules = ""
map.each do |k, v|
  bytes = []
  macro = k.gsub(/\./, '__DOT__')
  v.each_byte { |b| bytes << b }
  octal_seq =
    "\\" + bytes.inject([]) { |acc, b| acc << b.to_s(8); acc }.join("\\")

  @function_defs << "#{macro} = ({ALLOWED_PREFIXES})(#{octal_seq})(\\s*)(\\()\n"

  rule = <<EOS
{token,
 begin
     Nowhsp = re:replace(TokenChars, "\s+", "", [global,{return,list}]),
     Hd = hd(Nowhsp),
     case lists:member(Hd, [$+, $-, $*, $/, $=]) of
         true  -> {func, [Hd|"#{k.upcase}("]};
         false -> {func, "#{k.upcase}("}
     end
 end
}.
EOS
  
  @function_rules << "{#{macro}} : #{rule}"
end

template = ERB.new(IO.readlines("lexer_template.erb").flatten.join, 0, "%<>")
lexer = template.result
File.open("#{@lang.downcase}_lexer.xrl", "w") { |f| f << lexer }
