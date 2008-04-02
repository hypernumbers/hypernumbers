# <hasan@hypernumbers.com>

require "find"

# Takes filename and array of regexps describing lines not to include in the count.
def getloc(file, *comment)
  IO.readlines(file).inject(0) { |acc, line|
    if (line.strip != "" && !comment.all? { |rx| rx.match(line) })
      acc + 1
    else
      acc
    end
  }
end


namespace :stats do
  task(:loc) do
    exceptions = [/yaws-1.74/, /mochi-1.0/, /muin_lexer.erl/,
                  /muin_parser.erl/, /russian_lexer.erl/,
                  /jquery.js/]
    puts "Scanning source directories..."
    loc = 0
    ["lib", "include", "priv", "src"].map { |x| File.join(SVNROOT, x) }.each do |path|
      Find.find(path) do |file|
        if exceptions.any? { |rx| rx.match(file) }
          Find.prune
        elsif /(\.erl)$/.match(file)
          loc += getloc(file, /^%/)
        elsif /(\.rb)$/.match(file)
          loc += getloc(file, /^#/)
        elsif /(\.html)$/.match(file)
          loc += getloc(file, /^<!--/)
        elsif /(\.js)$/.match(file)
          loc += getloc(file, /^\/\//, /^\/\*/)
        end
      end
    end

    print_banner "LoC stats"
    puts loc
  end
end
