# ==============================================================================
# = Description: Shows some stats about the codebase.                          =
# = Author: Hasan Veldstra <hasan@hypernumbers.com>                            =
# ==============================================================================

namespace :stats do
  task :loc do
    print_banner("LoC stats")
    
    [
      ["Erlang app code", "find #{SVNROOT}/lib -name \"*.erl\" | xargs wc -l"],
      ["Test cases", "find #{SVNROOT}/testroot -name \"*.erl\" | grep -v \"test_server\" | xargs wc -l"]
    ].each do |x|
      wc_output = `#{x[1]}`.split($/)[-1] # Take the last line of wc's output.
      loc = /\d+/.match(wc_output)[0] # Extract the number.
      puts "#{x[0]}: #{loc}"
    end
    
  end
end