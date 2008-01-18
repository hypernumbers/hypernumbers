# TODO: Only recompile unit tests that have changed since last build.

require "find"

namespace(:ts) do
  
  desc "Recompiles unit tests"
  task(:make_unit) do
    print_banner("Recompiling unit tests")    
    Find.find(File.join(TESTROOT, "unit_tests")) do |path|
      if /.+(UNIT_TEST.erl)$/.match(path)
        eunit_ebin_path = "/opt/eunit/ebin"
        output_dir = File.dirname(path)
        output = %x{erlc -pa #{eunit_ebin_path} -o #{output_dir} #{path}}
        if $?.success?
          puts "- #{File.basename(path)} => OK"
        else
          puts "- #{File.basename(path)} => ERROR"
          output.split($/).map { |s| "\t#{s}" }.each { |s| puts s }
        end
      end
    end
  end

  desc "Runs unit tests"
  task(:run_unit => [:make_unit]) do
    print_banner("Running unit tests")

    # Dir[File.join(TESTROOT, "", "*.beam")].each do |fullname|
    #   print "\t* #{File.basename(fullname, ".beam")}... "
    #   modname = File.basename(fullname, ".beam")
    #   `cd #{TESTROOT + "/eunit"} && erl -noshell -s #{modname} test -s init stop`
    #   puts "ALL PASSED."
    # end
  end
  
end