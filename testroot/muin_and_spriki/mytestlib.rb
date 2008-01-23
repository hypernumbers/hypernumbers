
# Test library for Spriki system testing.
# <hasan@hypernumbers.com>

# TODO:
#   * Read data/formula/updates/other hashes from YAML.

$LOAD_PATH << File.join(ENV["HYPERNUMBERS_SVNROOT"], "priv", "ruby")
require "hypernumbers"

NAP_TIME = 3 # in seconds

# I want to colorize my output. (Using ANSI escape codes.)
class String
  def ansi_red
    "\e[31m#{self}\e[0m"
  end

  def ansi_green
    "\e[32m#{self}\e[0m"
  end

  def ansi_blue
    "\e[34m#{self}\e[0m"
  end
end

def with_flush(&block)
  block.call
  $stdout.flush
end

def nap_for_updates
  with_flush { print "Going for a nap (#{NAP_TIME} seconds)... ".ansi_blue }
  Kernel.sleep(NAP_TIME)
  puts "done.".ansi_blue
end

def with_nap(&block)
  block.call
  nap_for_updates
end

def batch_post(data)
  with_flush { print "Posting data... ".ansi_blue }
  
  hn = Hypernumbers::Connection.new("127.0.0.1", 9000)
  
  data.each do |d|
    hn.sheet = d[:sheet]
    
    d[:data].each do |cv|
      hn.post(cv[0], cv[1])
    end
  end

  puts "done.".ansi_blue
end


# Compare the answers, complain if not equal.
def compare(answers)
  hn = Hypernumbers::Connection.new("127.0.0.1", 9000)
  answers.each do |a|
    hn.sheet = a[:sheet]

    a[:data].each do |cv|
      srv_val = hn.get(cv[0])
      if srv_val == cv[1].to_f
        puts "#{hn.sheet}#{cv[0].to_s} #{"OK".ansi_green} (#{srv_val})"
      else
        puts "#{hn.sheet}#{cv[0].to_s} #{"FAIL".ansi_red}: #{srv_val} <> #{cv[1]}"
      end
    end
  end
end
