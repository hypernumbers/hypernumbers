# =============================================================================
# = Description: The main rakefile, loads tasks dynamically.                  =
# = Author:      Hasan Veldstra <hasan@hypernumbers.com>                      =
# = Created:     3 Nov 2007                                                   =
#==============================================================================

require "find"

# NOTE: Do not add tasks to this file.
# Put your tasks anywhere under priv/tasks to have them loaded automatically.

# All loaded tasks will have access to these constants.
SVNROOT = File.dirname(__FILE__)
TESTROOT = File.join(SVNROOT, "testroot")

# Find and load tasks.
Find.find(File.join(SVNROOT, "priv", "tasks")) do |path|
  if /(.rake)$/.match(path)
    load(path)
  end
end

# ===================
# = Utility methods =
# ===================

# All loaded tasks will have access to these functions.

# Prints a banner like this the one above that says "Utility methods".
def print_banner(str)
  puts(
    ("=" * (str.length + 4)) + "\n" +
    "= #{str} =" + "\n" +
    ("=" * (str.length + 4)) + "\n"
  )
end

# Prints an error message returned by a shell command executed with ` or %x{}.
def print_error(str)
  str.split($/).map { |s| "\t#{s}" }.each { |s| puts s }
end