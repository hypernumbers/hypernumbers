# SVN helper tasks.
# Hasan Veldstra <hasan@hypernumbers.com>

namespace :svn do

  desc "Show modified files."
  task :mod do
    print_banner "Modified files"
    puts `svn status #{SVNROOT} | grep ^M`
  end

  desc "Show new files."
  task :new do
    print_banner "New files"
    puts `svn status #{SVNROOT} | grep -v ".beam" | grep -v "test_server" | grep -v ^M | grep -v ^D | grep -v "erl_crash.dump"`
  end

end
