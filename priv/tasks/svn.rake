# ==============================================================================
# = Description: SVN helper tasks.                                             =
# = Author: Hasan Veldstra <hasan@hypernumbers.com>                            =
# ==============================================================================

namespace :svn do
  
  task :show_changed do
    print_banner "Modified files"
    puts `svn status #{SVNROOT} | grep ^M`
  end

  task :show_new do
    print_banner "New files"
    puts `svn status #{SVNROOT} | grep -v ".beam" | grep -v "test_server" | grep -v ^M | grep -v ^D`
  end
  
end