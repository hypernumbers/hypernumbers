namespace :ts do

  desc "Starts Test Server shell (as root)."
  task(:shell) do
    Kernel.exec("cd #{File.join(TESTROOT, "test_server")};erl") 
  end
  
  desc "Loads HTML report in a web browser."
  task(:html) do
    path = File.join(TESTROOT, "test_server", "index.html")
    if File.exist?("/Users") # OSX.
      `open #{path}`
    else # Assume Ubuntu with Firefox installed and in $PATH.
      `firefox #{path}`
    end
  end

end