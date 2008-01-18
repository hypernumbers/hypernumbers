namespace :ts do

  desc "Runs Excel import tests"
  task(:run_excel_import) do
    puts "Not implemented yet."
  end

  # TODO: Regenerate tests only for modified Excel files (when timestamp xls > timestamp test suite).
  # TODO: Handle the case when a file we want to write already exists (not sure what happens right now).
  desc "Generates test suites for all Excel files under testroot/excel_import_test/files/"
  task(:generate_import) do
    xlspath = File.join(TESTROOT, "excel_import_test", "files")
    readexcel = File.join(SVNROOT, "priv", "testserver", "readexcel.rb")
    generatetest = File.join(SVNROOT, "priv", "testserver", "generatetest.rb")

    Dir[File.join(xlspath, "*.xls")].each do |fullname|
      basename = File.basename(fullname) # e.g. quadratic_equations.xls
      yamlfile = File.join(SVNROOT, basename + ".yaml") # e.g. /full/path/to/quadratic_equations.xls.yaml

      # Default range, should be enough for most files?
      `ruby #{readexcel} #{fullname} A1 Z99`
      `ruby #{generatetest} #{yamlfile}`

      # Sometimes it's useful to keep the YAML files...
      File.delete(yamlfile) unless ENV.include?("keepyaml")

      # Move the erl file to where it should be.
      erlfile = File.basename(basename, ".xls").downcase + "_SUITE.erl"
      src = File.join(SVNROOT, erlfile)
      dest = File.join(TESTROOT, "excel_import_test", erlfile)
      File.rename(src, dest)

      puts "Generated: #{erlfile}"
    end

    puts "DONE."
  end
  
end