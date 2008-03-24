# Driver for userdefs in Ruby.
# <hasan@hypernumbers.com>

require "rubygems"
require "erlectricity"

# NOTE: Depends on current dir being CODEROOT/ebin
currdir = `pwd`.strip
userdefpath = File.join(currdir, "..", "lib", "formula_engine-1.0", "src", "userdef.rb")
load userdefpath

receive do |f|
  f.when(:test) do
    f.send! :result, 1986
    f.receive_loop()
  end

  f.when(:test, Symbol, Array) do |fname, args|
     res = 
       begin
         self.method(fname).call(args)
       rescue
         :error_not_defined
       end
    f.send!(:result, res)
    f.receive_loop
  end
  
  # matches {test, hi}
  f.when(:test, Any) do
    f.send! :result, 123
    f.receive_loop()
  end

  # matches {test}
  f.when(Any) do
    f.send! :result, 456
    f.receive_loop()
  end
end
