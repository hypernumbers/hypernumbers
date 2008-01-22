#!/usr/bin/env ruby -wKU

# Various tests.
# <hasan@hypernumbers.com>

$LOAD_PATH << File.expand_path(File.dirname(__FILE__), "../../priv/ruby")

load "mytestlib.rb"

data = [
        { :sheet => "/arrays/", :data =>
          { :a1 => "={1, 2, 3, true}"
          }}
       ]

formulas = [
            { :sheet => "/arrays/", :data =>
              { :b1 => "=sum(a1)"
              }}
           ]

answers = [
           { :sheet => "/arrays/", :data =>
             {
               :b1 => "6"
             }}
          ]

batch_post(data)
batch_post(formulas)
nap_for_updates
compare(answers)
