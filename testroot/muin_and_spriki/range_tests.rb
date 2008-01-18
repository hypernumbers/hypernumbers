#!/usr/bin/env ruby -wKU

# Tests for ranges.
# <hasan@hypernumbers.com>

$LOAD_PATH << File.expand_path(File.dirname(__FILE__), "../../priv/ruby")

require "hypernumbers"
load "mytestlib.rb"


data = [ {:sheet => "/",     :data =>
           { :a1 => 1, :a2 => 2, :a3 => 3, :a6 => 6,
             :b1 => 7, :b2 => 17, :b10 => 2, :b42 => 10,
             :c1 => 1,
             :d12 => 12
           }
         },
         {:sheet => "/foo/", :data => {:b1 => 4, :b2 => 5, :b3 => 6}}
       ]

formulas = [ {:sheet => "/", :data => 
               { :f1 => "=sum(a1:a3)",
                 :f2 => "=sum(./foo/b1:b3)",
                 :f3 => "=sum(./A1:A3)",
                 :f4 => "=sum($A$1:$A$3, 2)",
                 :f5 => "=sum(A1:A3, B1:B42)",
                 :f6 => "=sum(A:B)",
                 :f7 => "=sum(C:D)",
                 :f8 => "=sum(1:2)"
               }
             },

             {:sheet => "/foo/", :data =>
               { :f1 => "=sum(b1:b3)",
                 :f2 => "=sum(../a1:a3)",
                 :f3 => "=sum(./b1:b3)",
                 :f4 => "=sum(./b1:b3, 5)",
                 :f5 => "=sum(B1:B3, ../A1:A3)",
                 :f6 => "=sum(B1:B3, ../$A$1:$A$3)",
                 :f7 => "=sum(../A:B, 1, 2, 3)",
                 :f8 => "=sum(../1:2, ../A:B, 1, 2, 3)"
               }
             }
           ]

# Encode in formulas?
answers = [ {:sheet => "/",     :data =>
              {
                :f1 => 6, :f2 => 15, :f3 => 6, :f4 => 8, :f5 => 42, :f6 => 48, :f7 => 13,
                :f8 => 49
              }},
            
            {:sheet => "/foo/", :data =>
              {
                :f1 => 15, :f2 => 6, :f3 => 15, :f4 => 20, :f5 => 21, :f6 => 21, :f7 => 54,
                :f8 => 103
              }}
          ]

batch_post(data)
batch_post(formulas)
compare(answers)
