#!/usr/bin/env ruby -wKU

# Tests for INDIRECT().
# <hasan@hypernumbers.com>

load "mytestlib.rb"

data = [
        { :sheet => "/", :data =>
          { :a1 => 1, :a2 => 2, :a3 => 3,
            :b1 => 10, :b2 => 20, :b3 => 30
          }}
       ]

formulas = [
            { :sheet => "/", :data =>
              { :c1 => "=indirect(\"a1\")",
                :c2 => "=sum(indirect(\"a1\"):a3)",
                :c3 => "=sum(indirect(\"a1\"):indirect(\"b3\"))",
                :c4 => "=sum(indirect(\"a1\"):indirect(\"a3\"), b1:b3)",
                :c5 => "=sum(indirect(\"a1:b2\"))",
                :c6 => "=sum(indirect(\"./a1:b2\"))",
                :c7 => "=sum(indirect(\"./a1\"):indirect(\"b2\"))",
                :c8 => "=sum(indirect(\"1:2\"))",
                :c9 => "=sum(indirect(\"A:B\"))",
                :c10 => "=sum(indirect(\"./1:2\"))",
                :c11 => "=sum(indirect(\"./A:B\"))",
              }},

            { :sheet => "/foo/", :data =>
              {
                :a1 => "=sum(indirect(\"../A1\"), 2, 3)",
                :a2 => "=sum(indirect(\"../A1:B2\"))",
                :a3 => "=sum(indirect(\"../1:2\"))",
                :a4 => "=sum(indirect(\"../A:B\"))",
              }}
           ]

answers = [
           { :sheet => "/", :data =>
             {
               :c1 => 1,
               :c2 => 6,
               :c3 => 66,
               :c4 => 66,
               :c5 => 33,
               :c6 => 33,
               :c7 => 33,
               :c8 => 40,
               :c9 => 66,
               :c10 => 40,
               :c11 => 66
             }},

           { :sheet => "/foo/", :data =>
             {
               :a1 => 6,
               :a2 => 33,
               :a3 => 40,
               :a4 => 66
             }}
          ]

batch_post(data)
batch_post(formulas)
compare(answers)
