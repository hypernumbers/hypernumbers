#!/usr/bin/env ruby -wKU

# Tests for RC refs (cells only, no ranges).
# <hasan@hypernumbers.com>

$LOAD_PATH << File.expand_path(File.dirname(__FILE__), "../../priv/ruby")

load "mytestlib.rb"

data = [
        { :sheet => "/rc_foo/", :data =>
          { :a1 => 1, :b1 => 2,
            :a2 => 7, :b2 => 21,

            :k7 => 999
          }},
       ]

formulas = [
            { :sheet => "/rc_foo/", :data =>
              {
                :c3 => "=r[1]c[1]",
                :d3 => "=R[2]C[1] + 1",
                :e3 => "=R[-1]C[-3]", # b2
                :f3 => "=R[+4]C[+5]" # k7 ##FIXME: 20,14 for z17 => fails in getxy
              }},

            { :sheet => "/rc_foo/rc_bar/", :data =>
              { :a1 => "=../r[2]c[2] * 5",
                :a2 => "=/rc_foo/R[1]C[2]",

                :e3 => "=../r[-1]c[-3]", # b2
                :f3 => "=/rc_foo/r[+4]C[+5]"
              }},
            
            { :sheet => "/", :data =>
              {
                :a1 => "=./rc_foo/R[1]C[1]",
                :a2 => "=./rc_foo/rc_bar/R[1]C[1]",

                :e3 => "=./rc_foo/R[-1]C[-3]",
                :f3 => "=./rc_foo/r[+4]c[+5]"
              }}
           ]

answers = [
           { :sheet => "/rc_foo/", :data =>
             {
               :c3 => 1,
               :d3 => 8,
               :e3 => 21,
               :f3 => 999
             }},

           { :sheet => "/rc_foo/rc_bar/", :data =>
             {
               :a1 => 105,
               :a2 => 2,
               :e3 => 21,
               :f3 => 999
             }},

           { :sheet => "/", :data =>
             {
               :a1 => 1,
               :a2 => 105,
               :e3 => 21,
               :f3 => 999
             }}
          ]

batch_post(data)
with_nap { batch_post(formulas) }
compare(answers)
