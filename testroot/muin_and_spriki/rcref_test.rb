#!/usr/bin/env ruby -wKU

# Tests for RC refs (cells only, no ranges).
# <hasan@hypernumbers.com>

$LOAD_PATH << File.expand_path(File.dirname(__FILE__), "../../priv/ruby")

load "mytestlib.rb"

data = [
        { :sheet => "/rc_foo/", :data =>
          { :a1 => 1, :b1 => 2,
            :a2 => 7, :b2 => 21
          }},
       ]

formulas = [
            { :sheet => "/rc_foo/", :data =>
              {
                :c3 => "=r[1]c[1]",
                :d3 => "=R[2]C[1] + 1"
              }},

            { :sheet => "/rc_foo/rc_bar/", :data =>
              { :a1 => "=../r[2]c[2] * 5",
                :a2 => "=/rc_foo/R[1]C[2]"
              }},
            
            { :sheet => "/", :data =>
              {
                :a1 => "=./rc_foo/R[1]C[1]",
                :a2 => "=./rc_foo/rc_bar/R[1]C[1]"
              }}
           ]

answers = [
           { :sheet => "/rc_foo/", :data =>
             {
               :c3 => 1,
               :d3 => 8
             }},

           { :sheet => "/rc_foo/rc_bar/", :data =>
             {
               :a1 => 105,
               :a2 => 2
             }},

           { :sheet => "/", :data =>
             {
               :a1 => 1,
               :a2 => 105
             }}
          ]

batch_post(data)
with_nap { batch_post(formulas) }
compare(answers)
