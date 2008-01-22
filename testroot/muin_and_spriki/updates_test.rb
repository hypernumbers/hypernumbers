#!/usr/bin/env ruby -wKU

# Test propagation of updates (locally).
# <hasan@hypernumbers.com>

load "mytestlib.rb"

data = [
        { :sheet => "/foo/", :data =>
          { :a1 => 1, :a2 => 2, :a3 => "=a1 + a2",
            :b1 => 7, :b2 => 14,
            :c1 => 21
          }},

        { :sheet => "/", :data =>
          {
            :a1 => "=./foo/a1 + ./foo/a2",
            :b1 => "=sum(./foo/a1:b2)",
            :c1 => "=sum(./foo/a:b)",
            :d1 => "=sum(./foo/1:2)"
            
          }},

        { :sheet => "/foo/bar/", :data =>
          { :a1 => "=../a1 + ../a2",
            :b1 => "=sum(../a1:b2)",
            :c1 => "=sum(../a:b)",
            :d1 => "=sum(../1:2)"
          }}
       ]

updates = [
           { :sheet => "/foo/", :data =>
             { :a1 => 123, :a2 => 456
             }}
          ]

answers = [
           { :sheet => "/foo/", :data =>
             { :a3 => 579
             }},

           { :sheet => "/", :data =>
             { :a1 => 579,
               :b1 => 600,
               :c1 => 1179,
               :d1 => 621
             }},

           { :sheet => "/foo/bar/", :data =>
             { :a1 => 579,
               :b1 => 600,
               :c1 => 1179,
               :d1 => 621
             }}
          ]

batch_post(data)
nap_for_updates
batch_post(updates)
nap_for_updates
compare(answers)
