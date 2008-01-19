#!/usr/bin/env ruby -wKU

# Test propagation of updates (locally).
# <hasan@hypernumbers.com>

load "mytestlib.rb"

data = [
        { :sheet => "/foo/", :data =>
          { :a1 => 1, :a2 => 2, :a3 => "=a1 + a2"
          }},

        { :sheet => "/", :data =>
          {
            :a1 => "=./foo/a1 + ./foo/a2"
          }},

        { :sheet => "/foo/bar/", :data =>
          { :a1 => "=../a1 + ../a2"
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
             { :a1 => 579
             }},

           { :sheet => "/foo/bar/", :data =>
             { :a1 => 579
             }}
          ]

batch_post(data)
batch_post(updates)
compare(answers)
