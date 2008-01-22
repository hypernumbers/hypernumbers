#!/usr/bin/env ruby -wKU

# Test propagation of updates for hypernumbers -- on the same site.
# <hasan@hypernumbers.com>

load "mytestlib.rb"

data = [
        { :sheet => "/", :data =>
          {
            :a1 => 5
          }},

        { :sheet => "/bar/", :data =>
          {
            :a1 => "=hn(\"http://127.0.0.1:9000/a1?hypernumber\")"
          }}
       ]

updates = [
           { :sheet => "/", :data =>
             { :a1 => 42
             }}
          ]

answers = [
           { :sheet => "/bar/", :data =>
             {
               :a1 => 42
             }}
          ]

batch_post(data)
batch_post(updates)
nap_for_updates
compare(answers)
