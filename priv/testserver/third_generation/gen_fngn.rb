#!/usr/bin/env ruby -wKU

# Hash from read.rb => Test for Muin + Spriki.

require "yaml"
require "erb"

TestCase = Struct.new(:name, :path, :cell, :post_data, :expected_value)

hash = File.open(ARGV[0]) { |f| YAML::load(f) }

@module_name = File.basename(hash["source-file"], ".xls").gsub(/\s+/, "")
#puts "module_name is #{@module_name}"
subpage = @module_name


@suite = hash["data"].inject([]) { |acc, sheet_data|
  #puts "acc is #{acc} and sheet_data is #{sheet_data}"
  sheet_name = sheet_data[0]
  lc_sheet_name=sheet_name.downcase
  cell_data = sheet_data[1]

  acc + cell_data.map { |cell|
    cell_name = cell[0]
    cell_value = cell[1]["value"] # What about strings and stuff here?
    cell_formula = cell[1]["formula"]

    TestCase.new("#{lc_sheet_name}_#{cell_name}",
                 "\"/#{subpage}/#{sheet_name}/\"",
                 cell_name,
                 cell_formula,
                 cell_value)
  }
}

template = ERB.new(IO.readlines("test_suite_template.erb").flatten.join, 0, "%<>")
test_suite = template.result
File.open("#{@module_name}_SUITE.erl", "w") { |f| f << test_suite }
