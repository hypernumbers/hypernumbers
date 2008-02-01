#!/usr/bin/env ruby -wKU

# Reads an Excel file and dumps the result in YAML.
# <hasan@hypernumbers.com>

require "workbook"
require "yaml"
require "win32ole"

filename = ARGV[0]
ranges = ARGV[1..ARGV.length - 1]

w = Workbook.new(ARGV[0])

hash = {}
hash["data"] = {}

(0..ranges.length - 1).to_a.zip(ranges).each { |pair|
  sheet_idx = pair[0]
  range = pair[1]

  hash["data"] = w.range(range, sheet_idx)
}

hash["source-file"] = filename
hash["generated-on"] = Time.now

File.open("#{File.basename(ARGV[0])}.yaml", "w") { |f| f << hash.to_yaml }
