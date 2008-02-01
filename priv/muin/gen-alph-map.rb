
ALPHABET_MAP = ["A", "B", "V", "G", "D", "E", "YO", "ZH", "Z", "I", "IJ",
                "K", "L", "M", "N", "O", "P", "R", "S", "T", "U", "F",
                "H", "TS", "CH", "SH", "SHH", "SOFT", "Y", "HARD", "EH",
                "YU", "YA"]

# Read code points for each letter in UTF-8.
codepoints = File.open("ru-alphabet.txt") { |f|
  bytes = []
  f.each_byte { |b| bytes << b }
  pairs = []
  bytes.each_with_index { |b, i|
    # Russian characters are two bytes long, followed by LF character.
    if i % 3 == 0
      pairs << [b, bytes[i + 1]]
    end
  }
  pairs
}

str = ""
codepoints.each_with_index { |cp, i|
  str << "R_#{ALPHABET_MAP[i]} = (\\#{cp[0].to_s(8)}\\#{cp[1].to_s(8)})\n"
}

File.open("alph-map.txt", "w") { |f| f << str }
