tram_regexp = /\d+\s[a-zA-ZöäåÖÄÅ\s\.]+\d+\smin/

text1 = "Next departure: Line 5 Lilla Torg. 25 min"
matches = text1.scan(tram_regexp) # => ["5 Lilla Torg. 25 min"]

text2 = "There is no next departure"
matches = text2.scan(tram_regexp) # => []