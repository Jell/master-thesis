language_list = [
                    {:name => "ruby", :extension => "rb"},
                    {:name => "ruby", :extension => "rake"},
                    {:name => "ruby", :extension => "yml"},
                    {:name => "erlang", :extension => "erl"},
                    {:name => "sql", :extension => "sql"},
                    {:name => "objective-c", :extension => "m"},
                    
                  ]

`rm *.tex`
language_list.each do |language|
  appendix = File.open("appendix_#{language[:name]}.tex", "a")
  filestring = `find *.#{language[:extension]}`
  filelist = filestring.split("\n")
  filelist.each do |file|
    name = file.gsub(".#{language[:extension]}", "")
    puts name
    `pygmentize -f latex -l #{language[:name]} -o #{name}.tex #{file}`
    appendix.puts "\\noindent"
    appendix.puts "\\rule{2cm}{0.5pt} File: #{file.gsub('_', "\\_")} \\hrulefill"
    appendix.puts "\\input{code/#{name}}"
    appendix.puts ""
  end
  appendix.close
end