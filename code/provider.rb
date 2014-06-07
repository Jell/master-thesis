module Provider
  module Base
    require 'timeout'
    require 'net/http'
    require 'uri'
    
    def delete_obsolete_stops(provider_name)
      BusStop.delete_all ["provider_name = ? AND updated_at < ?",
                                                   provider_name,
                                                    24.hours.ago]
    end

    def fetch_xml_from_url_string(given_url)
      retries = 2
      response = nil
      begin
        Timeout.timeout(2) do 
          response = Net::HTTP.get(URI.parse(given_url))
        end
      rescue Timeout::Error 
        if retries > 0 
          retries -= 1 
          retry 
        else 
          return nil
        end 
      end

      xml_tree = Nokogiri::XML.parse(response, nil, 'UTF-8')
      return nil if xml_tree.css("string").size == 0
      encapsuled_xml = xml_tree.css("string").text
      xml_tree = Nokogiri::XML.parse(encapsuled_xml, nil, 'UTF-8')
    end

    def sort_lines(lines)
      lines.sort! do |a,b|
        if a[0].size == b[0].size ||
                 (a[0] =~ /[^\d]/ && b[0] =~ /[^\d]/)
          # Normal sort for lines of equal length or 
          # if both are lines with names.
          a[0] <=> b[0]            
        else
          # Sort by size if line names has different length.
          a[0].size <=> b[0].size  
        end
      end
      return lines
    end
  end
end