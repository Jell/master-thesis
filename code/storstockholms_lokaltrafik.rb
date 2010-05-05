# coding: utf-8
module Provider::StorstockholmsLokaltrafik
  include Provider::Base

  def update_stop_database
    provider_name = "StorstockholmsLokaltrafik"
    for stop_id in 0000..9999 do
      a_stop = fetch_stop_with_id("%04d" % stop_id)
      if not a_stop == nil
        test_valid = get_forecast("%04d" % stop_id, a_stop[:name]) != []
        if test_valid
          bus_stop =
            BusStop.find_or_initialize_by_stop_id_and_provider_name(
                                                                a_stop,
                                                         provider_name)
          bus_stop.provider_name = provider_name
          bus_stop.name          = a_stop[:name]
          bus_stop.lat           = a_stop[:lat]
          bus_stop.lng           = a_stop[:lng]
          bus_stop.touch # ?
          bus_stop.save
        end
      end
    end
    delete_obsolete_stops(provider_name)
  end

  def get_forecast_url(poi_id)
    url = "http://realtid.sl.se/?epslanguage=SV&WbSgnMdl=" +
                                                    poi_id +
                                          "-U3Rv-_-1-_-1-1"
  end
  
  def parse_forecast(response, poi_id, poi_name)
    html_tree = Nokogiri::HTML(response)

    lines = []

    # find Bus Forecasts
    bus_forecast = html_tree.xpath('//div[@class="TrafficTypeItem"]')
    bus_forecast.each do |element|
      stop_name = element.css('div h3').text.split(", ")[1]
      if(stop_name == poi_name)
        departure_list =    element.css('div[@class="Departure"]') +
          element.css('div[@class="DepartureAlternating"]')
        departure_list.each do |departure|
          next_trip_text = departure.css('span[@class="DisplayTime"]').
            text.squeeze(" ").strip
          if next_trip_text =~ /[Nn]u/
            next_trip = "0"
          else
            next_trip = departure.css('span[@class="DisplayTime"]').
              text.squeeze(" ").strip.scan(/\d+\smin/).first
          end
          if next_trip != nil
            attributes = {}
            attributes[:line_number]       =
              departure.css('span[@class="LineDesignation"]').
                text.squeeze(" ").strip
            attributes[:color]             = "#FFFFFF"
            attributes[:background_color]  = "#BB0000"
            attributes[:destination]       =
              departure.css('span[@class="DestinationName"]').
                text.squeeze(" ").strip
            attributes[:next_trip]         = next_trip.scan(/\d+/).first
            attributes[:next_handicap]        = false
            attributes[:next_low_floor]       = false
            attributes[:next_next_trip]       = ""
            attributes[:next_next_handicap]   = false
            attributes[:next_next_low_floor]  = false

            lines << [attributes[:line_number], attributes]
          end
        end
      end
    end

    # find Tram Forecasts
    tram_forecast = html_tree.xpath('//div[@class="TPIHolder"]')
    is_correct_stop = false
    tram_forecast.css('span[@class="Row1"]').each do |element|
      is_correct_stop |= (element.text == poi_name)
    end
    if is_correct_stop
      colors = {"T10" => "#0080FF",
                "T11" => "#0080FF",
                "T13" => "#FF0000",
                "T14" => "#FF0000",
                "T17" => "#00BB00",
                "T18" => "#00BB00",
                "T19" => "#00BB00"}
                
      departures = tram_forecast.css('div[@class="Row2"]')
      tram_regexp = /\d+[a-zA-ZöäåÖÄÅ\s\.]+\d+\smin/
      
      list_tmp = []
      departures.each do |departure|
        matches = departure.text.scan(tram_regexp)
        if matches != nil
          matches.each do |match|
            list_tmp << match
          end
        end
      end
      list_tmp.each do |to_parse|
        parsed = to_parse.squeeze(" ").split(" ")
        parsed_size = parsed.count
        line_number = "T" + parsed[0]
        next_trip = parsed[parsed_size-2]
        destination = parsed[1]
        for i in 2..parsed_size-3
          destination += " " + parsed[i]
        end
        
        attributes = {}
        attributes[:line_number]       = line_number
        attributes[:color]             = "#FFFFFF"
        attributes[:background_color]  = colors[line_number]
        attributes[:destination]       = destination
        attributes[:next_trip]         = next_trip
        attributes[:next_handicap]        = false
        attributes[:next_low_floor]       = false
        attributes[:next_next_trip]    = ""
        attributes[:next_next_handicap]        = false
        attributes[:next_next_low_floor]       = false
        lines << [attributes[:line_number], attributes]
      end
    end
    
    # find Pendeltåg
    pendeltag_forecast = html_tree.xpath('//div[@class="PPIHolder"]')
    pendeltag_header =
      pendeltag_forecast.css('div[@class="Header"]').css('h3')
    
    is_correct_stop = (pendeltag_header.text =~ /#{poi_name}/)
    if is_correct_stop
      Time.zone = "Stockholm"
      current_hour    = Time.zone.now.hour
      current_minute  = Time.zone.now.min
      departure_list  =
            pendeltag_forecast.css('div[@class="Departure TrainRow"]') +
            pendeltag_forecast.
              css('div[@class="DepartureAlternating TrainRow"]')
      departure_list.each do |departure|
        striped_name = departure.css('span[@class="TrainCell Col2"]').
          text.scan(/[a-zA-ZöäåÖÄÅ\s]+/)
        destination = striped_name[0]
        for i in 1..striped_name.count - 2
          destination += " " + striped_name[i]
        end
        
        unformated_time = departure.css('span[@class="TrainCell"]').
          text.scan(/\d\d*:\d\d/)[0]
        if unformated_time == nil
          unformated_time =
            departure.css('span[@class="TrainCell Col1"]').
              text.scan(/\d\d*:\d\d/)[0]
        end
        splited_time = unformated_time.split(":")
        departure_hour = splited_time[0].to_i
        departure_minute = splited_time[1].to_i
        
        time_num = 60*(departure_hour - current_hour) +
                      (departure_minute - current_minute)
        if time_num >=0
          next_trip = time_num.to_s
        else
          next_trip = ""
        end
        attributes = {}
        attributes[:line_number]       = "Pendeltåg"
        attributes[:color]             = "#FFFFFF"
        attributes[:background_color]  = "#000000"
        attributes[:destination]       = destination
        attributes[:next_trip]         = next_trip
        attributes[:next_handicap]        = false
        attributes[:next_low_floor]       = false
        attributes[:next_next_trip]    = ""
        attributes[:next_next_handicap]        = false
        attributes[:next_next_low_floor]       = false
        lines << [attributes[:line_number], attributes]
      end
      
    end
    
    return sort_lines(lines)
  end
  
  private
  
  def fetch_stop_with_id(id)
    params = {
      "REQ0HafasSearchForw" =>"1",
      "REQ0JourneyDate"     =>"25.03.10",
      "REQ0JourneyStopsS0A" =>"255",
      "REQ0JourneyStopsS0G" =>id,
      "REQ0JourneyStopsSID" =>"",
      "REQ0JourneyStopsZ0A" =>"255",
      "REQ0JourneyStopsZ0G" =>"",
      "REQ0JourneyStopsZID" =>"",
      "REQ0JourneyTime"     =>"10:10",
      "existUnsharpSearch"  =>"yes",
      "ignoreTypeCheck"     =>"yes",
      "queryPageDisplayed"  =>"no",
      "start"               =>"Sök resa",
      "start.x"             =>"0",
      "start.y"             =>"0"
    }

    x = Net::HTTP.post_form(URI.parse(
      "http://reseplanerare.sl.se/bin/query.exe/sn"), params)
    html_tree = Nokogiri::HTML(x.body)

    error = html_tree.xpath('//label[@class="ErrorText"]')

    return nil unless error.empty?

    form = html_tree.xpath('//div[@class="FieldRow"]').first
    name = form.css("strong").text

    latlong_unparsed =
      form.xpath('//input[@type="submit"]').attribute("name").value

    lat = nil
    lng = nil
    latlong_unparsed.split('&').each do |element|
      attribute = element.split('=')
      if(attribute[0] == "REQMapRoute0.Location0.Y")
        lat = Float(attribute[1])
      end
      if(attribute[0] == "REQMapRoute0.Location0.X")
        lng = Float(attribute[1])
      end
    end

    attributes = {}
    attributes[:name] = name.split(" (")[0]
    attributes[:lat] = lat / 1000000
    attributes[:lng] = lng / 1000000
    attributes[:stop_id] = id

    return attributes
  end

end