module Provider::Vasttrafik
  include Provider::Base
  def update_stop_database
    provider_name = "Vasttrafik"
    use_identifier

    xml_tree = fetch_all_stops()
    return nil unless xml_tree

    rt90grid = SwedishGrid.new(:rt90)

    xml_tree.css("all_stops item").each do |element|
      stop_id = element.attribute("stop_id").try(:text).to_s
      bus_stop =
        BusStop.find_or_initialize_by_stop_id_and_provider_name(stop_id,
                                                          provider_name)

      rt90_x = Float(element.attribute("rt90_x").try(:text).to_s)
      rt90_y = Float(element.attribute("rt90_y").try(:text).to_s)
      latlng = rt90grid.grid_to_geodetic(rt90_x, rt90_y)

      bus_stop.provider_name = provider_name
      bus_stop.name          = element.css("stop_name").try(:text).to_s
      bus_stop.lat           = latlng[0]
      bus_stop.lng           = latlng[1]
      bus_stop.touch # ?
      bus_stop.save
    end

    delete_obsolete_stops(provider_name)
  end

  def get_forecast_url(poi_id)
    use_identifier
    url =
      "http://www.vasttrafik.se/External_Services/NextTrip.asmx/" + 
                                        "GetForecast?identifier=" +
                                                      @identifier +
                                                       "&stopId=" +
                                                           poi_id
  end
  
  def parse_forecast(response, poi_id, poi_name)
    provider_name = "Vasttrafik"

    xml_tree = Nokogiri::XML.parse(response, nil, 'UTF-8')
    return [] if xml_tree.css("string").size == 0
    encapsuled_xml = xml_tree.css("string").text
    xml_tree = Nokogiri::XML.parse(encapsuled_xml, nil, 'UTF-8')
    
    # Extract data
    lines = []
    xml_tree.css("forecast item").each do |element|
      attributes = {}
      attributes[:line_number]          =
        element.attribute("line_number").try(:text).to_s
      attributes[:color]                =
        element.attribute("line_number_foreground_color").try(:text).to_s
      attributes[:background_color]     =
        element.attribute("line_number_background_color").try(:text).to_s
      attributes[:destination]          =
        element.css("destination").try(:text).to_s
      
      attributes[:next_trip]            =
        element.attribute("next_trip").try(:text).to_s
      alternate_text                    =
        element.attribute("next_trip_alternate_text").try(:text).to_s
      attributes[:next_handicap]        =
        (alternate_text == "Handikappanpassad".force_encoding("UTF-8")) 
      attributes[:next_low_floor]       =
        (alternate_text == "Low Floor")
      
      attributes[:next_next_trip]       =
        element.attribute("next_next_trip").try(:text).to_s
      alternate_text                    =
       element.attribute("next_next_trip_alternate_text").try(:text).to_s
      attributes[:next_next_handicap]   =
        (alternate_text == "Handikappanpassad")
      attributes[:next_next_low_floor]  =
        (alternate_text == "Low Floor")

      lines << [attributes[:line_number], attributes]
    end

    return sort_lines(lines)
  end

  private
  def fetch_all_stops
    get_poi_list_url = "http://www.vasttrafik.se/External_Services/" + 
                        "TravelPlanner.asmx/GetAllStops?identifier=" +
                                                        @identifier
    xml_tree = fetch_xml_from_url_string(get_poi_list_url)
  end

  def fetch_forecast(poi_id)
    get_poi_forecast_url =
                        "http://www.vasttrafik.se/External_Services/" +
                              "NextTrip.asmx/GetForecast?identifier=" +
                                                          @identifier +
                                                           "&stopId=" +
                                                              poi_id
    xml_tree = fetch_xml_from_url_string(get_poi_forecast_url)
  end
end