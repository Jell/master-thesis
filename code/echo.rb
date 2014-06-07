# coding: utf-8
require 'rubygems'
require 'active_support/inflector'
require 'active_support/json'
require 'erlectricity'
require 'nokogiri'
require '../Rails/app/models/bus_stop_loader.rb'
require '../Rails/app/models/provider.rb'
require '../Rails/app/models/provider/vasttrafik.rb'
require '../Rails/app/models/provider/storstockholms_lokaltrafik.rb'

puts "loaded"

receive do |f|
  f.when([:parse, Array]) do |array|
    result = []
    i = 0
    while array[i] do
      element = {}
      stop_info = array[i][0]
      #element[:id] = stop_info[0].map(&:chr).join
      element[:name] = stop_info[1].map(&:chr).join
      element[:lat] = stop_info[2].map(&:chr).join
      element[:lng] = stop_info[3].map(&:chr).join
      #element[:stop_id] = stop_info[4].map(&:chr).join
      #element[:provider_name] = stop_info[5].map(&:chr).join
      #element[:created_at] = stop_info[6].map(&:chr).join
      #element[:updated_at] = stop_info[7].map(&:chr).join
      #element[:url] = stop_info[8].map(&:chr).join
      
      stop_id = stop_info[4].map(&:chr).join
      provider_name = stop_info[5].map(&:chr).join
      
      tobeparsed = array[i][1]
      loader = 
             BusStopLoader.new("Provider::#{provider_name}".constantize)
      parsed = loader.parse_forecast(
                             tobeparsed.force_encoding("UTF-8"),
                             stop_id,
                             element[:name].force_encoding("UTF-8"))
      element[:forecast] = parsed
      result << element
      i += 1
    end
    
    f.send!([:result, result.to_json])

    #f.send!([:result, [].to_json])
    f.receive_loop
  end
end