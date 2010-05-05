# coding: utf-8

def get_stops(lat,lng)
  result = []
  stop_list = get_nearest_stops(lat,lng)
  stop_list.each do |element|
    attributes = []
    attributes << [:name, element.name]
    attributes << [:lat, element.lat]
    attributes << [:lng, element.lng]
    attributes << [:forecast, []]
    attributes << [:provider_name, element.provider_name]
    attributes << [:stop_id, element.stop_id]
    attributes << [:url, element.url]
    result << attributes
  end
  return result
end

def get_nearest_stops(lat,lng)
  lat = Float lat
  lng = Float lng
  area = 0.002
  list = BusStop.where(["lat > ? AND lat < ? AND lng > ? AND lng < ?",
                        lat - area, lat + area, lng- area, lng + area])
  while list.count < 10 do
    list = list + BusStop.where(
        ["lat > ? AND lat < ? AND lng > ? AND lng < ?
          AND NOT (lat > ? AND lat < ? AND lng > ? AND lng < ?)",
                                                    lat - 2*area,
                                                    lat + 2*area,
                                                     lng- 2*area,
                                                    lng + 2*area,
                                                      lat - area,
                                                      lat + area,
                                                       lng- area,
                                                      lng + area])
    area = area * 2
  end
  return sort_stops(list, lat, lng).first(10)
end

def sort_stops(list, lat, lng)
  #sort by distance to origin
  list.sort! do |a,b|
    distance_geodesic(Float(a.lat), Float(a.lng), lat, lng) <=>
      distance_geodesic(Float(b.lat), Float(b.lng), lat, lng)
  end
  return list
end

def distance_geodesic(lat1, long1, lat2, long2)
  #convert from degrees to radians
  a1 = lat1 * (Math::PI / 180)
  b1 = long1 * (Math::PI / 180)
  a2 = lat2 * (Math::PI / 180)
  b2 = long2 * (Math::PI / 180)

  r = 6356.75 #radius of earth
  #do the calculation with radians as units
  d = r * Math.acos(
            Math.cos(a1)*Math.cos(b1)*Math.cos(a2)*Math.cos(b2) +
            Math.cos(a1)*Math.sin(b1)*Math.cos(a2)*Math.sin(b2) +
            Math.sin(a1)*Math.sin(a2));
  
end

receive do |f|
  f.when([:parse, Array]) do |array|
    result = []
    i = 0
    while array[i] do
      element = {}
      stop_info = array[i][0]
      j = 0
      while stop_info[j] do
        element[stop_info[j][0].to_sym] = stop_info[j][1]
        j += 1
      end
      tobeparsed = array[i][1]
      loader = BusStopLoader.new(
                    "Provider::#{element[:provider_name]}".constantize)
      parsed = loader.parse_forecast(tobeparsed.force_encoding("UTF-8"),
                                                       element[:poi_id],
                                  element[:name].force_encoding("UTF-8"))
      element[:forecast] = parsed
      result << element
      i += 1
    end
    
    f.send!([:result, result.to_json])
    f.receive_loop
  end
  
  f.when([:echo, Array]) do |array|
    result = {}
    i = 0
    while array[i] do
      result[array[i][0]] = array[i][1].map(&:chr).join
      i += 1
    end
    nearest_stops = get_stops(result[:lat], result[:lng])
    f.send!([:result, nearest_stops])
    
    f.receive_loop
  end
end