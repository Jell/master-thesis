SELECT * FROM bus_stops WHERE lat >= lat1 AND
                              lat <= lat2 AND
                              lng >= lng1 AND
                              lng <= lng2 AND NOT
                                      (lat > lat3 AND
                                       lat < lat4 AND
                                       lng > lng3 AND
                                       lng < lng4)