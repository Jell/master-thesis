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
  end
end