class BusStop < ActiveRecord::Base
  validates_presence_of :name, :lat, :lng, :stop_id,
                                :provider_name, :url
end
