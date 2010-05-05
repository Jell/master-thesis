class BusStop < ActiveRecord::Base
  validates_presence_of :name, :lat, :lng, :stop_id, :provider_name

  after_initialize :extend_module
  
  def url
    get_forecast_url(stop_id)
  end
  
  private
  def extend_module
    self.extend "Provider::#{provider_name}".constantize
  end
  
end
