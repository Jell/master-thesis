class BusStop < ActiveRecord::Base
  validates_presence_of :name, :lat, :lng, :stop_id, :provider_name, :url
  
  after_initialize :extend_module
  
  private
  def extend_module
    self.extend "Provider::#{provider_name}".constantize
  end
end
