namespace :bus_stop do
  desc 'Load bus stops from all providers'
  task :load_all => :environment do
    [Provider::Vasttrafik, Provider::StorstockholmsLokaltrafik].
                                                    each do |provider|
                                                      
      loader = BusStopLoader.new(provider)
      loader.update_stop_database
    end
  end
end