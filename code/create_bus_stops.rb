class CreateBusStops < ActiveRecord::Migration
  def self.up
    create_table :bus_stops do |t|
      t.string :name
      t.float :lat
      t.float :lng
      t.string :stop_id
      t.string :provider_name
      t.string :url
      
      t.timestamps
    end
  end

  def self.down
    drop_table :bus_stops
  end
end
