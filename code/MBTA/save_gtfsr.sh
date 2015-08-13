echo "Adding latest GTFS Realtime results to database `gtfs` ..."

echo "This will continue forever until you stop it ..."

/home/tell029/Documents/uni/gtfsrdb/gtfsrdb.py -p http://developer.mbta.com/lib/GTRTFS/Alerts/VehiclePositions.pb -d sqlite:///gtfs-historical.db -c
