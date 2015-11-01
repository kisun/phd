echo "Saving latest GTFS Realtime results to database `gtfs` ..."

~/Documents/uni/gtfsrdb/gtfsrdb.py -p http://developer.mbta.com/lib/GTRTFS/Alerts/VehiclePositions.pb -d sqlite:///gtfs.db -c -o

echo "Done?"
