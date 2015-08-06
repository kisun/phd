from google.transit import gtfs_realtime_pb2
import urllib
import datetime

url = "http://developer.mbta.com/lib/GTRTFS/Alerts/VehiclePositions.pb"

feed = gtfs_realtime_pb2.FeedMessage()
response = urllib.urlopen(url)
feed.ParseFromString(response.read())

timestamp = feed.header.timestamp
dt = datetime.datetime.fromtimestamp(timestamp).strftime('%Y-%m-%d %H:%M:%S')

print 'The latest feed was created at ' + dt

f = open('latest_feed.csv', 'wb')

f.write('vehicle_id,trip_id,route_id,lat,lon\n')
for entity in feed.entity:
    if entity.HasField('vehicle'):
        f.write(entity.vehicle.vehicle.id + ',' +
                entity.vehicle.trip.trip_id + ',' +
                entity.vehicle.trip.route_id + ',' +
                str(entity.vehicle.position.latitude) + ',' +
                str(entity.vehicle.position.longitude) + '\n')

f.close()
