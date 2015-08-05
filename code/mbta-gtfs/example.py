from google.transit import gtfs_realtime_pb2
import urllib

feed = gtfs_realtime_pb2.FeedMessage()
response = urllib.urlopen('http://onebusaway.gatech.edu:8080/vehicle-positions')
feed.ParseFromString(response.read())

f = open('feed.csv', 'wb')

f.write('vehicle_id,lat,lon\n')
for entity in feed.entity:
    if entity.HasField('vehicle'):
        f.write(entity.id + ',' + str(entity.vehicle.position.latitude) + ',' + str(entity.vehicle.position.longitude) + '\n')

f.close()
