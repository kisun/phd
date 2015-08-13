#!/bin/bash

rm -rf gtfs
curl -o gtfs.zip http://www.mbta.com/uploadedfiles/MBTA_GTFS.zip
unzip -d gtfs gtfs.zip
rm gtfs.zip

curl -o gtfs-realtime.proto https://developers.google.com/transit/gtfs-realtime/gtfs-realtime.proto
