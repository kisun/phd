#!/bin/bash

curl -o gtfs-realtime.proto https://developers.google.com/transit/gtfs-realtime/gtfs-realtime.proto

gtfsdb-load --database_url sqlite:///gtfs.db https://cdn01.at.govt.nz/data/gtfs.zip
