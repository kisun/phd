default:
	@echo sudo apt-get update && sudo apt-get -y install inotify-tools
	#@echo Downloading GTFS schedule files ...
	@echo
	@mkdir -p _data
	#@curl -o _data/gtfs.zip https://cdn01.at.govt.nz/data/gtfs.zip

	@echo
	#@echo Extracting files from ZIP archive ...
	#@rm -rf _data/gtfs
	#@unzip _data/gtfs.zip -d _data/gtfs

	@echo
	@echo Creating GTFS database ...
	@make database
	@echo

	@echo "Done! If you haven't done so already, add your API key to apikey.txt."
	@echo

database:
	@echo Adding GTFS schedule data to SQLite database ...
	@bin/fetch-gtfs

old:
	@echo
	@mkdir -p db/backups
	-@mv db/gtfs-static.db db/backups/gtfs-static_$(shell date +%Y%m%d%H%M%S).db
	-@touch db/gtfs-static.db
	@R -f scripts/build_gtfs_database.R


saveRT:
	@echo Saving historical data to database ... cancel with CTRL + C
	@echo
	@bin/gtfsrdb/gtfsrdb.py -p https://api.at.govt.nz/v1/public/realtime/vehiclelocations?api_key=$(shell cat apikey.txt) --accept="application/x-protobuf" -d sqlite:///db/gtfs-history.db -c

getLatestOld:
	@echo Getting latest vehicle locations ... cancel with CTRL + C
	@echo
	@bin/gtfsrdb/gtfsrdb.py -p https://api.at.govt.nz/v1/public/realtime/vehiclelocations?api_key=$(shell cat apikey.txt) --accept="application/x-protobuf" -d sqlite:///db/gtfs-realtime.db -c -o -w 10


getLatest:
		@echo Getting latest vehicle locations ... cancel with CTRL + C
		@echo
		bin/gtfsrdb/gtfsrdb.py -p https://api.at.govt.nz/v2/public/realtime --accept="application/x-protobuf" --key="$(shell cat apikey.txt)" -d sqlite:///db/gtfs-realtime.db -c -o -w 30
