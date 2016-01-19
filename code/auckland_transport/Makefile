default:
	@echo Downloading GTFS schedule files ...
	@echo
	@mkdir -p _data
	@curl -o _data/gtfs.zip https://cdn01.at.govt.nz/data/gtfs.zip

	@echo
	@echo Extracting files from ZIP archive ...
	@unzip _data/gtfs.zip -d _data/gtfs

	@echo
	@echo Creating GTFS database ...
	@echo "(not yet available)"
	@echo

	@echo "Done! If you haven't done so already, add your API key to apikey.txt."
	@echo

database:
	@echo Adding GTFS schedule data to SQLite database ...
	@echo
	@mkdir -p db
#	-@touch db/gtfs-static.db
#	@R -f src/build_gtfs_database.R --slave

	@cp db/gtfs-static.db db/gtfs-realtime.db
	@rm -rf bin/gtfsrdb
	@wget -q -O gtfsrdb.zip https://github.com/tmelliott/gtfsrdb/archive/master.zip
	@unzip -q gtfsrdb.zip -d bin
	@mv bin/gtfsrdb-master bin/gtfsrdb

	@echo Done!
	@echo

saveRT:
	@echo Saving historical data to database ... cancel with CTRC + C
	@echo
	@bin/gtfsrdb/gtfsrdb.py -p https://api.at.govt.nz/v1/public/realtime/vehiclelocations?api_key=$(shell cat apikey.txt) --accept="application/x-protobuf" -d sqlite:///db/gtfs-realtime.db -c
