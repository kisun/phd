CREATE TABLE travel_history 
( "oid" INTEGER,
  "trip_id" TEXT,
  "route_id" TEXT,
  "date" TEXT,
  "vehicle_id" TEXT,
  "stop_no" INTEGER,
  "arrival_time" INTEGER,
  "dwell_time" REAL,
  "pr_stop" REAL,
  "pr_stop_se" REAL,
  "travel_time" REAL,
   PRIMARY KEY(oid)
);
