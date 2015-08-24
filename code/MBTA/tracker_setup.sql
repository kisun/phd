CREATE TABLE tracks(
       oid INTEGER PRIMARY KEY,
       vehicle_id VARCHAR(10) NOT NULL,
       timestamp BIGINT,
       pos_lat FLOAT NOT NULL,
       pos_lon FLOAT NOT NULL,
       trip_id VARCHAR(10),
       shape_id VARCHAR(10),
       distance_into_trip FLOAT,
       TPI INT,
       distance_into_TPI FLOAT,
       deviation INT,
       validity BOOLEAN NOT NULL DEFAULT 0,
       n_rejects INT NOT NULL DEFAULT 0,
       CHECK (validity IN (0, 1))
);


