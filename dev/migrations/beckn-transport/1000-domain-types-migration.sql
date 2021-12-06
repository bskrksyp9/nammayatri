DELETE FROM atlas_transporter.product_instance AS T1 WHERE T1.type = 'LOCATIONTRACKER';

UPDATE atlas_transporter.product_instance AS T1
	SET case_id = (SELECT case_id FROM atlas_transporter.product_instance AS T2 WHERE T2.id = T1.parent_id)
	WHERE T1.type <> 'RIDESEARCH';

DELETE FROM atlas_transporter."case" AS T1 WHERE T1.type <> 'RIDESEARCH';

ALTER TABLE atlas_transporter."case" RENAME TO search_request;

ALTER TABLE atlas_transporter.product_instance RENAME COLUMN case_id TO request_id;

ALTER TABLE atlas_transporter.search_request DROP COLUMN parent_case_id;


CREATE TABLE atlas_transporter.ride AS SELECT * FROM atlas_transporter.product_instance where type = 'RIDEORDER'; 

ALTER TABLE atlas_transporter.ride OWNER TO atlas;

ALTER TABLE atlas_transporter.ride RENAME COLUMN parent_id TO product_instance_id; 

ALTER TABLE ONLY atlas_transporter.ride
    ADD CONSTRAINT idx_16395_primary PRIMARY KEY (id);

CREATE INDEX idx_16395_case_id ON atlas_transporter.ride USING btree (request_id);

CREATE INDEX idx_16395_entity_id ON atlas_transporter.ride USING btree (entity_id);

CREATE INDEX idx_16395_organization_id ON atlas_transporter.ride USING btree (organization_id);

CREATE INDEX idx_16395_product_instance_id ON atlas_transporter.ride USING btree (product_instance_id);

CREATE INDEX idx_16395_person_id ON atlas_transporter.ride USING btree (person_id);

CREATE INDEX idx_16395_product_id ON atlas_transporter.ride USING btree (product_id);

CREATE INDEX idx_16395_status ON atlas_transporter.ride USING btree (status);

ALTER TABLE atlas_transporter.rating RENAME COLUMN product_instance_id TO ride_id;

ALTER TABLE atlas_transporter.ride_cancellation_reason
   DROP CONSTRAINT ride_cancellation_reason_ride_id_fkey
 , ADD  CONSTRAINT ride_cancellation_reason_ride_id_fkey FOREIGN KEY (ride_id)
      REFERENCES atlas_transporter.ride (id) on delete cascade;

ALTER TABLE atlas_transporter.ride_request
   DROP CONSTRAINT ride_request_ride_id_fkey
 , ADD  CONSTRAINT ride_request_ride_id_fkey FOREIGN KEY (ride_id)
      REFERENCES atlas_transporter.ride (id) on delete cascade;

ALTER TABLE atlas_transporter.notification_status
   DROP CONSTRAINT notification_status_ride_id_fkey
 , ADD  CONSTRAINT notification_status_ride_id_fkey FOREIGN KEY (ride_id)
      REFERENCES atlas_transporter.ride (id) on delete cascade;

DELETE FROM atlas_transporter.product_instance AS T1 WHERE T1.type = 'RIDEORDER';

ALTER TABLE atlas_transporter.search_request DROP COLUMN type;

ALTER TABLE atlas_transporter.product_instance DROP COLUMN type;

ALTER TABLE atlas_transporter.product_instance DROP COLUMN parent_id;

ALTER TABLE atlas_transporter.product_instance RENAME TO quote;

ALTER TABLE atlas_transporter.ride RENAME COLUMN product_instance_id TO quote_id;

ALTER TABLE atlas_transporter.search_request DROP COLUMN name;
ALTER TABLE atlas_transporter.search_request DROP COLUMN description;
ALTER TABLE atlas_transporter.search_request DROP COLUMN status;
ALTER TABLE atlas_transporter.search_request DROP COLUMN industry;
ALTER TABLE atlas_transporter.search_request DROP COLUMN end_time;
ALTER TABLE atlas_transporter.search_request DROP COLUMN exchange_type;
ALTER TABLE atlas_transporter.search_request DROP COLUMN provider_type;
ALTER TABLE atlas_transporter.search_request DROP COLUMN requestor_type;
ALTER TABLE atlas_transporter.search_request DROP COLUMN udf2;
ALTER TABLE atlas_transporter.search_request DROP COLUMN udf3;
ALTER TABLE atlas_transporter.search_request DROP COLUMN info;
ALTER TABLE atlas_transporter.search_request DROP COLUMN updated_at;

ALTER TABLE atlas_transporter.search_request RENAME COLUMN udf1 TO vehicle_variant;
ALTER TABLE atlas_transporter.search_request RENAME COLUMN udf4 TO bap_id;
ALTER TABLE atlas_transporter.search_request RENAME COLUMN requestor TO requestor_id;
ALTER TABLE atlas_transporter.search_request RENAME COLUMN provider TO provider_id;

ALTER TABLE atlas_transporter.search_request ADD COLUMN transaction_id character(36);
WITH txnIdsTable AS (SELECT id, regexp_split_to_array(T3.short_id, E'_') AS txn_id FROM atlas_transporter.search_request AS T3)
UPDATE atlas_transporter.search_request AS T1
	SET transaction_id = (SELECT txn_id[2] FROM txnIdsTable AS T2 WHERE T2.id = T1.id);
ALTER TABLE atlas_transporter.search_request DROP COLUMN short_id;
ALTER TABLE atlas_transporter.search_request ADD COLUMN bap_uri character varying(255);
UPDATE atlas_transporter.search_request SET bap_uri = (SELECT callback_url FROM atlas_transporter.organization where id = atlas_transporter.search_request.bap_id);
UPDATE atlas_transporter.search_request AS T1
	SET requestor_id = 'UNKNOWN' WHERE requestor_id IS NULL;
UPDATE atlas_transporter.search_request AS T1
	SET provider_id = 'UNKNOWN' WHERE provider_id IS NULL;
UPDATE atlas_transporter.search_request AS T1
	SET bap_id = 'UNKNOWN' WHERE bap_id IS NULL;
UPDATE atlas_transporter.search_request AS T1
	SET bap_uri = 'UNKNOWN' WHERE bap_uri IS NULL;

ALTER TABLE atlas_transporter.search_request ALTER COLUMN transaction_id SET NOT NULL;
ALTER TABLE atlas_transporter.search_request ALTER COLUMN requestor_id SET NOT NULL;
ALTER TABLE atlas_transporter.search_request ALTER COLUMN provider_id SET NOT NULL;
ALTER TABLE atlas_transporter.search_request ALTER COLUMN bap_id SET NOT NULL;
ALTER TABLE atlas_transporter.search_request ALTER COLUMN bap_uri SET NOT NULL;

ALTER TABLE atlas_transporter.ride RENAME TO old_ride;

CREATE TABLE atlas_transporter.ride_booking (
    id character(36) PRIMARY KEY NOT NULL,
    transaction_id character(36) NOT NULL,
    request_id character(36) NOT NULL REFERENCES atlas_transporter.search_request (id),
    quote_id character(36) NOT NULL REFERENCES atlas_transporter.quote (id),
    status character varying(255) NOT NULL,
    provider_id character(36) NOT NULL REFERENCES atlas_transporter.organization (id),
    vehicle_variant character varying(255) NOT NULL,
    bap_id character varying(255) NOT NULL,
    start_time timestamp with time zone NOT NULL,
    requestor_id character(36) NOT NULL,
    from_location_id character(36) NOT NULL REFERENCES atlas_transporter.search_request_location (id),
    to_location_id character(36) NOT NULL REFERENCES atlas_transporter.search_request_location (id),
    estimated_fare double precision NOT NULL,
    discount double precision,
    estimated_total_fare numeric(30,2) NOT NULL,
    distance double precision NOT NULL,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE TABLE atlas_transporter.ride (
    id character(36) PRIMARY KEY NOT NULL,
    booking_id character(36) NOT NULL REFERENCES atlas_transporter.ride_booking (id),
    short_id character varying(36) NOT NULL,
    status character varying(255) NOT NULL,
    driver_id character(36) NOT NULL REFERENCES atlas_transporter.person (id),
    vehicle_id character(36) NOT NULL REFERENCES atlas_transporter.vehicle (id),
    otp character(4) NOT NULL,
    tracking_url character varying(255) NOT NULL,
    fare double precision,
    total_fare numeric(30,2),
    traveled_distance double precision NOT NULL DEFAULT 0,
    chargeable_distance double precision,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE atlas_transporter.quote ALTER COLUMN estimated_fare SET NOT NULL;
ALTER TABLE atlas_transporter.quote RENAME COLUMN organization_id TO provider_id;
ALTER TABLE atlas_transporter.quote ADD COLUMN distance_to_nearest_driver float;
ALTER TABLE atlas_transporter.quote ADD COLUMN distance float;

UPDATE atlas_transporter.quote AS T1
	SET udf1 = '0' WHERE udf1 IN ('SEDAN', 'HATCHBACK', 'SUV');
UPDATE atlas_transporter.quote AS T1
	SET distance_to_nearest_driver = CAST (udf1 AS float);
UPDATE atlas_transporter.quote AS T1
	SET distance_to_nearest_driver = 0 WHERE distance_to_nearest_driver IS NULL;
UPDATE atlas_transporter.search_request AS T1
    SET udf5 = '0' WHERE udf5 IS NULL OR udf5 = '';
UPDATE atlas_transporter.quote AS T1
	SET distance = CAST ((SELECT udf5 FROM atlas_transporter.search_request AS T2 WHERE T1.request_id = T2.id) AS float);
UPDATE atlas_transporter.quote AS T1
	SET distance = 0 WHERE distance_to_nearest_driver IS NULL;


ALTER TABLE atlas_transporter.search_request DROP COLUMN udf5;
ALTER TABLE atlas_transporter.quote ALTER COLUMN distance_to_nearest_driver SET NOT NULL;
ALTER TABLE atlas_transporter.quote ALTER COLUMN estimated_fare SET NOT NULL;
ALTER TABLE atlas_transporter.quote ALTER COLUMN distance SET NOT NULL;

INSERT INTO atlas_transporter.ride_booking
    SELECT
        T1.id,
        T2.transaction_id,
        T1.request_id,
        T1.quote_id,
        T1.status,
        T1.organization_id,
        T3.vehicle_variant,
        T2.bap_id,
        T2.start_time,
        T2.requestor_id,
        T1.from_location_id,
        T1.to_location_id,
        T1.estimated_fare,
        T1.discount,
        T1.estimated_total_fare,
        T3.distance,
        T1.created_at,
        T1.updated_at
    FROM atlas_transporter.old_ride AS T1
    JOIN atlas_transporter.search_request AS T2
        ON T1.request_id = T2.id
    JOIN atlas_transporter.quote AS T3
        ON T1.quote_id = T3.id;

UPDATE atlas_transporter.ride_booking AS T1
	SET status = 'TRIP_ASSIGNED' WHERE T1.status = 'INPROGRESS';

INSERT INTO atlas_transporter.ride
    SELECT
        T1.id,
        T1.id,
        T1.short_id,
        T1.status,
        T1.person_id,
        T1.entity_id,
        T1.udf4,
        'UNKNOWN',
        T1.fare,
        T1.total_fare,
        T1.traveled_distance,
        T1.chargeable_distance,
        T1.created_at,
        T1.updated_at
    FROM atlas_transporter.old_ride AS T1
    WHERE T1.status != 'CONFIRMED' AND T1.person_id IS NOT NULL;

UPDATE atlas_transporter.ride AS T1
	SET status = 'NEW' WHERE T1.status = 'TRIP_ASSIGNED';

ALTER TABLE atlas_transporter.quote DROP COLUMN person_id;
ALTER TABLE atlas_transporter.quote DROP COLUMN person_updated_at;
ALTER TABLE atlas_transporter.quote DROP COLUMN short_id;
ALTER TABLE atlas_transporter.quote DROP COLUMN entity_id;
ALTER TABLE atlas_transporter.quote DROP COLUMN entity_type;
ALTER TABLE atlas_transporter.quote DROP COLUMN quantity;
ALTER TABLE atlas_transporter.quote DROP COLUMN status;
ALTER TABLE atlas_transporter.quote DROP COLUMN start_time;
ALTER TABLE atlas_transporter.quote DROP COLUMN end_time;
ALTER TABLE atlas_transporter.quote DROP COLUMN valid_till;
ALTER TABLE atlas_transporter.quote DROP COLUMN from_location_id;
ALTER TABLE atlas_transporter.quote DROP COLUMN to_location_id;
ALTER TABLE atlas_transporter.quote DROP COLUMN info;
ALTER TABLE atlas_transporter.quote DROP COLUMN udf1;
ALTER TABLE atlas_transporter.quote DROP COLUMN udf2;
ALTER TABLE atlas_transporter.quote DROP COLUMN udf3;
ALTER TABLE atlas_transporter.quote DROP COLUMN udf4;
ALTER TABLE atlas_transporter.quote DROP COLUMN udf5;
ALTER TABLE atlas_transporter.quote DROP COLUMN updated_at;
ALTER TABLE atlas_transporter.quote DROP COLUMN fare;

ALTER TABLE atlas_transporter.ride_request RENAME COLUMN ride_id TO ride_booking_id;
ALTER TABLE atlas_transporter.ride_cancellation_reason RENAME COLUMN ride_id TO ride_booking_id;
ALTER TABLE atlas_transporter.notification_status RENAME COLUMN ride_id TO ride_booking_id;
ALTER TABLE atlas_transporter.allocation_event RENAME COLUMN ride_id TO ride_booking_id;

ALTER TABLE atlas_transporter.ride_cancellation_reason
   DROP CONSTRAINT ride_cancellation_reason_ride_id_fkey
 , ADD  CONSTRAINT ride_cancellation_reason_ride_booking_id_fkey FOREIGN KEY (ride_booking_id)
      REFERENCES atlas_transporter.ride_booking (id) on delete cascade;

ALTER TABLE atlas_transporter.ride_request
   DROP CONSTRAINT ride_request_ride_id_fkey
 , ADD  CONSTRAINT ride_request_ride_booking_id_fkey FOREIGN KEY (ride_booking_id)
      REFERENCES atlas_transporter.ride_booking (id) on delete cascade;

ALTER TABLE atlas_transporter.notification_status
   DROP CONSTRAINT notification_status_ride_id_fkey
 , ADD  CONSTRAINT notification_status_ride_booking_id_fkey FOREIGN KEY (ride_booking_id)
      REFERENCES atlas_transporter.ride_booking (id) on delete cascade;

ALTER TABLE atlas_transporter.allocation_event
   ADD  CONSTRAINT allocation_event_ride_booking_id_fkey FOREIGN KEY (ride_booking_id)
      REFERENCES atlas_transporter.ride_booking (id) on delete cascade;

ALTER TABLE atlas_transporter.discount_transaction
   DROP CONSTRAINT discount_transaction_ride_booking_id_fkey
 , ADD  CONSTRAINT discount_transaction_ride_booking_id_fkey FOREIGN KEY (ride_booking_id)
      REFERENCES atlas_transporter.ride_booking (id) on delete cascade;

DROP TABLE atlas_transporter.old_ride;