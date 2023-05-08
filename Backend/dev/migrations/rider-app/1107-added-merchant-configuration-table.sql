CREATE TABLE atlas_app.merchant_config (
    merchant_id character(36) NOT NULL PRIMARY KEY,
    fraud_booking_cancellation_count_threshold int NOT NULL,
    fraud_booking_total_count_threshold int NOT NULL,
    fraud_booking_detection_window json NOT NULL
);

ALTER TABLE atlas_app.person ADD COLUMN blocked_at TIMESTAMP;

ALTER TABLE atlas_app.merchant_service_usage_config ADD COLUMN use_fraud_detection boolean NOT NULL DEFAULT FALSE;

INSERT INTO atlas_app.merchant_config VALUES ('favorit0-0000-0000-0000-00000favorit', 10, 0, '{"period":24, "periodType":"Hours"}');
