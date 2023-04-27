ALTER TABLE atlas_app.booking_cancellation_reason ADD COLUMN merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id) DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51';

ALTER TABLE atlas_app.driver_offer ADD COLUMN merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id) DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51';

ALTER TABLE atlas_app.estimate ADD COLUMN merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id) DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51';

ALTER TABLE atlas_app.ride ADD COLUMN merchant_id character(36) NOT NULL REFERENCES atlas_app.merchant (id) DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51';
