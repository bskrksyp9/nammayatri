ALTER TABLE atlas_driver_offer_bpp.booking_cancellation_reason ADD COLUMN merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id) DEFAULT 'favorit0-0000-0000-0000-00000favorit';

ALTER TABLE atlas_driver_offer_bpp.driver_information ADD COLUMN merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id) DEFAULT 'favorit0-0000-0000-0000-00000favorit';


ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id) DEFAULT 'favorit0-0000-0000-0000-00000favorit';

ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id) DEFAULT 'favorit0-0000-0000-0000-00000favorit';