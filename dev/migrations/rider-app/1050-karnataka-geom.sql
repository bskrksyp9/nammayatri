UPDATE atlas_app.merchant
SET origin_restriction = '{"Ernakulam", "Karnataka"}',
    destination_restriction = '{"Ernakulam", "Karnataka", "Kerala"}'
WHERE id = 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a51';

ALTER TABLE atlas_app.geometry
    DROP COLUMN id,
    ADD COLUMN id character(36) DEFAULT atlas_app.uuid_generate_v4() NOT NULL;
