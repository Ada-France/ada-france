ALTER TABLE adafr_member ADD COLUMN `membership` TINYINT NOT NULL DEFAULT 0;
UPDATE awa_audit_field SET name = 'membership' WHERE
  id = (SELECT id FROM ado_entity_type WHERE name = 'adafr_member')
  AND name = 'ada_europe';
UPDATE adafr_member SET membership = 1 WHERE ada_europe = 1;
ALTER TABLE adafr_member DROP COLUMN `ada_europe`;