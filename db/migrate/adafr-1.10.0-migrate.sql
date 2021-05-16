INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = "adafr_member"), "amount");
