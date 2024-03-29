/* File generated automatically by dynamo */
/* The Member table holds the list of Ada France members with the necessary
information so that we can send them the Ada User Journal if they are
member of Ada Europe. The member is first in the PENDING state
until we receive the validation of the email address. Then, it enters
int the WAITNG_PAYMENT state until the payment is acknowledged.
The payment process is manual (wire transfer or by check) and
switch to MEMBER once it is received.the member identifier */
CREATE TABLE IF NOT EXISTS adafr_member (
  /*  */
  `id` BIGINT NOT NULL,
  /* optimistic locking version */
  `version` INTEGER NOT NULL,
  /* the member's first name. */
  `first_name` VARCHAR(255) BINARY NOT NULL,
  /* the member's last name. */
  `last_name` VARCHAR(255) BINARY NOT NULL,
  /* the optional member's company name. */
  `company` VARCHAR(255) BINARY NOT NULL,
  /* first adress field. */
  `address1` VARCHAR(255) BINARY NOT NULL,
  /* second address field. */
  `address2` VARCHAR(255) BINARY NOT NULL,
  /* third address field. */
  `address3` VARCHAR(255) BINARY NOT NULL,
  /* address postal code. */
  `postal_code` VARCHAR(255) BINARY NOT NULL,
  /* address tiown. */
  `city` VARCHAR(255) BINARY NOT NULL,
  /* the country. */
  `country` VARCHAR(255) BINARY NOT NULL,
  /* the date when the member record was created. */
  `create_date` DATETIME NOT NULL,
  /* the date when the member's email was validated. */
  `mail_verify_date` DATETIME ,
  /* the date when the payment was received. */
  `payment_date` DATETIME ,
  /*  */
  `status` TINYINT NOT NULL,
  /* whether the member is also member of Ada Europe. */
  `ada_europe` TINYINT NOT NULL,
  /* secure key salt. */
  `salt` VARCHAR(255) BINARY NOT NULL,
  /* date when the information was updated. */
  `update_date` DATETIME NOT NULL,
  /* the subscription deadline */
  `subscription_deadline` DATE ,
  /* amount in euros */
  `amount` INTEGER NOT NULL,
  /*  */
  `receipt_id` BIGINT ,
  /* the member's email address. */
  `email_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*  */
CREATE TABLE IF NOT EXISTS adafr_receipt (
  /* the receipt id */
  `id` BIGINT NOT NULL,
  /* the receipt creation date */
  `create_date` DATE NOT NULL,
  /* the amount in euros */
  `amount` INTEGER NOT NULL,
  /*  */
  `member` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
INSERT IGNORE INTO ado_entity_type (name) VALUES
("adafr_member"), ("adafr_receipt");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "adafr_member"), "first_name");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "adafr_member"), "last_name");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "adafr_member"), "company");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "adafr_member"), "address1");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "adafr_member"), "address2");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "adafr_member"), "address3");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "adafr_member"), "postal_code");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "adafr_member"), "city");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "adafr_member"), "country");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "adafr_member"), "mail_verify_date");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "adafr_member"), "payment_date");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "adafr_member"), "status");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "adafr_member"), "ada_europe");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "adafr_member"), "amount");
INSERT IGNORE INTO ado_version (name, version) VALUES ("adafr", 1);
