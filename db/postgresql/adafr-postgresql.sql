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
  "id" BIGINT NOT NULL,
  /* optimistic locking version */
  "version" INTEGER NOT NULL,
  /* the member's first name. */
  "first_name" VARCHAR(255) NOT NULL,
  /* the member's last name. */
  "last_name" VARCHAR(255) NOT NULL,
  /* the optional member's company name. */
  "company" VARCHAR(255) NOT NULL,
  /* first adress field. */
  "address1" VARCHAR(255) NOT NULL,
  /* second address field. */
  "address2" VARCHAR(255) NOT NULL,
  /* third address field. */
  "address3" VARCHAR(255) NOT NULL,
  /* address postal code. */
  "postal_code" VARCHAR(255) NOT NULL,
  /* address tiown. */
  "city" VARCHAR(255) NOT NULL,
  /* the country. */
  "country" VARCHAR(255) NOT NULL,
  /* the date when the member record was created. */
  "create_date" TIMESTAMP NOT NULL,
  /* the date when the member's email was validated. */
  "mail_verify_date" TIMESTAMP ,
  /* the date when the payment was received. */
  "payment_date" TIMESTAMP ,
  /*  */
  "status" SMALLINT NOT NULL,
  /* whether the member is also member of Ada Europe. */
  "ada_europe" BOOLEAN NOT NULL,
  /* secure key salt. */
  "salt" VARCHAR(255) NOT NULL,
  /* date when the information was updated. */
  "update_date" TIMESTAMP NOT NULL,
  /* the subscription deadline */
  "subscription_deadline" DATE ,
  /* amount in euros */
  "amount" INTEGER NOT NULL,
  /*  */
  "receipt_id" BIGINT ,
  /* the member's email address. */
  "email_id" BIGINT NOT NULL,
  PRIMARY KEY ("id")
);
/*  */
CREATE TABLE IF NOT EXISTS adafr_receipt (
  /* the receipt id */
  "id" BIGINT NOT NULL,
  /* the receipt creation date */
  "create_date" DATE NOT NULL,
  /* the amount in euros */
  "amount" INTEGER NOT NULL,
  /*  */
  "member" BIGINT NOT NULL,
  PRIMARY KEY ("id")
);
INSERT INTO entity_type (name) VALUES
('adafr_member'), ('adafr_receipt')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = 'adafr_member'), 'first_name')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = 'adafr_member'), 'last_name')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = 'adafr_member'), 'company')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = 'adafr_member'), 'address1')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = 'adafr_member'), 'address2')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = 'adafr_member'), 'address3')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = 'adafr_member'), 'postal_code')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = 'adafr_member'), 'city')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = 'adafr_member'), 'country')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = 'adafr_member'), 'mail_verify_date')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = 'adafr_member'), 'payment_date')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = 'adafr_member'), 'status')
  ON CONFLICT DO NOTHING;
INSERT INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM entity_type WHERE name = 'adafr_member'), 'ada_europe')
  ON CONFLICT DO NOTHING;
