ALTER TABLE adafr_member ADD COLUMN `receipt_id` BIGINT;

CREATE TABLE adafr_receipt (
  /* the receipt id */
  `id` BIGINT NOT NULL,
  /* the receipt creation date */
  `create_date` DATE NOT NULL,
  /*  */
  `member` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
INSERT INTO entity_type (name) VALUES
("adafr_receipt")
;

UPDATE awa_blog SET format = 1 WHERE id = 1;
