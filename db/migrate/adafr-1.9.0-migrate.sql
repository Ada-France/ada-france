ALTER TABLE adafr_member ADD COLUMN `subscription_deadline` DATE;
ALTER TABLE adafr_member ADD COLUMN `amount` INTEGER NOT NULL;
ALTER TABLE adafr_receipt ADD COLUMN `amount` INTEGER NOT NULL;

UPDATE adafr_member SET subscription_deadline = '2020-12-31 23:59:59' WHERE payment_date IS NOT NULL;
UPDATE adafr_member SET amount = '30' WHERE payment_date IS NOT NULL AND ada_europe = 0;
UPDATE adafr_member SET amount = '65' WHERE payment_date IS NOT NULL AND ada_europe = 1;
UPDATE adafr_receipt INNER JOIN adafr_member ON adafr_receipt.id = adafr_member.receipt_id SET adafr_receipt.amount = adafr_member.amount;
