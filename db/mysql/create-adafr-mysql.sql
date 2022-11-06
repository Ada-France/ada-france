/* Copied from ado-mysql.sql*/
/* File generated automatically by dynamo */
/* Entity table that enumerates all known database tables */
CREATE TABLE IF NOT EXISTS ado_entity_type (
  /* the database table unique entity index */
  `id` INTEGER  AUTO_INCREMENT,
  /* the database entity name */
  `name` VARCHAR(127) BINARY UNIQUE ,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* Sequence generator */
CREATE TABLE IF NOT EXISTS ado_sequence (
  /* the sequence name */
  `name` VARCHAR(127) UNIQUE NOT NULL,
  /* the sequence record version */
  `version` INTEGER NOT NULL,
  /* the sequence value */
  `value` BIGINT NOT NULL,
  /* the sequence block size */
  `block_size` BIGINT NOT NULL,
  PRIMARY KEY (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* Database schema version (per module) */
CREATE TABLE IF NOT EXISTS ado_version (
  /* the module name */
  `name` VARCHAR(127) UNIQUE NOT NULL,
  /* the database version schema for this module */
  `version` INTEGER NOT NULL,
  PRIMARY KEY (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
INSERT IGNORE INTO ado_entity_type (name) VALUES
("ado_entity_type"), ("ado_sequence"), ("ado_version");
/* Copied from awa-mysql.sql*/
/* File generated automatically by dynamo */
/* The Audit table records the changes made on database on behalf of a user.
The record indicates the database table and row, the field being updated,
the old and new value. The old and new values are converted to a string
and they truncated if necessary to 256 characters. */
CREATE TABLE IF NOT EXISTS awa_audit (
  /* the audit identifier */
  `id` BIGINT NOT NULL,
  /* the date when the field was modified. */
  `date` DATETIME NOT NULL,
  /* the old field value. */
  `old_value` VARCHAR(255) BINARY ,
  /* the new field value. */
  `new_value` VARCHAR(255) BINARY ,
  /* the database entity identifier to which the audit is associated. */
  `entity_id` BIGINT NOT NULL,
  /*  */
  `field` INTEGER NOT NULL,
  /* the user session under which the field was modified. */
  `session_id` BIGINT ,
  /* the entity type. */
  `entity_type` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* The Audit_Field table describes
the database field being updated. */
CREATE TABLE IF NOT EXISTS awa_audit_field (
  /* the audit field identifier. */
  `id` INTEGER NOT NULL AUTO_INCREMENT,
  /* the audit field name. */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the entity type */
  `entity_type` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*  */
CREATE TABLE IF NOT EXISTS awa_message (
  /* the message identifier */
  `id` BIGINT NOT NULL,
  /* the message creation date */
  `create_date` DATETIME NOT NULL,
  /* the message priority */
  `priority` INTEGER NOT NULL,
  /* the message count */
  `count` INTEGER NOT NULL,
  /* the message parameters */
  `parameters` VARCHAR(255) BINARY NOT NULL,
  /* the server identifier which processes the message */
  `server_id` INTEGER NOT NULL,
  /* the task identifier on the server which processes the message */
  `task_id` INTEGER NOT NULL,
  /* the message status */
  `status` TINYINT NOT NULL,
  /* the message processing date */
  `processing_date` DATETIME ,
  /*  */
  `version` INTEGER NOT NULL,
  /* the entity identifier to which this event is associated. */
  `entity_id` BIGINT NOT NULL,
  /* the entity type of the entity identifier to which this event is associated. */
  `entity_type` INTEGER NOT NULL,
  /* the date and time when the event was finished to be processed. */
  `finish_date` DATETIME ,
  /*  */
  `queue_id` BIGINT NOT NULL,
  /* the message type */
  `message_type_id` BIGINT NOT NULL,
  /* the optional user who triggered the event message creation */
  `user_id` BIGINT ,
  /* the optional user session that triggered the message creation */
  `session_id` BIGINT ,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*  */
CREATE TABLE IF NOT EXISTS awa_message_type (
  /*  */
  `id` BIGINT NOT NULL,
  /* the message type name */
  `name` VARCHAR(255) BINARY NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* The message queue tracks the event messages that must be dispatched by
a given server. */
CREATE TABLE IF NOT EXISTS awa_queue (
  /*  */
  `id` BIGINT NOT NULL,
  /*  */
  `server_id` INTEGER NOT NULL,
  /* the message queue name */
  `name` VARCHAR(255) BINARY NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* The application that is granted access to the database. */
CREATE TABLE IF NOT EXISTS awa_application (
  /* the application identifier. */
  `id` BIGINT NOT NULL,
  /* the application name. */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* the application secret key. */
  `secret_key` VARCHAR(255) BINARY NOT NULL,
  /* the application public identifier. */
  `client_id` VARCHAR(255) BINARY NOT NULL,
  /* the optimistic lock version. */
  `version` INTEGER NOT NULL,
  /* the application create date. */
  `create_date` DATETIME NOT NULL,
  /* the application update date. */
  `update_date` DATETIME NOT NULL,
  /* the application title displayed in the OAuth login form. */
  `title` VARCHAR(255) BINARY NOT NULL,
  /* the application description. */
  `description` VARCHAR(255) BINARY NOT NULL,
  /* the optional login URL. */
  `app_login_url` VARCHAR(255) BINARY NOT NULL,
  /* the application logo URL. */
  `app_logo_url` VARCHAR(255) BINARY NOT NULL,
  /*  */
  `user_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*  */
CREATE TABLE IF NOT EXISTS awa_callback (
  /*  */
  `id` BIGINT NOT NULL,
  /*  */
  `url` VARCHAR(255) BINARY NOT NULL,
  /* the optimistic lock version. */
  `version` INTEGER NOT NULL,
  /*  */
  `application_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* The session is created when the user has granted an access to an application
or when the application has refreshed its access token. */
CREATE TABLE IF NOT EXISTS awa_oauth_session (
  /* the session identifier. */
  `id` BIGINT NOT NULL,
  /* the session creation date. */
  `create_date` DATETIME NOT NULL,
  /* a random salt string to access/request token generation. */
  `salt` VARCHAR(255) BINARY NOT NULL,
  /* the expiration date. */
  `expire_date` DATETIME NOT NULL,
  /* the application that is granted access. */
  `application_id` BIGINT NOT NULL,
  /*  */
  `user_id` BIGINT NOT NULL,
  /*  */
  `session_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* The ACL table records permissions which are granted for a user to access a given database entity. */
CREATE TABLE IF NOT EXISTS awa_acl (
  /* the ACL identifier */
  `id` BIGINT NOT NULL,
  /* the entity identifier to which the ACL applies */
  `entity_id` BIGINT NOT NULL,
  /* the writeable flag */
  `writeable` TINYINT NOT NULL,
  /*  */
  `user_id` BIGINT NOT NULL,
  /*  */
  `workspace_id` BIGINT NOT NULL,
  /* the entity type concerned by the ACL. */
  `entity_type` INTEGER NOT NULL,
  /* the permission that is granted. */
  `permission` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* The permission table lists all the application permissions that are defined.
This is a system table shared by every user and workspace.
The list of permission is fixed and never changes. */
CREATE TABLE IF NOT EXISTS awa_permission (
  /* the permission database identifier. */
  `id` BIGINT NOT NULL,
  /* the permission name */
  `name` VARCHAR(255) BINARY NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*  */
CREATE TABLE IF NOT EXISTS awa_access_key (
  /* the secure access key. */
  `access_key` VARCHAR(255) BINARY NOT NULL,
  /* the access key expiration date. */
  `expire_date` DATE NOT NULL,
  /* the access key identifier. */
  `id` BIGINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the access key type. */
  `kind` TINYINT NOT NULL,
  /*  */
  `user_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* The Email entity defines the user email addresses.
The user has a primary email address that is obtained
from the registration process (either through a form
submission or through OpenID authentication). */
CREATE TABLE IF NOT EXISTS awa_email (
  /* the email address. */
  `email` VARCHAR(255) BINARY NOT NULL,
  /* the last mail delivery status (if known). */
  `status` TINYINT NOT NULL,
  /* the date when the last email error was detected. */
  `last_error_date` DATETIME NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the email primary key. */
  `id` BIGINT NOT NULL,
  /* the user. */
  `user_id` BIGINT ,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*  */
CREATE TABLE IF NOT EXISTS awa_session (
  /*  */
  `start_date` DATETIME NOT NULL,
  /*  */
  `end_date` DATETIME ,
  /*  */
  `ip_address` VARCHAR(255) BINARY NOT NULL,
  /*  */
  `stype` TINYINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /*  */
  `server_id` INTEGER NOT NULL,
  /*  */
  `id` BIGINT NOT NULL,
  /*  */
  `auth_id` BIGINT ,
  /*  */
  `user_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/* The User entity represents a user that can access and use the application. */
CREATE TABLE IF NOT EXISTS awa_user (
  /* the user first name. */
  `first_name` VARCHAR(255) BINARY NOT NULL,
  /* the user last name. */
  `last_name` VARCHAR(255) BINARY NOT NULL,
  /* the user password hash. */
  `password` VARCHAR(255) BINARY NOT NULL,
  /* the user OpenID identifier. */
  `open_id` VARCHAR(255) BINARY NOT NULL,
  /* the user country. */
  `country` VARCHAR(255) BINARY NOT NULL,
  /* the user display name. */
  `name` VARCHAR(255) BINARY NOT NULL,
  /* version number. */
  `version` INTEGER NOT NULL,
  /* the user identifier. */
  `id` BIGINT NOT NULL,
  /* the password salt. */
  `salt` VARCHAR(255) BINARY NOT NULL,
  /* the status of this user. */
  `status` TINYINT NOT NULL,
  /*  */
  `email_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
INSERT IGNORE INTO ado_entity_type (name) VALUES
("awa_audit"), ("awa_audit_field"), ("awa_message"), ("awa_message_type"), ("awa_queue"), ("awa_application"), ("awa_callback"), ("awa_oauth_session"), ("awa_acl"), ("awa_permission"), ("awa_access_key"), ("awa_email"), ("awa_session"), ("awa_user");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_user"), "first_name");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_user"), "last_name");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_user"), "country");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_user"), "name");
INSERT IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_user"), "status");
INSERT IGNORE INTO ado_version (name, version) VALUES ("awa", 2);
/* Copied from adafr-mysql.sql*/
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
/* Copied from adafr-init-mysql.sql*/
