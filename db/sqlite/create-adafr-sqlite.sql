pragma synchronous=OFF;
/* Copied from ado-sqlite.sql*/
/* File generated automatically by dynamo */
/* Entity table that enumerates all known database tables */
CREATE TABLE IF NOT EXISTS ado_entity_type (
  /* the database table unique entity index */
  `id` INTEGER  PRIMARY KEY AUTOINCREMENT,
  /* the database entity name */
  `name` VARCHAR(127) UNIQUE );
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
);
/* Database schema version (per module) */
CREATE TABLE IF NOT EXISTS ado_version (
  /* the module name */
  `name` VARCHAR(127) UNIQUE NOT NULL,
  /* the database version schema for this module */
  `version` INTEGER NOT NULL,
  PRIMARY KEY (`name`)
);
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('ado_entity_type');
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('ado_sequence');
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('ado_version');
INSERT OR IGNORE INTO ado_version (name, version) VALUES ('ado', 2);
/* Copied from awa-sqlite.sql*/
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
  `old_value` VARCHAR(255) ,
  /* the new field value. */
  `new_value` VARCHAR(255) ,
  /* the database entity identifier to which the audit is associated. */
  `entity_id` BIGINT NOT NULL,
  /*  */
  `field` INTEGER NOT NULL,
  /* the user session under which the field was modified. */
  `session_id` BIGINT ,
  /* the entity type. */
  `entity_type` INTEGER NOT NULL,
  PRIMARY KEY (`id`)
);
/* The Audit_Field table describes
the database field being updated. */
CREATE TABLE IF NOT EXISTS awa_audit_field (
  /* the audit field identifier. */
  `id` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
  /* the audit field name. */
  `name` VARCHAR(255) NOT NULL,
  /* the entity type */
  `entity_type` INTEGER NOT NULL);
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
  `parameters` VARCHAR(255) NOT NULL,
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
);
/*  */
CREATE TABLE IF NOT EXISTS awa_message_type (
  /*  */
  `id` BIGINT NOT NULL,
  /* the message type name */
  `name` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`id`)
);
/* The message queue tracks the event messages that must be dispatched by
a given server. */
CREATE TABLE IF NOT EXISTS awa_queue (
  /*  */
  `id` BIGINT NOT NULL,
  /*  */
  `server_id` INTEGER NOT NULL,
  /* the message queue name */
  `name` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`id`)
);
/* The application that is granted access to the database. */
CREATE TABLE IF NOT EXISTS awa_application (
  /* the application identifier. */
  `id` BIGINT NOT NULL,
  /* the application name. */
  `name` VARCHAR(255) NOT NULL,
  /* the application secret key. */
  `secret_key` VARCHAR(255) NOT NULL,
  /* the application public identifier. */
  `client_id` VARCHAR(255) NOT NULL,
  /* the optimistic lock version. */
  `version` INTEGER NOT NULL,
  /* the application create date. */
  `create_date` DATETIME NOT NULL,
  /* the application update date. */
  `update_date` DATETIME NOT NULL,
  /* the application title displayed in the OAuth login form. */
  `title` VARCHAR(255) NOT NULL,
  /* the application description. */
  `description` VARCHAR(255) NOT NULL,
  /* the optional login URL. */
  `app_login_url` VARCHAR(255) NOT NULL,
  /* the application logo URL. */
  `app_logo_url` VARCHAR(255) NOT NULL,
  /*  */
  `user_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
/*  */
CREATE TABLE IF NOT EXISTS awa_callback (
  /*  */
  `id` BIGINT NOT NULL,
  /*  */
  `url` VARCHAR(255) NOT NULL,
  /* the optimistic lock version. */
  `version` INTEGER NOT NULL,
  /*  */
  `application_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
/* The session is created when the user has granted an access to an application
or when the application has refreshed its access token. */
CREATE TABLE IF NOT EXISTS awa_oauth_session (
  /* the session identifier. */
  `id` BIGINT NOT NULL,
  /* the session creation date. */
  `create_date` DATETIME NOT NULL,
  /* a random salt string to access/request token generation. */
  `salt` VARCHAR(255) NOT NULL,
  /* the expiration date. */
  `expire_date` DATETIME NOT NULL,
  /* the application that is granted access. */
  `application_id` BIGINT NOT NULL,
  /*  */
  `user_id` BIGINT NOT NULL,
  /*  */
  `session_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
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
);
/* The permission table lists all the application permissions that are defined.
This is a system table shared by every user and workspace.
The list of permission is fixed and never changes. */
CREATE TABLE IF NOT EXISTS awa_permission (
  /* the permission database identifier. */
  `id` BIGINT NOT NULL,
  /* the permission name */
  `name` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`id`)
);
/*  */
CREATE TABLE IF NOT EXISTS awa_access_key (
  /* the secure access key. */
  `access_key` VARCHAR(255) NOT NULL,
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
);
/*  */
CREATE TABLE IF NOT EXISTS awa_authenticate (
  /* the identifier */
  `id` BIGINT NOT NULL,
  /* the optimistic lock version. */
  `version` INTEGER NOT NULL,
  /* the identification string */
  `ident` VARCHAR(255) NOT NULL,
  /* the optional salt */
  `salt` VARCHAR(255) NOT NULL,
  /* the optional hash */
  `hash` VARCHAR(255) NOT NULL,
  /* the authenticate method */
  `method` TINYINT NOT NULL,
  /* the email that we authenticate */
  `email_id` BIGINT NOT NULL,
  /* the user that is authenticated */
  `user_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
/* The Email entity defines the user email addresses.
The user has a primary email address that is obtained
from the registration process (either through a form
submission or through OpenID authentication). */
CREATE TABLE IF NOT EXISTS awa_email (
  /* the email address. */
  `email` VARCHAR(255) NOT NULL,
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
);
/*  */
CREATE TABLE IF NOT EXISTS awa_session (
  /*  */
  `start_date` DATETIME NOT NULL,
  /*  */
  `end_date` DATETIME ,
  /*  */
  `ip_address` VARCHAR(255) NOT NULL,
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
  /* the user authenticate record that authentified this session. */
  `user_auth_id` BIGINT ,
  PRIMARY KEY (`id`)
);
/* The User entity represents a user that can access and use the application. */
CREATE TABLE IF NOT EXISTS awa_user (
  /* the user first name. */
  `first_name` VARCHAR(255) NOT NULL,
  /* the user last name. */
  `last_name` VARCHAR(255) NOT NULL,
  /* the user country. */
  `country` VARCHAR(255) NOT NULL,
  /* the user display name. */
  `name` VARCHAR(255) NOT NULL,
  /* version number. */
  `version` INTEGER NOT NULL,
  /* the user identifier. */
  `id` BIGINT NOT NULL,
  /* the status of this user. */
  `status` TINYINT NOT NULL,
  /*  */
  `email_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_audit");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_audit_field");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_message");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_message_type");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_queue");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_application");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_callback");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_oauth_session");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_acl");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_permission");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_access_key");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_authenticate");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_email");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_session");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_user");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_user"), "first_name");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_user"), "last_name");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_user"), "country");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_user"), "name");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_user"), "status");
INSERT OR IGNORE INTO ado_version (name, version) VALUES ("awa", 3);
/* Copied from awa-workspaces-sqlite.sql*/
/* File generated automatically by dynamo */
/*  */
CREATE TABLE IF NOT EXISTS awa_invitation (
  /* the invitation identifier. */
  `id` BIGINT NOT NULL,
  /* version optimistic lock. */
  `version` INTEGER NOT NULL,
  /* date when the invitation was created and sent. */
  `create_date` DATETIME NOT NULL,
  /* the email address to which the invitation was sent. */
  `email` VARCHAR(255) NOT NULL,
  /* the invitation message. */
  `message` text NOT NULL,
  /* the date when the invitation was accepted. */
  `acceptance_date` DATETIME ,
  /* the workspace where the user is invited. */
  `workspace_id` BIGINT NOT NULL,
  /*  */
  `access_key_id` BIGINT ,
  /* the user being invited. */
  `invitee_id` BIGINT ,
  /*  */
  `inviter_id` BIGINT NOT NULL,
  /*  */
  `member_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
/* The workspace controls the features available in the application
for a set of users: the workspace members.  A user could create
several workspaces and be part of several workspaces that other
users have created. */
CREATE TABLE IF NOT EXISTS awa_workspace (
  /* the workspace identifier */
  `id` BIGINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /*  */
  `create_date` DATETIME NOT NULL,
  /*  */
  `owner_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
/*  */
CREATE TABLE IF NOT EXISTS awa_workspace_feature (
  /*  */
  `id` BIGINT NOT NULL,
  /*  */
  `limit` INTEGER NOT NULL,
  /*  */
  `workspace_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
/* The workspace member indicates the users who
are part of the workspace. The join_date is NULL when
a user was invited but has not accepted the invitation. */
CREATE TABLE IF NOT EXISTS awa_workspace_member (
  /*  */
  `id` BIGINT NOT NULL,
  /* the date when the user has joined the workspace. */
  `join_date` DATETIME ,
  /* the member role. */
  `role` VARCHAR(255) NOT NULL,
  /*  */
  `member_id` BIGINT NOT NULL,
  /*  */
  `workspace_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_invitation");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_workspace");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_workspace_feature");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_workspace_member");
INSERT OR IGNORE INTO ado_version (name, version) VALUES ("awa-workspaces", 1);
/* Copied from awa-comments-sqlite.sql*/
/* File generated automatically by dynamo */
/* The Comment table records a user comment associated with a database entity.
The comment can be associated with any other database record. */
CREATE TABLE IF NOT EXISTS awa_comment (
  /* the comment publication date */
  `create_date` DATETIME NOT NULL,
  /* the comment message. */
  `message` TEXT NOT NULL,
  /* the entity identifier to which this comment is associated */
  `entity_id` BIGINT NOT NULL,
  /* the comment identifier */
  `id` BIGINT NOT NULL,
  /* the optimistic lock version. */
  `version` INTEGER NOT NULL,
  /* the entity type that identifies the table to which the comment is associated. */
  `entity_type` INTEGER NOT NULL,
  /* the comment status to decide whether the comment is visible (published) or not. */
  `status` INTEGER NOT NULL,
  /* the comment format type. */
  `format` INTEGER NOT NULL,
  /*  */
  `author_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_comment");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_comment"), "message");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_comment"), "status");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_comment"), "format");
INSERT OR IGNORE INTO ado_version (name, version) VALUES ("awa-comments", 1);
/* Copied from awa-counters-sqlite.sql*/
/* File generated automatically by dynamo */
/*  */
CREATE TABLE IF NOT EXISTS awa_counter (
  /* the object associated with the counter. */
  `object_id` BIGINT NOT NULL,
  /* the day associated with the counter. */
  `date` DATE NOT NULL,
  /* the counter value. */
  `counter` INTEGER NOT NULL,
  /* the counter definition identifier. */
  `definition_id` BIGINT NOT NULL,
  PRIMARY KEY (`object_id`, `date`, `definition_id`)
);
/* A counter definition defines what the counter represents. It uniquely identifies
the counter for the Counter table. A counter may be associated with a database
table. In that case, the counter definition has a relation to the corresponding Entity_Type. */
CREATE TABLE IF NOT EXISTS awa_counter_definition (
  /* the counter name. */
  `name` VARCHAR(255) NOT NULL,
  /* the counter unique id. */
  `id` INTEGER NOT NULL,
  /* the optional entity type that identifies the database table. */
  `entity_type` INTEGER ,
  PRIMARY KEY (`id`)
);
/*  */
CREATE TABLE IF NOT EXISTS awa_visit (
  /* the entity identifier. */
  `object_id` BIGINT NOT NULL,
  /* the number of times the entity was visited by the user. */
  `counter` INTEGER NOT NULL,
  /* the date and time when the entity was last visited. */
  `date` DATETIME NOT NULL,
  /* the user who visited the entity. */
  `user` BIGINT NOT NULL,
  /* the counter definition identifier. */
  `definition_id` BIGINT NOT NULL,
  PRIMARY KEY (`object_id`, `user`, `definition_id`)
);
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_counter");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_counter_definition");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_visit");
INSERT OR IGNORE INTO ado_version (name, version) VALUES ("awa-counters", 1);
/* Copied from awa-tags-sqlite.sql*/
/* File generated automatically by dynamo */
/* The tag definition. */
CREATE TABLE IF NOT EXISTS awa_tag (
  /* the tag identifier */
  `id` BIGINT NOT NULL,
  /* the tag name */
  `name` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`id`)
);
/*  */
CREATE TABLE IF NOT EXISTS awa_tagged_entity (
  /* the tag entity identifier */
  `id` BIGINT NOT NULL,
  /* Title: Tag model
Date: 2013-02-23the database entity to which the tag is associated */
  `for_entity_id` BIGINT NOT NULL,
  /* the entity type */
  `entity_type` INTEGER NOT NULL,
  /*  */
  `tag_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_tag");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_tagged_entity");
INSERT OR IGNORE INTO ado_version (name, version) VALUES ("awa-tags", 1);
/* Copied from awa-jobs-sqlite.sql*/
/* File generated automatically by dynamo */
/* The job is associated with a dispatching queue. */
CREATE TABLE IF NOT EXISTS awa_job (
  /* the job identifier */
  `id` BIGINT NOT NULL,
  /* the job status */
  `status` TINYINT NOT NULL,
  /* the job name */
  `name` VARCHAR(255) NOT NULL,
  /* the job start date */
  `start_date` DATETIME ,
  /* the job creation date */
  `create_date` DATETIME NOT NULL,
  /* the job finish date */
  `finish_date` DATETIME ,
  /* the job progress indicator */
  `progress` INTEGER NOT NULL,
  /* the job parameters */
  `parameters` TEXT NOT NULL,
  /* the job result */
  `results` TEXT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the job priority */
  `priority` INTEGER NOT NULL,
  /*  */
  `user_id` BIGINT ,
  /*  */
  `event_id` BIGINT ,
  /*  */
  `session_id` BIGINT ,
  PRIMARY KEY (`id`)
);
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_job");
INSERT OR IGNORE INTO ado_version (name, version) VALUES ("awa-jobs", 1);
/* Copied from awa-storages-sqlite.sql*/
/* File generated automatically by dynamo */
/* The uri member holds the URI if the storage type is URL.

When storage is FILE, the local file path is built by using
the workspace identifier and the storage identifier. */
CREATE TABLE IF NOT EXISTS awa_storage (
  /* the storage type which defines where the content is stored */
  `storage` TINYINT NOT NULL,
  /* the storage creation date */
  `create_date` DATETIME NOT NULL,
  /* the file name */
  `name` VARCHAR(255) NOT NULL,
  /* the file size */
  `file_size` INTEGER NOT NULL,
  /* the mime type */
  `mime_type` VARCHAR(255) NOT NULL,
  /* the storage URI */
  `uri` VARCHAR(255) NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the storage identifier */
  `id` BIGINT NOT NULL,
  /* whether the document is public or not. */
  `is_public` TINYINT NOT NULL,
  /*  */
  `original_id` BIGINT ,
  /*  */
  `store_data_id` BIGINT ,
  /*  */
  `owner_id` BIGINT NOT NULL,
  /*  */
  `workspace_id` BIGINT NOT NULL,
  /*  */
  `folder_id` BIGINT ,
  PRIMARY KEY (`id`)
);
/* The storage data is created only if the storage type
is set to DATABASE.  It holds the file content in the blob. */
CREATE TABLE IF NOT EXISTS awa_storage_data (
  /* the storage data identifier */
  `id` BIGINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the storage content */
  `data` LONGBLOB NOT NULL,
  PRIMARY KEY (`id`)
);
/*  */
CREATE TABLE IF NOT EXISTS awa_storage_folder (
  /* the storage folder identifier */
  `id` BIGINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the folder creation date */
  `create_date` DATETIME NOT NULL,
  /*  */
  `name` VARCHAR(255) NOT NULL,
  /*  */
  `workspace_id` BIGINT NOT NULL,
  /*  */
  `owner_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
/* The local store record is created when a copy of the data is needed on the local file system.
The creation date refers to the date when the data was copied to the local file system.
The expiration date indicates a date after which the local file can be removed
from the local file system. */
CREATE TABLE IF NOT EXISTS awa_store_local (
  /* the local store identifier */
  `id` BIGINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /*  */
  `store_version` INTEGER NOT NULL,
  /* the shared flag which indicates whether this local store can be shared by several clients. */
  `shared` TINYINT NOT NULL,
  /* the local store path */
  `path` VARCHAR(255) NOT NULL,
  /* the local store expiration date */
  `expire_date` DATE NOT NULL,
  /* the creation date */
  `create_date` DATETIME NOT NULL,
  /*  */
  `storage_id` BIGINT ,
  PRIMARY KEY (`id`)
);
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_storage");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_storage_data");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_storage_folder");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_store_local");
INSERT OR IGNORE INTO ado_version (name, version) VALUES ("awa-storages", 1);
/* Copied from awa-images-sqlite.sql*/
/* File generated automatically by dynamo */
/* - The workspace contains one or several folders.
- Each image folder contains a set of images that have been uploaded by the user.
- An image can be visible if a user has an ACL permission to read the associated folder.
- An image marked as 'public=True' can be visible by anybody */
CREATE TABLE IF NOT EXISTS awa_image (
  /* the image identifier */
  `id` BIGINT NOT NULL,
  /* the image width */
  `width` INTEGER NOT NULL,
  /* the image height */
  `height` INTEGER NOT NULL,
  /* the thumbnail width */
  `thumb_width` INTEGER NOT NULL,
  /* the thumbnail height */
  `thumb_height` INTEGER NOT NULL,
  /*  */
  `path` VARCHAR(255) NOT NULL,
  /*  */
  `public` TINYINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the thumbnail storage */
  `thumbnail_id` BIGINT ,
  /* the folder where the image is stored */
  `folder_id` BIGINT NOT NULL,
  /* the user who uploaded the image */
  `owner_id` BIGINT NOT NULL,
  /* the image storage */
  `storage_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_image");
INSERT OR IGNORE INTO ado_version (name, version) VALUES ("awa-images", 1);
/* Copied from awa-wikis-sqlite.sql*/
/* File generated automatically by dynamo */
/*  */
CREATE TABLE IF NOT EXISTS awa_wiki_content (
  /* the wiki page content identifier */
  `id` BIGINT NOT NULL,
  /* the wiki content creation date */
  `create_date` DATETIME NOT NULL,
  /* the wiki text content */
  `content` TEXT NOT NULL,
  /* the format type used used by the wiki content */
  `format` TINYINT NOT NULL,
  /* the content comment string */
  `save_comment` VARCHAR(255) NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the wiki page version */
  `page_version` INTEGER NOT NULL,
  /* the wiki page that this Wiki_Content belongs to */
  `page_id` BIGINT NOT NULL,
  /* the page version author */
  `author_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
/* The wiki page represents a page with its versions.
It refers to the last version which is currently visible.
It has an optional preview image which defines
the thumbnail preview of the last/current wiki content. */
CREATE TABLE IF NOT EXISTS awa_wiki_page (
  /* the wiki page identifier */
  `id` BIGINT NOT NULL,
  /* the wiki page name */
  `name` VARCHAR(255) NOT NULL,
  /* the last page version number */
  `last_version` INTEGER NOT NULL,
  /* whether the wiki page is public */
  `is_public` TINYINT NOT NULL,
  /* the page title */
  `title` VARCHAR(255) NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* a read counter which indicates how many times the page was read. */
  `read_count` INTEGER NOT NULL,
  /* the wiki page preview. */
  `preview_id` BIGINT ,
  /* the wiki space that this page belongs to */
  `wiki_id` BIGINT NOT NULL,
  /* the current content (or last version) */
  `content_id` BIGINT ,
  PRIMARY KEY (`id`)
);
/* Permission is granted to display a wiki page if there is
an ACL entry between the wiki space and the user. */
CREATE TABLE IF NOT EXISTS awa_wiki_space (
  /* the wiki space identifier */
  `id` BIGINT NOT NULL,
  /* the wiki name */
  `name` VARCHAR(255) NOT NULL,
  /* whether the wiki is public */
  `is_public` TINYINT NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the wiki creation date. */
  `create_date` DATETIME NOT NULL,
  /* the left panel side wiki text for every page. */
  `left_side` TEXT NOT NULL,
  /* the right panel wiki text for every page. */
  `right_side` TEXT NOT NULL,
  /* the default wiki page format. */
  `format` TINYINT NOT NULL,
  /*  */
  `workspace_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_wiki_content");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_wiki_page");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_wiki_space");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_wiki_page"), "name");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_wiki_page"), "last_version");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_wiki_page"), "is_public");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_wiki_page"), "title");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_wiki_space"), "name");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_wiki_space"), "is_public");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_wiki_space"), "format");
INSERT OR IGNORE INTO ado_version (name, version) VALUES ("awa-wikis", 1);
/* Copied from awa-blogs-sqlite.sql*/
/* File generated automatically by dynamo */
/*  */
CREATE TABLE IF NOT EXISTS awa_blog (
  /* the blog identifier */
  `id` BIGINT NOT NULL,
  /* the blog name */
  `name` VARCHAR(255) NOT NULL,
  /* the version */
  `version` INTEGER NOT NULL,
  /* the blog uuid */
  `uid` VARCHAR(255) NOT NULL,
  /* the blog creation date */
  `create_date` DATETIME NOT NULL,
  /* the date when the blog was updated */
  `update_date` DATETIME NOT NULL,
  /* The blog base URL. */
  `url` VARCHAR(255) NOT NULL,
  /* the default post format. */
  `format` TINYINT NOT NULL,
  /* the default image URL to be used */
  `default_image_url` VARCHAR(255) NOT NULL,
  /* the workspace that this blog belongs to */
  `workspace_id` BIGINT NOT NULL,
  PRIMARY KEY (`id`)
);
/*  */
CREATE TABLE IF NOT EXISTS awa_post (
  /* the post identifier */
  `id` BIGINT NOT NULL,
  /* the post title */
  `title` VARCHAR(255) NOT NULL,
  /* the post text content */
  `text` TEXT NOT NULL,
  /* the post creation date */
  `create_date` DATETIME NOT NULL,
  /* the post URI */
  `uri` VARCHAR(255) NOT NULL,
  /*  */
  `version` INTEGER NOT NULL,
  /* the post publication date */
  `publish_date` DATETIME ,
  /* the post status */
  `status` TINYINT NOT NULL,
  /*  */
  `allow_comments` TINYINT NOT NULL,
  /* the number of times the post was read. */
  `read_count` INTEGER NOT NULL,
  /* the post summary. */
  `summary` VARCHAR(4096) NOT NULL,
  /* the blog post format. */
  `format` TINYINT NOT NULL,
  /*  */
  `author_id` BIGINT NOT NULL,
  /*  */
  `blog_id` BIGINT NOT NULL,
  /*  */
  `image_id` BIGINT ,
  PRIMARY KEY (`id`)
);
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_blog");
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ("awa_post");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_blog"), "name");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_blog"), "uid");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_blog"), "url");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_blog"), "format");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_blog"), "default_image_url");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_post"), "title");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_post"), "uri");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_post"), "publish_date");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_post"), "status");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_post"), "allow_comments");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_post"), "summary");
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = "awa_post"), "format");
INSERT OR IGNORE INTO ado_version (name, version) VALUES ("awa-blogs", 1);
/* Copied from adafr-sqlite.sql*/
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
  `first_name` VARCHAR(255) NOT NULL,
  /* the member's last name. */
  `last_name` VARCHAR(255) NOT NULL,
  /* the optional member's company name. */
  `company` VARCHAR(255) NOT NULL,
  /* first adress field. */
  `address1` VARCHAR(255) NOT NULL,
  /* second address field. */
  `address2` VARCHAR(255) NOT NULL,
  /* third address field. */
  `address3` VARCHAR(255) NOT NULL,
  /* address postal code. */
  `postal_code` VARCHAR(255) NOT NULL,
  /* address tiown. */
  `city` VARCHAR(255) NOT NULL,
  /* the country. */
  `country` VARCHAR(255) NOT NULL,
  /* the date when the member record was created. */
  `create_date` DATETIME NOT NULL,
  /* the date when the member's email was validated. */
  `mail_verify_date` DATETIME ,
  /* the date when the payment was received. */
  `payment_date` DATETIME ,
  /*  */
  `status` TINYINT NOT NULL,
  /* the membership type asked by the member. */
  `membership` TINYINT NOT NULL,
  /* secure key salt. */
  `salt` VARCHAR(255) NOT NULL,
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
);
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
);
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('adafr_member');
INSERT OR IGNORE INTO ado_entity_type (name) VALUES ('adafr_receipt');
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'adafr_member'), 'first_name');
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'adafr_member'), 'last_name');
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'adafr_member'), 'company');
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'adafr_member'), 'address1');
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'adafr_member'), 'address2');
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'adafr_member'), 'address3');
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'adafr_member'), 'postal_code');
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'adafr_member'), 'city');
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'adafr_member'), 'country');
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'adafr_member'), 'mail_verify_date');
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'adafr_member'), 'payment_date');
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'adafr_member'), 'status');
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'adafr_member'), 'membership');
INSERT OR IGNORE INTO awa_audit_field (entity_type, name)
  VALUES ((SELECT id FROM ado_entity_type WHERE name = 'adafr_member'), 'amount');
INSERT OR IGNORE INTO ado_version (name, version) VALUES ('adafr', 2);
/* Copied from adafr-init-sqlite.sql*/
/* Setup permission */
INSERT INTO `awa_permission` VALUES (1,'url');
INSERT INTO `awa_permission` VALUES (2,'blog-create');
INSERT INTO `awa_permission` VALUES (3,'blog-delete');
INSERT INTO `awa_permission` VALUES (4,'blog-create-post');
INSERT INTO `awa_permission` VALUES (5,'blog-delete-post');
INSERT INTO `awa_permission` VALUES (6,'blog-update-post');
INSERT INTO `awa_permission` VALUES (7,'storage-create');
INSERT INTO `awa_permission` VALUES (8,'storage-delete');
INSERT INTO `awa_permission` VALUES (9,'folder-create');
INSERT INTO `awa_permission` VALUES (10,'workspace-create');
INSERT INTO `awa_permission` VALUES (11,'wiki-page-create');
INSERT INTO `awa_permission` VALUES (12,'wiki-page-delete');
INSERT INTO `awa_permission` VALUES (13,'wiki-page-update');
INSERT INTO `awa_permission` VALUES (14,'wiki-page-view');
INSERT INTO `awa_permission` VALUES (15,'wiki-space-create');
INSERT INTO `awa_permission` VALUES (16,'wiki-space-delete');
INSERT INTO `awa_permission` VALUES (17,'wiki-space-update');
INSERT INTO `awa_permission` VALUES (18,'anonymous');
INSERT INTO `awa_permission` VALUES (19,'logged-user');
INSERT INTO `awa_permission` VALUES (20,'workspaces-create');
INSERT INTO `awa_permission` VALUES (21,'blog-add-comment');
INSERT INTO `awa_permission` VALUES (22,'blog-publish-comment');
INSERT INTO `awa_permission` VALUES (23,'blog-delete-comment');
INSERT INTO `awa_permission` VALUES (24,'workspace-invite-user');
INSERT INTO `awa_permission` VALUES (25,'workspace-delete-user');
INSERT INTO `awa_permission` VALUES (26,'member-view');
INSERT INTO `awa_permission` VALUES (27,'member-update');
INSERT INTO `awa_permission` VALUES (28,'member-create');

/* Setup ACL for super admin */
INSERT INTO `awa_acl` VALUES (1,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_workspace'),2);
INSERT INTO `awa_acl` VALUES (2,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_workspace'),7);
INSERT INTO `awa_acl` VALUES (3,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_workspace'),8);
INSERT INTO `awa_acl` VALUES (4,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_workspace'),9);
INSERT INTO `awa_acl` VALUES (5,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_workspace'),15);
INSERT INTO `awa_acl` VALUES (8,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_blog'),3);
INSERT INTO `awa_acl` VALUES (9,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_blog'),4);
INSERT INTO `awa_acl` VALUES (10,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_blog'),5);
INSERT INTO `awa_acl` VALUES (11,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_blog'),6);
INSERT INTO `awa_acl` VALUES (12,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_blog'),21);
INSERT INTO `awa_acl` VALUES (13,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_blog'),22);
INSERT INTO `awa_acl` VALUES (14,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_blog'),23);
INSERT INTO `awa_acl` VALUES (15,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_wiki_space'),11);
INSERT INTO `awa_acl` VALUES (16,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_wiki_space'),12);
INSERT INTO `awa_acl` VALUES (17,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_wiki_space'),13);
INSERT INTO `awa_acl` VALUES (18,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_wiki_space'),14);
INSERT INTO `awa_acl` VALUES (19,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_wiki_space'),16);
INSERT INTO `awa_acl` VALUES (20,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_wiki_space'),17);
INSERT INTO `awa_acl` VALUES (21,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_workspace'),24);
INSERT INTO `awa_acl` VALUES (22,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_workspace'),25);
INSERT INTO `awa_acl` VALUES (23,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_workspace'),26);
INSERT INTO `awa_acl` VALUES (24,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_workspace'),27);
INSERT INTO `awa_acl` VALUES (25,1,0,1,1,(SELECT id from ado_entity_type where name = 'awa_workspace'),28);

INSERT INTO `ado_sequence` VALUES ('awa_acl',1,101,100);
INSERT INTO `ado_sequence` VALUES ('awa_blog',1,101,100);
INSERT INTO `ado_sequence` VALUES ('awa_email',1,101,100);
INSERT INTO `ado_sequence` VALUES ('awa_user',1,101,100);
INSERT INTO `ado_sequence` VALUES ('awa_wiki_space',1,101,100);
INSERT INTO `ado_sequence` VALUES ('awa_workspace',1,601,100);
INSERT INTO `ado_sequence` VALUES ('awa_workspace_member',1,601,100);

INSERT INTO `awa_blog` VALUES (1,'Ada France',1,'8628f625c0602f47396ea8013da3411674243a11','2017-05-17 16:16:10','1901-01-01 23:00:00','https://www.ada-france.org',1,'/adafr/images/AdaFrance-small.png',1);
INSERT INTO `awa_wiki_space` VALUES (1,'Ada France',0,3,'2017-05-17 16:12:13','','',4,1);
INSERT INTO `awa_workspace` VALUES (1,1,'2017-05-17 16:12:04',1);
INSERT INTO `awa_workspace_member` VALUES (1,'2017-05-17 16:12:04','Admin',1,1);
INSERT INTO `awa_email` VALUES ('admin@ada-france.org', 0, '1901-01-01 23:00:00.00', 1, 1, 1);
INSERT INTO `awa_user` VALUES('Admin', '', '', 'Admin', 1, 1, 1, 1);
INSERT INTO `awa_authenticate` VALUES(1, 1, 'admin@ada-france.org', '3CJkIMsK/TSSTpVnnwUf1irOQH4=', 'qPnDIWMGFg2IVCZAL8dRvvYbcuM=', 0, 1, 1);
