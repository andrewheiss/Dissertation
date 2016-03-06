# Enable foreign keys
PRAGMA foreign_keys = ON;

# Enable foreign key support in the ODBC connection by adding this key:value
# pair in the DSN configuration:
#
# FKSupport: true
#
# See other options at http://ch-werner.de/sqliteodbc/html/index.html

# Additionally, LibreOffice will complain about parameter errors when using
# subforms linked to tables that use foreign keys. To fix these errors, go to
# Edit > Database > Advanced Settings… and make sure "Replace named parameters
# with ?" is checked

CREATE TABLE IF NOT EXISTS "organizations" (
  "id_org" integer PRIMARY KEY AUTOINCREMENT NOT NULL,
  "org_inserted" date NOT NULL DEFAULT(CURRENT_TIMESTAMP),
  "org_name" text COLLATE NOCASE NOT NULL,
  "org_acronym" text COLLATE NOCASE,
  "org_country_hq" text COLLATE NOCASE,
  "org_country_hq_iso3" text COLLATE NOCASE,
  "org_country_targets" text COLLATE NOCASE,
  "org_address" text COLLATE NOCASE,
  "org_phone" text COLLATE NOCASE,
  "org_email" text COLLATE NOCASE,
  "org_url" text COLLATE NOCASE,
  "org_issue" text COLLATE NOCASE,
  "org_description" text COLLATE NOCASE,
  "org_other" text COLLATE NOCASE,
  "org_notes" text COLLATE NOCASE
);

CREATE TABLE IF NOT EXISTS "people" (
  "id_person" integer PRIMARY KEY AUTOINCREMENT NOT NULL,
  "fk_org" integer NOT NULL,
  "person_inserted" date NOT NULL DEFAULT(CURRENT_TIMESTAMP),
  "person_name" text COLLATE NOCASE NOT NULL,
  "person_position" text COLLATE NOCASE,
  "person_email" text COLLATE NOCASE,
  "person_phone" text COLLATE NOCASE,
  "person_url" text COLLATE NOCASE,
  "person_other" text COLLATE NOCASE,
  "person_notes" text COLLATE NOCASE,
  "person_address" text COLLATE NOCASE,
  FOREIGN KEY (fk_org) REFERENCES "organizations" (id_org) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "interviews" (
  "id_intv" integer PRIMARY KEY AUTOINCREMENT NOT NULL,
  "fk_person" integer NOT NULL,
  "intv_date_inserted" date NOT NULL DEFAULT(CURRENT_TIMESTAMP),
  "intv_date_conducted" date NOT NULL,
  "intv_time_conducted" time,
  "intv_location" text COLLATE NOCASE,
  "intv_questions" text COLLATE NOCASE,
  "intv_notes" text COLLATE NOCASE,
  "intv_summary" text COLLATE NOCASE,
  "intv_other" text COLLATE NOCASE,
  "intv_followed_up" boolean DEFAULT(0),
  FOREIGN KEY (fk_person) REFERENCES "people" (id_person) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "org_contact" (
  "id_org_contact" integer PRIMARY KEY AUTOINCREMENT NOT NULL,
  "org_contact_inserted" date NOT NULL DEFAULT(CURRENT_TIMESTAMP),
  "org_contact_date" date NOT NULL,
  "fk_org" integer NOT NULL,
  "org_contact_notes" text,
  FOREIGN KEY (fk_org) REFERENCES "organizations" (id_org) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "person_contact" (
  "id_person_contact" integer PRIMARY KEY AUTOINCREMENT NOT NULL,
  "person_contact_inserted" date NOT NULL DEFAULT(CURRENT_TIMESTAMP),
  "person_contact_date" date NOT NULL,
  "fk_person" integer NOT NULL,
  "person_contact_notes" text,
  FOREIGN KEY (fk_person) REFERENCES "people" (id_person) ON DELETE CASCADE
);
