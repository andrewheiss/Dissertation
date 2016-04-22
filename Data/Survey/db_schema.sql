-- Enable foreign keys
PRAGMA foreign_keys = ON;

-- Create tables
CREATE TABLE IF NOT EXISTS "full_list" (
  "index_org" integer PRIMARY KEY AUTOINCREMENT NOT NULL,
  "id_org" text COLLATE NOCASE NOT NULL,
  "org_name" text COLLATE NOCASE,
  "email" text COLLATE NOCASE,
  "org_url" text COLLATE NOCASE,
  "country_hq" text COLLATE NOCASE,
  "contact" text COLLATE NOCASE,
  "phone" text COLLATE NOCASE,
  "year_founded" text COLLATE NOCASE,
  "subject_dir" text COLLATE NOCASE,
  "url_id" text COLLATE NOCASE,
  "original_list" text COLLATE NOCASE,
  "country_work" text COLLATE NOCASE,
  "geographic_scope" text COLLATE NOCASE,
  "email_type" text COLLATE NOCASE,
  "country_hq_iso3" text COLLATE NOCASE,
  "org_name_arabic" text COLLATE NOCASE,
  "org_name_email" text COLLATE NOCASE,
  "group" text COLLATE NOCASE,
  FOREIGN KEY ('group') REFERENCES "groups" (id_group)
);

CREATE TABLE IF NOT EXISTS "groups" (
  "id_group" integer PRIMARY KEY AUTOINCREMENT NOT NULL,
  "email_invitation" date NOT NULL DEFAULT(CURRENT_TIMESTAMP),
  "email_reminder" date,
  "notes" text COLLATE NOCASE
);

CREATE TABLE IF NOT EXISTS "bounces" (
  "id_bounce" integer PRIMARY KEY AUTOINCREMENT NOT NULL,
  "fk_org" integer NOT NULL,
  "hard_bounce" boolean DEFAULT(0),
  "email_dead" boolean DEFAULT(0),
  "email_bounce" boolean DEFAULT(0),
  FOREIGN KEY (fk_org) REFERENCES "full_list" (index_org) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "survey_completed" (
  "id_survey" integer PRIMARY KEY AUTOINCREMENT NOT NULL,
  "fk_org" integer NOT NULL,
  "completed" boolean DEFAULT(0),
  "qualtrics_id" text COLLATE NOCASE,
  FOREIGN KEY (fk_org) REFERENCES "full_list" (index_org) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "remove" (
  "id_removal" integer PRIMARY KEY AUTOINCREMENT NOT NULL,
  "fk_org" integer NOT NULL,
  "remove" boolean DEFAULT(0),
  "remove_notes" text COLLATE NOCASE,
  FOREIGN KEY (fk_org) REFERENCES "full_list" (index_org) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "contacts" (
  "id_contact" integer PRIMARY KEY AUTOINCREMENT NOT NULL,
  "fk_org" integer NOT NULL,
  "contact_inserted" date NOT NULL DEFAULT(CURRENT_TIMESTAMP),
  "contact_date" date NOT NULL,
  "contact_notes" text COLLATE NOCASE,
  FOREIGN KEY (fk_org) REFERENCES "full_list" (index_org) ON DELETE CASCADE
);

-- Create views
CREATE VIEW search_list AS
SELECT index_org, id_org, "group", org_name_email, email, country_hq, org_url
FROM full_list
ORDER BY "group", index_org
