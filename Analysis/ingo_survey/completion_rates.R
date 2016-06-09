#' ---
#' title: "Survey completion rates"
#' author: "Andrew Heiss"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' output: 
#'   html_document: 
#'     css: ../html/fixes.css
#'     toc: yes
#'     highlight: pygments
#'     theme: cosmo
#'     includes:
#'       after_body: ../html/jump.html
#' bibliography: /Users/andrew/Dropbox/Readings/Papers.bib
#' csl: /Users/andrew/.pandoc/csl/american-political-science-association.csl
#' ...

#' ## Load and process all data
#+ message=FALSE
library(dplyr)
library(tidyr)
library(stringr)
library(yaml)
library(purrr)
library(pander)
library(scales)

panderOptions('table.split.table', Inf)
panderOptions('table.split.cells', Inf)
panderOptions('keep.line.breaks', TRUE)
panderOptions('table.style', 'multiline')
panderOptions('table.alignment.default', 'left')

# Load YAML metadata for survey lists
raw.lists <- yaml.load_file(file.path(PROJHOME, "data", "data_raw",
                                      "NGO lists", "ngo_lists.yml"),
                            as.named.list=TRUE)

# Convert to nice dataframe with purrr::map_df()
list.details <- seq(1:length(raw.lists$lists)) %>%
  map_df(function(x) raw.lists$lists[[x]][c("title", 'name',
                                            "num_rows_raw", "description")]) %>%
  arrange(desc(num_rows_raw))

#' Load data from tracking database
db.email <- src_sqlite(path=file.path(PROJHOME, "Data", "Survey", "list", 
                                      "final_list.db"))
email.full <- tbl(db.email, "full_list") %>% collect() %>%
  separate(id_org, c("db", "id.in.db"))
removed <- tbl(db.email, "remove") %>% collect()
bounced.raw <- tbl(db.email, "bounces") %>% collect()
completed <- tbl(db.email, "survey_completed") %>% collect()

email.by.db <- email.full %>%
  group_by(db) %>%
  summarise(num.apparently.valid = n())


#' Dead addresses, domains, and bounces
dead.searches <- paste(c("Hippo", "Invalid", "Dead", "weird opt"), collapse="|")
dead <- removed %>%
  filter(str_detect(remove_notes, regex(dead.searches, ignore_case=TRUE))) %>%
  select(fk_org, notes = remove_notes)

bounced <- bounced.raw %>%
  gather(notes, value, hard_bounce, email_dead, email_bounce) %>%
  filter(value != 0) %>%
  select(fk_org, notes)
  
dead.and.bounced <- bind_rows(dead, bounced) %>%
  group_by(fk_org) %>%
  slice(1) %>%  # Get rid of duplicate removal entries
  ungroup() %>%
  left_join(select(email.full, fk_org = index_org, db), by="fk_org")

dead.and.bounced.by.db <- dead.and.bounced %>%
  group_by(db) %>%
  summarise(num.dead.bounced = n())

#' Summarize response rates from each database
response.summary <- list.details %>%
  left_join(email.by.db, by=c("name" = "db")) %>%
  left_join(dead.and.bounced.by.db, by=c("name" = "db")) %>%
  mutate(num.invited = num.apparently.valid - num.dead.bounced) %>%
  select(-c(description, name))

response.summary.total <- response.summary %>%
  summarise_each(funs(sum), -title) %>%
  mutate(title = "**Total**")

response.summary.with.total <- bind_rows(response.summary,
                                         response.summary.total) %>%
  mutate(perc.valid = num.apparently.valid / num_rows_raw,
         perc.bounced.from.valid = num.dead.bounced / num.apparently.valid,
         perc.invited.from.raw = num.invited / num_rows_raw,
         perc.invited.from.valid = num.invited / num.apparently.valid)

response.summary.display <- response.summary.with.total %>%
  mutate_each(funs(comma), starts_with("num")) %>%
  mutate_each(funs(percent), starts_with("perc"))


#' ## From raw lists to workable, usable lists
#' 
#' Full technical details of how I ran the survey are available at my [research
#' notebook](https://notebook.andrewheiss.com/project/diss-ingos-in-autocracies/survey-technical-details/).
#' 
#' The complete database of NGOs to receive a survey invitation came from `r
#' length(raw.lists$lists)` different sources. After collecting the details of
#' each organization listed at each source, I cleaned the raw lists by removing
#' all organizations without valid e-mail addresses and by attempting to filter
#' out obviously domestic NGOs. I filtered out domestic NGOs either by not
#' collecting them in the first place (in the case of the Yearbook of
#' International Organizations), or using information from the database to
#' identify them. For example, the UN iCSCO database includes a field for an
#' organization's geographic scope: local, national, regional, and
#' international. I omitted local and national.
#' 
#' I filtered out invalid e-mail addresses using Email Hippo, which pings each
#' address to verify (1) that the domain exists, and (2) that the address
#' exists at the domain.
#' 
#+ echo=FALSE
# Cool use of DT::datatable: https://raw.githubusercontent.com/BuzzFeedNews/2016-04-federal-surveillance-planes/master/analysis.Rmd

#+ results="asis"
pandoc.table(response.summary.display)


#' ## Absorption rate
#' 
#' > The absorption rate [measures] the ability of the survey company to manage
#' and keep up-to-date their database of email addresses and communications
#' with panel members [@CallegaroDiSogra:2008, 1026].
#' 
#' $$
#' \frac{EI - BB - NET}{EI}
#' $$
#' 

EI <- response.summary.total$num.invited
BB.NET <- response.summary.total$num.dead.bounced

absorption.rate <- (EI - BB.NET) / EI

#' - EI = e-mail invitations sent (`r comma(EI)`)
#' - BB = bounced (`r comma(BB.NET)`)
#' - NET = network undeliverable (included in `BB`)
#' - **Absorption rate:** `r percent(absorption.rate)`
#' 


#' ## Break-off rate
#' 
#' > The break-off rate is a possible indicator of problems in the design of
#' the questionnaire (e.g., too long, boring…) or struggle with technical
#' problems during the survey administration (e.g., streaming media or
#' animations that may “break” a survey at some point) 
#' [@CallegaroDiSogra:2008, 1026].
#' 
#' $$
#' \frac{BO}{I + P + BO}
#' $$
#' 
#' - BO = number of surveys broken off by my definition (i.e. incomplete and 
#'   not partial)
#' - I = complete
#' - P = partial
#' 


#' ## Completion rate (participation rate)
#'
#' > The most intuitive response metric is the survey's completion rate. It is
#' also the one metric most often mislabeled as a response rate. The completion
#' rate is the proportion of those who completed the Web survey among all the
#' eligible panel members who were invited to take the survey
#' [@CallegaroDiSogra:2008, 1021-22].
#'
#' > Using such a rate as an indicator of possible nonresponse error makes
#' little sense; however, the participation rate may serve as a useful
#' indicator of panel efficiency [@AAPOR:2016, 49].
#' 
#' $$
#' \frac{I + P}{(I + P) + (R + NC + O)}
#' $$
#' 
#' - I = complete
#' - P = partial
#' - R = refusal and break off
#' - NC = non-contact
#' - O = other
#' 


#' ## Study-specific screening completion rates
#' 
#' > Study-specific screening completion rates and eligibility rates measure
#' the incidence of a particular phenomenon among panel members. When these
#' rates are significantly different from an external "gold standard," they may
#' indicate issues of question wording in the screener module or respondents
#' purposively self-selecting themselves for a particular study (e.g., to gain
#' rewards) even if they do not really qualify.… These rates may also reveal a
#' skew in the panel membership along a particular dimension that may raise
#' concerns regarding bias [@CallegaroDiSogra:2008, 1026].
#' 
#' $$
#' \frac{SCQ + SCNQ}{INV}
#' $$
#' 
#' - SCQ = screening completed and qualified
#' - SCNQ = screening completed and not qualified (i.e. screened out)
#' - INV = survey invitations sent out
#' 


#' ## Study-specific eligibility rate
#' 
#' > The problem with a screening rate is that nonresponse is confounded with
#' the screening. In fact, we do not know if a person qualifies unless they
#' provide that information by answering the screening questions. For this
#' reason, we talk about screening completion rate and not screening rate
#' [@CallegaroDiSogra:2008, 1023].
#' 
#' $$
#' \frac{SCQ}{SCQ - SCNQ}
#' $$
#' 
#' - SCQ = screening completed and qualified
#' - SCNQ = screening completed and not qualified (i.e. screened out)
#' 


#' ## Other details
#' 
#' > In addition to these rates, we also believe that it is the best practice
#' to report the length of the field period with its start and close dates, the
#' number of reminders sent and their form (email, letter, IVR call, or
#' personal call), and the use of any incentive [@CallegaroDiSogra:2008, 1026].
#' 

#' # References
#' 
