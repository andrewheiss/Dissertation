library(dplyr)
library(readODS)
library(pander)

panderOptions('table.split.table', Inf)
panderOptions('table.split.cells', 50)
panderOptions('table.style', 'multiline')
panderOptions('table.alignment.default', 'left')

egypt <- read_ods(file.path(PROJHOME, "Data", "data_base",
                            "restrictions.ods"), sheet="De jure restrictions",
                  col_names=TRUE) %>%
  select(Year=year, Law=short_name) %>%
  arrange(Year)

caption <- "INGO-related legislation and regulations in Egypt {#tbl:egypt_laws}"
cat(pandoc.table.return(egypt, caption=caption), 
    file=file.path(PROJHOME, "Output", "tables", "egypt_legislation.md"))
