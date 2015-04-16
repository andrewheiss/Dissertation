library(pander)

thing1 <- read.csv("case_selection.csv")
colnames(thing1)[1] <- "&nbsp;"

cat(pandoc.table.return(thing1, split.tables=Inf, style="multiline", 
                        caption="Example mix of INGO cases {#tbl:case_mix}"), 
    file="case_selection.md")


thing <- read.csv("methods.csv")

cat(pandoc.table.return(thing, split.tables=Inf, style="multiline", 
                        caption="Data and methods for each hypothesis {#tbl:methods}",
                        justify="left"), 
    file="methods.md")
