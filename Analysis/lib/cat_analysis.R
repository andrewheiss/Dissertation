library(rstanarm)

# ------------------
# Useful functions
# ------------------
# 
# NB: xtabs() and productplots::prodplot(..., mosaic()) need to be mirror
# images of each other to get the same plot as vcd::mosaic()
# 
# Example:
#   prodplot(df, ~ x1 + x2 + x3, mosaic())
#   xtabs(~ x3 + x2 + x1)
analyze.cat.var <- function(cat.table) {
  cat.table.chi <- chisq.test(ftable(cat.table))
  
  cat("Table counts\n")
  ftable(cat.table) %>% print(method="col.compact")
  
  cat("\nExpected values\n")
  expected.values <- cat.table.chi$expected
  
  # Add nice labels if possible
  if(length(dim(cat.table)) == length(dim(expected.values))) {
    dimnames(expected.values) <- dimnames(cat.table)
  }
  
  expected.values %>% print(method="col.compact")
  
  cat("\nRow proporitions\n")
  ftable(prop.table(cat.table, margin=1)) %>% print(method="col.compact")
  
  cat("\nColumn proporitions\n")
  ftable(prop.table(cat.table, margin=2)) %>% print(method="col.compact")
  
  cat("\nChi-squared test for table\n")
  cat.table.chi %>% print()
  
  cat("Cramer's V\n")
  vcd::assocstats(ftable(cat.table))$cramer %>% print()
  
  cat("\nPearson residuals\n",
      "2 is used as critical value by convention\n", sep="")
  pearson.residuals <- cat.table.chi$residuals %>% print(method="col.compact")
  
  cat("\nComponents of chi-squared\n",
      "Critical value (0.05 with ", 
      cat.table.chi$parameter, " df) is ", 
      round(qchisq(0.95, cat.table.chi$parameter), 2), "\n", sep="")
  components <- pearson.residuals^2 %>% print(method="col.compact")
  
  cat("\np for components\n")
  round(1-pchisq(components, cat.table.chi$parameter), 3) %>% print(method="col.compact")
}

# Bayesian proportion tests!
#
# Adapted from
# - https://lingpipe-blog.com/2009/10/13/bayesian-counterpart-to-fisher-exact-test-on-contingency-tables/
# - http://www.sumsar.net/blog/2014/06/bayesian-first-aid-prop-test/
# - http://www.sumsar.net/blog/2014/01/bayesian-first-aid/
# - https://github.com/rasmusab/bayesian_first_aid (because "inside every classical test there is a Bayesian model trying to get out")
# - https://cran.r-project.org/web/packages/rstanarm/vignettes/binomial.html
#
prop.test.bayes <- function(df, group.formula) {
  # --------------------
  # Run binomial model
  # --------------------
  capture.output({
    model <- stan_glm(group.formula, data=df, family=binomial("logit"),
                      # Weakly informative prior on log-odds
                      prior_intercept=normal(-1, 1),
                      chains=CHAINS, iter=ITER, warmup=WARMUP,
                      algorithm="sampling", seed=my.seed)
  }, file="/dev/null")
  
  # ---------------------------
  # MCMC draws for each group
  # ---------------------------
  group.names <- levels(df[[attr(terms(group.formula), "term.labels")]])
  
  samples.clean <- as.data.frame(model) %>%
    # The first column is the intercept/base case. Add it to all the other columns
    mutate_at(vars(-1), funs(. + `(Intercept)`)) %>%
    # Exponentiate
    mutate_all(exp) %>%
    magrittr::set_colnames(group.names)
  
  # Summarize draws
  samples.summary <- samples.clean %>%
    gather(group.name, value) %>%
    group_by(group.name) %>%
    summarise(mean = mean(value),
              median = median(value),
              q2.5 = quantile(value, probs=0.025),
              q25 = quantile(value, probs=0.25),
              q75 = quantile(value, probs=0.75),
              q97.5 = quantile(value, probs=0.975)) %>%
    mutate(group.name = factor(group.name, levels=group.names, ordered=TRUE)) %>%
    arrange(group.name)
  
  # ----------------------------------------------
  # Differences between MCMC draws of each group
  # ----------------------------------------------
  # Calculate the pairwise differences between columns
  # Adapted from http://stackoverflow.com/a/28187446/120898
  diff.names <- outer(colnames(samples.clean), colnames(samples.clean),
                      paste, sep=" − ")
  
  diffs.to.omit <- which(lower.tri(diff.names, diag=TRUE))
  
  diffs.full <- outer(1:ncol(samples.clean), 1:ncol(samples.clean),
                      function(x, y) samples.clean[,x] - samples.clean[,y])
  
  colnames(diffs.full) <- diff.names
  
  samples.diffs <- diffs.full[-diffs.to.omit]
  
  # Summarize differences
  diffs.summary <- as.data.frame(samples.diffs) %>%
    gather(diff.group, value) %>%
    group_by(diff.group) %>%
    summarise(mean = mean(value),
              median = median(value),
              q2.5 = quantile(value, probs=0.025),
              q25 = quantile(value, probs=0.25),
              q75 = quantile(value, probs=0.75),
              q97.5 = quantile(value, probs=0.975),
              p.greater0 = mean(value > 0),
              p.less0 = mean(value < 0))
  
  # ----------------
  # Generate plots
  # ----------------
  # Plot posterior medians
  plot.groups <- samples.clean %>%
    gather(group.name, value) %>%
    mutate(group.name = factor(group.name, levels=group.names, ordered=TRUE)) %>%
    ggplot(data=., aes(x=value, fill=group.name)) +
    geom_density(colour=NA, alpha=0.4) + 
    geom_segment(data=samples.summary, aes(x=q2.5, xend=q97.5, y=0, yend=0, 
                                           colour=group.name),
                 size=3, alpha=0.8) +
    geom_vline(data=samples.summary, aes(xintercept=median, colour=group.name),
               size=0.5) +
    scale_x_continuous(labels=scales::percent) +
    scale_fill_viridis(discrete=TRUE, option="plasma") +
    scale_color_viridis(discrete=TRUE, option="plasma") +
    labs(x="Proportion", y="Density",
         title="Group distributions") +
    guides(fill=guide_legend(title=NULL), colour="none") +
    theme_ath_density() + 
    # Legend in the plot for cbind.gtable
    theme(legend.position=c(1, 1), legend.justification=c(1,1))
  
  # Plot differences
  diffs.summary.plot <- diffs.summary %>%
    mutate(diff.group1 = diff.group) %>%
    separate(diff.group1, into=c("group1", "group2"), sep=" − ") %>%
    mutate(diff.group = ifelse(nchar(diff.group) > 25,
                               str_replace(diff.group, "−", "−\n"),
                               diff.group))
  
  plot.diffs <- samples.diffs %>%
    gather(diff.group, value) %>%
    mutate(diff.group1 = diff.group) %>%
    separate(diff.group1, into=c("group1", "group2"), sep=" − ") %>%
    mutate(diff.group = ifelse(nchar(diff.group) > 25,
                               str_replace(diff.group, "−", "−\n"),
                               diff.group)) %>%
    ggplot(data=., aes(x=value, fill=group1)) + 
    geom_density(colour=NA, alpha=0.4) + 
    geom_segment(data=diffs.summary.plot, aes(x=q2.5, xend=q97.5, y=0, yend=0, 
                                              colour=group1),
                 size=3) +
    geom_vline(data=diffs.summary.plot, aes(xintercept=median), size=0.25) +
    geom_vline(xintercept=0, linetype="dotted") +
    scale_x_continuous(labels=scales::percent) +
    scale_fill_brewer(palette="Set1") +
    scale_color_brewer(palette="Set1") +
    guides(fill="none", colour="none") +
    labs(x="Difference in proportion",
         title="Differences between group distributions") +
    theme_ath_density() +
    facet_wrap(~ diff.group, scales="free_x")
  plot.diffs
  
  # -----------------------------
  # Return everything as a list
  # -----------------------------
  output <- list(samples=samples.clean, samples.summary=samples.summary,
                 diffs=samples.diffs, diffs.summary=diffs.summary,
                 plot.groups=plot.groups, plot.diffs=plot.diffs, model=model)
  return(output)
}
