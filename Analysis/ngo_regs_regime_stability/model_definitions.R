# Frequentist versions
lna.AFI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.stability +
       icrg.pol.risk_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.AGI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.stability +
       coups.activity.bin_sum_nb +
       protests.violent.std_wt + protests.nonviolent.std_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.AHI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.stability +
       icrg.pol.risk_wt +
       coups.activity.bin_sum_nb +
       protests.violent.std_wt + protests.nonviolent.std_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.BFI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.pol.risk.internal.scaled +
       icrg.pol.risk_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.BGI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.pol.risk.internal.scaled +
       coups.activity.bin_sum_nb +
       protests.violent.std_wt + protests.nonviolent.std_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.BHI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.pol.risk.internal.scaled +
       icrg.pol.risk_wt +
       coups.activity.bin_sum_nb +
       protests.violent.std_wt + protests.nonviolent.std_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.CFI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.stability +
       yrsoffc + years.since.comp + opp1vote +
       icrg.pol.risk_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.CGI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.stability +
       yrsoffc + years.since.comp + opp1vote +
       coups.activity.bin_sum_nb +
       protests.violent.std_wt + protests.nonviolent.std_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.CHI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.stability +
       yrsoffc + years.since.comp + opp1vote +
       icrg.pol.risk_wt +
       coups.activity.bin_sum_nb +
       protests.violent.std_wt + protests.nonviolent.std_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.DFI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.pol.risk.internal.scaled +
       yrsoffc + years.since.comp + opp1vote +
       icrg.pol.risk_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.DGI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.pol.risk.internal.scaled +
       yrsoffc + years.since.comp + opp1vote +
       coups.activity.bin_sum_nb +
       protests.violent.std_wt + protests.nonviolent.std_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.DHI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.pol.risk.internal.scaled +
       yrsoffc + years.since.comp + opp1vote +
       icrg.pol.risk_wt +
       coups.activity.bin_sum_nb +
       protests.violent.std_wt + protests.nonviolent.std_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.EFI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.stability + icrg.pol.risk.internal.nostab +
       yrsoffc + years.since.comp + opp1vote +
       icrg.pol.risk_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.EGI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.stability + icrg.pol.risk.internal.nostab +
       yrsoffc + years.since.comp + opp1vote +
       coups.activity.bin_sum_nb +
       protests.violent.std_wt + protests.nonviolent.std_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.EHI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.stability + icrg.pol.risk.internal.nostab +
       yrsoffc + years.since.comp + opp1vote +
       icrg.pol.risk_wt +
       coups.activity.bin_sum_nb +
       protests.violent.std_wt + protests.nonviolent.std_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.JFI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.stability + icrg.pol.risk.internal.nostab +
       icrg.pol.risk_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.JGI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.stability + icrg.pol.risk.internal.nostab +
       coups.activity.bin_sum_nb +
       protests.violent.std_wt + protests.nonviolent.std_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

lna.JHI <- function(df) {
  lm(cs_env_sum.lead ~
       icrg.stability + icrg.pol.risk.internal.nostab +
       icrg.pol.risk_wt +
       coups.activity.bin_sum_nb +
       protests.violent.std_wt + protests.nonviolent.std_wt +
       shaming.states.std +
       as.factor(year.num),
     data=df)
}

# Bayesian versions
lna.AFI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.stability +
             icrg.pol.risk_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.AGI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.stability +
             coups.activity.bin_sum_nb +
             protests.violent.std_wt + protests.nonviolent.std_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.AHI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.stability +
             icrg.pol.risk_wt +
             coups.activity.bin_sum_nb +
             protests.violent.std_wt + protests.nonviolent.std_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.BFI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.pol.risk.internal.scaled +
             icrg.pol.risk_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.BGI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.pol.risk.internal.scaled +
             coups.activity.bin_sum_nb +
             protests.violent.std_wt + protests.nonviolent.std_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.BHI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.pol.risk.internal.scaled +
             icrg.pol.risk_wt +
             coups.activity.bin_sum_nb +
             protests.violent.std_wt + protests.nonviolent.std_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.CFI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.stability +
             yrsoffc + years.since.comp + opp1vote +
             icrg.pol.risk_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.CGI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.stability +
             yrsoffc + years.since.comp + opp1vote +
             coups.activity.bin_sum_nb +
             protests.violent.std_wt + protests.nonviolent.std_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.CHI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.stability +
             yrsoffc + years.since.comp + opp1vote +
             icrg.pol.risk_wt +
             coups.activity.bin_sum_nb +
             protests.violent.std_wt + protests.nonviolent.std_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.DFI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.pol.risk.internal.scaled +
             yrsoffc + years.since.comp + opp1vote +
             icrg.pol.risk_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.DGI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.pol.risk.internal.scaled +
             yrsoffc + years.since.comp + opp1vote +
             coups.activity.bin_sum_nb +
             protests.violent.std_wt + protests.nonviolent.std_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.DHI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.pol.risk.internal.scaled +
             yrsoffc + years.since.comp + opp1vote +
             icrg.pol.risk_wt +
             coups.activity.bin_sum_nb +
             protests.violent.std_wt + protests.nonviolent.std_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.EFI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.stability + icrg.pol.risk.internal.nostab +
             yrsoffc + years.since.comp + opp1vote +
             icrg.pol.risk_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.EGI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.stability + icrg.pol.risk.internal.nostab +
             yrsoffc + years.since.comp + opp1vote +
             coups.activity.bin_sum_nb +
             protests.violent.std_wt + protests.nonviolent.std_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.EHI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.stability + icrg.pol.risk.internal.nostab +
             yrsoffc + years.since.comp + opp1vote +
             icrg.pol.risk_wt +
             coups.activity.bin_sum_nb +
             protests.violent.std_wt + protests.nonviolent.std_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.JFI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.stability + icrg.pol.risk.internal.nostab +
             icrg.pol.risk_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.JGI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.stability + icrg.pol.risk.internal.nostab +
             coups.activity.bin_sum_nb +
             protests.violent.std_wt + protests.nonviolent.std_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}

lna.JHI.b <- function(df) {
  stan_glm(cs_env_sum.lead ~
             icrg.stability + icrg.pol.risk.internal.nostab +
             icrg.pol.risk_wt +
             coups.activity.bin_sum_nb +
             protests.violent.std_wt + protests.nonviolent.std_wt +
             shaming.states.std +
             as.factor(year.num),
           data=df, family=gaussian(),
           prior=cauchy(), prior_intercept=cauchy(),
           chains=CHAINS, iter=ITER, warmup=WARMUP,
           algorithm="sampling", seed=my.seed)
}
