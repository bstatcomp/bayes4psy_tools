# libs
library(bayes4psy)
library(ggplot2)

## load data ------------------------------------------------------------------
data <- read.table("../examples/data/stroop_simple.csv", sep="\t", header=TRUE)

## ttest fits -----------------------------------------------------------------
# priors
mu_prior <- b_prior(family="normal", pars=c(75, 50))
sigma_prior <- b_prior(family="uniform", pars=c(0, 200))

# attach priors to relevant parameters
priors <- list(c("mu", mu_prior),
               c("sigma", sigma_prior))


# fit
fit_reading_neutral <- b_ttest(data$reading_neutral,
                               priors=priors, iter=5000, warmup=500)
fit_reading_incongruent <- b_ttest(data$reading_incongruent,
                                   priors=priors, iter=5000, warmup=500)
fit_naming_neutral <- b_ttest(data$naming_neutral,
                              priors=priors, iter=5000, warmup=500)
fit_naming_incongruent <- b_ttest(data$naming_incongruent,
                                  priors=priors, iter=5000, warmup=500)

# plot trace
plot_trace(fit_reading_neutral)
plot_trace(fit_reading_incongruent)
plot_trace(fit_naming_neutral)
plot_trace(fit_naming_incongruent)

# check fit (Rhat and n_eff)
print(fit_reading_neutral)
print(fit_reading_incongruent)
print(fit_naming_neutral)
print(fit_naming_incongruent)

# check fits
plot_fit(fit_reading_neutral)
plot_fit(fit_reading_incongruent)
plot_fit(fit_naming_neutral)
plot_fit(fit_naming_incongruent)


## analysis -------------------------------------------------------------------
## cross compare all fits -----------------------------------------------------
fit_list <- c(fit_reading_incongruent, fit_naming_neutral, fit_naming_incongruent)
multiple_comparison <- compare_means(fit_reading_neutral, fits=fit_list)

# plot pairwise difference between means
plot_means_difference(fit_reading_neutral, fits=fit_list)

# plot means
plot_means(fit_reading_neutral, fits=fit_list) +
  scale_fill_hue(labels=c("Reading neutral",
                          "Reading incongruent",
                          "Naming neutral",
                          "Naming incongruent")) +
  theme(legend.title=element_blank())
