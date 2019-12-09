# libs
library(bayes4psy)
library(dplyr)

## load data ------------------------------------------------------------------
data <- read.table("../examples/data/adaptation_level.csv", sep="\t", header=TRUE)


## data prep and fitting ------------------------------------------------------
group1 <- data %>% filter(group == 1)
group2 <- data %>% filter(group == 2)

# number of subjects
n1 <- length(unique(group1$subject))
n2 <- length(unique(group2$subject))

# map subject to 1..m interval
group1$subject <- plyr::mapvalues(group1$subject,
                                  from=unique(group1$subject),
                                  to=1:n1)
group2$subject <- plyr::mapvalues(group2$subject,
                                  from=unique(group2$subject),
                                  to=1:n2)

# we will use only part 2
group1_part2 <- group1 %>% filter(part == 2)
group2_part2 <- group2 %>% filter(part == 2)

# fit (increase the amount of steps to get n_eff 10000+ on relevant parameters)
fit1 <- b_linear(x=group1_part2$sequence,
                 y=group1_part2$response,
                 s=group1_part2$subject,
                 iter=10000, warmup=500)

fit2 <- b_linear(x=group2_part2$sequence,
                 y=group2_part2$response,
                 s=group2_part2$subject,
                 iter=10000, warmup=500)

## diagnose group 1 fits ------------------------------------------------------
# plot trace
plot_trace(fit1)
plot_trace(fit2)

# check fits
plot_fit(fit1)
plot_fit(fit2)

# check n_eff and RHat
print(fit1)
print(fit2)


## comparison and visualizations ----------------------------------------------
# difference
summary(fit1)
summary(fit2)
comparison_results <- compare_means(fit1, fit2=fit2)

# visualize difference
plot_means_difference(fit1, fit2=fit2, par="intercept")

# visualize
plot_distributions(fit1, fit2) +
  labs(x="measurement number", y="weight") +
  theme(legend.position=) +
  theme(legend.position="none") +
  scale_x_continuous(limits=c(1, 10), breaks=seq(1:10)) +
  scale_y_continuous(limits=c(1, 10), breaks=seq(1:10))

