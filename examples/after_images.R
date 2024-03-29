# libs
library(bayes4psy)
library(dplyr)
library(ggplot2)


## data wrangling and fitting -------------------------------------------------
# load data
data_all <- read.table("../examples/data/after_images.csv", sep="\t", header=TRUE)
stimuli <- read.table("../examples/data/after_images_stimuli.csv", sep="\t", header=TRUE)

## red stimuli ----------------------------------------------------------------
data_red <- data_all %>% filter(stimuli == "red")
data_red <- data.frame(r=data_red$r, g=data_red$g, b=data_red$b)
fit_red <- b_color(colors=data_red)

# check fit
plot_trace(fit_red)
print(fit_red)

# additional fit visualization for hue
plot_fit_hsv(fit_red)


## blue stimuli ---------------------------------------------------------------
data_blue <- data_all %>% filter(stimuli == "blue")
data_blue <- data.frame(r=data_blue$r, g=data_blue$g, b=data_blue$b)
fit_blue <- b_color(colors=data_blue)

# check fit
plot_trace(fit_blue)
print(fit_blue)

# additional fit visualization for hue
plot_fit_hsv(fit_blue)


## green -----------------------------------------------------------------
data_green <- data_all %>% filter(stimuli == "green")
data_green <- data.frame(r=data_green$r, g=data_green$g, b=data_green$b)
fit_green <- b_color(colors=data_green)

# check fit
plot_trace(fit_green)
print(fit_green)

# additional fit visualization for hue
plot_fit_hsv(fit_green)


## yellow ----------------------------------------------------------------
data_yellow <- data_all %>% filter(stimuli == "yellow")
data_yellow <- data.frame(r=data_yellow$r, g=data_yellow$g, b=data_yellow$b)
fit_yellow <- b_color(colors=data_yellow)

# check fit
plot_trace(fit_yellow)
print(fit_yellow)

# additional fit visualization for hue
plot_fit_hsv(fit_yellow)


## magenta ---------------------------------------------------------------
data_magenta <- data_all %>% filter(stimuli == "magenta")
data_magenta <- data.frame(r=data_magenta$r, g=data_magenta$g, b=data_magenta$b)
fit_magenta <- b_color(colors=data_magenta)

# check fit
plot_trace(fit_magenta)
print(fit_magenta)

# additional fit visualization for hue
plot_fit_hsv(fit_magenta)


## cyan ------------------------------------------------------------------
data_cyan <- data_all %>% filter(stimuli == "cyan")
data_cyan <- data.frame(r=data_cyan$r, g=data_cyan$g, b=data_cyan$b)
fit_cyan <- b_color(colors=data_cyan)

# check fit
plot_trace(fit_cyan)
print(fit_cyan)

# additional fit visualization for hue
plot_fit_hsv(fit_cyan)


## analysis plots -------------------------------------------------------------
trichromatic <- read.table("../examples/data/after_images_trichromatic.csv", sep="\t", header=TRUE)
opponent_process <- read.table("../examples/data/after_images_opponent_process.csv", sep="\t", header=TRUE)

# red
stimulus <- "red"
lines <- list()
lines[[1]] <- c(trichromatic[trichromatic$stimuli == stimulus, ]$h,
                trichromatic[trichromatic$stimuli == stimulus, ]$s,
                trichromatic[trichromatic$stimuli == stimulus, ]$v)
lines[[2]] <- c(opponent_process[opponent_process$stimuli == stimulus, ]$h,
                opponent_process[opponent_process$stimuli == stimulus, ]$s,
                opponent_process[opponent_process$stimuli == stimulus, ]$v)

points <- list()
points[[1]] <- c(stimuli[stimuli$stimuli == stimulus, ]$h_s,
                 stimuli[stimuli$stimuli == stimulus, ]$s_s,
                 stimuli[stimuli$stimuli == stimulus, ]$v_s)

plot_red <- plot_distributions_hsv(fit_red, points=points, lines=lines, hsv=TRUE)
plot_red <- plot_red + ggtitle("Red") + theme(plot.title = element_text(hjust = 0.5))


# blue
stimulus <- "blue"
lines <- list()
lines[[1]] <- c(trichromatic[trichromatic$stimuli == stimulus, ]$h,
                trichromatic[trichromatic$stimuli == stimulus, ]$s,
                trichromatic[trichromatic$stimuli == stimulus, ]$v)
lines[[2]] <- c(opponent_process[opponent_process$stimuli == stimulus, ]$h,
                opponent_process[opponent_process$stimuli == stimulus, ]$s,
                opponent_process[opponent_process$stimuli == stimulus, ]$v)

points <- list()
points[[1]] <- c(stimuli[stimuli$stimuli == stimulus, ]$h_s,
                 stimuli[stimuli$stimuli == stimulus, ]$s_s,
                 stimuli[stimuli$stimuli == stimulus, ]$v_s)

plot_blue <- plot_distributions_hsv(fit_blue, points=points, lines=lines, hsv=TRUE)
plot_blue <- plot_blue + ggtitle("Blue") + theme(plot.title = element_text(hjust = 0.5))


# green
stimulus <- "green"
lines <- list()
lines[[1]] <- c(trichromatic[trichromatic$stimuli == stimulus, ]$h,
                trichromatic[trichromatic$stimuli == stimulus, ]$s,
                trichromatic[trichromatic$stimuli == stimulus, ]$v)
lines[[2]] <- c(opponent_process[opponent_process$stimuli == stimulus, ]$h,
                opponent_process[opponent_process$stimuli == stimulus, ]$s,
                opponent_process[opponent_process$stimuli == stimulus, ]$v)

points <- list()
points[[1]] <- c(stimuli[stimuli$stimuli == stimulus, ]$h_s,
                 stimuli[stimuli$stimuli == stimulus, ]$s_s,
                 stimuli[stimuli$stimuli == stimulus, ]$v_s)

plot_green <- plot_distributions_hsv(fit_green, points=points, lines=lines, hsv=TRUE)
plot_green <- plot_green + ggtitle("Green") + theme(plot.title = element_text(hjust = 0.5))


# yellow
stimulus <- "yellow"
lines <- list()
lines[[1]] <- c(trichromatic[trichromatic$stimuli == stimulus, ]$h,
                trichromatic[trichromatic$stimuli == stimulus, ]$s,
                trichromatic[trichromatic$stimuli == stimulus, ]$v)
lines[[2]] <- c(opponent_process[opponent_process$stimuli == stimulus, ]$h,
                opponent_process[opponent_process$stimuli == stimulus, ]$s,
                opponent_process[opponent_process$stimuli == stimulus, ]$v)

points <- list()
points[[1]] <- c(stimuli[stimuli$stimuli == stimulus, ]$h_s,
                 stimuli[stimuli$stimuli == stimulus, ]$s_s,
                 stimuli[stimuli$stimuli == stimulus, ]$v_s)

plot_yellow <- plot_distributions_hsv(fit_yellow, points=points, lines=lines, hsv=TRUE)
plot_yellow <- plot_yellow + ggtitle("Yellow") + theme(plot.title = element_text(hjust = 0.5))


# cyan
stimulus <- "cyan"
lines <- list()
lines[[1]] <- c(trichromatic[trichromatic$stimuli == stimulus, ]$h,
                trichromatic[trichromatic$stimuli == stimulus, ]$s,
                trichromatic[trichromatic$stimuli == stimulus, ]$v)
lines[[2]] <- c(opponent_process[opponent_process$stimuli == stimulus, ]$h,
                opponent_process[opponent_process$stimuli == stimulus, ]$s,
                opponent_process[opponent_process$stimuli == stimulus, ]$v)

points <- list()
points[[1]] <- c(stimuli[stimuli$stimuli == stimulus, ]$h_s,
                 stimuli[stimuli$stimuli == stimulus, ]$s_s,
                 stimuli[stimuli$stimuli == stimulus, ]$v_s)

plot_cyan <- plot_distributions_hsv(fit_cyan, points=points, lines=lines, hsv=TRUE)
plot_cyan <- plot_cyan + ggtitle("Cyan") + theme(plot.title = element_text(hjust = 0.5))


# magenta
stimulus <- "magenta"
lines <- list()
lines[[1]] <- c(trichromatic[trichromatic$stimuli == stimulus, ]$h,
                trichromatic[trichromatic$stimuli == stimulus, ]$s,
                trichromatic[trichromatic$stimuli == stimulus, ]$v)
lines[[2]] <- c(opponent_process[opponent_process$stimuli == stimulus, ]$h,
                opponent_process[opponent_process$stimuli == stimulus, ]$s,
                opponent_process[opponent_process$stimuli == stimulus, ]$v)

points <- list()
points[[1]] <- c(stimuli[stimuli$stimuli == stimulus, ]$h_s,
                 stimuli[stimuli$stimuli == stimulus, ]$s_s,
                 stimuli[stimuli$stimuli == stimulus, ]$v_s)

plot_magenta <- plot_distributions_hsv(fit_magenta, points=points, lines=lines, hsv=TRUE)
plot_magenta <- plot_magenta + ggtitle("Magenta") + theme(plot.title = element_text(hjust = 0.5))


# plot grid
cowplot::plot_grid(plot_red, plot_green, plot_blue, plot_yellow, plot_cyan, plot_magenta, ncol=3, nrow=2, scale=0.9)


# tiles plot
# get averages
color_levels <- c("red", "green", "blue", "yellow", "cyan", "magenta")
averages <- data.frame(r=numeric(), g=numeric(), b=numeric, stimuli=factor(), order=numeric())
averages <- rbind(averages, data.frame(r=mean(fit_red@extract$mu_r),
                                   g=mean(fit_red@extract$mu_g),
                                   b=mean(fit_red@extract$mu_b),
                                   stimuli="red"))
averages <- rbind(averages, data.frame(r=mean(fit_green@extract$mu_r),
                                   g=mean(fit_green@extract$mu_g),
                                   b=mean(fit_green@extract$mu_b),
                                   stimuli="green"))
averages <- rbind(averages, data.frame(r=mean(fit_blue@extract$mu_r),
                                   g=mean(fit_blue@extract$mu_g),
                                   b=mean(fit_blue@extract$mu_b),
                                   stimuli="blue"))
averages <- rbind(averages, data.frame(r=mean(fit_yellow@extract$mu_r),
                                   g=mean(fit_yellow@extract$mu_g),
                                   b=mean(fit_yellow@extract$mu_b),
                                   stimuli="yellow"))
averages <- rbind(averages, data.frame(r=mean(fit_cyan@extract$mu_r),
                                   g=mean(fit_cyan@extract$mu_g),
                                   b=mean(fit_cyan@extract$mu_b),
                                   stimuli="cyan"))
averages <- rbind(averages, data.frame(r=mean(fit_magenta@extract$mu_r),
                                   g=mean(fit_magenta@extract$mu_g),
                                   b=mean(fit_magenta@extract$mu_b),
                                   stimuli="magenta"))
levels(averages$stimuli) <- color_levels

# add hsv
averages[c("h", "s", "v")] <- t(rgb2hsv(averages$r, averages$g, averages$b, maxColorValue = 255))

# add stimuli
averages <- inner_join(averages, stimuli)

# plot
tiles_plot <- ggplot(averages, aes(ymin=-as.numeric(stimuli)*10 + 1, ymax=-as.numeric(stimuli)*10 + 10)) +
  geom_rect(aes(fill=rgb(r_s, g_s, b_s, maxColorValue=255), xmin=11, xmax=20)) +
  geom_rect(data=trichromatic, aes(fill=rgb(r, g, b, maxColorValue=255)), xmin=31-5, xmax=40-5) +
  geom_rect(data=opponent_process, aes(fill=rgb(r, g, b, maxColorValue=255)), xmin=41-5, xmax=50-5) +
  geom_rect(xmin=61-10, xmax=70-10, aes(fill=rgb(r, g, b, maxColorValue=255), xmin=11, xmax=20)) +
  geom_rect(xmin=71-10, xmax=80-10, aes(fill=hsv(h, 1, 1))) +
  scale_fill_identity() +
  coord_equal(ylim=c(-59-10, 0+10), xlim=c(11-10, 70+10)) +
  scale_x_continuous(expand=c(0, 0)) +
  scale_y_continuous(expand=c(0, 0)) +
  ylab(NULL) +
  xlab(NULL) +
  theme(panel.grid = element_blank(),
              axis.text.y = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              panel.background = element_rect(fill = "gray80"),
              plot.margin=unit(c(-0.5,-0.5,-0.5,-0.5),"line")) +
  annotate("text", x=15.3, y=2, label="Stimuli", size=5) +
  annotate("text", x=35.5, y=2, label="Prediction", size=5) +
  annotate("text", x=60.5, y=2, label="Response", size=5)

tiles_plot
