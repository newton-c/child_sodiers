library(rstanarm)
library(tidyverse)
library(bayesplot)

m1 <- stan_glm.nb(hom_count ~ Csdum + resources + duration + battle_deaths,
                  data = subset(a.out$imputations[[1]], postcon == 1))


m2 <- stan_glm.nb(hom_count ~ Csdum + resources + duration + battle_deaths +
                  pop_slums + education,
              data = subset(a.out$imputations[[1]], postcon == 1))

library(gridExtra)
library(patchwork)

# define models
dum_models <- function(i) {
    fit <- stan_glm.nb(hom_count ~ Csdum + resources + duration +
                                   battle_deaths + pop_slums + education,
              data = subset(a.out$imputations[[i]], postcon == 1), cores = 4)
    coefs <- cbind(coef(fit), se(fit))
}

index_models <- function(i) {
    fit <- stan_glm.nb(hom_count ~ Csindex + resources +
                                     duration + battle_deaths + pop_slums +
                                     education,
              data = subset(a.out$imputations[[i]], postcon == 1), cores = 4)
    coefs <- cbind(coef(fit), se(fit))
}

dum_inter_models <- function(i) {
    fit <- stan_glm.nb(hom_count ~ Csdum + resources + Csdum * resources + 
                       duration + battle_deaths + pop_slums + education,
              data = subset(a.out$imputations[[i]], postcon == 1), cores = 4)
    coefs <- cbind(coef(fit), se(fit))
}

index_inter_models <- function(i) {
    fit <- stan_glm.nb(hom_count ~ Csindex + resources + Csindex * resources +
                                     duration + battle_deaths + pop_slums +
                                     education,
              data = subset(a.out$imputations[[i]], postcon == 1), cores = 4)
    coefs <- cbind(coef(fit), se(fit))
}

# store results in an array
dum_coef <- array(NA, c(7, 2, 5))
for (i in seq_len(5)) {
    dum_coef[, , i] <- dum_models(i)
}


index_coef <- array(NA, c(7, 2, 5))
for (i in seq_len(5)) {
    index_coef[, , i] <- index_models(i)
}

dum_inter_coef <- array(NA, c(8, 2, 5))
for (i in seq_len(5)) {
    dum_inter_coef[, , i] <- dum_inter_models(i)
}


index_inter_coef <- array(NA, c(8, 2, 5))
for (i in seq_len(5)) {
    index_inter_coef[, , i] <- index_inter_models(i)
}

# coef plots of results
# child soldier binary
coef_names <- c("Intercept", "Child Soldier\n(Binary)", "Illicit Resources",
                "Duration",
                "Battle Related\nDeaths", "Population\nLiving in Slums",
                "Education")

dum_plots <- lapply(seq_along(coef_names), function(.x) {
    ggplot() +
        geom_point(aes(x = 1:5, y = dum_coef[.x, 1, ])) +
        geom_segment(aes(x = 1:5, xend = 1:5,
                         y = dum_coef[.x, 1, ] - 1.96 * dum_coef[.x, 2, ],
                         yend = dum_coef[.x, 1, ] + 1.96 * dum_coef[.x, 2, ]
                         )) +
        geom_hline(aes(yintercept = 0), linetype = 2) +
        xlab("Amelia II Dataset #") +
        ylab("Coeffecient") +
        labs(title = coef_names[[.x]])
           }
)

layout_grid <- rbind(c(2, 3, 4),
                     c(5, 6, 7),
                     c(NA, 1, NA))

nb_grid <- arrangeGrob(grobs = dum_plots, nrow = 3, ncol = 3,
                       layout_matrix = layout_grid)

ggsave("figures/dum_coef_plot.png", nb_grid, height = 10, width = 7)

# child soldier index
coef_names <- c("Intercept", "Child Soldier\n(Index)", "Illicit Resources",
                "Duration",
                "Battle Related\nDeaths", "Population\nLiving in Slums",
                "Education")

index_plots <- lapply(seq_along(coef_names), function(.x) {
    ggplot() +
        geom_point(aes(x = 1:5, y = index_coef[.x, 1, ])) +
        geom_segment(aes(x = 1:5, xend = 1:5,
                         y = index_coef[.x, 1, ] - 1.96 * index_coef[.x, 2, ],
                         yend = index_coef[.x, 1, ] + 1.96 * index_coef[.x, 2, ]
                         )) +
        geom_hline(aes(yintercept = 0), linetype = 2) +
        xlab("Amelia II Dataset #") +
        ylab("Coeffecient") +
        labs(title = coef_names[[.x]])
           }
)

layout_grid <- rbind(c(2, 3, 4),
                     c(5, 6, 7),
                     c(NA, 1, NA))

nb_grid <- arrangeGrob(grobs = index_plots, nrow = 3, ncol = 3,
                       layout_matrix = layout_grid)

ggsave("figures/index_coef_plot.png", nb_grid, height = 10, width = 7)

coef_names <- c("Intercept", "Child Soldier\n(Binary)", "Illicit Resources",
                "Child Soldier\n(Binary) *\nIllicit Resources", "Duration",
                "Battle Related\nDeaths", "Population\nLiving in Slums",
                "Education")

dum_inter_plots <- lapply(seq_along(coef_names), function(.x) {
    ggplot() +
        geom_point(aes(x = 1:5, y = dum_inter_coef[.x, 1, ])) +
        geom_segment(aes(x = 1:5, xend = 1:5,
                         y = dum_inter_coef[.x, 1, ] - 1.96 * dum_inter_coef[.x, 2, ],
                         yend = dum_inter_coef[.x, 1, ] + 1.96 * dum_inter_coef[.x, 2, ]
                         )) +
        geom_hline(aes(yintercept = 0), linetype = 2) +
        xlab("Amelia II Dataset #") +
        ylab("Coeffecient") +
        labs(title = coef_names[[.x]])
           }
)

layout_grid <- rbind(c(2, 3, 4),
                     c(5, 6, 7),
                     c(8, 1, NA))

nb_grid <- arrangeGrob(grobs = dum_inter_plots, nrow = 3, ncol = 3,
                       layout_matrix = layout_grid)

ggsave("figures/dum_inter_coef_plot.png", nb_grid, height = 10, width = 7)

coef_names <- c("Intercept", "Child Soldier\n(Index)", "Illicit Resources",
                "Child Soldier\n(Index) *\nIllicit Resources", "Duration",
                "Battle Related\nDeaths", "Population\nLiving in Slums",
                "Education")

index_inter_plots <- lapply(seq_along(coef_names), function(.x) {
    ggplot() +
        geom_point(aes(x = 1:5, y = index_inter_coef[.x, 1, ])) +
        geom_segment(aes(x = 1:5, xend = 1:5,
                         y = index_inter_coef[.x, 1, ] - 1.96 * 
                         index_inter_coef[.x, 2, ],
                         yend = index_inter_coef[.x, 1, ] + 1.96 * 
                         index_inter_coef[.x, 2, ]
                         )) +
        geom_hline(aes(yintercept = 0), linetype = 2) +
        xlab("Amelia II Dataset #") +
        ylab("Coeffecient") +
        labs(title = coef_names[[.x]])
           }
)

layout_grid <- rbind(c(2, 3, 4),
                     c(5, 6, 7),
                     c(8, 1, NA))

nb_grid <- arrangeGrob(grobs = index_inter_plots, nrow = 3, ncol = 3,
                       layout_matrix = layout_grid)

ggsave("figures/index_inter_coef_plot.png", nb_grid, height = 10, width = 7)

# Tables ---------------------------------------------------------------------
