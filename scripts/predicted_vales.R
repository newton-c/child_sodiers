library(rstanarm)
library(tidyverse)

set.seed(977634)

dat <- a.out$imputations[[3]]

m1 <- stan_glm.nb(hom_count ~ Csdum + resources + duration + battle_deaths +
                  pop_slums + education, data = subset(dat,
                  postcon == 1), cores = 4)

m2 <- stan_glm.nb(hom_count ~ Csindex + resources + duration + battle_deaths +
                  pop_slums + education, data = subset(dat,
                  postcon == 1), cores = 4)

nd1 <- data.frame(Csdum = seq(0, 1),
                 resources = mean(dat$resources),
                 duration = mean(dat$duration),
                 battle_deaths = mean(dat$battle_deaths),
                 pop_slums = mean(dat$pop_slums),
                 education = mean(dat$education)
)

nd2 <- data.frame(Csindex = seq(0, 2, by = 1),
                 resources = mean(dat$resources),
                 duration = mean(dat$duration),
                 battle_deaths = mean(dat$battle_deaths),
                 pop_slums = mean(dat$pop_slums),
                 education = mean(dat$education)
)

y_tilde1 <- posterior_predict(m1, nd1, draws = 1000)
y_tilde_df1 <- data.frame(hom_count = c(y_tilde1),
                         Csdum = rep(nd1$Csdum, each = 1000))

summary(filter(y_tilde_df1, Csdum == 0))
summary(filter(y_tilde_df1, Csdum == 1))

ggplot(y_tilde_df1, aes(x = Csdum, y = hom_count,
                       group = Csdum)) +
                 geom_boxplot() +
                 labs(title = "Binary Measure") +
                 ylab("Number of Homicides") +
                 xlab("Child Soldiers") +
                 scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
                 theme_bw() +
                 theme(text = element_text(size = 20),
                       axis.text.y = element_text(angle = 90))

y_tilde2 <- posterior_predict(m2, nd2, draws = 1000)
y_tilde_df2 <- data.frame(hom_count = c(y_tilde2),
                         Csindex = rep(nd2$Csindex, each = 1000))

summary(filter(y_tilde_df2, Csindex == 0))
summary(filter(y_tilde_df2, Csindex == 1))
summary(filter(y_tilde_df2, Csindex == 2))

ggplot(y_tilde_df2, aes(x = Csindex, y = hom_count,
                       group = Csindex)) +
                 geom_boxplot() +
                 labs(title = "Index Measure") +
                 ylab("Number of Homicides") +
                 xlab("Child Soldiers") +
                 scale_x_continuous(breaks = c(0, 1, 2), labels = c("No", "< 50%", "> 50%")) +
                 theme_bw() +
                 theme(text = element_text(size = 20),
                       axis.text.y = element_text(angle = 90))


# with interaction
m3 <- stan_glm.nb(hom_count ~ Csdum + resources + Csdum * resources + 
                  duration + battle_deaths +pop_slums + education,
                  data = subset(dat, postcon == 1), cores = 4)

m4 <- stan_glm.nb(hom_count ~ Csindex + resources + Csindex * resources +
                  duration + battle_deaths +pop_slums + education,
                  data = subset(dat, postcon == 1), cores = 4)

nd3.1 <- data.frame(Csdum = seq(0, 1),
                 resources = 0,
                 duration = mean(dat$duration),
                 battle_deaths = mean(dat$battle_deaths),
                 pop_slums = mean(dat$pop_slums),
                 education = mean(dat$education)
)

nd3.2 <- data.frame(Csdum = seq(0, 1),
                 resources = 1,
                 duration = mean(dat$duration),
                 battle_deaths = mean(dat$battle_deaths),
                 pop_slums = mean(dat$pop_slums),
                 education = mean(dat$education)
)


nd4 <- data.frame(Csindex = seq(0, 2, by = 1),
                 resources = seq(0, 1),
                 duration = mean(dat$duration),
                 battle_deaths = mean(dat$battle_deaths),
                 pop_slums = mean(dat$pop_slums),
                 education = mean(dat$education)
)

y_tilde3.1 <- posterior_predict(m3, nd3.1, draws = 1000)
y_tilde_df3.1 <- data.frame(hom_count = c(y_tilde3.1),
                         Csdum = rep(nd3.1$Csdum, each = 1000))

summary(filter(y_tilde_df3.1, Csdum == 0))
summary(filter(y_tilde_df3.1, Csdum == 1))

y_tilde3.2 <- posterior_predict(m3, nd3.2, draws = 1000)
y_tilde_df3.2 <- data.frame(hom_count = c(y_tilde3.2),
                         Csdum = rep(nd3.2$Csdum, each = 1000))

summary(filter(y_tilde_df3.2, Csdum == 0))
summary(filter(y_tilde_df3.2, Csdum == 1))

ggplot(y_tilde_df3, aes(x = Csdum, y = hom_count,
                       group = Csdum)) +
                 geom_boxplot() +
                 labs(title = "Binary Measure") +
                 ylab("Number of Homicides") +
                 xlab("Child Soldiers") +
                 scale_x_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
                 theme_bw() +
                 theme(text = element_text(size = 20),
                       axis.text.y = element_text(angle = 90))

y_tilde4 <- posterior_predict(m4, nd4, draws = 1000)
y_tilde_df4 <- data.frame(hom_count = c(y_tilde4),
                         Csdum = rep(nd4$Csdum, each = 1000),
                         resources = rep(nd4$resources, each = 1000))

summary(filter(y_tilde_df4, Csindex == 0, resources = 0))
summary(filter(y_tilde_df4, Csindex == 1, resources = 0))
summary(filter(y_tilde_df4, Csindex == 2, resources = 0))
summary(filter(y_tilde_df4, Csindex == 0, resources = 1))
summary(filter(y_tilde_df4, Csindex == 1, resources = 1))
summary(filter(y_tilde_df4, Csindex == 2, resources = 1))

ggplot(y_tilde_df4, aes(x = Csindex, y = hom_count,
                       group = Csindex)) +
                 geom_boxplot() +
                 labs(title = "Index Measure") +
                 ylab("Number of Homicides") +
                 xlab("Child Soldiers") +
                 scale_x_continuous(breaks = c(0, 1, 2), labels = c("No", "< 50%", "> 50%")) +
                 theme_bw() +
                 theme(text = element_text(size = 20),
                       axis.text.y = element_text(angle = 90))