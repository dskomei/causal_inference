library(bcf)
library(tidyverse)
library(haven)
set.seed(1)


nswdw_data <- read_dta('https://users.nber.org/~rdehejia/data/nsw_dw.dta')
cps1_data <- read_dta('https://users.nber.org/~rdehejia/data/cps_controls.dta')


cps1_nsw_data <- nswdw_data %>%
    dplyr::filter(treat == 1) %>% 
    dplyr::bind_rows(cps1_data) %>%
    dplyr::select(-data_id)


target_data <- nswdw_data %>% dplyr::select(-data_id)

y <- target_data$re78
z <- target_data$treat
x <- target_data %>% dplyr::select(-re78, -treat)
x <- as.matrix(x)

ps_score_model <- glm(
    formula = treat~. ,
    data=target_data %>% dplyr::select(-re78),
    family = binomial
)
pihat <- as.vector(ps_score_model$fitted.values)


bcf_fit = bcf(y, z, x, x, pihat=pihat, nburn=2000, nsim=2000)

tau_post <- bcf_fit$tau
tauhat <- colMeans(tau_post)

# t検定
t.test(tauhat)

plot_data <- target_data
plot_data$tau_mean <- tauhat
plot_data$ps_score <- pihat

plot(plot_data$ps_score, plot_data$tau_mean)
plot(plot_data$age, plot_data$tau_mean)
plot(plot_data$education, plot_data$tau_mean)
plot(plot_data$re74, plot_data$tau_mean)
plot(plot_data$re75, plot_data$tau_mean)

plot_list <- list(
    a=plot_data[plot_data$hispanic == 1,]$tau_mean ,
    b=plot_data[plot_data$hispanic == 0,]$tau_mean
)
boxplot(plot_list)


#
#---------   恣意的な割当データによる効果測定
#

target_data <- cps1_nsw_data 

y <- target_data$re78
z <- target_data$treat
x <- target_data %>% dplyr::select(-re78, -treat)
x <- as.matrix(x)

ps_score_model <- glm(
    formula = treat~. ,
    data=target_data %>% dplyr::select(-re78),
    family = binomial
)
pihat <- as.vector(ps_score_model$fitted.values)


bcf_fit = bcf(y, z, x, x, pihat=pihat, nburn=2000, nsim=2000)

tau_post <- bcf_fit$tau
tauhat <- colMeans(tau_post)

t.test(tauhat)


plot(plot_data$ps_score, plot_data$tau_mean)
plot(plot_data$age, plot_data$tau_mean)
plot(plot_data$education, plot_data$tau_mean)
plot(plot_data$re74, plot_data$tau_mean)
plot(plot_data$re75, plot_data$tau_mean)

plot_list <- list(
    a=plot_data[plot_data$hispanic == 1,]$tau_mean ,
    b=plot_data[plot_data$hispanic == 0,]$tau_mean
)
boxplot(plot_list)



#
# ------------- ゲームデータを使った効果測定
#

filepath <- "https://raw.githubusercontent.com/iwanami-datascience/vol3/master/kato%26hoshino/q_data_x.csv"
game_data <- read.csv(url(filepath))


target_data <- game_data

y <- target_data$gamesecond
z <- target_data$cm_dummy
x <- target_data %>%
    dplyr::select(
        gamesecond, TVwatch_day, age, sex, marry_dummy, child_dummy, inc, pmoney, area_kanto,
        area_tokai, area_keihanshin, job_dummy1, job_dummy2, job_dummy3, job_dummy4, job_dummy5, 
        job_dummy6, job_dummy7, fam_str_dummy1, fam_str_dummy2, fam_str_dummy3, fam_str_dummy4
    )
x <- as.matrix(x)

ps_score_model <- glm(
    formula = cm_dummy~. ,
    data=target_data %>% dplyr::select(-gamesecond),
    family = binomial
)
pihat <- as.vector(ps_score_model$fitted.values)


bcf_fit = bcf(y, z, x, x, pihat=pihat, nburn=2000, nsim=2000)

tau_post <- bcf_fit$tau
tauhat <- colMeans(tau_post)

t.test(tauhat)

plot_data <- target_data
plot_data$tau_mean <- tauhat
plot_data$ps_score <- pihat

plot(plot_data$ps_score, plot_data$tau_mean)
plot(plot_data$TVwatch_day, plot_data$tau_mean)
plot(plot_data$age, plot_data$tau_mean)

plot_list <- list(
    a=plot_data[plot_data$area_tokai == 1,]$tau_mean ,
    b=plot_data[plot_data$area_tokai == 0,]$tau_mean
)
boxplot(plot_list)
