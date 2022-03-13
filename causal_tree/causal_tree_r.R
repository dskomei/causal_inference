library(causalTree)
library(dplyr)
library(Matching)



dataSet <- read.csv("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/rhc.csv")

table(dataSet[,c("swang1", "death")])

CT_df <- dataSet %>% 
    dplyr::select(
        death , swang1, age , sex , race , edu , 
        income , ninsclas , cat1 , das2d3pc , dnr1 , 
        ca , surv2md1 , aps1 , scoma1 , wtkilo1 , temp1 ,
        meanbp1 , resp1 , hrt1 , pafi1 , paco21 , ph1 , 
        wblc1 , hema1 , sod1 , pot1 , crea1 , bili1 , 
        alb1 , resp , card , neuro , gastr , renal , 
        meta , hema , seps , trauma , ortho , cardiohx , 
        chfhx , dementhx , psychhx , chrpulhx , renalhx ,
        liverhx , gibledhx , malighx , immunhx , transhx , amihx
    ) %>% 
    dplyr::mutate(
        death = if_else(death=="Yes",1,0), 
        swang1 = if_else(swang1=="No RHC",0,1)
    )


PS_model = glm(swang1 ~ .,
               family = binomial(link = "logit"), 
               data = CT_df %>% dplyr::select(-death))



PSMatching <- Match(Y = as.integer(CT_df$death)-1, 
                    Tr = (CT_df$swang1==1),
                    X = PS_model$fitted.values,
                    M = 1,
                    caliper = 0.1,
                    ties = FALSE,
                    replace = FALSE)

summary(PSMatching)


CT_df.tr <- CT_df %>% sample_frac(0.6)
CT_df.te <- anti_join(CT_df,CT_df.tr)


causal_tree <- causalTree(death ~ .,
                          data = CT_df.tr %>% dplyr::select(-swang1), 
                          treatment = CT_df.tr$swang1,
                          split.Rule = "CT", 
                          cv.option = "CT", 
                          split.Honest = T, 
                          cv.Honest = T, 
                          split.Bucket = F, 
                          xval = 5, 
                          cp = 0, 
                          minsize = 30)

opcp <- causal_tree$cptable[,1][which.min(causal_tree$cptable[,4])]

causal_tree_pruned <- prune(causal_tree, opcp)

rpart.plot(causal_tree_pruned, roundint=FALSE)

est_treat <- predict(causal_tree_pruned, CT_df.te)

hist(est_treat)

mean(est_treat)


tree <- causalTree(y~ x1 + x2 + x3 + x4, data = simulation.1, treatment = simulation.1$treatment,
                   split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = T, split.Bucket = F, 
                   xval = 5, cp = 0, minsize = 20, propensity = 0.5)

opcp <- tree$cptable[,1][which.min(tree$cptable[,4])]

opfit <- prune(tree, opcp)

rpart.plot(opfit)


filepath <- "https://raw.githubusercontent.com/iwanami-datascience/vol3/master/kato%26hoshino/q_data_x.csv"
df <- read.csv(url(filepath))


set.seed(1234)
test_pos <- sample_frac(df[df$cm_dummy==1,], 0.2)
test_neg <- sample_frac(df[df$cm_dummy==0,], 0.2)
df_test <- rbind(test_pos, test_neg)
df_train <- anti_join(df, df_test)


causal_tree <- causalTree(gamesecond ~ .,
                          data = df_train %>%
                              dplyr::select(gamesecond, TVwatch_day, age, sex, 
                                            marry_dummy, child_dummy, 
                                            inc, pmoney, area_kanto,
                                            area_tokai, area_keihanshin, 
                                            job_dummy1, job_dummy2, 
                                            job_dummy3, job_dummy4,
                                            job_dummy5, job_dummy6, 
                                            job_dummy7, fam_str_dummy1, 
                                            fam_str_dummy2, fam_str_dummy3, 
                                            fam_str_dummy4
                              ), 
                          treatment = df_train$cm_dummy,
                          split.Rule = "CT", 
                          cv.option = "CT", 
                          split.Honest = T, 
                          cv.Honest = T, 
                          split.Bucket = F, 
                          xval = 5, 
                          cp = 0, 
                          minsize = 30)
tmp <- predict(causal_tree)
mean(tmp)

quantile(tmp, probs = c(.05, .25, .5, .75, .95))

hist(tmp)