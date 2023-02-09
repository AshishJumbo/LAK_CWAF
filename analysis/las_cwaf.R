library(lme4)
library(ggplot2)
library(sjPlot)
library(ggplot2)
library(dplyr)
library(XML)
install.packages('RCurl')
library(RCurl)
library(janitor)
library(dplyr)
library(knitr)
# print 

data <- read.csv('../data/processed0.2/cwaf_v1_v2_plogs_final_R.csv')
data_filtered <- read.csv('../data/processed0.2/cwaf_v1_v2_plogs_final_filtered_R.csv')

hist(data[data$control_treatments=="control",]$prior_pr_avg_correctness)
hist(data[data$control_treatments=="treatment",]$prior_pr_avg_correctness)

# PSAHQV
# PSAGF4
hist(data_filtered[data_filtered$control_treatments=="control" & data_filtered$psa_id == "PSAHQV",]$prior_pr_avg_correctness)
hist(data_filtered[data_filtered$control_treatments=="treatment" & data_filtered$psa_id == "PSAHQV",]$prior_pr_avg_correctness)

hist(data_filtered[data_filtered$control_treatments=="control" & data_filtered$psa_id == "PSAGF4",]$prior_pr_avg_correctness)
hist(data_filtered[data_filtered$control_treatments=="treatment" & data_filtered$psa_id == "PSAGF4",]$prior_pr_avg_correctness)

data_filtered_1 <- data_filtered[data_filtered$psa_id == "PSAHQV", ]
ggplot(data_filtered_1, aes(x = data_filtered_1$prior_pr_avg_correctness, fill = data_filtered_1$control_treatments, color=data_filtered_1$control_treatments)) +
  geom_histogram(alpha=0.5, position="identity")

data_filtered_2 <- data_filtered[data_filtered$psa_id == "PSAGF4", ]
ggplot(data_filtered_2, aes(x = data_filtered_2$prior_pr_avg_correctness, fill = data_filtered_2$control_treatments, color=data_filtered_2$control_treatments)) +
  geom_histogram(alpha=0.5, position="identity")

ggplot(data_filtered_1, aes(x = data_filtered_1$prior_5pr_avg_correctness, fill = data_filtered_1$control_treatments, color=data_filtered_1$control_treatments)) +
  geom_histogram(alpha=0.5, position="identity")

ggplot(data_filtered_2, aes(x = data_filtered_2$prior_5pr_avg_correctness, fill = data_filtered_2$control_treatments, color=data_filtered_2$control_treatments)) +
  geom_histogram(alpha=0.5, position="identity")


model1 <- glmer(mastery ~ control_treatments + skb_mastery_count + (1 | teacher_id), 
               data=data_filtered[data_filtered$psa_id == "PSAHQV", ], family = binomial(link = "logit"))
summary(model1)

model1 <- glmer(mastery ~ control_treatments + skb_mastery_count + (1 |teacher_id), 
                data=data_filtered[data_filtered$psa_id == "PSAGF4", ], family = binomial(link = "logit"))
summary(model1)


data_filtered$Operations <- ifelse(data_filtered$psa_id == 'PSAGF4', 1, 0)

#### RQ2: Intervention effecst ####
# Rq2: dependent is Mastery and Wheel-spinning
# 2-Step Equations
model2 <- glm(wheel_spinning ~ control_treatments, 
                data=data_filtered[data_filtered$psa_id == "PSAHQV", ], family = binomial(link = "logit"))
summary(model2)


# Order of Operations
model2 <- glm(wheel_spinning ~ control_treatments, 
              data=data_filtered[data_filtered$psa_id == "PSAGF4", ], family = binomial(link = "logit"))
summary(model2)

##### Mastery #####
RQ2_NULL <- glmer(mastery ~  (1 | teacher_id),
                  data=data_filtered,
                  family = binomial)
tab_model(RQ2_NULL)

RQ2_MOD1 <- glmer(mastery ~ control_treatments
                  + (1 | teacher_id),
                  data=data_filtered,
                  family = binomial)
summary(RQ2_MOD1)

RQ2_MOD1TwoStep <- glmer(mastery ~ control_treatments
                            + (1 | teacher_id),
                            data=data_filtered[data_filtered$Operations == 0, ],
                            family = binomial)
summary(RQ2_MOD1TwoStep)

RQ2_MOD1Operations <- glmer(mastery ~ control_treatments
                  + (1 | teacher_id),
                  data=data_filtered[data_filtered$Operations == 1, ],
                  family = binomial)
summary(RQ2_MOD1Operations)

RQ2_MOD1_Interaction <- glmer(mastery ~ control_treatments*Operations
                  + (1 | teacher_id),
                  data=data_filtered,
                  family = binomial)
summary(RQ2_MOD1_Interaction)

tab_model(RQ2_MOD1, RQ2_MOD1TwoStep, RQ2_MOD1Operations, RQ2_MOD1_Interaction)


##### Wheel-spinning Spinning #####
RQ2_NULL2 <- glmer(wheel_spinning ~  (1 | teacher_id),
                  data=data_filtered,
                  family = binomial)
tab_model(RQ2_NULL2)


RQ2_MOD2 <- glmer(wheel_spinning ~ control_treatments
                  + (1 | teacher_id),
                  data=data_filtered,
                  family = binomial)
summary(RQ2_MOD1)

RQ2_MOD2TwoStep <- glmer(wheel_spinning ~ control_treatments
                         + (1 | teacher_id),
                         data=data_filtered[data_filtered$Operations == 0, ],
                         family = binomial)
summary(RQ2_MOD2TwoStep)

RQ2_MOD2Operations <- glmer(wheel_spinning ~ control_treatments
                            + (1 | teacher_id),
                            data=data_filtered[data_filtered$Operations == 1, ],
                            family = binomial)
summary(RQ2_MOD2Operations)

RQ2_MOD2_Interaction <- glmer(wheel_spinning ~ control_treatments*Operations
                               + (1 | teacher_id),
                               data=data_filtered,
                               family = binomial)
summary(RQ2_MOD2_Interaction)

tab_model(RQ2_MOD2, RQ2_MOD2TwoStep, RQ2_MOD2Operations, RQ2_MOD2_Interaction)

##### MODEL TABLE RQ1 #####
tab_model(RQ2_MOD1, RQ2_MOD1TwoStep, RQ2_MOD1Operations, RQ2_MOD2, RQ2_MOD2TwoStep, RQ2_MOD2Operations,
          pred.labels	 = c("Intercept",
                           "Treatment"),
          dv.labels = c("Both", "Two-Step Equations", "Order of Operations", "Both", "Two-Step Equations", "Order of Operations"),
          transform = NULL,
          show.se = T,
          show.ci = F)


kable(
  effects,
  format = "latex",
  booktabs = TRUE,
  col.names = names(effects),
  align = c("l", rep("c", length(names(effects)) - 1)),
  caption = "Means and Standard Deviations of Scores on Baseline Measures"
)

##### VIZ #####

effects <- as.data.frame(rbind(
  coefficients(summary(RQ2_MOD1))[2,],
  coefficients(summary(RQ2_MOD1TwoStep))[2,],
  coefficients(summary(RQ2_MOD1Operations))[2,],
  coefficients(summary(RQ2_MOD2))[2,],
  coefficients(summary(RQ2_MOD2TwoStep))[2,],
  coefficients(summary(RQ2_MOD2Operations))[2,]
  
  )) %>%
  mutate(
    outcome = c("Mastery", "Mastery", "Mastery", "Wheel-Spinning", "Wheel-Spinning", "Wheel-Spinning"),
    ProblemSet = c("Both Acitivities", "Two-Step Equations", "Order of Operations", "Both Acitivities", "Two-Step Equations", "Order of Operations"),
    p_BH = round(p.adjust(`Pr(>|z|)`, method = "BH"), 4),
    p_dots = ifelse(p_BH > .05,  "",
                    ifelse(p_BH > .01, "*",
                           ifelse(p_BH > .001, "**", "***")))
  ) %>%
  dplyr::rename(
    "Effect Size (Log Odds)" = Estimate,
    SE = `Std. Error`
  )
  
effects

effects$ProblemSet <- factor(as.character(effects$ProblemSet), 
                                 levels = c(
                                   "Both Acitivities", "Two-Step Equations", "Order of Operations"
                                 ))


ggplot(effects,
       aes(
         x = `Effect Size (Log Odds)`,
         y = ProblemSet,
         xmin = `Effect Size (Log Odds)`-(SE*1.96),
         xmax = `Effect Size (Log Odds)`+(SE*1.96),
         label = p_dots
         
       )) +
  geom_point(alpha = 1.2, size = 1.2) +
  geom_errorbarh(height=.1, size = .7, alpha = .8) +
  facet_grid(cols = vars(outcome)) +
  scale_y_discrete(limits = rev(levels(effects$ProblemSet))) +
  geom_vline(xintercept = 0,  linetype="dotted") +
  geom_text(nudge_x = .25, size = 4) +
  coord_cartesian(xlim = c(-1, 1)) + 
  theme_classic() +
  labs(
    caption = "* p < .05. ** p < .01. *** p < .001"
  )+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size=20),
        axis.text.x = element_text(size=16),
        strip.text.x = element_text(size = 20), 
        axis.title.x = element_text(size = 16),
        plot.caption = element_text(size = 14))



### Research Question 3: Personalization effects based on  prior knowledge #####

table(data_filtered$prior_problems_count >= 10 & data_filtered$prior_pr_avg_correctness <= 1, data_filtered$control_treatments )


data_filtered_3 <- data_filtered[data_filtered$prior_problems_count >= 10 & data_filtered$prior_pr_avg_correctness <= 1 , ] %>%
  group_by(psa_id) %>%
  mutate(
    prior_pr_avg_correctness_Z = scale(prior_pr_avg_correctness)
  )
require(psych)
describe(data_filtered_3$prior_pr_avg_correctness)
t.test(data_filtered_3$prior_pr_avg_correctness~ data_filtered_3$psa_id, rm.na = T)
#data_filtered_3 <- data_filtered_3[!(data_filtered_3$mastery == TRUE & data_filtered_3$skb_problem_count == 3), ]

# model2 <- glm(mastery ~ control_treatments + prior_pr_avg_correctness + prior_5pr_avg_correctness, 
#               data=data_filtered[data_filtered$psa_id == "PSAHQV", ], family = binomial(link = "logit"))
# summary(model2)


ggplot(data_filtered_3, aes(x = data_filtered_3$prior_pr_avg_correctness, fill = control_treatments, color=control_treatments)) +
  geom_histogram(alpha=0.5, position="identity")

ggplot(data_filtered_3, aes(x = prior_pr_avg_correctness, fill = control_treatments, color=control_treatments)) +
  geom_density(alpha=0.5, position="identity")


RQ3_Model <-
  glmer(
    mastery ~ control_treatments * prior_pr_avg_correctness_Z  + (1 | teacher_id) ,
    data = data_filtered_3,
    family = binomial(link = "logit")
  )

RQ3_ModelTwoStep <-
  glmer(
    mastery ~ control_treatments *prior_pr_avg_correctness_Z  + (1 | teacher_id) ,
    data = data_filtered_3[data_filtered_3$Operations == 0, ],
    family = binomial(link = "logit")
  )

RQ3_ModelOperations <-
  glmer(
    mastery ~ control_treatments * prior_pr_avg_correctness_Z  + (1 | teacher_id) ,
    data = data_filtered_3[data_filtered_3$Operations == 1, ],
    family = binomial(link = "logit")
  )
tab_model(RQ3_Model, RQ3_ModelTwoStep, RQ3_ModelOperations,
          pred.labels	 = c("Intercept",
                           "Treatment",
                           "Prior Problem Correct (Z-Score)",
                           "Treatment  X Prior Problem Correct"),
          dv.labels = c("Both", "Two-Step Equations", "Order of Operations"),
          transform = NULL,
          show.se = T,
          show.ci = F)




RQ3_Model2 <-
  glmer(
    wheel_spinning ~ control_treatments * prior_pr_avg_correctness_Z  + (1 | teacher_id) ,
    data = data_filtered_3,
    family = binomial(link = "logit")
  )

RQ3_Model2TwoStep <-
  glmer(
    wheel_spinning ~ control_treatments * prior_pr_avg_correctness_Z  + (1 | teacher_id) ,
    data = data_filtered_3[data_filtered_3$Operations == 0, ],
    family = binomial(link = "logit")
  )

RQ3_Model2Operations <-
  glmer(
    wheel_spinning ~ control_treatments * prior_pr_avg_correctness_Z  + (1 | teacher_id) ,
    data = data_filtered_3[data_filtered_3$Operations == 1, ],
    family = binomial(link = "logit")
  )
tab_model(RQ3_Model2, RQ3_Model2TwoStep, RQ3_Model2Operations,
          pred.labels	 = c("Intercept",
                           "Treatment",
                           "Prior Problem Correct (Z-Score)",
                           "Treatment  X Prior Problem Correct"),
          dv.labels = c("Both", "Two-Step Equations", "Order of Operations"),
          transform = NULL,
          show.se = T,
          show.ci = F)



data_filtered_3$predicted = predict(RQ3_Model, data_filtered_3, allow.new.levels = T) 
#ad_sub$predicted_log = predict(RQ3_M1_StudentAbility_Z, ad_sub, allow.new.levels = T) 

data_filtered_forviz <- data_filtered_3[data_filtered_3$Operations == 1, ]
data_filtered_forviz$CWA = as.factor(as.character(data_filtered_forviz$control_treatments))
table(data_filtered_forviz$CWA)
levels(data_filtered_forviz$CWA) <- c("Control", "CWAF")
data_filtered_forviz$CWA <- ordered(data_filtered_forviz$CWA, levels = c("CWAF", "Control"))
pallet <- c( "#f03b20", "#2b8cbe")

library(ggExtra)

ggplot(data_filtered_forviz, aes(y = predicted,   x = scale(prior_pr_avg_correctness), color = as.factor(CWA) )) +
  geom_point(alpha = .2, aes(color = as.factor(CWA))) +
  scale_fill_manual( values=pallet, aesthetics = c("color", "fill")) +
  xlab( "Prior Performance (Z-Score)") +
  ylab("Likelihood of Mastery (Log-Odds)") +
  geom_smooth(formula= y ~ x,  method = lm, aes(color = as.factor(CWA)), size = 2.3, se = F, fullrange=TRUE, xseq = c(-3.5, 2.1) ) +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .9)) +
  coord_cartesian(xlim = c(-3.2, 1.9)) +
  theme(axis.title.y = element_text(size = 16),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14),
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size = 14))




