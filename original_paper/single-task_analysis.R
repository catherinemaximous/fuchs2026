### ========================================
### DUAL TASK STUDY
### Single task code
### ========================================

## This code cleans, plots, and analyzes data
## for the single-task portion of the main
## task for the DUAL task study. 

library(dplyr)
library(lme4)
library(lmerTest)
library(lsmeans)
library(ggplot2)
library(tidyr)
library(readr) 
library(visreg)
library(MASS)

## load in the data
singletaskraw <- read.csv("DUAL_single-task_raw_11-06.csv", header=TRUE)

## =======================================
## (NON-LINGUISTICS TASK) MOT SINGLE-TASK: CLEANING
## =======================================

# need to create a column that indexes difficulty level of MOT ("bubbles")
singletaskraw$bub_diff <- substr(singletaskraw$videostim,1,1)
singletaskraw$bub_diff <- as.factor(singletaskraw$bub_diff)
hist(singletaskraw$reaction_time)

single_bubbles <- singletaskraw %>% filter(condition=="bubble only")
dim(single_bubbles) #9960

## take out OUTLIERS:
# take out anything 2SDs above the mean by participant by condition
# and anything faster than 150ms
single_bubbles_clean <- single_bubbles %>% 
  filter(reaction_time > 150) %>%
  group_by(subject, bub_diff) %>% 
  filter(abs(reaction_time - mean(reaction_time)) < (sd(reaction_time) * 2.5))
dim(single_bubbles_clean) #9601
(9960-9601)/9960 # removed 3.6% of data

## ====================================
## PLOTTING MOT ACCURACY 
## ====================================

# create new table with accuracies per participant per condition
bubbles_accuracy <- single_bubbles_clean %>%
  group_by(group, subject, bub_diff) %>%
  summarise(accuracy=mean(bub_corr))

# Get a barplot from this
bubb_sum <- bubbles_accuracy %>% group_by(group, bub_diff) %>% 
  summarize(mean_acc=mean(accuracy),
            se=sqrt(mean(accuracy)*(1-mean(accuracy))/length(accuracy)))

dualFig2 <- bubb_sum %>% filter(group != "Gen1.5") %>%
  ggplot(aes(x=group,y=mean_acc,fill=bub_diff)) + 
  geom_bar(stat="identity", color="white",position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_acc-se, ymax=mean_acc+se),width=0.2,position=position_dodge(0.9))+
  ylim(c(0,1)) +
  ylab("Proportion of correct responses") + xlab("Group") + scale_fill_grey() +
  labs(fill="MOT difficulty") +
  scale_x_discrete(labels = c("CS" = "SRB",
                              "HS" = "HS",
                              "L2" = "ERB"))

ggsave(
  filename = "Fuchs-Fig2.png",
  plot = dualFig2,
  width = 6,         # width in inches
  height = 4,        # height in inches
  dpi = 600          # resolution
)

### ==============================================
### ANALYSIS MOT ACCURACY
### ==============================================

# prepare dataset for analysis
MOT_foranalysis <- single_bubbles_clean[single_bubbles_clean$group!="Gen1.5",]
MOT_foranalysis <- droplevels(MOT_foranalysis)

# deviation-coding for group 
MOT_foranalysis$group <- as.factor(MOT_foranalysis$group)
contrasts(MOT_foranalysis$group) = contr.sum(3) #  contrast1=CS; contrast2=HS
# create new level so we can see results for L2s
MOT_foranalysis$group_relevel <- relevel(MOT_foranalysis$group, ref="L2")
contrasts(MOT_foranalysis$group_relevel) = contr.sum(3)  #  contrast1=L2; contrast2=CS

# forward-difference coding for MOT difficulty levels
mot.forward.diff = matrix(c(2/3, -1/3, -1/3, 1/3, 1/3, -2/3), ncol = 2)
mot.forward.diff
contrasts(MOT_foranalysis$bub_diff) = mot.forward.diff

# mixed effects logistic regressions models
bub_glm1 <- glmer(bub_corr ~ group*bub_diff + (1+bub_diff|subject), 
                  data = MOT_foranalysis,
                  family = binomial) # does not converge
# model with Trial_Index_ (scaled) also did not converge
bub_glm1 <- glmer(bub_corr ~ group*bub_diff + (1|subject), 
                  data = MOT_foranalysis,
                  family = binomial)
    summary(bub_glm1)
    # main effect of 1 vs 2 bubbles
    # main effect of 2 vs 3 bubbles
    # interaction of CS and 1vs2 bubbles (marginal with 2vs3)

# same model but re-leveled group predictor to test for L2 contrast:
bub_glm1_relevel <- glmer(bub_corr ~ group_relevel*bub_diff + (1|subject), 
                          data = MOT_foranalysis,
                          family = binomial) 
summary(bub_glm1_relevel)
# significant interaction of L2 and 1vs2 bubbles

## suggests we should UNPACK by splitting by group:
    ## CSs
    bub_glm1_CS <- glmer(bub_corr ~ bub_diff + (1+bub_diff|subject), 
                         data = MOT_foranalysis[MOT_foranalysis$group=="CS",],
                         family = binomial) # is singular
                        # but this is likely due to high correlation between 
                        # random slopes, can still report
    summary(bub_glm1_CS) # effects of 1vs2 (B=2.4) and 2vs3 (B=0.7)
    ## HSs
    bub_glm1_HS <- glmer(bub_corr ~ bub_diff + (1+bub_diff|subject), 
                         data = MOT_foranalysis[MOT_foranalysis$group=="HS",],
                         family = binomial) 
    summary(bub_glm1_HS) # effects of 1vs2 (B=2.3) and 2vs3 (B=0.6)
    ## L2s
    bub_glm1_L2 <- glmer(bub_corr ~ bub_diff + (1+bub_diff|subject), 
                         data = MOT_foranalysis[MOT_foranalysis$group=="L2",],
                         family = binomial) 
    summary(bub_glm1_L2) # effects of 1vs2 (B=1.0) and 2vs3 (B=0.4)

## ===================================
## (LINGUISTIC TASK) SINGLE-TASK PICTURE-MATCHING: CLEANING
## ===================================

# take out outliers (same as before, anything faster than 150ms
# and anything outside of 2SDs of the mean by participant by condition)
single_audio <- singletaskraw %>% filter(condition=="audio only")
dim(single_audio) #16680    
single_audio_clean <- single_audio %>% 
  filter(reaction_time > 150) %>%
  group_by(subject, aud_diff) %>% 
  filter(abs(reaction_time - mean(reaction_time)) < (sd(reaction_time) * 2.5))
dim(single_audio_clean) #16207    
(16680-16207)/16680 # removed 2.8% of data

single_audio_clean <- single_audio_clean %>% mutate(aud_diff=factor(aud_diff,levels=c("RR","SR","OR")))

## ===================================
## PICTURE-MATCHING ACCURACY PLOT
## ===================================

# create new table with accuracies per participant per condition
audio_accuracy <- single_audio_clean %>%
  group_by(group, subject, aud_diff) %>%
  summarise(accuracy=mean(aud_corr))

# Get an overall barplot from this
aud_sum <- audio_accuracy %>% group_by(group, aud_diff) %>% 
  summarize(mean_acc=mean(accuracy),
            se=sqrt(mean(accuracy)*(1-mean(accuracy))/length(accuracy)))

dualFig3 <- aud_sum %>% filter(group!= "Gen1.5") %>% ggplot(aes(x=group,y=mean_acc,fill=aud_diff)) + 
  geom_bar(stat="identity", color="white",position=position_dodge()) +
  geom_errorbar(aes(ymin=mean_acc-se, ymax=mean_acc+se),width=0.2,position=position_dodge(0.9))+
  ylim(c(0,1)) +
  ylab("Proportion of correct responses to audio trials") + xlab("Group") + 
  scale_fill_grey() + labs(fill="RC type") + 
  scale_x_discrete(labels = c("CS" = "SRB",
                              "HS" = "HS",
                              "L2" = "ERB"))

ggsave(
  filename = "Fuchs-Fig3.png",
  plot = dualFig3,
  width = 6,         # width in inches
  height = 4,        # height in inches
  dpi = 600          # resolution
)

### ==============================================
### SINGLE-TASK PICTURE-MATCHING ACCURACY: ANALYSIS
### ==============================================

LING_foranalysis <- single_audio_clean
LING_foranalysis <- LING_foranalysis[LING_foranalysis$group!= "Gen1.5",]
LING_foranalysis <- droplevels(LING_foranalysis)

# prepare dataset for analysis
# deviation-coding for group 
LING_foranalysis$group <- as.factor(LING_foranalysis$group)
contrasts(LING_foranalysis$group) = contr.sum(3) #  contrast1=CS; contrast2=HS
# create new level so we can see results for L2s
LING_foranalysis$group_relevel <- relevel(LING_foranalysis$group, ref="L2")
contrasts(LING_foranalysis$group_relevel) = contr.sum(3)  #  contrast1=L2; contrast2=CS

# forward-difference coding for difficulty levels
my.forward.diff = matrix(c(2/3, -1/3, -1/3, 1/3, 1/3, -2/3), ncol = 2)
my.forward.diff
contrasts(LING_foranalysis$aud_diff) = my.forward.diff

# logistic regression models
aud_glm1 <- glmer(aud_corr ~ group*aud_diff + 
                    (1+aud_diff|subject), 
                  data = LING_foranalysis,
                  family = binomial) # is singular
aud_glm1 <- glmer(aud_corr ~ group*aud_diff + 
                    (1+aud_diff||subject), 
                  data = LING_foranalysis,
                  family = binomial) # failed to converge
aud_glm1 <- glmer(aud_corr ~ group*aud_diff + 
                    (1|subject), 
                  data = LING_foranalysis,
                  family = binomial) # failed to converge
aud_glm1 <- glmer(aud_corr ~ group*aud_diff + 
                    (1|audiostimulus), 
                  data = LING_foranalysis,
                  family = binomial)
summary(aud_glm1)

    # relevel to check effect of L2
    aud_glm1_relevel <- glmer(aud_corr ~ group_relevel*aud_diff + 
                                (1|audiostimulus), 
                              data = LING_foranalysis,
                              family = binomial) 
    summary(aud_glm1_relevel)

# we find an interaction between group and RC type (aud_diff), so need to unpack:
    # CSs
    aud_glm1_CS <- glmer(aud_corr ~ aud_diff + (1|audiostimulus), 
                         data = LING_foranalysis[LING_foranalysis$group=="CS",],
                         family = binomial) 
    summary(aud_glm1_CS) # main effect of both RC levels
    
    # HSs
    aud_glm1_HS <- glmer(aud_corr ~ aud_diff + (1|audiostimulus), 
                         data = LING_foranalysis[LING_foranalysis$group=="HS",],
                         family = binomial) # is singular
    aud_glm1_HS <- glmer(aud_corr ~ aud_diff + (1|subject), 
                         data = LING_foranalysis[LING_foranalysis$group=="HS",],
                         family = binomial) 
    summary(aud_glm1_HS)  # main effect of both RC levels
    
    # L2s
    aud_glm1_L2 <- glmer(aud_corr ~ aud_diff + (1|audiostimulus) + (1|subject), 
                         data = LING_foranalysis[LING_foranalysis$group=="L2",],
                         family = binomial) 
    summary(aud_glm1_L2)

## ===========================================================
# post-hoc pairwise comparisons in Section 4.2 of manuscript:
## ===========================================================
pairs(emmeans(aud_glm1,"group", by="aud_diff"))

