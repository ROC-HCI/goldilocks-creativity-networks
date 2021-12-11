library(lsmeans)
library(emmeans)
library(ARTool)
library(phia)
library(lme4) 
library(lmerTest)
library(car) 
library(multcomp)
library(reshape2)	
library(rstatix)
library(plyr)
library(ez)
library(dplyr)
library(xtable)
library(ggpubr)


######### Load data ######### 
df = read.csv("data/dxdydr.csv")
View(df)

# correlation test
x=df$dx #change in shown signals
y=df$dy #change in obtained followers
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

x=df$dx #change in shown signals
y=df$dr #change in obtained ratings
cor.test(x, y, method=c("pearson", "kendall", "spearman"))



######### Load data ######### 
df = read.csv("data/gini_analysis.csv")
View(df)
df$round = factor(df$round) # convert to nominal factor
df$tier = factor(df$tier) # convert to nominal factor
df$trial = factor(df$trial) # convert to nominal factor
df$condition = factor(df$condition) # convert to nominal factor
summary(df)


######### Plot with CI intervals ######### 
ggline(df, x = "condition", y = "gini", color = "tier", add = c("mean_ci"))


####### Linear Mixed Model ##########

m = lmer(gini ~ condition * tier + (1|round), data=df)
Anova(m, type=3, test.statistic="F")
# pairwise 2-tailed-tests
summary(glht(m, emm(pairwise ~ condition * tier)), test=adjusted(type="holm"))
