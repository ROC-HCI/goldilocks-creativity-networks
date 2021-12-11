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

######### Load data: Non redun, individual ######### 
df = read.csv("data/redundancy.csv")
View(df)
df$condition = factor(df$condition) # convert to nominal factor
df$roundID = factor(df$roundID) # convert to nominal factor
summary(df)



# correlation test
x=df$ego_ego_overlap
y=df$Non_redun
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

x=df$ego_ego_overlap
y=df$CQ
cor.test(x, y, method=c("pearson", "kendall", "spearman"))


x=df$ego_alter_overlap
y=df$Non_redun
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

x=df$ego_alter_overlap
y=df$CQ
cor.test(x, y, method=c("pearson", "kendall", "spearman"))



# Kruskal-Wallis test
kruskal_test(ego_ego_overlap ~ condition, data=df, distribution="asymptotic") # can't do exact with 3 levels
nrow(df) # for reporting Kruskal-Wallis as chi-square, we can get N with nrow(ide3)

# manual post hoc Mann-Whitney U pairwise comparisons
# note: wilcox_test we used above doesn't take two data vectors, so use wilcox.test
b.s = wilcox.test(df[df$condition == "control",]$ego_ego_overlap, df[df$condition == "real",]$ego_ego_overlap, exact=FALSE)
b.p = wilcox.test(df[df$condition == "control",]$ego_ego_overlap, df[df$condition == "mid",]$ego_ego_overlap, exact=FALSE)
b.n = wilcox.test(df[df$condition == "control",]$ego_ego_overlap, df[df$condition == "reverse",]$ego_ego_overlap, exact=FALSE)
s.p = wilcox.test(df[df$condition == "real",]$ego_ego_overlap, df[df$condition == "mid",]$ego_ego_overlap, exact=FALSE)
s.n = wilcox.test(df[df$condition == "real",]$ego_ego_overlap, df[df$condition == "reverse",]$ego_ego_overlap, exact=FALSE)
p.n = wilcox.test(df[df$condition == "mid",]$ego_ego_overlap, df[df$condition == "reverse",]$ego_ego_overlap, exact=FALSE)
p.adjust(c(b.s$p.value, b.p$p.value, b.n$p.value, s.p$p.value, s.n$p.value, p.n$p.value), method="holm")
b.s
b.p
b.n
s.p
s.n
p.n
summary(df[df$condition == "control",])
summary(df[df$condition == "real",])
summary(df[df$condition == "mid",])
summary(df[df$condition == "reverse",])



# Kruskal-Wallis test
kruskal_test(ego_alter_overlap ~ condition, data=df, distribution="asymptotic") # can't do exact with 3 levels
nrow(df) # for reporting Kruskal-Wallis as chi-square, we can get N with nrow(ide3)

# manual post hoc Mann-Whitney U pairwise comparisons
# note: wilcox_test we used above doesn't take two data vectors, so use wilcox.test
b.s = wilcox.test(df[df$condition == "control",]$ego_alter_overlap, df[df$condition == "real",]$ego_alter_overlap, exact=FALSE)
b.p = wilcox.test(df[df$condition == "control",]$ego_alter_overlap, df[df$condition == "mid",]$ego_alter_overlap, exact=FALSE)
b.n = wilcox.test(df[df$condition == "control",]$ego_alter_overlap, df[df$condition == "reverse",]$ego_alter_overlap, exact=FALSE)
s.p = wilcox.test(df[df$condition == "real",]$ego_alter_overlap, df[df$condition == "mid",]$ego_alter_overlap, exact=FALSE)
s.n = wilcox.test(df[df$condition == "real",]$ego_alter_overlap, df[df$condition == "reverse",]$ego_alter_overlap, exact=FALSE)
p.n = wilcox.test(df[df$condition == "mid",]$ego_alter_overlap, df[df$condition == "reverse",]$ego_alter_overlap, exact=FALSE)
p.adjust(c(b.s$p.value, b.p$p.value, b.n$p.value, s.p$p.value, s.n$p.value, p.n$p.value), method="holm")
b.s
b.p
b.n
s.p
s.n
p.n
summary(df[df$condition == "control",])
summary(df[df$condition == "real",])
summary(df[df$condition == "mid",])
summary(df[df$condition == "reverse",])



