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
df = read.csv("data/nonredun.csv")
View(df)
df$condition = factor(df$condition) # convert to nominal factor
summary(df)

## Nonparametric equivalent of one-way ANOVA

# Kruskal-Wallis test
library(coin)
kruskal_test(nonredun_score ~ condition, data=df, distribution="asymptotic") # can't do exact with 3 levels
nrow(df) # for reporting Kruskal-Wallis as chi-square, we can get N with nrow(ide3)

# manual post hoc Mann-Whitney U pairwise comparisons
# note: wilcox_test we used above doesn't take two data vectors, so use wilcox.test
b.s = wilcox.test(df[df$condition == "control",]$nonredun_score, df[df$condition == "real",]$nonredun_score, exact=FALSE)
b.p = wilcox.test(df[df$condition == "control",]$nonredun_score, df[df$condition == "mid",]$nonredun_score, exact=FALSE)
b.n = wilcox.test(df[df$condition == "control",]$nonredun_score, df[df$condition == "reverse",]$nonredun_score, exact=FALSE)
s.p = wilcox.test(df[df$condition == "real",]$nonredun_score, df[df$condition == "mid",]$nonredun_score, exact=FALSE)
s.n = wilcox.test(df[df$condition == "real",]$nonredun_score, df[df$condition == "reverse",]$nonredun_score, exact=FALSE)
p.n = wilcox.test(df[df$condition == "mid",]$nonredun_score, df[df$condition == "reverse",]$nonredun_score, exact=FALSE)
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


######### Load data: CQ, individual ######### 
df = read.csv("data/cq.csv")
View(df)
df$condition = factor(df$condition) # convert to nominal factor
summary(df)

# Kruskal-Wallis test
kruskal_test(cq_score ~ condition, data=df, distribution="asymptotic") # can't do exact with 3 levels
nrow(df) # for reporting Kruskal-Wallis as chi-square, we can get N with nrow(ide3)

# manual post hoc Mann-Whitney U pairwise comparisons
# note: wilcox_test we used above doesn't take two data vectors, so use wilcox.test
b.s = wilcox.test(df[df$condition == "control",]$cq_score, df[df$condition == "real",]$cq_score, exact=FALSE)
b.p = wilcox.test(df[df$condition == "control",]$cq_score, df[df$condition == "mid",]$cq_score, exact=FALSE)
b.n = wilcox.test(df[df$condition == "control",]$cq_score, df[df$condition == "reverse",]$cq_score, exact=FALSE)
s.p = wilcox.test(df[df$condition == "real",]$cq_score, df[df$condition == "mid",]$cq_score, exact=FALSE)
s.n = wilcox.test(df[df$condition == "real",]$cq_score, df[df$condition == "reverse",]$cq_score, exact=FALSE)
p.n = wilcox.test(df[df$condition == "mid",]$cq_score, df[df$condition == "reverse",]$cq_score, exact=FALSE)
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




######### Load data: distinct count, collective ######### 
df = read.csv("data/group_distinct.csv")
View(df)
df$condition = factor(df$condition) # convert to nominal factor
summary(df)

# Kruskal-Wallis test
kruskal_test(distinct_count ~ condition, data=df, distribution="asymptotic") # can't do exact with 3 levels
nrow(df) # for reporting Kruskal-Wallis as chi-square, we can get N with nrow(ide3)

# manual post hoc Mann-Whitney U pairwise comparisons
# note: wilcox_test we used above doesn't take two data vectors, so use wilcox.test
b.s = wilcox.test(df[df$condition == "control",]$distinct_count, df[df$condition == "real",]$distinct_count, exact=FALSE)
b.p = wilcox.test(df[df$condition == "control",]$distinct_count, df[df$condition == "mid",]$distinct_count, exact=FALSE)
b.n = wilcox.test(df[df$condition == "control",]$distinct_count, df[df$condition == "reverse",]$distinct_count, exact=FALSE)
s.p = wilcox.test(df[df$condition == "real",]$distinct_count, df[df$condition == "mid",]$distinct_count, exact=FALSE)
s.n = wilcox.test(df[df$condition == "real",]$distinct_count, df[df$condition == "reverse",]$distinct_count, exact=FALSE)
p.n = wilcox.test(df[df$condition == "mid",]$distinct_count, df[df$condition == "reverse",]$distinct_count, exact=FALSE)
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



######### Load data: CQ, collective ######### 
df = read.csv("data/group_cq.csv")
View(df)
df$condition = factor(df$condition) # convert to nominal factor
summary(df)

# Kruskal-Wallis test
kruskal_test(group_cq_score ~ condition, data=df, distribution="asymptotic") # can't do exact with 3 levels
nrow(df) # for reporting Kruskal-Wallis as chi-square, we can get N with nrow(ide3)

# manual post hoc Mann-Whitney U pairwise comparisons
# note: wilcox_test we used above doesn't take two data vectors, so use wilcox.test
b.s = wilcox.test(df[df$condition == "control",]$group_cq_score, df[df$condition == "real",]$group_cq_score, exact=FALSE)
b.p = wilcox.test(df[df$condition == "control",]$group_cq_score, df[df$condition == "mid",]$group_cq_score, exact=FALSE)
b.n = wilcox.test(df[df$condition == "control",]$group_cq_score, df[df$condition == "reverse",]$group_cq_score, exact=FALSE)
s.p = wilcox.test(df[df$condition == "real",]$group_cq_score, df[df$condition == "mid",]$group_cq_score, exact=FALSE)
s.n = wilcox.test(df[df$condition == "real",]$group_cq_score, df[df$condition == "reverse",]$group_cq_score, exact=FALSE)
p.n = wilcox.test(df[df$condition == "mid",]$group_cq_score, df[df$condition == "reverse",]$group_cq_score, exact=FALSE)
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
