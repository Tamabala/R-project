library(reshape2)
library(cowplot)
library(ggplot2)
library(dplyr)
library(asbio)
library(bruceR)

source("R_rainclouds.R")

path = r'(D:\MATLAB\a_data\octa_glm\anal\version_1\)'
data = read.csv(paste(path,'peaks.csv',sep = ""),skip = 1,header = 0)
data$subject = paste0('sub',1:24)

# names(data) = c("individual","local","global")

b = melt(data,id.var = 'subject', variable.name = 'condition',value.name = 'latency')

m = aggregate(latency ~ condition, data = b, FUN = mean)
v = aggregate(latency ~ condition, data = b, FUN = sd)

fit3 = aov(latency ~ condition + Error(subject/condition), data = b)

summary(fit3)

multicomp = pairwise.t.test(b$latency, b$condition, p.adjust.method = "bonferroni", paired = 1)

c = data[, -4]
a = MANOVA(c, dvs="V1:V3", dvs.pattern="V(.)",within="V",sph.correction="HF")
p = EMMEANS(a, effect = "V", p.adjust = "bonferroni")

data <- data.frame('group'=c(rep("c",30),rep("t",30),rep("v",30)),
                  'shannon'=c(rnorm(30,20,3),rnorm(30,15,3),rnorm(30,10,3)))

ggplot(b,aes(x=condition,y=latency))+
  geom_flat_violin(position = position_nudge(x=.2, y=0),adjust = 1)+
  # geom_boxplot(width=.05,position = position_nudge(x=.05),outlier.colour =NA)+
  stat_boxplot(geom="errorbar",width=0.05, size=0.5, position =position_nudge(x=.15),color="black")+
  geom_boxplot(position = position_nudge(x=.15),width = .05) +
  # geom_dotplot(binaxis = "y",binwidth = 0.1,stackdir = "down",dotsize = 0.01)+
  geom_point(position = position_jitter(width = .2, height = 0), size = 1)+
  # ylab('Score')+xlab('Group')+coord_flip()+theme_cowplot()+guides(fill = FALSE)+
  coord_flip()+
  theme_bw()

