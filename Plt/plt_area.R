# plt 

# install.packages('installr')
# require(installr)
# updateR()

library(ggplot2)
library(ggridges)
library(R.matlab)
library(RColorBrewer)

models = list('single','local','inte')
path = r'(D:\MATLAB\a_data\octa_glm\newdata\)'

rsquare <- list()
for (i in 1:length(models)){
  mode = models[i]
  dat = readMat(paste(path,mode,'.mat',sep = ""))
  rsquare[[i]] <- dat[[1]]
}

single = rsquare[models =='single'][[1]]
local = rsquare[models =='local'][[1]]
inte = rsquare[models =='inte'][[1]]

#####
avg = rowMeans(single)
se = apply(single,1,sd)/sqrt(ncol(single))
top = avg + se
bot = avg - se
mydata = data.frame(top=top, bot=bot)
  
ggplot(mydate, aes(x=1:500)) +
  geom_ribbon(aes(ymin=bot, ymax=top),alpha=0.5,fill="grey70",color=NA)+
  geom_line(aes(y=avg,color="00B2F6"),size=0.75)+

  # scale_x_continuous(breaks=c(-0.2,0,0.2,0.4,0.6,0.8))+
  # xlim(-0.2,0.8)
  # labs()
  scale_x_continuous(name = "Time(s)",
                     breaks = c(1,100,200,300,400,500),
                     labels = c('-0.2','0','0.2','0.4','0.6','0.8'),
                     expand = c(0,0))+

  scale_y_continuous(name = "Rsquare",
                     breaks = c(0,0.01,0.02,0.03),
                     labels = c('0','0.01','0.02','0.03'),
                     limits = c(-0.0015,0.03),
                     expand = c(0,0))+

  theme( # axis.title=element_text(size=10,face="plain",color="black"),
         # axis.text=element_text(size=10,face="plain",color="black"),
         # legend.position = c(0.15,0.8),
         axis.line=element_line(colour="black"),
         panel.background = element_rect(fill="transparent",colour=NA),
         legend.position = 'none',
         axis.ticks.length = unit(-0.2, "cm"))

#####
avg = rowMeans(local)
se = apply(local,1,sd)/sqrt(ncol(local))
top = avg + se
bot = avg - se

ggplot(data = NULL, aes(x=1:500)) +
  geom_ribbon(aes(ymin=bot, ymax=top),alpha=0.5,fill="grey70",color=NA)+
  geom_line(aes(y=avg,color="00B2F6"),size=0.75)+
  
  # scale_x_continuous(breaks=c(-0.2,0,0.2,0.4,0.6,0.8))+
  # xlim(-0.2,0.8)
  # labs()
  scale_x_continuous(name = "Time(s)",
                     breaks = c(1,100,200,300,400,500),
                     labels = c('-0.2','0','0.2','0.4','0.6','0.8'),
                     expand = c(0,0))+
  
  scale_y_continuous(name = "Rsquare",
                     breaks = c(0,0.002,0.004,0.006),
                     # labels = c('0','0.002','0.004','0.006'),
                     limits = c(-0.001,0.006),
                     expand = c(0,0))+
  
  theme( # axis.title=element_text(size=10,face="plain",color="black"),
    # axis.text=element_text(size=10,face="plain",color="black"),
    # legend.position = c(0.15,0.8),
    axis.line=element_line(colour="black"),
    panel.background = element_rect(fill="transparent",colour=NA),
    panel.grid = element_blank(),
    # panel.border=element_blank(),
    legend.position = 'none',
    axis.ticks.length = unit(-0.2, "cm"))
#####
avg = rowMeans(inte)
se = apply(inte,1,sd)/sqrt(ncol(inte))
top = avg + se
bot = avg - se

ggplot(data = NULL, aes(x=1:500)) +
  geom_ribbon(aes(ymin=bot, ymax=top),alpha=0.5,fill="grey70",color=NA)+
  geom_line(aes(y=avg,color="00B2F6"),size=0.75)+
  
  # scale_x_continuous(breaks=c(-0.2,0,0.2,0.4,0.6,0.8))+
  # xlim(-0.2,0.8)
  # labs()
  scale_x_continuous(name = "Time(s)",
                     breaks = c(1,100,200,300,400,500),
                     labels = c('-0.2','0','0.2','0.4','0.6','0.8'),
                     expand = c(0,0))+
  
  scale_y_continuous(name = "Rsquare",
                     breaks = c(0,0.002,0.004,0.006),
                     # labels = c('0','0.002','0.004','0.006'),
                     limits = c(-0.0005,0.006),
                     expand = c(0,0))+
  
  theme( # axis.title=element_text(size=10,face="plain",color="black"),
    # axis.text=element_text(size=10,face="plain",color="black"),
    # legend.position = c(0.15,0.8),
    axis.line=element_line(colour="black"),
    panel.background = element_rect(fill="transparent",colour=NA),
    panel.grid = element_blank(),
    panel.border=element_blank(),
    legend.position = 'none',
    axis.ticks.length = unit(-0.2, "cm"))

