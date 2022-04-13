library(ggplot2)
library(reshape2)

path = r'(D:\MATLAB\a_data\octa_glm\anal\version_1\)'
# data = readMat(paste(path,'rsps.mat',sep = ""))

data = read.csv(paste(path,'rs.csv',sep = ""))

idx = list(c(1,2,3), c(4,5), c(6,7))

i = 3
rs = data[,idx[[i]]]
rs = round(rs, 3)
rs$mode = c('individual','local','global')
if(i==3){
  rs$rs2_3 = c(NA, NA, NA)
}
r = melt(rs, id.vars = 'mode')

r$mode = factor(r$mode,levels=c('individual','local','global'))

ggplot(r, aes(mode, value, fill=variable)) +
  geom_bar(stat = "identity", position=position_dodge(0.8), width= 0.7)+
  
  # scale_x_continuous(name = "Time(s)",
  #                    breaks = c(1,100,200,300,400,500),
  #                    labels = c('-0.2','0','0.2','0.4','0.6','0.8'),
  #                    expand = c(0,0))+
  
  scale_y_continuous(name = "R",
                     breaks = c(-0.6,-0.3,0,0.3,0.6),
                     labels = c('-0.6','-0.3','0','0.3','0.6'),
                     limits = c(-0.6,0.6),
                     expand = c(0,0))+
  
  scale_fill_manual(values = c('#ef8a62','#0571b0' ,'#67a9cf'))+
  
  # '#ef8a62', '#67a9cf', '#0571b0'
  
  # '#d8b365', '#5ab4ac', '#018571'
  
  # '#1b9e77', '#d95f02', '#7570b3'
  
  # '#66c2a5', '#fc8d62', '#8da0cb'
  #
  # '#b2df8a', '#a6cee3', '#1f78b4'
  #
  # theme_classic    '#009E73','#56B4E9','#CC79A7' '#D55E00','#0072B2','#F0E442')
  theme_classic()+
  theme( # axis.title=element_text(size=10,face="plain",color="black"),
    # axis.text=element_text(size=10,face="plain",color="black"),
    # legend.position = c(0.15,0.8),
    axis.line=element_line(colour="black"),
    panel.background = element_rect(fill="transparent",colour=NA),
    legend.position = c(0.2,0.9),
    axis.ticks.length = unit(-0.2, "cm"),
    axis.title = element_blank())
