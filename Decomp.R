#Sargassum Decomposition Experiment
install.packages("ggplot2")
library(ggplot2)
file.choose()
avg.data.n = read.csv("/Users/daniellehatt/Desktop/Decomp experiment/Nut.Decomp.Avg.csv", stringsAsFactors = FALSE)
avg.data.n
nit.plot= ggplot(avg.data.n, aes(x=Time,y=N))+ geom_point(aes(color=Species)) +
  stat_smooth(method = "lm") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "black"))
nit.plot
data= data.frame(cbind())

data.nut= read.csv("/Users/daniellehatt/Desktop/Decomp experiment/Nutrient_Decomp.csv")
data.nut
nit.plot2= ggplot(data.nut, aes(x=Time,y=X.N))+ geom_point(aes(color=Species)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "black"))
nit.plot2
car.plot2= ggplot(data.nut, aes(x=Time,y=X.C))+ geom_point(aes(color=Species)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "black"))
car.plot2
pho.plot2= ggplot(data.nut, aes(x=Time,y=X.P))+ geom_point(aes(color=Species)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "black"))
pho.plot2

nit.plot3= ggplot(data.nut, aes(x=Time,y=X.N, fill=Species))+ geom_boxplot() + ylab("% Nitrogen") + xlab("") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "black"))
nit.plot3
car.plot3= ggplot(data.nut, aes(x=Time,y=X.C, fill=Species))+ geom_boxplot() + ylab("% Carbon") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "black"))
car.plot3
pho.plot3= ggplot(data.nut, aes(x=Time,y=X.P, fill=Species))+ geom_boxplot() + ylab("% Phosphorus") + xlab("") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(color = "black"))
pho.plot3

install.packages("ggpubr")
library(ggpubr)
ggarrange(nit.plot3, car.plot3, pho.plot3, labels= c("A", "B", "C"), ncol=3, nrow=1, common.legend = TRUE, legend = "bottom")





