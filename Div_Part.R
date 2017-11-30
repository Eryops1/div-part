#clean routine
rm (list=ls())

####Package Installer####
#delete "#" if needed#
#install.packages("ggplot2")
#install.packages("ggthemes")
#install.packages("plyr")
#install.packages("gridExtra")

####load installed libaries####
library(ggplot2)
library(ggthemes)
library(plyr)
library(gridExtra)

####Read the Data####
#local if downloaded to your computer
#Data <- read.csv("//diversities.csv", row.names = NULL, sep=",")
#web
Data <- read.csv(url("https://raw.githubusercontent.com/fossilrich/div-part/master/diversities.csv"), row.names = NULL, sep=";")


#add factor Period which is important for later grouping
attach(Data)
Data$Period[Age < 550 & Age > 485.4] <- "Cambrian"
Data$Period[Age < 485.4 & Age > 443.8] <- "Ordovician"
Data$Period[Age < 443.8 & Age > 419.2] <- "Silurian"
Data$Period[Age < 419.2 & Age > 358.9] <- "Devonian"
Data$Period[Age < 358.9 & Age > 298] <- "Carboniferous"
Data$Period[Age < 298.9 & Age > 252.17] <- "Permian"
Data$Period[Age < 252.17 & Age > 201.3] <- "Triassic"
Data$Period[Age < 201.3 & Age > 145.0] <- "Jurassic"
Data$Period[Age < 145.0 & Age > 66.0] <- "Cretaceous"
Data$Period[Age < 66.0 & Age > 23.03] <- "Paleogene"
Data$Period[Age < 23.03 & Age > 0] <- "Neogene"
detach(Data)
#make it factor
Data$Period.new = factor(Data$Period, levels=c("Cambrian","Ordovician","Silurian","Devonian", "Carboniferous", "Permian", "Triassic", "Jurassic", "Cretaceous", "Paleogene", "Neogene"))

####FIGURE 1####
boundaries <- c(485.4, 443.8, 419.2, 358.9, 298.9, 252.17, 201.3, 145, 66, 23.03)
extinctions <- c(444, 372, 252, 201.5, 66)
alpha <- .3
Alphaplot <- ggplot(Data, aes(x=Age, y=AlphaForm)) + 
  geom_vline(xintercept=boundaries, colour="black", linetype="dashed")+
  geom_vline(xintercept=extinctions, colour="darkgrey", linetype="solid", alpha = 0.4, size = 3)+
  geom_point(shape = 19, size = 3, colour = "red3", alpha = 0.3) +
  geom_smooth(span = 0.15, se=TRUE, colour ="red3",  alpha = alpha)+
  labs(title = "A")+
  scale_x_continuous(trans = "reverse", name = "Age (Ma)", position = "top", expand = c(0,3), 
                     breaks = c(50,100,150,200,250,300,350,400,450,500))+
  scale_y_continuous(name = "Alpha", expand = c(0,0.5))+
  theme(panel.background = element_rect(fill = "white", colour = "black"), 
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank())

Gammaplot <- ggplot(Data, aes(x=Age, y=GammaForm)) + 
  geom_vline(xintercept=boundaries, colour="black", linetype="dashed")+
  geom_vline(xintercept=extinctions, colour="darkgrey", linetype="solid", alpha = 0.4, size = 3)+
  geom_point(shape = 19, size = 3, colour = "orange", alpha = alpha) +
  geom_smooth(span = 0.15, se=TRUE, colour ="orange", alpha = alpha)+
  labs(title = "B")+
  scale_x_continuous(trans = "reverse", expand = c(0,3))+
  #scale_x_continuous(name = "Period", trans = "reverse",breaks = c(513, 465, 431.5, 389, 329, 275, 227, 173.3, 100, 44.5, 2),#these are no exact ages, they just make the labels appear in the middle of each period
  #                   labels = c("Cambrian", "Ordovician", "Silurian", "Devonian", "Carboniferous", "Permian", "Triassic",
  #                              "Jurassic", "Cretaceous", "Palaeogene", "Neogene"))+
  scale_y_continuous(name = "Gamma", expand = c(0,0.5))+
  theme(axis.ticks.x = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black"), 
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank())

Betaplot <- ggplot(Data, aes(x=Age, y=BetaWForm)) + 
  geom_vline(xintercept=boundaries, colour="black", linetype="dashed")+
  geom_vline(xintercept=extinctions, colour="darkgrey", linetype="solid", alpha = 0.4, size = 3)+
  geom_smooth(span = 0.15, se=TRUE, colour ="blue", alpha = alpha)+
  geom_point(shape = 19, size = 3, colour = "darkblue", alpha = alpha) +
  labs(title = "C")+
  scale_x_continuous(name = "Period", trans = "reverse", expand = c(0,3),
                     breaks = c(502, 465, 431.5, 389, 329, 275, 227, 173.3, 105, 44.5, 8),#these are no exact ages, they just make the labels appear in the middle of each period
                     labels = c("Cambrian", "Ordovician", "Silurian", "Devonian", "Carboniferous",
                                "Permian", "Triassic", "Jurassic", "Cretaceous", "Palaeogene", "Neogene"))+
  scale_y_continuous(name = "Whittaker's beta", expand = c(0,0.5))+
  theme(axis.ticks.x = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black"), 
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank(), 
        panel.ontop = FALSE,
        axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(Alphaplot, Gammaplot, Betaplot, ncol=1, left="Mean diversity per formation")

#ggsave("figure1.png", width=12, height=22, dpi = 300, units = "cm",
#       grid.arrange(Alphaplot, Gammaplot, Betaplot, ncol=1, left="Mean diversity per formation"))
#ggsave("figure1.pdf", width=12, height=22, units = "cm",
#       grid.arrange(Alphaplot, Gammaplot, Betaplot, ncol=1, left="Mean diversity per formation"))

####FIGURE 2####
ggplot(Data, aes(x=GammaForm, y=BetaWForm)) +
  stat_smooth(method="loess", span = 1, colour="darkblue", se = TRUE, alpha = 0.3)+
  geom_point(shape=19, size = 3, alpha = 0.2, colour = "darkblue")+
  stat_smooth(method="loess", span = 1, colour="red3", se = TRUE, alpha = 0.3, aes(x=GammaForm, y=AlphaForm)) +
  geom_point(shape=19, size = 3, alpha = 0.2, colour = "red3", data = Data, aes(x=GammaForm, y=AlphaForm))+
  scale_x_continuous(name = "mean gamma diversity per formation", expand = c(0,1)) +
  scale_y_continuous(name = "mean alpha (red) and beta (blue) diversity per formation", expand = c(0,1))+
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank())

#ggsave(("figure2.png"), plot = last_plot(), width=12, height=12, units = "cm", dpi = 300)
#ggsave(("figure2.pdf"), plot = last_plot(), width=12, height=12, units = "cm")

####FIGURE 3####
ggplot(Data, aes(x=GammaForm, y=BetaWForm)) +
  stat_smooth(method="lm", formula = y ~ log(x), colour="blue", se = TRUE, alpha = 0.4)+
  geom_point(shape=19, size = 2, alpha = 0.4, colour = "darkblue")+
  stat_smooth(method="lm", colour="red3", se = TRUE, alpha = 0.4, aes(x=GammaForm, y=AlphaForm)) +
  geom_point(shape=19, size = 2, alpha = 0.4, colour = "red3", data = Data, aes(x=GammaForm, y=AlphaForm))+
  scale_x_continuous(name = "mean gamma diversity per formation",  expand = c(0,1)) +
  scale_y_continuous(name = "mean alpha (red) and beta (blue) diversity per formation",  expand = c(0,1))+  
  facet_wrap( ~ Period.new, scales = "free")+
  theme(panel.background = element_rect(fill = "white", colour = "black"), 
        strip.background = element_rect(colour ="black", fill="grey90"), 
        strip.text.x = element_text(face = "bold"), 
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank())

#ggsave(("figure3.png"), plot = last_plot(), width=20, height=19, units = "cm", dpi = 300)
#ggsave(("figure3.pdf"), plot = last_plot(), width=20, height=19, units = "cm")

####SUPPLEMENTARY MATERIAL####
ggplot(subset(Data, RefForm < 15), aes(x=RefForm, y=GammaForm)) +
  geom_point(shape=16) +
  geom_smooth(method=lm,   # Add linear regression lines
              se=TRUE)    # Don"t add shaded confidence region


####Spearman's with all Data####
cor.test(Data$GammaForm,Data$RefForm, method="spearman", exact=FALSE) #0.427
cor.test(Data$AlphaForm,Data$RefForm, method="spearman", exact=FALSE) #0.212
cor.test(Data$BetaWForm,Data$RefForm, method="spearman", exact=FALSE) #0.445
cor.test(Data$BetaSForm,Data$RefForm, method="spearman", exact=FALSE) #0.457
cor.test(Data$BetaJForm,Data$RefForm, method="spearman", exact=FALSE) #0.441
# -> Reference per Formation influences the signal, too large to ingore

cor.test(Data$GammaForm,Data$Duration, method="spearman", exact=FALSE) #0.166
cor.test(Data$AlphaForm,Data$Duration, method="spearman", exact=FALSE) #-0.005
cor.test(Data$BetaWForm,Data$Duration, method="spearman", exact=FALSE) #0.361
cor.test(Data$BetaSForm,Data$Duration, method="spearman", exact=FALSE) #0.336
cor.test(Data$BetaJForm,Data$Duration, method="spearman", exact=FALSE) #0.319
# -> Duration no influence except, maybe on Beta

cor.test(Data$GammaForm,Data$CollpF, method="spearman", exact=FALSE) #0.166
cor.test(Data$AlphaForm,Data$CollpF, method="spearman", exact=FALSE) #0.088
cor.test(Data$BetaWForm,Data$CollpF, method="spearman", exact=FALSE) #0.154
cor.test(Data$BetaSForm,Data$CollpF, method="spearman", exact=FALSE) #0.194
cor.test(Data$BetaJForm,Data$CollpF, method="spearman", exact=FALSE) #0.197
# -> CollpF no influence 

cor.test(Data$GammaForm,Data$Environments, method="spearman", exact=FALSE) #0.332
cor.test(Data$AlphaForm,Data$Environments, method="spearman", exact=FALSE) #0.208
cor.test(Data$BetaWForm,Data$Environments, method="spearman", exact=FALSE) #0.245
cor.test(Data$BetaSForm,Data$Environments, method="spearman", exact=FALSE) #0.269
cor.test(Data$BetaJForm,Data$Environments, method="spearman", exact=FALSE) #0.260
# -> Number of environments no influence

####Subsetting Data####
Data2 <- subset(Data, RefForm < 10)

cor.test(Data2$GammaForm,Data2$RefForm, method="spearman") #0.411, p < 0.05
cor.test(Data2$AlphaForm,Data2$RefForm, method="spearman") #0.220, p < 0.05
cor.test(Data2$BetaWForm,Data2$RefForm, method="spearman") #0.373, p < 0.05
cor.test(Data2$BetaSForm,Data2$RefForm, method="spearman") #0.386, p < 0.05
cor.test(Data2$BetaJForm,Data2$RefForm, method="spearman") #0.374, p < 0.05

####Plotting Subset and Original Data####



####FIGURE S1####
boundaries <- c(485.4, 443.8, 419.2, 358.9, 298.9, 252.17, 201.3, 145, 66, 23.03)
extinctions <- c(444, 372, 252, 201.5, 66)
alpha <- .1
Alphaplot_S <- ggplot(Data, aes(x=Age, y=AlphaForm)) + 
  geom_vline(xintercept=boundaries, colour="black", linetype="dashed")+
  geom_point(shape = 19, size = 3, colour = "red3", alpha = alpha) +
  geom_smooth(span = 0.15, se=TRUE, colour ="red3",  alpha = alpha)+
  geom_point(shape = 21, size = 3, colour = "black",  data = Data2, aes(x=Age, y=AlphaForm))+
  geom_smooth(span = 0.15, se=TRUE, colour ="darkgrey", data = Data2, alpha = 0.2)+
  geom_smooth(span = 0.15, se=TRUE, colour ="red3")+
  labs(title = "A")+
  scale_x_continuous(trans = "reverse", name = "Age (Ma)", position = "top", expand = c(0,3), 
                     breaks = c(50,100,150,200,250,300,350,400,450,500))+
  scale_y_continuous(name = "Alpha", expand = c(0,0.5))+
  theme(panel.background = element_rect(fill = "white", colour = "black"), 
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank())


Gammaplot_S <- ggplot(Data, aes(x=Age, y=GammaForm)) + 
  geom_vline(xintercept=boundaries, colour="black", linetype="dashed")+
  geom_point(shape = 19, size = 3, colour = "orange", alpha = alpha) +
  geom_smooth(span = 0.15, se=TRUE, colour ="orange", alpha = alpha)+
  geom_point(shape = 21, size = 3, colour = "black",  data = Data2, aes(x=Age, y=GammaForm))+
  geom_smooth(span = 0.15, se=TRUE, colour ="darkgrey", data = Data2, alpha = 0.2)+
  
  geom_smooth(span = 0.15, se=TRUE, colour ="orange")+
  labs(title = "B")+
  scale_x_continuous(trans = "reverse", expand = c(0,3))+
  scale_y_continuous(name = "Gamma", expand = c(0,0.5))+
  theme(axis.ticks.x = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black"), 
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_blank())

Betaplot_S <- ggplot(Data, aes(x=Age, y=BetaWForm)) + 
  geom_vline(xintercept=boundaries, colour="black", linetype="dashed")+
  geom_smooth(span = 0.15, se=TRUE, colour ="blue", alpha = alpha)+
  geom_point(shape = 19, size = 3, colour = "darkblue", alpha = alpha) +
  geom_point(shape = 21, size = 3, colour = "black",  data = Data2, aes(x=Age, y=BetaWForm))+
  geom_smooth(span = 0.15, se=TRUE, colour ="darkgrey", data = Data2, alpha = 0.2)+
  geom_smooth(span = 0.15, se=TRUE, colour ="darkblue")+
  labs(title = "C")+
  scale_x_continuous(name = "Period", trans = "reverse", expand = c(0,3),
                     breaks = c(502, 465, 431.5, 389, 329, 275, 227, 173.3, 105, 44.5, 8),#these are no exact ages, they just make the labels appear in the middle of each period
                     labels = c("Cambrian", "Ordovician", "Silurian", "Devonian", "Carboniferous",
                                "Permian", "Triassic", "Jurassic", "Cretaceous", "Palaeogene", "Neogene"))+
  scale_y_continuous(name = "Whittaker's beta", expand = c(0,0.5))+
  theme(axis.ticks.x = element_blank(), 
        panel.background = element_rect(fill = "white", colour = "black"), 
        panel.grid.minor = element_blank(),  
        panel.grid.major = element_blank(), 
        panel.ontop = FALSE,
        axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(Alphaplot_S, Gammaplot_S, Betaplot_S, ncol=1, left="Mean diversity per formation")

#ggsave("figureS1.png", width=12, height=22, dpi = 300, units = "cm",
#       grid.arrange(Alphaplot_S, Gammaplot_S, Betaplot_S, ncol=1, left="Mean diversity per formation"))
#ggsave("figureS1.pdf", width=12, height=22, units = "cm",
#       grid.arrange(Alphaplot_S, Gammaplot_S, Betaplot_S, ncol=1, left="Mean diversity per formation"))

####FIGURE S2####
ggplot(Data, aes(x=GammaForm, y=BetaWForm)) +
  stat_smooth(method="loess", span = 1, colour="darkblue", se = TRUE, alpha = 0.3)+
  geom_point(shape=19, size = 3, alpha = 0.3, colour = "darkblue")+
  stat_smooth(method="loess", span = 1, colour="maroon", se = TRUE, alpha = 0.3, aes(x=GammaForm, y=AlphaForm)) +
  geom_point(shape=19, size = 3, alpha = 0.3, colour = "maroon", data = Data, aes(x=GammaForm, y=AlphaForm))+
  #subsampled points
  stat_smooth(method="loess", span = 1, lty="longdash" , colour="darkgrey", se = TRUE, alpha = 0.4, data = Data2)+
  geom_point(shape=21, size = 3, colour = "black", data = Data2, aes(x=GammaForm, y=BetaWForm))+
  stat_smooth(method="loess", span = 1,lty="longdash" , colour="darkgrey", se = TRUE, alpha = 0.4, data = Data2, aes(x=GammaForm, y=AlphaForm)) +
  geom_point(shape=21, size = 3, colour = "black", data = Data2, aes(x=GammaForm, y=AlphaForm))+
  scale_x_continuous(name = "mean gamma diversity per formation") +
  scale_y_continuous(name = "mean alpha and beta diversity per formation")+
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        panel.grid.minor = element_blank(),  panel.grid.major = element_blank())
#  ggsave(("figureS2.png"), plot = last_plot(), width=12, height=12, units = "cm", dpi = 300)
#  ggsave(("figureS2.pdf"), plot = last_plot(), width=12, height=12, units = "cm")
  
####FIGURE S4####
  means.BW <- ddply(Data, "Period.new", summarise, rating.mean=mean(BetaWForm))
  means.Alpha <- ddply(Data, "Period.new", summarise, rating.mean=mean(AlphaForm))
  means.Gamma <- ddply(Data, "Period.new", summarise, rating.mean=mean(GammaForm))
  
  ggplot(Data, aes(x=BetaWForm)) +
    scale_x_log10(name = "log10")+
    geom_density(alpha= 0.3, fill = "darkblue", colour = NA)+
    geom_vline(data=means.BW, aes(xintercept=rating.mean),
              linetype="dashed", size=0.8, colour = "darkblue")  +
    geom_density(alpha= 0.3, fill = "orange", colour = NA, data = Data, aes(x=GammaForm))+
    geom_vline(data=means.Gamma, aes(xintercept=rating.mean),
              linetype="dashed", size =0.8, colour = "orange")+
    geom_density(alpha= 0.3, fill = "red3", colour = NA, data = Data, aes(x=AlphaForm))+
    geom_vline(data=means.Alpha, aes(xintercept=rating.mean),
              linetype="dashed", size=0.8, colour = "red3")+
                 #(limits = c(0, 300), 
              #name = "alpha diversity (red) and gamma diversity (yellow) in number of species, 
              #Whittaker's beta diversity (blue), dimensionless") +
    theme(panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.minor = element_blank(),  panel.grid.major = element_blank(), 
          strip.background = element_rect(colour = "black", fill = "white"),
          strip.text.y = element_text(size = 7))+
          #element_text(face = "bold"))+
    facet_grid(Period.new ~ .)
#  ggsave(("figureS4.png"), plot = last_plot(), width=12, height=22, units = "cm", dpi = 300)
#  ggsave(("figureS4.pdf"), plot = last_plot(), width=12, height=22, units = "cm")

  strip.text.x = element_text(colour = "white", face = "bold")
  

