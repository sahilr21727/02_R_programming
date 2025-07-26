#WHere is our directory location----
getwd()

#repositories----
setRepositories()

#packages----

install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)
install.packages("readxl", dependencies = TRUE)
library(readxl)

install.packages("gridExtra")
library(gridExtra)
grid.arrange(plot1, plot2, ncol=2)

#import excel file----
dfn <- read_excel("DNA purity results.xlsx", 
                  range = "D3:H43", col_types = c("text",
                                      "text", "numeric", "numeric", "numeric"))
View(dfn)

#data transormation----
dfn %>% filter(`Group ID` == "NPK")
dfn %>% group_by(`Group ID`) %>% 
  summarise(avg=mean(`Conc (µg/ml)`)) %>% arrange(desc(avg))
 
s1 <-dfn %>% group_by(`Group ID`) %>% 
  summarise(avg=mean(`Conc (µg/ml)`)) %>% arrange(desc(avg))


#graph between group and mean of DNA conc----
##scattor plot
ggplot(s1, mapping = aes(x=`Group ID`, y=avg, colour = `Group ID`))+
  geom_point(size=5)+ 
theme_classic()+
  labs(x="Mean of treatment group", y="DNA Conc (µg/ml)" )+
  theme(legend.key.height = unit(0.25, "cm"),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(color  = "blue"),
        legend.title = element_text(colour = "green", size = 12, face = "bold"),
        legend.position = ("right"),legend.direction = ("vertical")) 
  

##barplot
ggplot(s1, mapping = aes(x=`Group ID`, y=avg, fill = `Group ID`)) + 
  geom_bar(stat ="identity")+theme_classic()+
  labs(x="Mean of treatment groups", y="DNA Conc (µg/ml)" )
ggsave("MeanDNA.png", width = 8, height = 8, units = "in", dpi = 300 )
ggsave("MeanDNA.png", width = 8, height = 8, units = "in", dpi = 300 )

ggplot(s1, mapping = aes(x=avg, y=`Group ID`, , fill = `Group ID`)) + 
  geom_bar(stat ="identity")+theme_classic()+
  labs(y="Mean of treatment groups", x="DNA Conc (µg/ml)" )



#graph between group and mean of A260/280----
s2 <-dfn %>% group_by(`Group ID`) %>% 
  summarise(avg=mean(`Purity A260/280`)) %>% arrange(desc(avg))

ggplot(s2, mapping = aes(y=`Group ID`, x=avg, fill = `Group ID`))+
  geom_bar(stat = "identity")+theme_classic()+
  labs(x="Mean of Purity A260/280", y="Group ID" )

ggsave("Meanpurity.png", width = 8, height = 8, units = "in", dpi = 300 )




# Load the gridExtra package for plot on 1 page----

alinn<- ggplot(s1, mapping = aes(x=`Group ID`, y=avg, fill = `Group ID`)) + 
  geom_bar(stat ="identity")+theme_classic()+
  labs(x="Mean of treatment groups", y="DNA Conc (µg/ml)" )
ggsave("MeanDNA.png", width = 8, height = 8, units = "in", dpi = 300 )

alinnb <- ggplot(s2, mapping = aes(y=`Group ID`, x=avg, fill = `Group ID`))+
  geom_bar(stat = "identity")+theme_classic()+
  labs(x="Purity A260/280", y="Group ID" )
ggsave("Meanpurity.png", width = 8, height = 8, units = "in", dpi = 300 )

install.packages("gridExtra")
library(gridExtra)
grid.arrange(alinn, alinnb, ncol=2)







#plotting----
library(ggplot2)

ggplot(dfn, mapping = aes(x=`Conc (µg/ml)`, y=`Purity A260/280`, colour = `Group ID`))+
  geom_point(size=5,alpha=3)+theme_classic()+
  labs(x="DNA Conc (µg/ml)", y="DNA A260/280")+
  theme(legend.key.height = unit(0.25, "cm"),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(color  = "blue"),
        legend.title = element_text(colour = "green", size = 12, face = "bold"),
        legend.position = ("right"),legend.direction = ("vertical"))


        
ggplot(dfn, mapping = aes(x=`Conc (µg/ml)`, y=`Purity A260/280`, color=`Group ID`))+
  geom_point(size=5,alpha=3)+
  geom_point(aes(x = `Conc (µg/ml)`, y = `Purity A260/230`, shape = `Group ID`))+
  theme_classic()+labs(x="DNA Conc (µg/ml)", y="Purity (A260/280),(A260/230) ")+
  theme(legend.key.height = unit(0.25, "cm"),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(color  = "blue"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"),
        legend.position = ("right"),legend.direction = ("vertical"))

ggsave("plotn1.png", width = 8, height = 8, units = "in", dpi = 300)
ggsave("plotn1.pdf", width = 8, height = 8, units = "in", dpi = 300)

#bar chart
 ggplot(dfn, mapping = aes(x=`Sample ID`, y=`Conc (µg/ml)`, fill = `Group ID`)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(x="Sample ID", y="DNA Conc (µg/ml) ")+ 
  theme_classic() +
  theme(legend.key.height = unit(0.25, "cm"),
        legend.background = element_rect(fill = "white"),
        legend.text = element_text(color  = "blue"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"),
        legend.position = "right",
        legend.direction = "vertical")




ggsave("plotb.png", width = 8, height = 8, units = "in", dpi = 300)
ggsave("plotnb.pdf", width = 8, height = 8, units = "in", dpi = 300)
#Code useful for plotting but i cant add in ploting due to x is categorical data----  
#To equalize the range of boht axaes especially for numeric axes
coord_fixed()
#To make multipanel of your selectted variable 
facet_wrap(~dfn$`Group ID`)

#saving plot----
ggsave("plotn1.png", width = 8, height = 8, units = "in", dpi = 300)
ggsave("plotn1.pdf", width = 8, height = 8, units = "in", dpi = 300)


#automatic ploting----
install.packages("esquisse", dependencies = TRUE)
library(esquisse)
install.packages("hrbrthemes", dependencies = TRUE)
library(plotly)
 esquisser()

 ggplot(dfn) +
  aes(x = `Conc (µg/ml)`, y = `Purity A260/280`, colour = `Group ID`) +
  geom_point(size = 5,
             shape = "circle small") +
  geom_point(aes(x = `Conc (µg/ml)`, y = `Purity A260/230`, colour = `Group ID`),
             size = 5, shape = "square") +
  scale_fill_hue(direction = 1) +
  labs(x = "DNA Conc (ug/ml)",
       y = "DNA Purity (A260/230, A260/280)") +
  theme_classic()



