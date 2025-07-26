#packages-----
setRepositories()
install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)
install.packages("readxl", dependencies = TRUE)
library(readxl)
install.packages("writexl")
library(writexl)

#distribution
hist()
qqnorm()
shapiro.test()


#composition in which we check homogeneity
install.packages("car", dependencies = TRUE) #for levene test
library(car)
leveneTest()

#OR 

install.packages("lattice", dependencies = TRUE)
library(lattice)
dotplot()

#for relationship
install.packages("corrplot", dependencies = TRUE)  
library(corrplot)

# for comparison
install.packages("stats")  
library(stats)



#applying post hoc test
install.packages("multcomp", dependencies = TRUE)
library(multcomp)
TukeyHSD()



#for correlation studies----
install.packages("corrplot", dependencies = TRUE)
library(corrplot)
data("iris")

df1 <- cor(iris[,1:4])

A2 <- cor(iris[,c(1, 2)])


corrplot(df1, method = "ellipse", type = "full")
corr
corrplot(df1, metho =  'ellipse', typ = 'upper')

print(df1)





#t test-----

install.packages("stats")
library(stats)



#normal distribution
data(iris)
hist(iris$Sepal.Width)
qqnorm(iris$Sepal.Width)
shapiro.test(iris$Sepal.Width)


#one sample t test
df <- iris
summary(df)

t.test(df$Sepal.Width, mu=5)


#two sample t test
###unpaired t test
unique(df$Species)

t.test(df$Sepal.Width ~ df$Species=="setosa" | df$Species=="versicolor")


install.packages("palmerpenguins")
library(palmerpenguins)
df <- penguins
unique(df$species)
unique(df$island)
unique(df$sex)
t.test(df$body_mass_g ~ df$species=="Adelie" |df$species=="Chinstrap")
#show difference in mean by plot
ggplot(df, mapping = aes(x=species, y=body_mass_g))+geom_boxplot()

###paired 
pre_treatment <- c(rnorm(2000, mean = 140, sd=10 ))
tibble(pre_treatment)
post_treatment <- c(rnorm(2000, mean = 160, sd=12))
tibble(post_treatment)
t.test(pre_treatment, post_treatment, paired = TRUE)





#ANOVA
install.packages("palmerpenguins")
library(palmerpenguins)
df <- penguins
library(tidyverse)

df %>% select(., contains("flipper_length_mm")) 
df %>% select("flipper_length_mm", "species") 
df %>% select("flipper_length_mm")

df1 <- df %>% select(species, flipper_length_mm) %>% drop_na()

#normal distribution
hist(df1$flipper_length_mm)
qqnorm(df1$flipper_length_mm)

s<- shapiro.test(df1$flipper_length_mm)
s$p.value

#homogeneity
install.packages("car", dependencies = TRUE) #for leveneTest test
library(car)
library(tidyverse)

leveneTest(flipper_length_mm~ species, data = df1)
leveneTest(df1$flipper_length_mm ~ df1$species, data = df1) 

df2 <- df1 %>% group_by(species) %>% 
  summarise(shapiro_pvalue = shapiro.test(flipper_length_mm)$p.value) #?



res_aov <- aov(df1$flipper_length_mm ~ df1$species, data= df1)
summary(res_aov)


#graph
df1 %>% group_by(species) %>% summarise(avg =mean(flipper_length_mm)) 
ggplot(df1, mapping= aes(x=species, y=flipper_length_mm, fill = species))+
  geom_boxplot()








#to check the homogeneity  of data----
install.packages("lattice", dependencies = TRUE)
library(lattice)

dotplot(df1$flipper_length_mm ~ df1$species, data = df1)

ggplot(df1, mapping = aes(x=df1$species, y=df1$flipper_length_mm, fill = df1$species))+geom_boxplot()



#clone code
df %>% group_by(species) %>% 
  summarise(mean = mean(flipper_length_mm, na.rm = TRUE),
            sd = sd(flipper_length_mm, na.rm= TRUE))
aggregate(df1$flipper_length_mm ~ df1$species , data = df1,
          function(x) round(c(mean= mean(x), sd= sd(x)), 2))

#another way for anova----
oneway.test(df1$flipper_length_mm ~ df1$species,  data = df1, var.equal = TRUE)

#two way anova
aov(df1$flipper_length_mm ~ df1$species*df1$flipper_length_mm,  data = df1)




#applying post hoc test----
install.packages("multcomp", dependencies = TRUE)
library(multcomp)

TukeyHSD(res_aov)
plot(TukeyHSD(res_aov))

tukey_result <- TukeyHSD(res_aov)
summary(tukey_result)




# Assuming you have already conducted an ANOVA
model <- aov(response ~ factor, data = your_data)

# Perform Tukey's HSD test
tukey_result <- TukeyHSD(res_aov)

# View the results
summary(tukey_result)








#----
setRepositories()
install.packages("readxl", dependencies = TRUE)
library(readxl)
install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)
library(ggplot2)

##############################################################@@@@@@
install.packages("ggthemes", dependencies = TRUE)
library(ggthemes)
library(tidyverse)
install.packages("multcomp", dependencies = TRUE)
library(multcomp)
library(multcompView) #for lettering ??????????????????????????
library(stats)

#######
data("chickwts")
df <- chickwts
unique(df$feed)
mean_data <- group_by(df, feed) %>% summarise(weight_mean = mean(weight),
                                              sd = sd(weight)) %>% 
  arrange(desc(weight_mean))

#sd()/sqrt(n) = std error



tibble(mean_data)

anova <- aov(weight ~ feed, data=df)

summary(anova)

tukey <- TukeyHSD(anova)
tukey
plot(TukeyHSD(anova))

#barplot----
group_letters <- multcompLetters4(anova, tukey)
group_letters


group_letters <- as.data.frame.list(group_letters$feed)

mean_data$group_letters <- group_letters$Letters

tibble(mean_data)

#####
ggplot(mean_data, mapping = aes(x=feed, y=weight_mean))+
  geom_bar(stat = "identity")

ggplot(mean_data, mapping = aes(x=feed, y=weight_mean))+
  geom_bar(stat = "identity", aes(fill = feed), show.legend = FALSE)

ggplot(mean_data, mapping = aes(x=feed, y=weight_mean))+
  geom_bar(stat = "identity", aes(fill = feed), show.legend = FALSE, width = 0.6)

ggplot(mean_data, mapping = aes(x=feed, y=weight_mean))+
  geom_bar(stat = "identity", aes(fill = feed), show.legend = FALSE, width = 0.6)+
  geom_errorbar(aes(ymin = weight_mean-sd, ymax = weight_mean+sd), width= 0.1)

ggplot(mean_data, mapping = aes(x=feed, y=weight_mean))+
  geom_bar(stat = "identity", aes(fill = feed), show.legend = FALSE, width = 0.6)+
  geom_errorbar(aes(ymin = weight_mean-sd, ymax = weight_mean+sd), width= 0.1)+
  geom_text(aes(label = group_letters, y= weight_mean+sd))


ggplot(mean_data, mapping = aes(x=feed, y=weight_mean))+
  geom_bar(stat = "identity", aes(fill = feed), show.legend = FALSE, width = 0.6)+
  geom_errorbar(aes(ymin = weight_mean-sd, ymax = weight_mean+sd), width= 0.1)+
  geom_text(aes(label = group_letters, y= weight_mean+sd), vjust= -0.4)


ggplot(mean_data, mapping = aes(x=feed, y=weight_mean))+
  geom_bar(stat = "identity", aes(fill = feed), show.legend = FALSE, width = 0.6)+
  geom_errorbar(aes(ymin = weight_mean-sd, ymax = weight_mean+sd), width= 0.1)+
  geom_text(aes(label = group_letters, y= weight_mean+sd), vjust= -0.4)+
  scale_fill_brewer(palette = "Spectral", direction = 1)

display.brewer.all()



ggplot(mean_data, mapping = aes(x=feed, y=weight_mean))+
  geom_bar(stat = "identity", aes(fill = feed), show.legend = FALSE, width = 0.6)+
  geom_errorbar(aes(ymin = weight_mean-sd, ymax = weight_mean+sd), width= 0.1)+
  geom_text(aes(label = group_letters, y= weight_mean+sd), vjust= -0.4)+
  scale_fill_brewer(palette = "RdYlGn", direction = 1)+
  labs(title = "summer session bar-plot", x="Feed", y="Chicken Weight", fill= "Feed Type")


p <- ggplot(mean_data, mapping = aes(x=feed, y=weight_mean))+
  geom_bar(stat = "identity", aes(fill = feed), show.legend = FALSE, width = 0.6)+
  geom_errorbar(aes(ymin = weight_mean-sd, ymax = weight_mean+sd), width= 0.1)+
  geom_text(aes(label = group_letters, y= weight_mean+sd), vjust= -0.4)+
  scale_fill_brewer(palette = "RdYlGn", direction = 1)+
  labs(title = "summer session bar-plot", x="Feed", y="Chicken Weight", fill= "Feed Type")+
  ylim(0,410)+ ggthemes::theme_hc() ;p


tiff("barplot.tiff", units = "in", width = 10, height = 6, res = 300, compression = "lzw")
p
dev.off()