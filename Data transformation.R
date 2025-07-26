#activating repositories-----
setRepositories()
install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)

install.packages("readxl", dependencies = TRUE) #for excel import
library(readxl)
library(readr) #readr for CSV, TSV and so on

install.packages("writexl", dependencies = TRUE) #to convert R file into excel
library(writexl)

install.packages("plotly", dependencies = TRUE)
library(plotly)


# drop_na() function will remove rows with missing values NA from your data frame

#file import and export----
#import excel file
library(readxl)
imp_data <- read_excel("iris.xlsx", range = "A1:E151", 
                       col_types = c("numeric", "numeric", "numeric", 
                                     "numeric", "text"))
View(imp_data)
remove(imp_data)

#save R into excel
install.packages("writexl", dependencies = TRUE)
library(writexl)
df <- iris
write_xlsx(df, "iris.xlsx")

#data transformation-----
#by dplyr
install.packages("nycflights13", dependencies = TRUE) #it is data package
library(nycflights13)
nycflights13::flights


x=2
x==3
remove(x)

#data viewing
data("iris")
view(iris)
force(iris)
glimpse(iris)
dplyr::tbl_df(iris)
head(iris, n=25)
tail(iris, n=25)
summary(iris)

#Filter
#act on data point of column transformation
dfn<- flights
view(dfn)

dela<- filter(dfn, dep_delay==3)
dela_more1 <- filter(dfn, dep_delay %in% c(3,1))#for multiple conditions
dela_more2 <- filter(dfn, dep_delay==3 | dep_delay==1) #for limited conditions

month <- filter(dfn, month>=7, month<=9)
month1 <- filter(dfn, month==7 | month==9)

##inside multiple column manipulation
dela_month <- filter(dfn, dep_delay==3, month==2)
dela_month_day <- filter(dfn, dep_delay==3, month==2, day==1)
dela_month_daym <- filter(dfn, dep_delay==3, month%in% c(2,1), day==1)



#agroup_by& arrange data transformation and piping (%>%) 
dfn %>% group_by(carrier) %>% summarise(avg= mean(sched_dep_time)) %>% 
  arrange(avg)
dfn %>% group_by(carrier) %>% summarise(avg= mean(sched_dep_time)) %>% 
  arrange(desc(avg))

View(iris)

iris %>%
  group_by(Species) %>%
  summarise(avg = mean(Sepal.Width)) %>%
  arrange(avg)


iris %>% 
  group_by(Species) %>% 
  summarise(avg= mean(Sepal.Width)) %>% arrange(desc(avg))







#readr for CSV
library(readr)
mindfulness <- read_csv("mindfulness.csv")
View(mindfulness)

mindfulness <- read_csv("mindfulness.csv", 
                        col_types = cols(mind1 = col_double(), 
                                         mind2 = col_double()))
#unique shows what data is inside column
unique(mindfulness$gender) 
unique(age)

#select transformation is used to select column
df3<- mindfulness
summary(df3)
df3 %>% select(., contains("mind"))
s1n22 <- df3 %>% select(., contains("mind"))
remove(s1n)

install.packages("palmerpenguins")
library(palmerpenguins)
df <- penguins
library(tidyverse)
df1 <- df %>% select(species, flipper_length_mm) %>% drop_na()

df %>% select(., contains("flipper_length_mm")) 
df %>% select("flipper_length_mm", "species") 
df %>% select("flipper_length_mm")

#mutate transformation is used to add, subtract and so on 

 df3 %>% mutate(mindfull = rowSums(select(., contains("mind"))),
                stress=rowSums(select(., contains("stress"))),
                depression=rowSums(select(., contains("dep")))
                ) %>% 
  group_by(gender) %>% arrange(gender) %>% 
   summarise(avg=mean(mindfull)) %>% 
   arrange(desc(avg))
 
 #filter, mutate, select, group_by, arrange

 df3 %>% filter(age==18) %>% mutate(mindfull = rowSums(select(., contains("mind"))),
                stress=rowSums(select(., contains("stress"))),
                depression=rowSums(select(., contains("dep")))
 ) %>% 
   group_by(gender) %>% arrange(gender) %>% 
   summarise(avg=mean(mindfull)) %>% 
   arrange(desc(avg))

 
 df3 %>% filter(age==18) %>% mutate(mindfull = rowSums(select(., contains("mind"))),
                                    stress=rowSums(select(., contains("stress"))),
                                    depression=rowSums(select(., contains("dep")))
 ) %>% 
   group_by(gender) %>% arrange(gender) %>% 
   summarise(avg=mean(stress)) %>% 
   arrange(desc(avg))


 
 df3 %>% filter(age==18) %>% mutate(mindfull = rowSums(select(., contains("mind"))),
                                    stress=rowSums(select(., contains("stress"))),
                                    depression=rowSums(select(., contains("dep")))
 ) %>% 
   group_by(gender) %>% arrange(gender) %>% 
   summarise(avg=mean(depression)) %>% 
   arrange(desc(avg))

 
 
 df3 %>% filter(age==19) %>% mutate(mindfull = rowSums(select(., contains("mind"))),
                                    stress=rowSums(select(., contains("stress"))),
                                    depression=rowSums(select(., contains("dep")))
 ) %>% 
   group_by(gender) %>% arrange(gender) %>% 
   summarise(sd=sd(stress)) %>% 
   arrange(desc(sd))
                            
#tell what data point in the column
 distinct()
 unique()
 


 
 
 
 
 
 
 
 