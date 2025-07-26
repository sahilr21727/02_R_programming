#activating repositories-----
setRepositories()
install.packages("tidyverse", dependencies = TRUE)
library(tidyverse)
install.packages("readxl", dependencies = TRUE)
library(readxl)
install.packages("writexl", dependencies = TRUE)
library(writexl)

#data set----
install.packages("palmerpenguins") #dataset
library(palmerpenguins)
library(ggthemes)

#viewing dataset----
palmerpenguins::penguins
view(penguins)
glimpse(penguins)
dplyr::tbl_df(penguins)
names(penguins)
head(penguins, n=10) #just n=10 limited
print(penguins, n=25)

#ploting by ggplot2----
#global level color
ggplot( penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm")

#local level color
ggplot(penguins,mapping = aes(x = flipper_length_mm, y = body_mass_g)
) + geom_point(mapping=aes(color=species)) +
  geom_smooth(method = "lm")

ggplot(data = penguins,mapping = aes(x = flipper_length_mm, y = body_mass_g)
) + geom_point(mapping=aes(color=species, shape = species)) +
  geom_smooth(method = "lm")


ggplot(penguins) +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )

#  categorial data
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")
ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")


#  numerical data    
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 20)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)

ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()
ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5)

ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()


