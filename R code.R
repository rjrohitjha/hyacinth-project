#For plotting the study country on world map

if(!require("xfun"))install.packages("sf")

library(sf)

## Loading required package: xfun
if(!require("xfun")) install.packages("xfun")

xfun::pkg_attach2(c("sf", "spData", "tidyverse"))


world_tbl = read_sf(system.file("shapes/world.shp", package = "spData"))
world_tbl

#Plotting the country from where the works have been cited in R


setwd("~/Documents/Biol4800_Intro_Data/wh data")

hyacinth <- read.csv("hyacinth3.csv", header=TRUE)


##dropping last few columns which are unnecesarily created.
drop <- c("X.1","X.2", "X.3", "X.4", "X.5","X.6", "X.7")
hyacinth = hyacinth[,!(names(hyacinth) %in% drop)]

#renaming the last column as Title

hyacinth <- hyacinth %>%
  rename(
    Title = X
  )

##subsetting the data get unique studies
data2 <-hyacinth %>%
  distinct(author, Title, .keep_all = T)

#changing the names of two countries that are different from one in sf folder
data2$country[data2$country == 'india'] <- 'India'
data2$country[data2$country == 'USA'] <- 'United States'

# We have 24 studies, now getting the table for the country

country_table <- table(data2$country)

# India is also written as india, and United States is written as US
#We need to chaange it to match the name given in world map

country_table <- data.frame(country_table)

x <- c("name_long", "Frequency")

colnames(country_table) <- x


# Linking this table to the map of the world

world_hyacinth = left_join(world_tbl, country_table)

length(which(world_hyacinth$Frequency>=1))


plot(world_hyacinth["Frequency"],
     main = "Countries where studies were conducted" )


###################################################################

##For ggplot and meta-analysis

###################################################

########################################
## there are 25 entities

#for now we are carrying out meta-analysis about the impact on
# dissolved oxygen, temperature, nitrogen and pH

##################################################



#######################################################################
## Dissolved oxygen

#####################################################################

#filtering the data to get data for dissolved oxygen

d_oxygen<- hyacinth %>%
  filter(impacted_entity =='Dissolved oxygen')

##Removing column TSI as it has only NA

d_oxygen = select(d_oxygen, -TSI)

## some of our data in invertebrate has NA for mean and sd
#and we can not use those data for effect side
#so removing those rows from data which has NA

if(!require(tidyr)) install.packages("tidyr")



##################################################################
##making ggplot to see the difference in dissolved oxygen
#in impacted and control zone
if(!require(ggplot2)) install.packages("ggplot2")

#we need to change the data to long format
# first lets create a dataframe with the column that is needed


d_oxygen2 <- d_oxygen[c("mean_control", "mean_invaded")]

#creating a column for ID
d_oxygen2$Rank <- c(1:36)

d_oxygen2_long <- d_oxygen2 %>%
  pivot_longer(!Rank, names_to = "System", values_to = "Mean_value")


a<- ggplot(d_oxygen2_long, aes(x = System, y = Mean_value),
           group = Rank)
a + geom_point() + geom_line(aes(group = Rank))


##Meta-analysis
##Installing package meta

if(!require(meta)) install.packages("meta")
if(!require(metasens)) install.packages("metasens")
library(meta)
library(metasens)

#A couple of dataset is not numereic/integer which we have to convert

d_oxygen$mean_control <- as.numeric(d_oxygen$mean_control)
d_oxygen$mean_invaded <- as.numeric(d_oxygen$mean_invaded)


#### conducting meta-analysis
mc_doxygen <- metacont(ss_invaded, mean_invaded, sd_invaded,
                        ss_control, mean_control, sd_control,
                        sm="SMD", data= d_oxygen)


print(summary(mc_doxygen), digits= 2)

forest(mc_doxygen, xlab="Dissolved oxygen level")



#######################################################################
## Temperature

#####################################################################

#filtering the data to get data for dissolved oxygen

temp<- hyacinth %>%
  filter(impacted_entity =='Temperature')

##Removing column TSI as it has only NA

temp = select(temp, -TSI)

#we need to change the data to long format
# first lets create a dataframe with the column that is needed


temp2 <- temp[c("mean_control", "mean_invaded")]

#creating a column for ID
temp2$Rank <- c(1:25)

temp2_long <- temp2 %>%
  pivot_longer(!Rank, names_to = "System", values_to = "Mean_value")


a<- ggplot(temp2_long, aes(x = System, y = Mean_value),
           group = Rank)
a + geom_point() + geom_line(aes(group = Rank))


##Meta-analysis

#A couple of dataset is not numereic/integer which we have to convert

temp$mean_control <- as.numeric(temp$mean_control)
temp$mean_invaded <- as.numeric(temp$mean_invaded)


#### conducting meta-analysis
mc_temp <- metacont(ss_invaded, mean_invaded, sd_invaded,
                       ss_control, mean_control, sd_control,
                       sm="SMD", data= temp)


print(summary(mc_temp), digits= 2)

forest(mc_temp, xlab="Temperature")




#######################################################################
## Nitrogen

#####################################################################

#filtering the data to get data for nitrogen

nitro<- hyacinth %>%
  filter(impacted_entity =='Nitrogen')

##Removing column TSI as it has only NA

nitro = select(nitro, -TSI)

#we need to change the data to long format
# first lets create a dataframe with the column that is needed


nitro2 <- nitro[c("mean_control", "mean_invaded")]

#creating a column for ID
nitro2$Rank <- c(1:39)

nitro2_long <- nitro2 %>%
  pivot_longer(!Rank, names_to = "System", values_to = "Mean_value")


a<- ggplot(nitro2_long, aes(x = System, y = Mean_value),
           group = Rank)
a + geom_point() + geom_line(aes(group = Rank))


##Meta-analysis

#A couple of dataset is not numereic/integer which we have to convert

nitro$mean_control <- as.numeric(nitro$mean_control)
nitro$mean_invaded <- as.numeric(nitro$mean_invaded)


#### conducting meta-analysis
mc_nitro <- metacont(ss_invaded, mean_invaded, sd_invaded,
                    ss_control, mean_control, sd_control,
                    sm="SMD", data= nitro)


print(summary(mc_nitro), digits= 2)

forest(mc_nitro, xlab="Nitrogen")



#######################################################################
## pH

#####################################################################

#filtering the data to get data for pH

ph<- hyacinth %>%
  filter(impacted_entity =='pH')



#we need to change the data to long format
# first lets create a dataframe with the column that is needed


ph2 <- ph[c("mean_control", "mean_invaded")]

#creating a column for ID
ph2$Rank <- c(1:30)

ph2_long <- ph2 %>%
  pivot_longer(!Rank, names_to = "System", values_to = "Mean_value")


a<- ggplot(ph2_long, aes(x = System, y = Mean_value),
           group = Rank)
a + geom_point() + geom_line(aes(group = Rank))


##Meta-analysis

#A couple of dataset is not numereic/integer which we have to convert

ph$mean_control <- as.numeric(ph$mean_control)
ph$mean_invaded <- as.numeric(ph$mean_invaded)


#### conducting meta-analysis
mc_ph <- metacont(ss_invaded, mean_invaded, sd_invaded,
                     ss_control, mean_control, sd_control,
                     sm="SMD", data= ph)


print(summary(mc_ph), digits= 2)

forest(mc_ph, xlab="pH")


