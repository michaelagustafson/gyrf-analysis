############ POINT COUNT DATA PREP ################
# Adapted from code created by Jen Cruz for the 
# Applied Population Ecology class

### My code has been altered to accomodate time-
### removal sampling (multinomial N-mixture model)

##### Set up your workspace and load relevant packages -----------
# Clean your workspace to reset your R environment. #
rm( list = ls() )
# Check that you are in the right project folder
getwd()

library(dplyr)
library(tidyverse)
library(ggplot2)
################### End setup #############################
#################### Load data ############################

alaska <- read.csv("Data/alaska.csv")

############ End load data ###########
############# Data prep ##############
######## Okay, now lets do this for all sites! #############3

head(alaska);dim(alaska)

# change all species codes of squirrles to a 1 and all other species to 0
alaska$Species_Alpha_Code[alaska$Species_Alpha_Code == "SQUIR"] <- 1
alaska$Species_Alpha_Code[alaska$Species_Alpha_Code != "1"] <- 0

# now change group size (counts) to NA/s for the zeros
alaska$Group.size[alaska$Species_Alpha_Code == 0] <- 0
head(alaska)
# now species alpha code represents counts for ground squirrels

# get rid of unnecessary columns
colnames(alaska)
alaska <- subset(alaska, select = -c(1:6, 8:9, 11:16, 18))
colnames(alaska)

# there are NA values:
sum(is.na(alaska))
sum(is.na(alaska$Time_Interval))

# since they are all in the time interval column, and 4/5 have 0 counts for 
# squirrels I'm going to change all to the time interval of 0 because it won't
# make a huge difference for the one site that had counts with a NA time interval
# and based on the other counts seen at that site, I can safely assume those 4 
# squirrels were counted during the first minute since there were so many at that site

alaska[is.na(alaska)] <- 0
#check
sum(is.na(alaska))


# Change column names
names(alaska)[names(alaska) == "Group.size"] <- "Count"
head(alaska)
# reorder columns
alaska <- alaska[ ,c(4, 2, 3, 1)]
head(alaska)


# now, need to figure out how to combine time stamps and sum group size to a total count column:
# EVERY 2 MINUTES:
# 0,1
# 2,3
# 4,5
# 6,7
# 8,9

### Okay, finally figured out how to change the values of the time intervals for 2 minute windows! Nested 'ifelse'
### calls within 'mutate'

two.min.int <- data.frame(alaska$Identity, alaska$Time_Interval, alaska$Count, alaska$Julian)

# change names
names(two.min.int)[names(two.min.int) == "alaska.Identity"] <- "Identity"
names(two.min.int)[names(two.min.int) == "alaska.Count"] <- "Count"
names(two.min.int)[names(two.min.int) == "alaska.Time_Interval"] <- "Time_Interval"
names(two.min.int)[names(two.min.int) == "alaska.Julian"] <- "Julian"
head(two.min.int)


# nest 'if else' statements to collapse time intervals to follow two minute intervals:
two.min.int <- mutate(two.min.int, time_interval = ifelse(two.min.int$Time_Interval %in% 0:1, "1",
                                                                      ifelse(two.min.int$Time_Interval %in% 2:3, "2",
                                                                             ifelse(two.min.int$Time_Interval %in% 4:5, "3",
                                                                                    ifelse(two.min.int$Time_Interval %in% 6:7, "4",
                                                                                           ifelse(two.min.int$Time_Interval %in% 8:9, "5", "NA"))))))

head(two.min.int)
# drop old time interval:
two.min.int <- two.min.int[ , -2, drop = FALSE]
head(two.min.int)


#### 5 MINUTE INTERVALS #####
five.min.int <- data.frame(alaska$Identity, alaska$Time_Interval, alaska$Count, alaska$Julian)
names(five.min.int)[names(five.min.int) == "alaska.Identity"] <- "Identity"
names(five.min.int)[names(five.min.int) == "alaska.Count"] <- "Count"
names(five.min.int)[names(five.min.int) == "alaska.Time_Interval"] <- "Time_Interval"
names(five.min.int)[names(five.min.int) == "alaska.Julian"] <- "Julian"
head(five.min.int)
five.min.int <- mutate(five.min.int, fivemin_time = ifelse(five.min.int$Time_Interval %in% 0:4, "1",
                                                                       ifelse(five.min.int$Time_Interval %in% 5:9, "2", "NA")))


# drop old time interval:
five.min.int <- five.min.int[ , -2, drop = FALSE]
head(five.min.int)



#### Sum the counts for each unique identity point ####
# 2 min windows:

# change variable type
str(two.min.int)
two.min.int$Identity <- as.factor(two.min.int$Identity)
two.min.int$Count <- as.integer(two.min.int$Count)
two.min.int$time_interval <- as.factor(two.min.int$time_interval)

str(two.min.int)

one.counts <- aggregate(x = one.min.int)


# aggregate rows and sum counts for each group and time interval
two.counts <- aggregate(x = two.min.int$Count, 
                      by = list(two.min.int$Identity, two.min.int$time_interval, two.min.int$Julian), 
                      FUN = sum)

head(two.counts);dim(two.counts)

# change names
colnames(two.counts)
names(two.counts)[names(two.counts) == "Group.1"] <- "Identity"
names(two.counts)[names(two.counts) == "Group.2"] <- "Time_Interval"
names(two.counts)[names(two.counts) == "Group.3"] <- "Julian"
names(two.counts)[names(two.counts) == "x"] <- "Count"

head(two.counts); dim(two.counts)


# 5 min windows

# change variable type
str(five.min.int)
five.min.int$Identity <- as.factor(five.min.int$Identity)
five.min.int$Count <- as.integer(five.min.int$Count)
five.min.int$time_interval <- as.factor(five.min.int$time_interval)
str(five.min.int)

# aggregate rows and sum counts for each group and time interval
five.counts <- aggregate(x = five.min.int$Count, 
                      by = list(five.min.int$Identity, five.min.int$fivemin_time, five.min.int$Julian), 
                      FUN = sum)


head(five.counts); dim(five.counts)


names(five.counts)[names(five.counts) == "Group.1"] <- "Identity"
names(five.counts)[names(five.counts) == "Group.2"] <- "Time_Interval"
names(five.counts)[names(five.counts) == "Group.3"] <- "Julian"
names(five.counts)[names(five.counts) == "x"] <- "Count"

head(five.counts); dim(five.counts)


# now need to prep ecological predictor table

envpreds <- read.csv("Data/environ_preds_final.csv")
head(envpreds)
envpreds$Identity <- paste(envpreds$Road_ID, envpreds$UNIT_ID, envpreds$Transect_ID, envpreds$Point_ID, 2019, sep = "_")
head(envpreds)

### create site level dataframe column = predictors, row = pointcountid

colnames(envpreds)
sitepreds <- envpreds[ ,c(26, 5, 8, 9, 12, 22:24)]

############# End data prep ######################

########### Inspect data relationships ##############

# create a vector with predictor names do prednames of sitepreds [-1]
prednames <- c("Prim_Obs_initials", "min_before_sun", "Temp_F", "Wind", "pct_T", "pct_LS" )

# loop over each to create histograms for each predictor:
for( p in 1:length(prednames) ){
  # create an object with the ggplot so that you can display it 
  # in a loop 
  a <- ggplot( sitepreds ) + #choose your data
    theme_bw( base_size = 15 ) + #choose a preset theme
    labs( x = prednames[p] ) + #label x axis using our predictor names
    geom_histogram( aes( get(prednames[p]) ), bins= 10, stat = "count" ) #plot histogram
  # display your plot object
  print( a )
}


# Julian date:
head(two.counts)

hist(two.counts$Julian, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Julian Date",
     main = "2019")
lines(density(two.counts$Julian), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")



# correlations between julian date and temp:

cor(sitepreds.twocounts$Julian, sitepreds.twocounts$Temp_F) 
# 0.6 correlation

# correlation between tundra and low shrub (the two vegetation predictors I'm going to include in the model)
cor(sitepreds.twocounts$pct_T, sitepreds.twocounts$pct_LS) 
# and a -0.6 corr here...




#######################################################################
# save your workspace:
save.image("alaska_squirrel_data_prep_Workspace.Rdata")

# save two and five minute count intervals as csv

#write.csv(two.counts, "Data/twominint_counts.csv")
#write.csv(five.counts, "Data/fiveminint_counts.csv")

write.csv(alaska, "Data/oneminint_counts.csv")
# save site predictors:

#write.csv(sitepreds, "Data/squirrel_sitepreds.csv")



 























