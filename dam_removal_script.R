# March 4, 2019
# Molly Daniels
# Exploratory Analysis of the Dam Removal Dataset

# Data Source:
http://figshare.com/articles/_/5234068 

ddata <- read.csv(file.choose())

View(ddata)


damrem <- ddply(ddata, c("Year_Removed"), summarise, 
                count = length(Year_Removed))

damrem
head(damrem, n=10)
damrem <- damrem[-1,]


#adjust the way year is coded in the dataframe
str(damrem) #note year is a factor
damrem$Year_Removed <- as.character(damrem$Year_Removed)
damrem$Year_Removed <- as.numeric(damrem$Year_Removed)

# now to plot





#plotting first question as bar graph
plotdamsovertime <- ggplot(damrem, aes(Year_Removed, count)) +
  geom_bar(stat = "identity") + theme_classic(base_size = 16) +
  scale_x_continuous(breaks = c(seq(1900,2020, by = 10))) +
  labs(y = "Dam Removals", x = "Year")

plotdamsovertime


# plotting first question as line graph
plotdamsovertime2 <- ggplot(damrem, aes(Year_Removed, count)) +
  geom_line(stat = "identity") + theme_classic(base_size = 16) +
  scale_x_continuous(breaks = c(seq(1900,2020, by = 10))) +
  labs(y = "Dam Removals", x = "Year")

plotdamsovertime2

###

##### dam removals across states #####

ddata$State <- as.factor(ddata$State)
summary(ddata$State)

# creating tibble by tallying, then filtering by states with 10+ removals:
state_counts <- ddata %>%
    group_by(State) %>%
    tally
head(state_counts)

frequent_states <-  state_counts %>%
  filter(n >= 10) %>%
  select(State, n)

head(frequent_states) # ok that filtered to states with 10+ removals

###

##### now to plot states #####

plotremovalbystate <- ggplot(frequent_states, aes(State, n)) + 
  geom_bar(stat = "identity", aes(fill = factor(State))) + 
  theme_classic() +
  labs(y = "Number of Dam Removals", x = "State")

# let's make it better
plotremovalbystate2 <- ggplot(frequent_states, aes(State, n)) +
  geom_bar(stat = "identity", aes(fill = n)) +
  theme_classic() +
  labs(y = "Number of Dam Removals", x = "State") 

plotremovalbystate2 + scale_fill_gradientn(colours = rainbow(5)) -> plotremovalbystate2
plotremovalbystate2
###

##### histogram of when the dams were built #####
# 
dambuilt <- ddply(ddata, c("Year_Built"), summarise, 
                count = length(Year_Built))
dambuilt
head(dambuilt, n=10) 
# looks like we have blanks
dambuilt <- dambuilt[-1,] 
# blanks are gone, but a lot of approx.'s (i.e. ~1900, ~1700)

#adjust the way year is coded in the dataframe
str(dambuilt) #note year_built is a factor
dambuilt$Year_Built <- as.character(dambuilt$Year_Built)
dambuilt$Year_Built <- as.numeric(dambuilt$Year_Built)

summary(dambuilt)


# now to plot

plotdamsbuilt <- ggplot(dambuilt, aes(Year_Built, count)) +
  geom_line(stat = "identity") + theme_classic() +
  scale_x_continuous(breaks = c(seq(1900,2020, by = 50))) +
  labs(y = "Dams Built", x = "Year")
plotdamsbuilt
#looks like a nearly flat line...

plotdamsbuilt2 <- ggplot(dambuilt, aes(Year_Built, count)) +
  geom_line(stat = "identity") + theme_classic() +
  scale_x_continuous(limits = c(1720, 2000)) +
  scale_y_continuous(limits = c(0,20)) +
  labs(y = "Dams Built", x = "Year")
plotdamsbuilt2

plotdamsbuilt3 <- ggplot(dambuilt, aes(Year_Built, count)) +
  geom_point(stat = "identity", shape = 18, size = 1.5) + theme_classic() +
  geom_smooth() +
  scale_x_continuous(limits = c(1720, 2000)) +
  scale_y_continuous(limits = c(0,15)) +
  labs(y = "Dams Built", x = "Year")
plotdamsbuilt3

# now a histogram
hist(dambuilt$Year_Built)

# adding a meanline for year built
#determining mean year built
mean(dambuilt$Year_Built, na.rm = TRUE)
# 1900.32
#adding line to histogram:
abline(v=1900.32,col="red")

# now for summary stats:
summary(dambuilt$Year_Built)

###

##### dam heights #####
# make sure dam height is numeric:
ddata$Dam_Height_ft<- as.numeric(ddata$Dam_Height_ft)
#create histogram:
hist(ddata$Dam_Height_ft)
# calculate stats:
mean(ddata$Dam_Height_ft, na.rm = TRUE)
# 50.727
summary(ddata$Dam_Height_ft)

# add mean line to histogram
abline(v = 50.727, col = "red")

# re-do with height in meters

ddata %>%
  mutate(Dam_Height_m = conv_unit(Dam_Height_ft, "ft", "m")) -> ddata
ddata$Dam_Height_m <- as.numeric(ddata$Dam_Height_m)
mean(ddata$Dam_Height_m, na.rm = TRUE)
# mean is 15.46m

# histogram of heigh in meters
hist(ddata$Dam_Height_m)
abline(v = 15.46, col = "red")

###

##### saving plots #####
# Save plots
#ggsave("time.trends.pdf", plot = plotdamsovertime2, width = 8, height = 5, units = c("in"))

#ggsave("removal.by.state.pdf", plot = plotremovalbystate2, width = 8, height = 5, units = c("in"))

#ggsave("built.trend.pdf", plot = plotdamsbuilt3, width = 7, height = 5, units = c("in"))


