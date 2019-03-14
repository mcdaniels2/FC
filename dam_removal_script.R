# March 4-12, 2019
# Molly Daniels
# Exploratory Analysis of the Dam Removal Dataset

# Data Source:
http://figshare.com/articles/_/5234068 

ddata <- read.csv(file.choose())

View(ddata)
# add ddata to GitHub project directory
write.csv(ddata, file = "ddata.csv")

damrem <- ddply(ddata, c("Year_Removed"), summarise, 
                count = length(Year_Removed))

damrem
head(damrem, n=10)
damrem <- damrem[-1,]


#adjust the way year is coded in the dataframe
str(damrem) #note year is a factor
damrem$Year_Removed <- as.character(damrem$Year_Removed)
damrem$Year_Removed <- as.numeric(damrem$Year_Removed)

##### plotting dams over time #####
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

#adjust the way year is coded in the dataframe
ddata$Year_Built <- as.character(ddata$Year_Built)
ddata$Year_Built <- as.numeric(ddata$Year_Built)

# now for summary stats:
mean(ddata$Year_Built, na.rm = TRUE) # mean provided is 1916.002
min(ddata$Year_Built, na.rm = TRUE)
max(ddata$Year_Built, na.rm = TRUE)
median(ddata$Year_Built, na.rm = TRUE)


# now to plot
plotdamsbuilt3 <- ggplot(ddata, aes(Year_Built)) +
  geom_histogram(size = 1.5, bins = 50, aes(fill = "orange")) + 
  theme_classic() +
  geom_vline(xintercept = 1916.002, size = 1) + 
  labs(y = "Dam Count", x = "Year Built")
plotdamsbuilt3 + theme(legend.position="none")

###

##### dam heights #####

# make sure dam height is numeric:
ddata$Dam_Height_ft<- as.character(ddata$Dam_Height_ft)
ddata$Dam_Height_ft<- as.numeric(ddata$Dam_Height_ft)

# create column with height in meters
ddata %>%
  mutate(Dam_Height_m = conv_unit(Dam_Height_ft, "ft", "m")) -> ddata
ddata$Dam_Height_m <- as.numeric(ddata$Dam_Height_m)

mean(ddata$Dam_Height_m, na.rm = TRUE) # 4.4234
min(ddata$Dam_Height_m, na.rm = TRUE)
max(ddata$Dam_Height_m, na.rm = TRUE)
median(ddata$Dam_Height_m, na.rm = TRUE)


#create histogram:
plotdamheight <- ggplot(ddata, aes(Dam_Height_m)) +
  geom_histogram(size = 1.5, bins = 50, fill = "purple") + 
  theme_classic() +
  geom_vline(xintercept = 4.42, size = 1) + 
  labs(y = "Frequency", x = "Dam Height (meters)")
plotdamheight + theme(legend.position="none")

###

##### saving plots #####
# Save plots
#ggsave("time.trends.pdf", plot = plotdamsovertime2, width = 8, height = 5, units = c("in"))

#ggsave("removal.by.state.pdf", plot = plotremovalbystate2, width = 8, height = 5, units = c("in"))

#ggsave("built.hist3.pdf", plot = plotdamsbuilt3, width = 7, height = 5, units = c("in"))


