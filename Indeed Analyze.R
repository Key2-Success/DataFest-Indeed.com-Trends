# ideas
# most popular industries in each state
# which industries' job postings leave faster in each state
# pca plot of industry - by salary, location, clicks, etc


# load packages
library(readr)

# full data
indeed2 <- read_csv(file = "Kitu/College/Senior Year/Extracurriculars/DataFest/2018 DataFest/datafest2018-Updated-April12.csv")

# subsample of full data
set.seed(12345)
data <- indeed2[sample(nrow(indeed2), 0.10*dim(indeed2)[1]), ]

# small data
indeed <- read_csv(file ="Kitu/College/Senior Year/Extracurriculars/DataFest/data20K.csv")

# niamh data
indeed <- read_csv(file ="Kitu/College/Senior Year/Extracurriculars/DataFest/data20K.csv")

# only US
indeed <- indeed[indeed$country == "US", ]

# all job IDs
allJobIds <- unique(indeed2$jobId)
sample_allJobIds <- sample(allJobIds, 20000, replace = F)
subset <- indeed2[indeed2$jobId %in% sample_allJobIds, ]


# --- subset to only those rows with refreshers
# jobId list
jobId <- unique(indeed$jobId)

# refresh list
refresh <- character()

for(i in 1:length(jobId))
{
  df <- indeed[indeed$jobId == jobId[i], "jobAgeDays"]
  if (0 %in% df$jobAgeDays && (df$jobAgeDays[i] != 0 || length(which(df$jobAgeDays == 0)) > 1))
  {
      refresh <- append(refresh, jobId[i], df)
  }
}

#### #new jobs/day

# jobIds just added
new <- indeed[indeed$jobAgeDays == 0, ]

# # new jobs/day
library(plyr)
library(lubridate)
posts_per_day <- ddply(new, "date", mutate, total_counts = length(date))
daily_posts <- posts_per_day %>% group_by(date)  %>% filter(row_number() == 1)
daily_posts <- daily_posts[ , c(1:3, 24)]

# assign weeks
weeks <- trunc((daily_posts$date - min(daily_posts$date))/7)+1
weeks <- as.integer(weeks)
weeks <- weeks[-1]
weeks <- append(weeks, 57)

# dplyr weeks
weeks <- as.data.frame(weeks)
daily_posts <- as.data.frame(daily_posts)
daily_posts <- cbind(daily_posts, weeks)
weekly_sum <- aggregate(total_counts ~ weeks, daily_posts, sum)
plot(weekly_sum, type = "l")
ggplot(data = weekly_sum, aes(x = weeks, y = total_counts)) + 
  geom_point(col = "red", size = 3) + geom_line(lwd = 1.25) + geom_density(fill = T)

# days
days <- rep(seq(1, 7), 57)
days <- days[-1]
days <- days[-c(396:398)]
days <- as.data.frame(days)
daily_posts <- cbind(daily_posts, days)
daily_sum <- aggregate(total_counts ~ days, daily_posts, sum)
plot(daily_sum, type = "l")

# plot
library(gcookbook)



### start over kinda lol
# new posts: USA and nonzero salary
counts <- new[new$country == "US", ]
counts <- counts[counts$estimatedSalary != 0, ]
hist(counts$estimatedSalary, breaks = 20)
summary(counts$estimatedSalary)

counts <- counts[ , c(1:3, 15)]

# add labels for price cut offs
counts$salary <- 0
for(i in 1:nrow(counts))
{
  if(counts$estimatedSalary[i] < 25000)
  {
    counts$salary[i] <- 1
  }
  else if(counts$estimatedSalary[i] >= 25000 && counts$estimatedSalary[i] < 50000)
  {
    counts$salary[i] <- 2
  }
  else if(counts$estimatedSalary[i] >= 50000 && counts$estimatedSalary[i] < 75000)
  {
    counts$salary[i] <- 3
  }
  else if(counts$estimatedSalary[i] >= 75000 && counts$estimatedSalary[i] < 100000)
  {
    counts$salary[i] <- 4
  }
  else if(counts$estimatedSalary[i] >= 100000 && counts$estimatedSalary[i] < 150000)
  {
    counts$salary[i] <- 5
  }
  else if(counts$estimatedSalary[i] >= 150000)
  {
    counts$salary[i] <- 6
  }
}

library(dplyr)
count_all <- counts %>% dplyr::count(date, salary)

# merge days and weeks
mergecounts <- daily_posts[ , c(3, 5, 6)]
test <- left_join(count_all, mergecounts)
count_all <- test
rm(test)

# ggplot
test <- count_all
test$salary <- as.numeric(test$salary)
test <- test[test$salary < 2, ]
test$salary <- as.factor(test$salary)
count_all$salary <- as.factor(count_all$salary)

test2 <- aggregate(n ~ weeks, count_all, sum)
names(test2)[2] <- "weekly_sum"

count_all <- left_join(count_all, test2)

ggplot(count_all, aes(x = date, y = weekly_sum, fill = salary)) + geom_area()

dates <- unique(count_all$date)

test3 <- count_all

# new all
all_dates <- as.data.frame(rep(mergecounts$date, each = 6))
names(all_dates) <- "date"
vals <- as.data.frame(rep(1:6, 395))
names(vals) <- "salary"
all_dates <- cbind(all_dates, vals)
all_dates$salary <- as.factor(all_dates$salary)
all_dates <- left_join(all_dates, count_all)

for (i in 1:nrow(all_dates))
{
  if (is.na(all_dates$weeks[i]))
  {
    all_dates$weeks[i] <- all_dates$weeks[i-1]
  }
  if (is.na(all_dates$days[i]))
  {
    all_dates$days[i] <- all_dates$days[i-1]
  }
  if (is.na(all_dates$weekly_sum[i]))
  {
    all_dates$weekly_sum[i] <- all_dates$weekly_sum[i-1]
  }
    
}

all_dates[is.na(all_dates)] <- 0

all_dates <- all_dates[all_dates$weeks != 57, ]
all_dates <- all_dates[all_dates$weeks != 1, ]

aggregate(n ~ weeks, count_all, sum)

names(all_dates)[3] <- "posts"
test4 <- aggregate(posts ~ weeks + salary, all_dates, sum)
names(test4)[3] <- "total"

all_dates <- left_join(all_dates, test4)

# clean dataset
names(all_dates)[1] <- "Date"
names(all_dates)[2] <- "Salary Range"
names(all_dates)[7] <- "# of New Posts"

all_dates$salary <- ""
for(i in 1:nrow(all_dates))
{
  if (all_dates$`Salary Range`[i] == 1)
  {
    all_dates$salary[i] <- "1: < $25k"
  }
  if (all_dates$`Salary Range`[i] == 2)
  {
    all_dates$salary[i] <- "2: $25k to $50k"
  }
  if (all_dates$`Salary Range`[i] == 3)
  {
    all_dates$salary[i] <- "3: $50k to $75k"
  }
  if (all_dates$`Salary Range`[i] == 4)
  {
    all_dates$salary[i] <- "4: $75k to $100k"
  }
  if (all_dates$`Salary Range`[i] == 5)
  {
    all_dates$salary[i] <- "5: $100k to $150k"
  }
  if (all_dates$`Salary Range`[i] == 6)
  {
    all_dates$salary[i] <- "6: $150k +"
  }
}
all_dates$salary <- as.factor(all_dates$salary)
names(all_dates)[2] <- "sal"
names(all_dates)[8] <- "Salary Range"
library(scales)

ggplot(all_dates, aes(x = Date, y = `# of New Posts`, fill = `Salary Range`)) + geom_area() +
  ggtitle("# of New Job Listings on Indeed.com by Week and Salary Range") + theme_gray() +
  theme(text = element_text(size = 18), plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0, 300, by = 50)) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  scale_x_date(labels = date_format("%b %Y"), date_breaks = "1 month") + 
  ylab("# of New Job Listings")


##### days
test5 <- aggregate(posts ~ days + sal, all_dates, sum)
names(test5)[3] <- "monies"

all_dates <- left_join(all_dates, test5)
all_dates$days <- as.factor(all_dates$days)

test5$days <- as.numeric(test5$days)

smol2$days3 <- ""
for (i in 1:nrow(smol2))
{
  if (smol2$days[i] == 1)
  {
    smol2$days3[i] <- "2018-04-23"
  }
  if (smol2$days[i] == 2)
  {
    smol2$days3[i] <- "2018-04-24"
  }
  if (smol2$days[i] == 3)
  {
    smol2$days3[i] <- "2018-04-25"
  }
  if (smol2$days[i] == 4)
  {
    smol2$days3[i] <- "2018-04-26"
  }
  if (smol2$days[i] == 5)
  {
    smol2$days3[i] <- "2018-04-27"
  }
  if (smol2$days[i] == 6)
  {
    smol2$days3[i] <- "2018-04-28"
  }
  if (smol2$days[i] == 7)
  {
    smol2$days3[i] <- "2018-04-29"
  }
  
}

test5$`Salary Range` <- test5$sal
for(i in 1:nrow(test5))
{
  if (test5$`Salary Range`[i] == 1)
  {
    test5$salary[i] <- "1: < $25k"
  }
  if (test5$`Salary Range`[i] == 2)
  {
    test5$salary[i] <- "2: $25k to $50k"
  }
  if (test5$`Salary Range`[i] == 3)
  {
    test5$salary[i] <- "3: $50k to $75k"
  }
  if (test5$`Salary Range`[i] == 4)
  {
    test5$salary[i] <- "4: $75k to $100k"
  }
  if (test5$`Salary Range`[i] == 5)
  {
    test5$salary[i] <- "5: $100k to $150k"
  }
  if (test5$`Salary Range`[i] == 6)
  {
    test5$salary[i] <- "6: $150k +"
  }
}


test5$days2 <- wday(as.Date(test5$days, format = "%Y-%m-%d"), label = T)
test5$days2 <- as.numeric(test5$days2)

names(test5)[6] <- "Salary2"
names(test5)[7] <- "Salary Range"

ggplot(test5, aes(x = days2, y = posts, fill = `Salary Range`)) + geom_area(position = "stack") +    theme_gray() + 
  theme(text = element_text(size = 18), plot.title = element_text(hjust = 0.5)) + 
  ggtitle("# of New Job Listings on Indeed.com by Day and Salary Range") +   
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab("# of New Job Listings") + xlab("")


ggtitle("# of New Job Listings on Indeed.com by Week and Salary Range") + theme_gray() +
  theme(text = element_text(size = 18), plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0, 300, by = 50)) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  scale_x_date(labels = date_format("%b %Y"), date_breaks = "1 month") + 
  ylab("# of New Job Listings")


# do clicks!
smol <- indeed[ , c(1:3, 15, 22:23)]

# add labels for price cut offs
smol$salary <- 0
smol$salary <- ifelse(smol$estimatedSalary < 25000, 1, smol$salary)
smol$salary <- ifelse((smol$estimatedSalary >= 25000 & smol$estimatedSalary < 50000), 2, smol$salary)
smol$salary <- ifelse((smol$estimatedSalary >= 50000 & smol$estimatedSalary < 75000), 3, smol$salary)
smol$salary <- ifelse((smol$estimatedSalary >= 75000 & smol$estimatedSalary < 100000), 4, smol$salary)
smol$salary <- ifelse((smol$estimatedSalary >= 100000 & smol$estimatedSalary < 150000), 5, smol$salary)
smol$salary <- ifelse((smol$estimatedSalary >= 150000), 6, smol$salary)

smol2 <- aggregate(clicks~date+salary, smol, sum)

ggplot(test5, aes(x = days2, y = posts, fill = `Salary Range`)) + geom_area(position = "stack") +    theme_gray() + 
  theme(text = element_text(size = 18), plot.title = element_text(hjust = 0.5)) + 
  ggtitle("# of New Job Listings on Indeed.com by Day and Salary Range") +   
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab("# of New Job Listings") + xlab("")

library(dplyr)
# assign weeks
weeks <- trunc((smol2$date - min(smol2$date))/7)+1
weeks <- as.integer(weeks)
weeks <- weeks[-1]
weeks <- append(weeks, 57)
weeks <- as.data.frame(weeks)
smol2 <- cbind(smol2, weeks)


# days
days <- rep(seq(1, 7), 57)
days <- days[-1]
days <- days[-c(396:398)]
days <- rep(days, 6)
days <- as.data.frame(days)
smol2 <- cbind(smol2, days)

weekly_sum <- aggregate(clicks ~ salary + weeks, smol2, sum)
names(weekly_sum)[3] <- "weekly_sum"
smol2 <- left_join(smol2, weekly_sum)

smol2$salary <- as.factor(smol2$salary)
smol2 <- smol2[smol2$weeks != 1, ]
smol2 <- smol2[smol2$weeks != 57, ]

library(scales)

smol2$`Salary Range` <- ""
smol2$`Salary Range` <- ifelse(smol2$salary == 1, "1. < $25k", smol2$`Salary Range`)
smol2$`Salary Range` <- ifelse(smol2$salary == 2, "2. $25k to $50k", smol2$`Salary Range`)
smol2$`Salary Range` <- ifelse(smol2$salary == 3, "3. $50k to $75k", smol2$`Salary Range`)
smol2$`Salary Range` <- ifelse(smol2$salary == 4, "4. $75k to $100k", smol2$`Salary Range`)
smol2$`Salary Range` <- ifelse(smol2$salary == 5, "5. $100k to $150k", smol2$`Salary Range`)
smol2$`Salary Range` <- ifelse(smol2$salary == 6, "6. $150k +", smol2$`Salary Range`)

smol2$days3 <- as.Date(smol2$days3, format = "%Y-%m-%d")

ggplot(smol2, aes(x = days3, y = daily_sum, fill = `Salary Range`)) + geom_area(position = "stack") + 
  ggtitle("# of User Clicks on Indeed.com by Week and Salary Range") + theme_gray() +
  theme(text = element_text(size = 18), plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(breaks = seq(0, 200000, by = 25000)) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_x_date(labels = date_format("%b %Y"), date_breaks = "1 month") + 
  ylab("# of User Clicks") + xlab("Date")


daily_sum <- aggregate(clicks ~ salary + days, smol2, sum)
names(daily_sum)[3] <- "daily_sum"
smol2 <- left_join(smol2, daily_sum)

smol2$days <- as.factor(smol2$days)
smol2$`Salary Range` <- as.factor(smol2$`Salary Range`)

smol2$days2 <- as.Date(smol2$date, format = "%Y-%m-%d")
smol2$days2 <- as.integer(smol2$days2)


smol2$daily_sum <- as.numeric(smol2$daily_sum)

ggplot(data = daily_sum, aes(x = days, y = daily_sum)) + geom_area(aes(fill = "salary"))

#+ ggtitle("# of User Clicks on Indeed.com by Week and Salary Range") + theme_gray() +
  theme(text = element_text(size = 18), plot.title = element_text(hjust = 0.5))
#+ 
  scale_y_continuous(breaks = seq(0, 200000, by = 25000)) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_x_date(labels = date_format("%b %Y"), date_breaks = "1 month") + 
  ylab("# of User Clicks") + xlab("Date")
  
  daily_sum$days <- as.factor(daily_sum$days)
  test <- smol2 %>% group_by(days, `Salary Range`) %>% mutate(new = sum(clicks))
  
  smol3 <- data.frame(rep(c(1:7), each = 6))
  names(smol3)[1] <- "day"
  salary <- rep(seq(1, 6), 7)
  smol3 <- cbind(smol3, salary)
  smol3 <- left_join(smol3, daily_sum)
  
  
  set.seed(345)

  
  Sector <- rep(c( "1. < $25k",
                   "2. $25k to $50k",
                   "3. $50k to $75k",
                   "4. $75k to $100k","5. $100k to $150k","6. $150k +"),times=7)
  Year <- as.numeric(rep(c("1950","1960","1970","1980","1990","2000","2010"),each=6))
  Value <- runif(42, 10, 100)
  data <- data.frame(Sector,Year,Value)
  
  
  ggplot(data, aes(x=Year, y=daily_sum, fill=Sector)) + 
    geom_area() + theme_gray() + 
    theme(text = element_text(size = 18), plot.title = element_text(hjust = 0.5)) + 
    ggtitle("# of User Clicks on Indeed.com by Day and Salary Range") +   
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    ylab("# of User Clicks") + xlab("") +   scale_y_continuous(breaks = seq(0, 1500000, by = 250000))
  
  Year <- rep(seq(1950, 2010, by = 10), each = 6)
data <- left_join(data, daily_sum)  

data <- cbind(data, daily_sum$daily_sum)
names(data)[4] <- "daily_sum"
