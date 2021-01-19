library(dplyr)
library(data.table)
library(ggplot2)

#loading the dataset
dt = fread("StudentsPerformance.csv")
mydata <- as.data.frame(dt)

#data analysis
summary(mydata)
gender <- table(mydata$gender)
race <- table(mydata$race)
parent_edu <- table(mydata$`parental level of education`)
lunch <- table(mydata$lunch)
test_prep <- table(mydata$`test preparation course`)

##Exploratory data analysis##

#Students By gender:
plot1 <-
  ggplot() +
  geom_bar(data = mydata,
           aes(x = `gender`),
           width = 0.2,
           fill = "red") +
  geom_text(
    stat = 'count',
    data = mydata,
    aes(x = `gender`, label = ..count..),
    vjust = -0.2
  ) +
  theme_bw() +
  xlab("Gender") +
  ylab("Number of Students") +
  theme_classic() +
  ggtitle("Number of Students by Gender") +
  scale_fill_brewer(
    type = "qual",
    palette = 1,
    direction = 1,
    aesthetics = "fill"
  ) +
  ylim(0, 600)
plot1

#Students By race
plot2 <- ggplot() +
  geom_bar(
    data = mydata,
    aes(x = `race/ethnicity`),
    width = 0.6,
    fill = "green"
  ) +
  geom_text(
    data = mydata,
    aes(x = `race/ethnicity`, label = ..count..),
    stat = "count",
    vjust = -0.2
  ) +
  theme_bw() +
  xlab("Race/Ethnicity") +
  ylab("Number of Students") +
  theme(text = element_text(family = "Tahoma")) +
  theme_classic() +
  scale_fill_brewer(
    type = "qual",
    palette = 1,
    direction = 1,
    aesthetics = "fill"
  ) +
  ggtitle("Number of Students by Race/Ethnicity")
plot2


#Distribution of scores
data_new <- mydata %>% mutate(StudentID = row_number()) %>%
  gather(key = "subject", value = "score", `math score`:`writing score`)

data_new %>%
  ggplot(aes(x = score)) +
  geom_histogram(bins = 20,
                 colour = "red",
                 fill = "white")  +
  facet_grid(subject ~ .) +
  theme_bw() +
  theme_classic() +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  stat_bin(
    bins = 20,
    geom = "text",
    aes(label = ..count..),
    vjust = -1
  ) +
  ylim(0, 200)

#Preparation courses
plot3 <- ggplot() +
  geom_bar(
    data = mydata,
    aes(x = `test preparation course`),
    width = 0.6,
    fill = "blue"
  ) +
  geom_text(
    data = mydata,
    aes(x = `test preparation course`, label = ..count..),
    stat = "count",
    vjust = -0.2
  ) +
  facet_grid(. ~ `race/ethnicity`) +
  theme_bw() +
  xlab("Preparation course") +
  ylab("Number of Students") +
  theme(text = element_text(family = "Tahoma")) +
  theme_classic() +
  ggtitle("Number of Students who completed Preparation course by Race/Ethnicity") +
  scale_fill_brewer(
    type = "qual",
    palette = 1,
    direction = 1,
    aesthetics = "fill"
  )
plot3

#average scores by preparation
plot4 <-
  ggplot(data = data_new,
         aes(x = `test preparation course`, y = `score`, fill = `test preparation course`)) +
  geom_boxplot() +
  xlab("Preparation Course") +
  ylab("Average Score") +
  scale_fill_brewer(
    type = "qual",
    palette = 1,
    direction = 1,
    aesthetics = "fill"
  )

plot4

#scores by prep

math.prep = ggplot(data = mydata) +
  geom_boxplot(mapping = aes(x = `test preparation course`, y = `math score`, fill = `test preparation course`)) +
  labs(x = "Test Preparation Course", y = "Math Scores")

reading.prep = ggplot(data = mydata) +
  geom_boxplot(
    mapping = aes(x = `test preparation course`, y = `reading score`, fill = `test preparation course`)
  ) +
  labs(x = "Test Preparation Course", y = "Reading Scores")

writing.prep = ggplot(data = mydata) +
  geom_boxplot(
    mapping = aes(x = `test preparation course`, y = `writing score`, fill = `test preparation course`)
  ) +
  labs(x = "Test Preparation Course", y = "Writing Scores")

math.prep
reading.prep
writing.prep

#scores by prep

math.prep = ggplot(data = mydata) +
  geom_boxplot(mapping = aes(x = `test preparation course`, y = `math score`, fill = `test preparation course`)) +
  labs(x = "Test Preparation Course", y = "Math Scores")

reading.prep = ggplot(data = mydata) +
  geom_boxplot(
    mapping = aes(x = `test preparation course`, y = `reading score`, fill = `test preparation course`)
  ) +
  labs(x = "Test Preparation Course", y = "Reading Scores")

writing.prep = ggplot(data = mydata) +
  geom_boxplot(
    mapping = aes(x = `test preparation course`, y = `writing score`, fill = `test preparation course`)
  ) +
  labs(x = "Test Preparation Course", y = "Writing Scores")

math.prep
reading.prep
writing.prep

#Scores by race/ethnicity
plot5 <-
  ggplot(data = data_new,
         aes(x = `race/ethnicity`, y = `score`, fill = `race/ethnicity`)) +
  geom_boxplot() + theme(axis.text.x = element_blank()) +
  facet_grid(. ~ subject) +
  xlab("Race/ethnicity") +
  ylab("Score")

plot5

#Scores By gender
plot6 <-
  ggplot(data = data_new, aes(x = `gender`, y = `score`, fill = `gender`)) +
  geom_boxplot() +
  facet_grid(. ~ subject) +
  theme(legend.title = element_blank()) +
  xlab("Gender") +
  ylab("Score")

plot6

#t-test

t.test(`math score` ~ `test preparation course`, data = mydata)
t.test(`reading score` ~ `test preparation course`, data = mydata)
t.test(`writing score` ~ `test preparation course`, data = mydata)

t.test(`math score` ~ `gender`, data = mydata)
t.test(`reading score` ~ `gender`, data = mydata)
t.test(`writing score` ~ `gender`, data = mydata)

#linear model
my_lm <- lm(`reading score` ~ `math score`, data = mydata)
plot(mydata$`reading score`, mydata$`math score`)
abline(my_lm , col = "red")

my_lm <- lm(`writing score` ~ `math score`, data = mydata)
plot(mydata$`writing score`, mydata$`math score`)
abline(my_lm , col = "blue")
