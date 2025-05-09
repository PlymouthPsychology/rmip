## Pre-moderation script
library(tools)
library(tidyverse)
library(stringr)

base.folder <- "C:/Users/jbesle/OneDrive - University of Plymouth/Teaching/PSYC520/Module leader/Moderation 2025"
sample.folder <- "Reports sample marks 2025"

marks  <- NULL
markers  <- list.files(paste(base.folder,sample.folder,sep="/"))
for (this.marker in markers) {
    tmp  <- read_csv(paste(base.folder,sample.folder,this.marker,sep="/"))
    marker  <- file_path_sans_ext(this.marker)
    tmp  <- cbind(marker, tmp)
    marks  <- rbind(marks, tmp)
}

# Remove identical substring "Participant" at start of all identifiers
marks <- marks %>%
  mutate(Identifier = str_remove(Identifier, "^Participant "))

write_csv(marks, paste(base.folder,"mod-scores-2025.csv",sep="/"))

## 4 Participants - how many unique identifiers?
unique(marks$Identifier)

## Mean score
grandmean  <- marks %>% summarise(mark = round(mean(Score),1)) |> pull(mark)
grandmean

### 2025
## So, across four reports, the mean score across 10 markers was 3.5, slightly higher than in 2022 and 2024

### 2024
## So, across four reports, the mean score across 12 markers was 3.2
## This is the same as 2022.

### 2022
## So, across four reports, the mean score across 10 markers was 3.2

## Overall score by Identifier
marks %>% group_by(Identifier) %>% summarise(mark = round(mean(Score),1))

# Grades by marker and Identifier
grades  <- marks %>% group_by(marker, Identifier) %>% summarise(mark = mean(Score))
grades %>% pivot_wider(names_from = Identifier, values_from = mark)

# Ranks by marker
ranks <- grades %>%
  group_by(marker) %>%
  mutate(rank_per_marker = rank(mark)) %>%
  select(-mark)
ranks %>% pivot_wider(names_from = Identifier, values_from = rank_per_marker)

# Graph of marks per marker
samples <-levels(factor(grades$Identifier))
graph  <- grades %>%
  ggplot(aes(x=factor(Identifier, levels =samples),
             y=mark, group=marker)) +
  geom_line(aes(colour=marker)) +
  geom_point()

graph + facet_grid(marker ~ .)

#### 2025
## This year, I picked a narrower range than previous years, from 2.9 to 3.9
## The four reports are approx. equidistant in the range
## All markers agreed on the ranking of the best report,
## Most markers (12 out of 13)n on the ranking of the worst report
## and almost as many (10 out of 13 agree) on the ranking between 2 and 3

#### 2024
## This year, I managed to pick a wide range of - 2.6 to 4.1
## There's three clear categories of rank here: lowest (2), middle, highest
## Most markers agreed with this

### 2022
## This year, I managed to pick a wide range of - 2.0 to 4.3
## There's three clear categories of rank here: lowest, middle (2), highest
## Most markers agreed with this


## Mean scores, by marker
overall  <- marks %>% group_by(marker) %>% summarise(mark = round(mean(Score),1)) %>% arrange(mark)
hist(overall$mark)
overall


## Most markers were individually close to the group mean
## However the following were > +/- 0.3 away, suggesting over/under marking relative to peers:
overall %>% filter(mark < grandmean-0.3) #Too harsh
overall %>% filter(mark > grandmean+0.3) #Too generous


## Which components on which reports have the most variance between markers?
score.by.comp  <- marks %>% group_by(Description, Component, marker, Identifier) %>% summarize(Score = mean(Score))
score.sd  <- score.by.comp %>% group_by(Description, Component, Identifier) %>% summarise(SD = sd(Score)) %>% arrange(-SD)
hist(score.sd$SD)
tops <- score.sd %>% arrange(-SD)
View(tops)

#### 2025
## Fairly Impressive, SD below 1 for almost all component-Identifier pairs
## The top 6 SDs (>1) occurred for 2 reports (but different components).

## The highest SD is for 'Sample sizes are justified through a power calculation' (component 8)
marks %>% filter(Component == 8) %>% filter(Identifier == "13472268") %>%
  select(marker, Score)

## The next highest is for 'Discussion provides good considerations of the limitations of the current study' (component 20)
marks %>% filter(Component == 20) %>% filter(Identifier == "13472270") %>%
  select(marker, Score)


#### 2024
## Impressive, SD close or below 1 for almost all component-SRN pairs, and none
## of the top 3 SDs occurred for more than 1 report.
## Distribution tails are heavy, but there's not clear outlier

## It looks like the one clear outlier here this year is 'Figures have an
## appropriate legend that is referred to in the main text'

marks %>% filter(Component == 13) %>% filter(Identifier == 3) %>%
  select(marker, Score)

## The next highest variances is:

## Results section includes appropriate and accurate inferential stats (Bayesian ANOVA)

marks %>% filter(Component == 15) %>% filter(Identifier == 4) %>%
  select(marker, Score)


#### 2022
## Fairly impressive, SD close or below 1 for most component-SRN pairs, and none
## of the higher SDs occurred for more than 1 report.

## It looks like the one clear outlier here this year is 'Correct formatting of
## sections and sub-sections'
marks %>% filter(Component == 25) %>% filter(SRN == 10686215) %>%
    select(marker, Score)

## The next three highest variances are:

## Discussion concludes appropriately

marks %>% filter(Component == 22) %>% filter(SRN == 10685499) %>%
    select(marker, Score)

## Apparatus and materials sections are clear, accurate, and contain correct information

marks %>% filter(Component == 9) %>% filter(SRN == 10685499) %>%
    select(marker, Score)

## Discussion makes appropriate links to previous literature

marks %>% filter(Component == 19) %>% filter(SRN == 10685499) %>%
    select(marker, Score)

