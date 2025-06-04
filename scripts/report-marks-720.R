## Report mark moderation and distribution
## Andy Wills,  GPL 3

## Load packages
library(tools)
library(tidyverse)

base.folder <- "C:/Users/jbesle/OneDrive - University of Plymouth/Teaching/PSYC520/Module leader/Moderation 2024-25"
sample.folder <- "Report marks 720"
input.grading.sheet <- "ReportGradingSheet-PSYC720.csv"
output.grading.sheet <- "ReportGradingSheet-PSYC720-withMarks.csv"

## Load marks
markers  <- list.files(paste(base.folder,sample.folder,sep="/"))

marks  <- NULL
for (this.marker in markers) {
  tmp  <- read_csv(paste(base.folder,sample.folder,this.marker,sep="/"))
  marker  <- file_path_sans_ext(this.marker)
  tmp  <- cbind(marker, tmp)
  print(this.marker)
  marks  <- rbind(marks, tmp)
}

## Check/clean data
length(unique(marks$Component)) ## Should be 25 different components
unique(marks$Score) ## Valid scores

## Check we have the right number of components for each student
chk  <- table(marks$Identifier)
max(chk)
min(chk)

## Calculate overall scores
scores <- marks %>% group_by(marker, Identifier) %>% summarise(score = mean(Score))

## Compare markers on summary stats
cmp.markers  <- scores %>% group_by(marker) %>%
    summarise(
        N = n(),
        mean = mean(score),
        sd = sd(score)) %>%
    arrange(mean)
print(cmp.markers)
boxplot(cmp.markers$mean)

##2025
mean(cmp.markers$mean) # 3.28
sd(cmp.markers$mean) # 0.20

## No markers are outliers as assessed by a typical boxplot procedure.
## all markers have an average mark that is within one s.d. of the
## other markers' average marks (3.03-3.51)

## 2024
mean(cmp.markers$mean) # 3.6
sd(cmp.markers$mean) #

## No markers are outliers as assessed by a typical boxplot procedure.

## 2022
mean(cmp.markers$mean) # 3.9
sd(cmp.markers$mean) # 0.1

## To one d.p., all markers have an average mark that is within one s.d. of the
## other markers' average marks (3.8-4.0). There's little evidence here for
## differences in marking.

## Now look at score distribution

## 2025
mean(scores$score) # 3.3 - Down 0.3 from 2024 and 0.6 from 2022
sd(scores$score) # 0.6 - consistent with previous years
min(scores$score) # 2.6 - up 0.5 from 2024
max(scores$score) # 4.7 - consistent with previous years

## 2024
mean(scores$score) # 3.5 - Down 0.4 from 2022
sd(scores$score) # 0.7 - Up 0.2 from 2022
min(scores$score) # 2.1 - Down 0.5 from 2022
max(scores$score) # 4.8 - Same as 2022

# 2022
hist(scores$score)
mean(scores$score) # 3.9 - Up 0.3 from last year in depths of pandemic
sd(scores$score) # 0.5 - same as last year
min(scores$score) # 2.6 - Similar to last year where worst report was 2.7 and awarded C-
max(scores$score) # 4.8 - Up .3 from last year (4.5, awarded A-)

## Convert to mark, using the same thresholds as 520
## Marking criteria have already accounted for the higher standard expected
## And any mark below a 50 is a fail on this programme.
scores$mark <- 0

#### 2025
scores$mark[scores$score > 0.90] <- 15 ## Descriptively "Poor": F-
scores$mark[scores$score > 1.25] <- 25 ## Descriptively "Mostly poor": F
scores$mark[scores$score > 1.50] <- 42 ## Descriptively "More patchy than poor": D-
scores$mark[scores$score > 1.75] <- 45 ## Descriptively "Mostly patchy, some poor": D
scores$mark[scores$score > 1.99] <- 48 ## Descriptively "Mostly patchy": D+
scores$mark[scores$score > 2.33] <- 52 ## Descriptively "Mostly patchy, some OK": C-
scores$mark[scores$score > 2.66] <- 55 ## Descriptively "Mostly OK, some patchy", letter: C
scores$mark[scores$score > 2.99] <- 58 ## Descriptively "Mostly OK", letter: C+
scores$mark[scores$score > 3.33] <- 62 ## Descriptively "Mostly OK, some good", letter: B-
scores$mark[scores$score > 3.66] <- 65 ## Descriptively "Mostly good, some OK", letter: B
scores$mark[scores$score > 3.99] <- 68 ## Descriptively "Mostly good", letter: B+
scores$mark[scores$score > 4.25] <- 77 ## Descriptively "Mostly good, some aspects of excellent", letter: A-
scores$mark[scores$score > 4.50] <- 88 ## Descriptively "Mostly excellent". Letter: A
scores$mark[scores$score > 4.75] <- 100 ## Highest mark. Descriptively "Excellent". Letter: A+

# I modified the marking scheme compared to previous years to match the same changes in PSYC520,
# I moved all thresholds accordingly, such that:
# Mostly poor -> fail
# Mostly patchy -> D- to D+
# Mostly OK -> C+ to C-
# Mostly good -> B- to B+
# Mostly excellent -> A- to A+

## 2022-2024
scores$mark[scores$score > 1.5] <- 45 ## Descriptively "poor / patchy": D
scores$mark[scores$score > 1.75] <- 48 ## Descriptively "poor / patchy": D+
scores$mark[scores$score > 1.99] <- 52 ## Descriptively, patchy: C-
scores$mark[scores$score > 2.25 ] <- 55 ## Descriptively "more patchy than OK", letter: C
scores$mark[scores$score > 2.75 ] <- 58 ## Descrptively "Mainly OK, some patchy", letter: C+
scores$mark[scores$score > 3.25 ] <- 62 ## Descriptively "OK, some good", letter: B-
scores$mark[scores$score > 3.50] <- 65 ## Descriptively "OK/good", letter: B
scores$mark[scores$score > 3.99] <- 68 ## Descriptively "good", letter: B+
scores$mark[scores$score > 4.25] <- 77 ## Descriptively "mainly good, aspects of excellence", letter: A-
scores$mark[scores$score > 4.50] <- 88 ## Highest mark. Descriptively, more excellent than good. Letter: A
scores$mark[scores$score > 4.75] <- 100 ## Highest mark. Descriptively, mainly excellent. Letter: A+


## Resultant mean score

## 2025
mean(scores$mark) # 60.7
sd(scores$mark)   # 8.1

## 2024
mean(scores$mark) # 64.7
sd(scores$mark)   # 10.0

## 2022
mean(scores$mark) # 69.7
sd(scores$mark)   # 10.4

## Resultant mark distribution
table(scores$mark)
round(table(scores$mark) *100 / nrow(scores),1)

## Load DLE grade book
dle <- read_csv(paste(base.folder,input.grading.sheet,sep="/"))

## Reduces 'scores' to required columns
grades  <- scores %>% ungroup %>% select(Identifier, mark, marker)

## Combined by Identifier
full  <- full_join(dle, grades)

View(full)

## 2022
## Visual inspection reveals SDordoy coded one as "Participant_" rather than
## "Participant ", now hand corrected in CSV file.

## Where student did not submit, and hence we have NA as a mark, record mark as zero.
full$mark[is.na(full$mark)]  <- 0

## Apply mark penalties for reported infractions

## 2025
## Participant 13594056 - multiple page limit violations due to not following template margins
## Participant 13594058 - 1x page limit violation (discussion is 2 & 1/2 pages)
full$mark[full$Identifier == "Participant 13594056"] <- 52
full$mark[full$Identifier == "Participant 13594058"] <- 52


## The following reports showed evidence of collusion (as reported by the markers):
## - Participants 13547085 and 13521660 and 13547083 had identical word-for-word Materials sections
## - Participants 13547085 and 13521660  have nearly identical Procedure sections.
## - Participants 13594053, 13594056 & 13594061: these contained passages of text that were completely identical (word for word). They were only on smaller sections (typically methodology, hypotheses, or results)
## I looked at all 6 reports and confirmed extensive similarities in the method and/or results sections for each set of 3.
## i will report the students for academic misconduct. I'm leaving the marks as they are, import them into the DLE with the other, but not release the marks.
# (submissions can be identified from file names after revealing student identities on the DLE assignment submission point)

# 2022
## Participant 11859375 - Half-a-page over on Discussion. Marked dropped, student informed.
full$mark[full$Identifier == "Participant 11859375"] <- 62
## Participant 11859380 - 2.5 lines over. Warning given, mark not deducted.

## Copy across mark to correct column
full$Grade  <- full$mark

## Reduce to necessary columns
report  <- full %>% select(-mark, -marker)

## Clear feedback column
report$`Feedback comments`  <- ""
# change marking status
report$`Marking workflow state (Release grades and feedback)` <- "Marking completed"

## Save out as CSV for upload to DLE
write_csv(report, paste(base.folder,output.grading.sheet,sep="/"))

## I also checked the written feedback on a sample of each of the
## markers. It was good, and pretty consistent across markers.

## Now, module review form
attempt <- report %>% filter(Grade != 0)
attempt %>% summarise(mean = mean(Grade), sd = sd(Grade), n = n())
round(table(attempt$Grade) * 100 / nrow(attempt),1)

## 2022 DLE fuckup fixing
full$mark[full$Identifier == "Participant 11859380"]
full$mark[full$Identifier == "Participant 11859375"]
