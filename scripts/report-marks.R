## Report mark moderation and distribution
## Andy Wills,  GPL 3, modified by Julien Besle

## Load packages
library(tools)
library(tidyverse)

base.folder <- "C:/Users/jbesle/OneDrive - University of Plymouth/Teaching/PSYC520/Module leader/Moderation 2024-25"
sample.folder <- "Report marks 520"
input.grading.sheet <- "ReportGradingSheet-PSYC520.csv"
output.grading.sheet <- "ReportGradingSheet-PSYC520-withMarks.csv"


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
marks |> filter(is.na(Component) | is.na(Score))

#### 2025
# Greg left 3 students blank (who did not submit) and separated students by empty rows
# Matthew left 1 student blank (who did not submit)

#### 2024
# Nomi used one 6, I changed to a 5.

#### 2022
## SHartgen had missed one mark (no entry)

## NKout entered Participant 11859590 identifier twice (the 2nd one was
## actually Participant 11859560)

## CJones had entered combo of Participant 11859439 and SRN 10682068
## twice. I cannot recover which reports these two are without
## further input.

# Let's remove empty scores
marks <- marks |> filter(!is.na(Score))

nComponents <- length(unique(marks$Component)) ## Should be 2 different components (#16 missing from mark sheet)
unique(marks$Component)
unique(marks$Score) ## Valid scores

## Check we have the right number of components for each student
chk  <- table(marks$Identifier)
max(chk)
min(chk)
# If not, then need to find the issues:
marks %>% count(Identifier) %>% filter (n != nComponents)
print(marks %>% group_by(Identifier) %>% filter(n() != nComponents), n= Inf)

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

#### 2025
mean(cmp.markers$mean) # 3.48
sd(cmp.markers$mean) # 0.26

# Most markers have an average mark within 1 SD of the grand average (3.22-3.74), except:
# - Alvise is at 3.09: he was slightly below the average at pre-moderation, but not by much
# - Spencer is at 3.82: he was slightly above the average at pre-moderation, but, again, not my much
# - Louise is at 4.02: she was slightly below the average at pre-moderation
# Since there is no suggestions that any of them significantly under- or over-marked at pre-moderation,
# I won't moderate

#### 2024
mean(cmp.markers$mean) # 3.43
sd(cmp.markers$mean) # 0.14

## To two d.p., most markers have an average mark that is close to one s.d. of the other
## markers' average marks (3.29-3.57).

## The exception was:

## STalbot (3.77, N=25) - Spencer was in the middle of the disribution on
## pre-moderation. And he has a range of marks from 2.96 (OK) to 4.36 (good /
## excellent).

scores %>% filter(marker == "STalbot") %>% arrange(score) %>% print(n=Inf)
scores %>% filter(marker == "RStatton") %>% arrange(score) %>% print(n=Inf)

#### 2022
mean(cmp.markers$mean) # 3.6
sd(cmp.markers$mean) # 0.3

## To one d.p., most markers have an average mark that is within one s.d. of the other
## markers' average marks (3.3-3.9).

## The exceptions are:

## DGraham (3.14, N=18) - Daniel was new to marking this year and was towards
## the bottom of the distribution on pre-moderation. Of the 18 scripts he
## marked, none averaged a good (4.0) score, and about a third were below "OK"
## (3.0). Overall I think this merits a minor correction upwards for Daniel's
## marking. Increased by minimum required to bring mean within 1 s.d. of grand
## mean.
scores %>% filter(marker == "DGraham") %>% arrange(score)
scores$score[scores$marker == "DGraham"]  <- scores$score[scores$marker == "DGraham"] + .16

## DDjama (4.03, N=22) - Despina was close to the centre of the distribution in
## pre-moderation. She is a highly experienced marker on this module. There seems
## to have been four reports in her set that were exceptionally good (>4.5 average).
## It is likely these few reports that make the difference, so overall I think
## no correction is needed, and to do so would run the risk of inappopriately
## disadvantaging some high-performing students. NO CORRECTION MADE.
scores %>% filter(marker == "DDjama") %>% arrange(score) %>% print(n=Inf)


## Now look at score distribution
hist(scores$score)

#### 2025
mean(scores$score) # 3.5 - Pretty stable compared to previous years
sd(scores$score) # 0.6 - Pretty stable compared to previous years
min(scores$score) # 1 - Lower than 2022 (1.7), but this is because of one student submitted the wrong report
max(scores$score) # 4.7 - Pretty stable compared to previous years

#### 2024
mean(scores$score) # 3.4 - Down .2 from 2022, pretty stable
sd(scores$score) # 0.6 - Down .1 from 2022, fairly stable
min(scores$score) # 1 - Lower than 2022 (1.7), but this is because of one unusually poor report
max(scores$score) # 4.8 - Same as last year, where highest mark as an A.

#### 2022
mean(scores$score) # 3.6 - Up .2 from last year, in depths of pandemic
sd(scores$score) # 0.7 - Same as last year
min(scores$score) # 1.8 - Similar to last year (1.7), where we made this a D

## Convert to mark, using the same thresholds as last two years

scores$mark <- 0

#### 2025
scores$mark[scores$score > 0.90] <- 15 ## Descriptively "Poor": F-
scores$mark[scores$score > 1.20] <- 25 ## Descriptively "Mostly poor": F
scores$mark[scores$score > 1.35] <- 38 ## Descriptively "Mostly poor, some patchy": F+
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

# I modified the marking scheme compared to previous years to avoid passing students who submit a mostly poor report,
# I moved all thresholds accordingly, such that:
# Mostly poor -> fail
# Mostly patchy -> D- to D+
# Mostly OK -> C+ to C-
# Mostly good -> B- to B+
# Mostly excellent -> A- to A+

#### alternative for 2025 (not used)
scores$mark[scores$score > 0.90] <- 15 ## Descriptively "Poor": F-
scores$mark[scores$score > 1] <- 25 ## Descriptively "Mostly poor": F
scores$mark[scores$score > 1.2] <- 38 ## Descriptively "Mostly poor, some patchy": F+
scores$mark[scores$score > 1.5] <- 42 ## Descriptively "More patchy than poor": D-
scores$mark[scores$score > 1.8] <- 45 ## Descriptively "Mostly patchy, some poor": D
scores$mark[scores$score > 2.2] <- 48 ## Descriptively "Mostly patchy": D+
scores$mark[scores$score > 2.5] <- 52 ## Descriptively "Mostly patchy, some OK": C-
scores$mark[scores$score > 2.8] <- 55 ## Descriptively "Mostly OK, some patchy", letter: C
scores$mark[scores$score > 3.2] <- 58 ## Descriptively "Mostly OK", letter: C+
scores$mark[scores$score > 3.5] <- 62 ## Descriptively "Mostly OK, some good", letter: B-
scores$mark[scores$score > 3.8] <- 65 ## Descriptively "Mostly good, some OK", letter: B
scores$mark[scores$score > 4.2] <- 68 ## Descriptively "Mostly good", letter: B+
scores$mark[scores$score > 4.4] <- 77 ## Descriptively "Mostly good, some aspects of excellent", letter: A-
scores$mark[scores$score > 4.6] <- 88 ## Descriptively "Mostly excellent". Letter: A
scores$mark[scores$score > 4.8] <- 100 ## Highest mark. Descriptively "Excellent". Letter: A+


#### 2024
scores$mark[scores$score > .9] <- 15 ## Descriptively "poor": F-
scores$mark[scores$score > 1] <- 25 ## Descriptively "poor / patchy": F
scores$mark[scores$score > 1.1] <- 25 ## Descriptively "poor / patchy": F+
scores$mark[scores$score > 1.25] <- 42 ## Descriptively "mainly poor, some patchy": D-
scores$mark[scores$score > 1.5] <- 45 ## Descriptively "poor / patchy": D
scores$mark[scores$score > 1.75] <- 48 ## Descriptively "mainly patchy, some poor": D+
scores$mark[scores$score > 1.99] <- 52 ## Descriptively, patchy: C-
scores$mark[scores$score > 2.25 ] <- 55 ## Descriptively "more patchy than OK", letter: C
scores$mark[scores$score > 2.75 ] <- 58 ## Descriptively "Mainly OK, some patchy", letter: C+
scores$mark[scores$score > 3.25 ] <- 62 ## Descriptively "OK, some good", letter: B-
scores$mark[scores$score > 3.50] <- 65 ## Descriptively "OK/good", letter: B
scores$mark[scores$score > 3.99] <- 68 ## Descriptively "good", letter: B+
scores$mark[scores$score > 4.25] <- 77 ## Descriptively "mainly good, aspects of excellence", letter: A-
scores$mark[scores$score > 4.50] <- 88 ## Descriptively, more excellent than good. Letter: A
scores$mark[scores$score > 4.75] <- 100 ## Highest mark. Descriptively, mainly excellent. Letter: A+

## Resultant mean score

#### 2025
mean(scores$mark) # 62.3
sd(scores$mark)   # 8.0
nrow(scores)      # 277

#### 2024
mean(scores$mark) # 62.9
sd(scores$mark)   #  8.0
nrow(scores)      # 261

#### 2022
mean(scores$mark) # 65.8
sd(scores$mark)   # 10.3
nrow(scores)      # 173

## Resultant mark distribution
table(scores$mark)
round(table(scores$mark) *100 / nrow(scores))

# Marks are lower this year because I modified the marking scheme (see above)

## Load DLE grade book
dle <- read_csv(paste(base.folder,input.grading.sheet,sep="/"))

## Reduces 'scores' to required columns
grades  <- scores %>% ungroup %>% select(Identifier, mark, marker)

## Combined by Identifier
full  <- full_join(dle, grades)

View(full)
## Visual inspection (all seems OK)

## Where student did not submit, and hence we have NA as a mark, record mark as zero.
full$mark[is.na(full$mark)]  <- 0

#### 2025

## The following reports had technical infractions:
## Over page limit:
##  - participant 13472366: discussion 4 lines over the limit
##  - Participant 13478793: discussion almost a full page over the limit
##  - Participant 13650470: discussion over the page limit y more than a paragraph
##  - participant 13472353: discussion 4 lines over the limit

## Not conforming to style guidelines:
##  - Participant 13650491: margins too wide

full$mark[full$Identifier == "Participant 13472366"] <- 55
full$mark[full$Identifier == "Participant 13478793"] <- 52
full$mark[full$Identifier == "Participant 13650470"] <- 55
full$mark[full$Identifier == "Participant 13472353"] <- 52
full$mark[full$Identifier == "Participant 13650491"] <- 58

## I informed students by adding a line to the general feedback section on the first page of the report.

## The following reports showed evidence of collusion:
## Participant 13472378 & Participant 13650415), as reported by the marker
## I inspected the reports and judged that the infraction was not severe enough to be reported as academic offense


#### 2022
## Apply mark penalties for reported infractions
## Participant 11859420 - Full page over on Discussion. Marked dropped, student informed.
full$mark[full$Identifier == "Participant 11859420"] <- 55

## The following were technical infractions, but minor so warning given rather than
## mark deduction:
## Participant 11859536
## Participant 11859572
## Participant 11859429

## Plagiarism? No reports of plagiarism from markers.


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

## When uploading marks on the DLE, check the box that says "Allow updating records that have been modified more recently in Moodle than in the spreadsheet."

## I also checked the written feedback on a sample of each of the 14
## markers. It was mostly good, and pretty consistent across markers.
## One marker did not provide general feedback on the first page and
## I asked them to add it

## Now, module review form
attempt <- report %>% filter(Grade != 0)
attempt %>% summarise(mean = mean(Grade), sd = sd(Grade), n = n())
round(table(attempt$Grade) * 100 / nrow(attempt),1)
