## Presentation mark moderation and distribution
## Andy Wills,  GPL 3

rm(list=ls())

## Load packages
library(tidyverse)

## Load feedback and marks
fback.W1 <- read_csv("Presentation_feedback_workshop1_2024-25.csv") %>%
    add_column(work_ID = 1, .before = TRUE)

fback.W2 <- read_csv("Presentation_feedback_workshop2_2024-25.csv") %>%
    add_column(work_ID = 2, .before = TRUE)

fback.W3 <- read_csv("Presentation_feedback_workshop3_2024-25.csv") %>%
    add_column(work_ID = 3, .before = TRUE)

fback <- bind_rows(fback.W1, fback.W2, fback.W3)

## Load attendances

grps.W1 <- read_csv("Presentation_attendance_workshop1_2024-25.csv") %>%
    add_column(work_ID = 1, .before = TRUE)

grps.W2 <- read_csv("Presentation_attendance_workshop2_2024-25.csv") %>%
    add_column(work_ID = 2, .before = TRUE)

grps.W3 <- read_csv("Presentation_attendance_workshop3_2024-25.csv") %>%
    add_column(work_ID = 3, .before = TRUE)

grps <- bind_rows(grps.W1, grps.W2, grps.W3)


## Check/clean feedback data
fback <- fback |> filter(!is.na(Group_ID)) ## remove any row that doesn't specify a group ID
length(unique(fback$Component)) == 12 ## 12 feedback components
unique(fback$Component) ## They the correct components
data.frame(groupid = unique(fback$Group_ID)) %>% arrange(groupid) ## Sensible-looking group IDs
unique(fback$Score) ## Valid scores

ngroups  <- length(unique(fback$Group_ID)) ## number of groups
ngroups
nrow(fback) / ngroups ## number of rows / number of groups should equal 12
table(fback$Group_ID)  ## find groups with missing feedback.  JB: ???DOES NOT DO WHAT IT SAYS ON THE TIN


## Calculate overall scores
scores <- fback %>% group_by(work_ID, Group_ID) %>%
    summarise(score = mean(Score, na.rm = TRUE))

## Compare workshop groups on summary stats
scores %>% group_by(work_ID) %>%
    summarise(mean = mean(score),
              min = min(score),
              max = max(score),
              sd = sd(score))

##  summary stats  across workshops
scores %>% group_by(work_ID) %>% ungroup() %>%
  summarise(mean = mean(score),
            min = min(score),
            max = max(score),
            sd = sd(score))

#### 2025 comment (JB)
## Workshop 2 is slightly higher, workshop 1 slightly lower, both within one s.d. on the overall mean,
## and N = 12, so let's leave it.

#### 2024 comment (AW)
## Workshop 3 is slightly higher, and is within about one s.d. on the overall mean, 
## and N = 12, so let's leave it. 

#### 2022 comment (AW)
## Although Workshop 1 is slightly higher, the difference is small and given that each
## workshop is an N of 12, some variation is expected.
## Also, workshop 1 is lower mainly because of one very poorly performing group (score = 1.33)

## No statistical moderation required at workshop level.

## Ignore workshop grouping from here on
scores <- fback %>% group_by(Group_ID) %>%
    summarise(score = mean(Score, na.rm = TRUE))

## Summary stats
scores %>% summarise(mean = mean(score), min = min(score), max = max(score),
                     sd = sd(score))
hist(scores$score)

## Convert to mark
scores$mark <- 0

## Lowest mark overall, descrptively "Mainly poor, aspects of patchy", letter: D-
scores$mark[scores$score > 1.32] <- 42
## Descrptively "Mainly patchy, aspects of poor", letter: D
scores$mark[scores$score > 1.77] <- 45
## Descriptively "Mainly patchy, aspects of OK", letter: D+
scores$mark[scores$score > 2.21] <- 48

## Descriptively "Patchy / OK", letter: C-
scores$mark[scores$score > 2.50] <- 52
## Descriptively "Mainly OK, some patchy", letter: C
scores$mark[scores$score > 2.74] <- 55
## Descriptively "OK", letter: C+
scores$mark[scores$score > 2.99] <- 58

## One s.d. below mean, descriptively "OK with aspects of good", letter: B-
scores$mark[scores$score > 3.17] <- 62
## Mean mark, descriptively "good", letter: B
scores$mark[scores$score > 3.99] <- 65
## Interpolated, descriptively "mainly good, aspects of excellence", letter: B+
scores$mark[scores$score > 4.24] <- 68

## Descriptively, more excellent than good. letter: A-
scores$mark[scores$score > 4.50] <- 77
## Highest mark, descriptively "Mostly excellent". letter: A
scores$mark[scores$score > 4.74] <- 88
## Descriptively "Excellent". letter: A+
scores$mark[scores$score > 4.90] <- 100

## Resultant mark distribution
table(scores$mark)

#### 2025 comment (JB)
## five A-, one A, most groups are B- or B. Seems reasonable

#### 2024 comment (AW)
## Largely same above as previous years. Had to add an A+ for exceptional group
## Had to set B- Descriptively rather than -1 s.d. as latter was lower than C+ this year.


######## Create plain text feedback forms

## Recode scores as text
fback$Mark <- recode(fback$Score, `1` = "Poor", `2` = "Patchy",
                     `3` = "OK", `4` = "Good", `5` = "Excellent")

## Ensure 'NA' does not appear as a Mark
fback$Mark[is.na(fback$Mark)] <- ""

## Function to generate text of feedback email
feedback.folder = "feedback"
make.feedback <- function(oneg, mrk) {
    preamble <- c(
    "PSYC520 / PSYC720: Group Presentation",
    "",
    "Your group's presentation was marked independently by two markers, who then agreed the following scores and feedback. The overall mark for each group was calculated by taking the mean across the 12 scores below. The module leader took the set of overall marks, moderated them, and converted them to the grade you see below. This was done with reference to the generic marking criteria you can find in your Stage handbook. The mark awarded is for your group. Every member of the group who attended their group’s presentation gets this mark. Failure to attend results in a mark of zero.",
    "",
    "These marks are provisional, and will remain so until confirmed by the Assessment Board.",
    # "",
    # "DO NOT REPLY TO THIS AUTO-GENERATED EMAIL, as replies will not be received. If you have questions about this feedback, ask your group leader in your next session.",
    ""
)    
    gid <- oneg$Group_ID[1]
    fnam <- paste0(feedback.folder,"/",gid,".txt")
    txt <- c(
        paste0("Group: ", gid), "",
        preamble,"", 
        paste0("OVERALL MARK: ", mrk, "%"), ""
    )
    
    for(rw in 1:nrow(oneg)) {
        txt <- c(txt,
                 paste0(rw,". ", oneg$Component[rw]),
                 if(rw != 12) paste0("SCORE: ", oneg$Mark[rw]),
                 paste0("FEEDBACK: ", oneg$Feedback[rw]),
                 ""
                 )
    }
    fileConn <- file(fnam)
    writeLines(txt, fileConn)
    close(fileConn)
}

## Create feedback files
if (!dir.exists(feedback.folder)) {
  dir.create(feedback.folder) # Create the folder only if it does not exist
}
for(gid in unique(fback$Group_ID)) {
    onef <- fback %>% filter(Group_ID == gid) 
    omark <- scores$mark[scores$Group_ID == gid]
    make.feedback(onef, omark)
}

################### Process attendance data

## Number of duplicate email addresses
sum(duplicated(grps$PU_email))
allDuplicateIndices <- which(duplicated(grps$PU_email) | duplicated(grps$PU_email, fromLast = TRUE)) # get all instances of duplicates
allDuplicateIndices <- allDuplicateIndices[order(grps$PU_email[allDuplicateIndices])] # sort so that duplicates are consecutive
grps[allDuplicateIndices,c("work_ID","Group_ID","PU_email","present")]

## remove duplicates
grps <- grps |> filter(!duplicated(PU_email))

## Count N students on our lists
nrow(grps)

## Load DLE grade books (JB: this should be downloaded from the Grading menu, see https://plymouthpsychology.github.io/rmip/moodle-mark-release.html)
dle520 <- read_csv("PSYC520_DLE_grading_worksheet.csv")
dle720 <- read_csv("PSYC720_DLE_grading_worksheet.csv")
dle <- bind_rows(dle520, dle720)

## Number of duplicated email addresses
sum(duplicated(dle$`Email address`))
dle[duplicated(dle$`Email address`),]

## Count N students on DLE list
nrow(dle)

## Script to find any mismatches between our and DLE lists
grps$`Email address` <- grps$PU_email
full <- full_join(dle, grps, by="Email address")
nrow(full)

print(full |> filter(is.na(PU_email)), n = Inf) # students on the DLE, but not on any class list
print(full |> filter(is.na(`Full name`)), n = Inf) # students in class list, but not on the DLE

##### 2025 comment (JB)
## 3 students on the DLE are not in the group list:
## - AA: has not been attending for a while
## - CH: has not been attending for a while
## - MS: passed the module during 2024 referrals
## 2 students in attendance list, but not marked as students on DLE.
## they both marked as having "no role" and "suspended"
## and S4 indicates that they have withdrawn, so remove them

## Remove students who are not listed in the DLE's grading sheet
grps.dle <- grps |> filter(PU_email %in% dle$`Email address`)


##### 2024 comment (AW)
## Three students we have aren't on DLE. From S4:
## So we shouldn't return a mark for those
## Removed from CSV files

# full$`Full name`[is.na(full$`PU_email`)]

##### 2024 comment (AW)
## One student is LIVE on S4 but is missing from our group lists. 
## We should not return a mark for her

##### 2022 comment (AW)
## Differs by two students. One has interrupted, the other is in Stage 4
## We should not return a mark for either.

## Who was absent?
absent <- grps.dle %>% filter(present != 1)
write_csv(absent, file = "absent-students.csv")
## OK, contact those students

## Here are example emails for students who were absent with or without approved ECs (see later in this script):
## https://plymouthpsychology.github.io/rmip/scripts/absent-withECs.txt
## https://plymouthpsychology.github.io/rmip/scripts/absent-withoutECs.txt

## (They receive feedback via class - email to workshop leads)
# ## Check email system working
# cmd  <- 'mutt -s \"Absence from assessed presentation\" -- andy.wills@plymouth.ac.uk < ec-email.txt'
# system(cmd)
# 
# ## Email all absentees
# for(student in absent$PU_email) {
#     subj <- '"Absence from assessed presentation"'
#     cmd <- paste0(
#         "mutt -s ",
#         subj,
#         " -- ",
#         student,
#         " < ec-email.txt"
#     )
#     print(cmd)
# ##  system(cmd)
# }

## Right, so now who was present?
present <- grps.dle %>% filter(present == 1)

# ## OK, send feedback to those students
# ##sink("dump.txt")
# for(student in present$PU_email) {
#     subj <- '"PSYC520/720: Group presentation mark and feedback"'
#     groupid <- present$Group_ID[present$PU_email == student]
# ##  student <- "andy.wills@plymouth.ac.uk"        
#     cmd <- paste0(
#         "mutt -s ",
#         subj,
#         " -- ",
#         student,
#         " < feedback/",
#         groupid,
#         ".txt"
#     )
#     print(cmd)
#    ## system(cmd)  
# }
# ##sink()

#### Most of this isn't relevant any more, since EC presentations are now due in August.

## OK, so now the EC deadline has passed...

## load in mark sheet from that
## and merge with the original absent list
ec.marks  <- read_csv("ec-presentations.csv")
ec.comb  <- left_join(absent, ec.marks, by="PU_email") %>%  # (only considering absent students here because if students were present, it cancels their ECs)
    select(Group_ID, PU_email, present, EC_presented, EC_type)

# ## Remake feedback files with EC preamble
# make.ec.feedback <- function(oneg, mrk) {
#     preamble <- c(
#     "PSYC520 / PSYC720: Presentation",
#     "",
#     "Your Extenuating Circumstances presentation has been marked; below is your mark, which is the same as for the rest of your group.",
#     "",
#     "These marks are provisional, and will remain so until confirmed by the Board of Examiners.",
#     "",
#     "DO NOT REPLY TO THIS AUTO-GENERATED EMAIL, as replies will not be received. If you have questions about this mark, ask your workshop group leader (Chris Longmore, Michael Verde, or Clare Walsh) during their office hours.", ""
# )
#     gid <- oneg$Group_ID[1]
#     fnam <- paste0("ec_feedback/",gid,".txt")
#     txt <- c(
#         paste0("Group: ", gid), "",
#         preamble,"", 
#         paste0("OVERALL MARK: ", mrk, "%"), ""
#     )
#     fileConn <- file(fnam)
#     writeLines(txt, fileConn)
#     close(fileConn)
# }
# 
# ## Create feedback files
# for(gid in unique(fback$Group_ID)) {
#     onef <- fback %>% filter(Group_ID == gid) 
#     omark <- scores$mark[scores$Group_ID == gid]
#     make.ec.feedback(onef, omark)
# }

# ## Contact those who were present for the EC assessment, and had ECs approved
# ec.success  <- ec.comb %>% filter(EC_presented == 1 & EC_type == "extension")
# write_csv(ec.success, file = "scripts/pres-marks/ec-success.csv")

## Manual email (best to do this for pres, as notify students on DLE should 
## be 'no' as feedback already received via workshop)

# ##sink("dump.txt")
# for(student in ec.success$PU_email) {    
#     subj <- '"PSYC520/720: Extenuating Circumstances presentation mark"'
#     groupid <- ec.success$Group_ID[ec.success$PU_email == student]
#     ##student <- "andy.wills@plymouth.ac.uk"        
#     cmd <- paste0(
#         "mutt -s ",
#         subj,
#         " -- ",
#         student,
#         " < ec_feedback/",
#         groupid,
#         ".txt"
#     )
#     print(cmd)
#     system(cmd)    
# }
# ##sink()


# ## Email all fails
# ec.comb$EC_presented[is.na(ec.comb$EC_presented)] <- 0
# ec.comb$EC_type[is.na(ec.comb$EC_type)] <- "none"
# 
# ec.fail  <- ec.comb %>% filter(EC_presented != 1 & EC_type != "non-attendance")
# write_csv(ec.fail, file = "scripts/pres-marks/ec-fail.csv")
# 
## Manual email (if you want - again DLE handles return of marks)

# for(student in ec.fail$PU_email) {
#     ##student <- "andy.wills@plymouth.ac.uk"        
#     subj <- '"Failure of assessed presentation"'
#     cmd <- paste0(
#         "mutt -s ",
#         subj,
#         " -- ",
#         student,
#         " < ec-fail.txt"
#     )
#     print(cmd)
#     system(cmd)
# }


## Time to return marks to DLE

## 1. Remove those with approved ECs and who were actually absent (will have to take care of those at referrals)
ec.approved  <- ec.comb %>% filter(EC_presented == 1 & EC_type %in% c("non-submission","non-attendance"))
print(ec.approved, n = Inf)
final  <- grps.dle |> filter( !(PU_email %in% ec.approved$PU_email))

## 2. Combine group marks with individual students
all.marks  <- final %>% full_join(scores, by = "Group_ID")

## 3. Where student was not present, change mark to zero
all.marks$mark[all.marks$present == 0]  <- 0
length(which(all.marks$mark == 0))

## 4. Reduce to necessary columns and rename
all.marks  <- all.marks %>% select(`Email address`, mark)
colnames(all.marks)  <- c("Email address", "Graded")  # Use "Graded" instead of "Grade" here to avoid automatic renaming of the variables when left_joining below

## 5. Insert marks in PSYC720 spreadsheet
return720  <- dle720 %>% left_join(all.marks, by = "Email address")
return720$Grade  <- return720$Graded
return720  <- return720 %>% select(-Graded)
return720$`Feedback comments`  <- "" # replace NAs by empty strings because we don't want to see "NA" in the DLE feedback
nrow(return720)

## 6. Save out as CSV for upload to DLE
write_csv(return720, "return720.csv")

## 7. Same rigmarole for PSYC520
return520  <- dle520 %>% left_join(all.marks, by = "Email address")
return520$Grade  <- return520$Graded
return520  <- return520 %>% select(-Graded)
return520$`Feedback comments`  <- "" # replace NAs by empty strings because we don't want to see "NA" in the DLE feedback
return520$Marker  <- ""
nrow(return520)

write_csv(return520, "return520.csv")

## Instructions for uploading and releasing the marks to students
## https://plymouthpsychology.github.io/rmip/moodle-mark-release.html


## Now do summary stats for module report form

# rm(list=ls())
# library(tidyverse)
# return520  <- read_csv("return520.csv")

attempt520 <- return520 %>% filter(Grade != 0 | !is.na(Grade))
attempt520 %>% summarise(mean = mean(Grade), sd = sd(Grade), n = n())

round(table(attempt520$Grade) * 100 / nrow(attempt520),0)

# return720  <- read_csv("return720.csv")

attempt720 <- return720 %>% filter(Grade != 0)
attempt720 %>% filter(Grade != 0) %>% summarise(mean = mean(Grade), sd = sd(Grade), n = n())
round(table(attempt720$Grade) * 100 / nrow(attempt720),0)
