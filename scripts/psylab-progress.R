library(tidyverse)

## Load raw data from PsyLab website
raw520  <- read_csv("Psylab_PSYC520-2024-25.csv")
raw720  <- read_csv("Psylab_PSYC720-2024-25.csv")
raw520$Module <- "PSYC520"
raw720$Module <- "PSYC720"
raw <- rbind(raw520,raw720)

## Select colums for progress check
prog  <- raw %>% select(Module, Activity, Student, SRN, Status)

## Reduce to distinct rows
prog  <- distinct(prog)

## number of students
n520 <- length(unique(raw520$SRN))
n720 <- length(unique(raw720$SRN))

## Filter to completed
complete  <- prog %>% filter(Status == "completed")

## Percentage completions by student (520)
completed520  <- complete %>% filter(Module == "PSYC520") %>%
    group_by(SRN) %>% summarise(N = n()) %>% filter(N > 10)
print(sprintf("Overall PSYC520 completion: %d/%d = %.2f%%", nrow(completed520), n520, nrow(completed520) / n520 * 100))

## Count completions by activity (520)
complete %>% filter(Module == "PSYC520") %>% group_by(Module, Activity) %>%
    summarise(pc = round(n()*100/n520, 0)) %>%
    arrange(-pc)

## Percentage completions by student (720)
completed720  <- complete %>% filter(Module == "PSYC720") %>%
    group_by(SRN) %>% summarise(N = n()) %>% filter(N > 10)
print(sprintf("Overall PSYC720 completion: %d/%d = %.2f%%", nrow(completed720), n720, nrow(completed720) / n720 * 100))

## Count completions by activity (720)
complete %>% filter(Module == "PSYC720") %>% group_by(Module, Activity) %>%
    summarise(pc = round(n()*100/n720, 0)) %>%
    arrange(-pc)

## List of people who are failing 520
fails520  <- complete %>% filter(Module == "PSYC520") %>%
    group_by(SRN,Student) %>% summarise(N = n()) %>% filter(N < 11) %>%
    arrange(-N)

## Load grading worksheet from DLE
grad  <- read_csv("Grades-PSYC520.csv")

## Reduce to people who have actually submitted main report
submitted  <- grad %>% filter(Status != "No submission - Not marked")

## Who has submitted report but not complete PsyLab
contact  <- fails520 %>% filter(SRN %in% submitted$`Email address`)

## Urgent email sent via DLE announcements
## Now, go through the whole thing again for 720

## List of people who are failing 520
fails720  <- complete %>% filter(Module == "PSYC720") %>%
    group_by(SRN,Student) %>% summarise(N = n()) %>% filter(N < 11) %>%
    arrange(-N)

## Load grading worksheet from DLE
grad720  <- read_csv("Grades-PSYC720.csv")

## Reduce to people who have actually submitted main report
submitted720  <- grad720 %>% filter(Status != "No submission - Not marked")

## Who has submitted report but not complete PsyLab
contact720  <- fails720 %>% filter(SRN %in% submitted720$`Email address`)

