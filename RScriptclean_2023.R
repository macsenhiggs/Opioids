tedsd_puf_2023 <- read.csv("~/TEDSD 2017 to 2023/tedsd_puf_2023.csv", header=FALSE)
clean_2023 <- subset(tedsd_puf_2023, select = c(V26, V18, V3, V76, V13, V10, V25, V14, V50, V4, V21, V12, V31))
clean_2023 <- subset(tedsd_puf_2023, subset = V26 >= 3 & V26 <= 5 & V3 == 13 & V26 != -9 & V18 != -9 & V3 != -9 & V13 != -9 & V10 != -9 & V25 != -9 & V14 != -9 & V50 != -9 & V4 != -9 & V21 != -9 & V12 != -9 & V31 != -9, select = c(V26, V18, V3, V76, V13, V10, V25, V14, V50, V4, V21, V12, V31))
clean_2023$V25 <- ifelse(clean_2023$V25 %in% c(5, 12), 1, 0)
clean_2023$V25 <- factor(clean_2023$V25, levels = c(0, 1), labels = c("Other", "Opioid Misuse"))
clean_2023$V18 <- factor(clean_2023$V18, levels = c(1, 2, 3), labels = c("Experiencing Homelessness", "Dependent Living", "Independent Living"))
clean_2023$V3 <- factor(clean_2023$V3, levels = c(13), labels = c("Georgia"))
clean_2023$V13 <- factor(clean_2023$V13, levels = c(1, 2), labels = c("Yes", "No"))
clean_2023$V10 <- factor(clean_2023$V10, levels = c(0, 1), labels = c("No", "One Or More"))
clean_2023$V14 <- factor(clean_2023$V14, levels = c(1, 2), labels = c("Yes", "No"))
clean_2023$V50 <- factor(clean_2023$V50, levels = c(1, 2, 3, 4), labels = c("Private Insurance, Blue Cross/Blue Shield, HMO", "Medicaid", "Medicare, Other", "None"))
clean_2023$V4 <- factor(clean_2023$V4, levels = c(1, 2, 3, 4, 5), labels = c("Less Than One School grade, No Schooling, Nursery School, or Kindergarten To Grade 8", "Grades 9 To 11", "Grade 12 Or GED", "1-3 Years Of College, University, Or Vocational School", "4 Years Of College, University, BA/BS, Some Postgraduate Study, Or More"))
clean_2023$V21 <- factor(clean_2023$V21, levels = c(1, 2, 3, 4, 5, 6, 7), labels = c("Treatment Completed", "Dropped Out Of Treatment", "Terminated By Facility", "Transferred To Another Treatment Program/Facility", "Incarcerated", "Death", "Other"))
clean_2023$V12 <- factor(clean_2023$V12, levels = c(1, 2, 3, 4), labels = c("Full-Time", "Part-Time", "Unemployed", "Not In Labor Force"))
clean_2023$V31 <- factor(clean_2023$V31, levels = c(1, 2, 3, 4, 5), labels = c("Wages/Salary", "Public Assistance", "Retirement/Pension, Disability", "Other", "None"))
clean_2023$V26 <- factor(clean_2023$V26, levels = c(3, 4, 5), labels = c("18-20 Years Old", "21-24 Years Old", "25-29 Years Old"))
clean_2023$V76 <- factor(ifelse(clean_2023$V76 == -9, "Rural", "Other"), levels = c("Other", "Rural"))
names(clean_2023) <- c("Age", "LivingArrangement", "State", "Population", "OpioidTherapy", "PriorEpisode", "Diagnosis", "Co-occurring", "HealthInsurance", "Education", "ReasonDischarge", "Employment", "Income")
View(clean_2023)


clean_2023 <- clean_2023 |>
  mutate(
    V13 = factor(V13, levels = c(1, 2), labels = c("Yes", "No")),
    V14 = factor(V14, levels = c(1, 2), labels = c("Yes", "No"))
    
  )


