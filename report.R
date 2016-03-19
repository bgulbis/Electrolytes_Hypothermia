# report.R

project <- "Hypothermia Electrolyte Protocol - Regression Models"
authors <- "Michael Ha, Jennifer Gass"

library(readxl)
library(dplyr)
library(BGTools)

data <- read_excel("data/data.xlsx")
names(data) <- make.names(names(data))

data <- data %>%
    mutate(RRT.prior.to.arrest = ifelse(RRT.prior.to.arrest == "Y", TRUE, FALSE),
           Initial.rhythm = factor(Initial.rhythm),
           APACHE.II = ifelse(APACHE.II <= 15, "_<=15",
                              ifelse(APACHE.II >= 16 & APACHE.II <= 20, "_16-20",
                                     ifelse(APACHE.II >= 21 & APACHE.II <= 25, "_21-25",
                                            ifelse(APACHE.II >= 26 & APACHE.II <= 30, "_26-30", "_>=31")))),
           APACHE.II = factor(APACHE.II),
           SOFA = ifelse(SOFA <= 6, "_<=6",
                         ifelse(SOFA >= 7 & SOFA <= 9, "_7-9",
                                ifelse(SOFA >= 10 & SOFA <= 12, "_10-12",
                                       ifelse(SOFA >= 13 & SOFA <=14, "_13-14", "_>=15")))),
           SOFA = factor(SOFA),
           RIFLE = factor(RIFLE)) %>%
    filter(!is.na(Patient)) %>%
    rename(Age = Age..Years..Visit.)

mydoc <- result_docx(project, authors)

regr <- select(data, Age, Initial.rhythm:First.Potassium)
mod1 <- lm(First.Potassium ~ ., data = regr, na.action = na.exclude)
mod2 <- step(mod1, trace = 0)

mydoc <- result_regrmod(mydoc, mod1, "Initial Potassium, All Variables", FALSE)
mydoc <- result_regrmod(mydoc, mod2, "Initial Potassium, Backwards Stepwise", FALSE)

regr <- select(data, Age, Initial.rhythm:SCr.on.admission, First.Magnesium)
mod1 <- lm(First.Magnesium ~ ., data = regr, na.action = na.exclude)
mod2 <- step(mod1, trace = 0)

mydoc <- result_regrmod(mydoc, mod1, "Initial Magnesium, All Variables", FALSE)
mydoc <- result_regrmod(mydoc, mod2, "Initial Magnesium, Backwards Stepwise", FALSE)

regr <- select(data, Age, Initial.rhythm:SCr.on.admission, First.Phosphate)
mod1 <- lm(First.Phosphate ~ ., data = regr, na.action = na.exclude)
mod2 <- step(mod1, trace = 0)

mydoc <- result_regrmod(mydoc, mod1, "Initial Phosphate, All Variables", FALSE)
mydoc <- result_regrmod(mydoc, mod2, "Initial Phosphate, Backwards Stepwise", FALSE)

write_docx(mydoc, file.name = "regression_models.docx")

