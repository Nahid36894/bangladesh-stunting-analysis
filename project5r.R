# Bangladesh Child Stunting Analysis
# Data: BDHS 2017-18 | Method: Multilevel Logistic

library(haven)
library(lme4)
library(performance)
dhs <- read_dta("C:/Users/mahed/Downloads/BD_2017-18_DHS_05052026_1516_248073/BDKR7RDT/BDKR7RFL.DTA")
dim(dhs)

# Height-for-Age Z-score
dhs$haz <- ifelse(dhs$hw70 > 600 | dhs$hw70 < -600,
                   NA, dhs$hw70 / 100)
# Stunting (HAZ < -2)
dhs$stunted <- ifelse(dhs$haz < -2, 1,
                       ifelse(dhs$haz >= -2, 0, NA))

# Predictors
dhs$mother_educ <- as.numeric(dhs$v106)
dhs$wealth      <- as.numeric(dhs$v190)
dhs$child_age   <- as.numeric(dhs$hw1)
dhs$child_sex   <- ifelse(dhs$b4 == 1, "Male", "Female")
dhs$cluster     <- as.numeric(dhs$v001)

data_dhs <- na.omit(data.frame(
  stunted     = dhs$stunted,
  child_age   = dhs$child_age,
  child_sex   = dhs$child_sex,
  mother_educ = dhs$mother_educ,
  wealth      = dhs$wealth,
  cluster     = dhs$cluster
))

dim(data_dhs)
cat("Stunting prevalence:", mean(data_dhs$stunted) * 100, "%\n")
# Stunting by mother's education
cat("\nStunting by mother's education:\n")
round(tapply(data_dhs$stunted, data_dhs$mother_educ, mean) * 100, 1)
# Stunting by wealth
cat("\nStunting by wealth index:\n")
round(tapply(data_dhs$stunted, data_dhs$wealth, mean) * 100, 1)


model_mlm <- glmer(stunted ~ child_age + child_sex +
                              mother_educ + wealth +
                              (1 | cluster),
                   data   = data_dhs,
                   family = binomial)

summary(model_mlm)

or_table <- exp(cbind(
  OR    = fixef(model_mlm),
  Lower = fixef(model_mlm) - 1.96 * sqrt(diag(vcov(model_mlm))),
  Upper = fixef(model_mlm) + 1.96 * sqrt(diag(vcov(model_mlm)))
))
round(or_table, 3)
icc(model_mlm)