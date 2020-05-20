library(haven)
mlmdata <- read_dta("https://stats.idre.ucla.edu/stat/examples/imm/imm10.dta")

model <- lmer(math ~ homework + (1 | schid), data=mlmdata,REML = FALSE)
summary(model)

model <- lmer(math ~ ses+homework + (1 | schid), data=mlmdata,REML = FALSE)
summary(model)

