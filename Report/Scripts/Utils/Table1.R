library(here)
library(knitr)
smry <- read.csv(here("Report", "Output", "vpart.csv"))
kable(smry, format = "latex", caption = "CAP", booktabs = TRUE)