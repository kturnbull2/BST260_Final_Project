
library(tidyverse)

# policy data
url <- "https://healthdata.gov/sites/default/files/state_policy_updates_20201125_0719.csv"
tmp_filename <- tempfile()
download.file(url, tmp_filename)
policies <- read_csv(tmp_filename)
file.remove(tmp_filename)
write_csv(policies, "data/policies.csv")
