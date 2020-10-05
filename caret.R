library('caret')
library('tidyverse')
picks <- read.csv('Data/picks.csv')
glimpse(picks)
clean_picks <- select(picks, -RET_TO_PICK, -SUP_TO_PICK, -DURATION)

set.seed(1)
ind <- createDataPartition(y = clean_picks$SECONDS_PER_PICK, p=0.7, list=FALSE)
train <- slice(clean_picks, ind)
test <- slice(clean_picks, -ind)

skimr::skim(train)
