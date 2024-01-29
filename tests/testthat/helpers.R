# ------------------------------------------------------------------------------
# for 'test-predict-boots.R'

# read in data to use in tests
# test_wf: wf using xgboost to predict body_mass_g from all predictors in the
#          palmer penguins dataset. one recipe step - step_dummy(all_nominal())
# test_train: training df of palmer penguins
# test_test: testing df of palmer penguins
test_wf <- readRDS("data/test_wf.rds")
test_train <- read.csv("data/test_train.csv")
test_test <- read.csv("data/test_test.csv")

# load bad wf - same as test_wf but has 1 non-final tuning param
test_wf_bad <- readRDS("data/test_wf_bad.rds")

# ------------------------------------------------------------------------------
# for 'test-summarise-boots.R'

# read in test data
test_preds <- readRDS("data/test_preds.rds")
test_importances <- readRDS("data/test_importances.rds")

