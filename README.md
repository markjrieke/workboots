
# workboots <img src="man/figures/logo.png" align="right" width="120" />

**Author:** [Mark Rieke](https://www.thedatadiary.net/about/) <br/>
**License:**
[MIT](https://github.com/markjrieke/workboots/blob/main/LICENSE)

<!-- badges: start -->

[![R-CMD-check](https://github.com/markjrieke/workboots/workflows/R-CMD-check/badge.svg)](https://github.com/markjrieke/workboots/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/workboots)](https://CRAN.R-project.org/package=workboots)
[![](https://cranlogs.r-pkg.org/badges/grand-total/workboots)](https://cran.r-project.org/package=workboots)
<!-- badges: end -->

## Overview

`{workboots}` is a tidy method of generating bootstrap prediction
intervals for arbitrary model types from a tidymodel workflow.

By using [bootstrap
resampling](https://en.wikipedia.org/wiki/Bootstrapping_(statistics)),
we can create many models — one for each resample. Each model will be
slightly different based on the resample it was trained on. Each model
will also generate slightly different predictions for new data, allowing
us to generate a prediction distribution for models that otherwise just
return point predictions.

## Installation

You can install the released version of `{workboots}` from CRAN or the
development version from github with the
[devtools](https://cran.r-project.org/package=devtools) or
[remotes](https://cran.r-project.org/package=remotes) package:

``` r
# install from CRAN
install.packages("workboots")

# or the development version
devtools::install_github("markjrieke/workboots")
```

## Usage

`{workboots}` builds on top of the
[`{tidymodels}`](https://www.tidymodels.org/) suite of packages.
Teaching how to use `{tidymodels}` is beyond the scope of this package,
but some helpful resources are linked at the bottom of this README.

To get started, we’ll need to create a workflow.

``` r
library(tidymodels)

# load our dataset
data("penguins")
penguins <- penguins %>% drop_na()

# split data into testing & training sets
set.seed(123)
penguins_split <- initial_split(penguins)
penguins_test <- testing(penguins_split)
penguins_train <- training(penguins_split)

# create a workflow
penguins_wf <- 
  workflow() %>%
  add_recipe(recipe(body_mass_g ~ ., data = penguins_train) %>% step_dummy(all_nominal())) %>%
  add_model(boost_tree("regression") %>% set_engine("xgboost"))
```

XGBoost can only generate point predictions, but with `{workboots}` we
can train many models to generate many predictions and create a summary
with lower and upper bounds:

``` r
library(workboots)

# generate predictions from 2000 bootstrap models
set.seed(345)
penguins_preds <-
  penguins_wf %>%
  predict_boots(
    n = 2000,
    training_data = penguins_train,
    new_data = penguins_test
  )

# summarise predictions with an upper & lower bound
penguins_preds %>%
  summarise_predictions()
```

    #> # A tibble: 84 x 5
    #>    rowid .preds               .pred_lower .pred .pred_upper
    #>    <int> <list>                     <dbl> <dbl>       <dbl>
    #>  1     1 <tibble [2,000 x 2]>       2913. 3465.       3994.
    #>  2     2 <tibble [2,000 x 2]>       2982. 3535.       4100.
    #>  3     3 <tibble [2,000 x 2]>       3050. 3604.       4187.
    #>  4     4 <tibble [2,000 x 2]>       3477. 4157.       4764.
    #>  5     5 <tibble [2,000 x 2]>       3305. 3868.       4372.
    #>  6     6 <tibble [2,000 x 2]>       2996. 3519.       4078.
    #>  7     7 <tibble [2,000 x 2]>       2914. 3435.       3954.
    #>  8     8 <tibble [2,000 x 2]>       3483. 4072.       4653.
    #>  9     9 <tibble [2,000 x 2]>       2926. 3445.       3966.
    #> 10    10 <tibble [2,000 x 2]>       2876. 3405.       3938.
    #> # ... with 74 more rows

## Bug reports/feature requests

This package is still in its infancy — if you notice a bug, want to
request a new feature, or have recommendations on improving
documentation, please [open an
issue](https://github.com/markjrieke/workboots/issues) in this
repository.

### Tidymodels Resources

-   [Getting started with Tidymodels](https://www.tidymodels.org/start/)
-   [Tidy Modeling with R](https://www.tmwr.org/)
-   [Julia Silge’s Blog](https://juliasilge.com/blog/) provides use
    cases of tidymodels with weekly
    [\#tidytuesday](https://github.com/rfordatascience/tidytuesday)
    datasets.
