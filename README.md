
## workboots

**Author:** [Mark Rieke](https://www.thedatadiary.net/about/) <br/>
**License:**
[MIT](https://github.com/markjrieke/workboots/blob/main/LICENSE)

<!-- badges: start -->

[![R-CMD-check](https://github.com/markjrieke/workboots/workflows/R-CMD-check/badge.svg)](https://github.com/markjrieke/workboots/actions)
<!-- badges: end -->

### Overview

`{workboots}` is a tidy method of generating bootstrap prediction
intervals for arbitrary model types from a tidymodel workflow.

By using [bootstrap
resampling](https://en.wikipedia.org/wiki/Bootstrapping_(statistics)),
we can create many models — one for each resample. Each model will be
slightly different based on the resample it was trained on. Each model
will also generate slightly different predictions for new data, allowing
us to generate a prediction distribution for models that otherwise just
return point predictions.

### Installation

Currently, this package lives exclusively on github, though work is in
progress to publish to CRAN. You can install the development version
with the [devtools](https://cran.r-project.org/package=devtools) or
[remotes](https://cran.r-project.org/package=remotes) package:

``` r
devtools::install_github("markjrieke/workboots")
```

### Usage

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

penguins_preds <-
  penguins_wf %>%
  predict_boots(
    n = 100,
    training_data = penguins_train,
    new_data = penguins_test
  )

penguins_preds %>%
  summarise_predictions()
```

    ## # A tibble: 84 x 5
    ##    rowid .preds             .pred_lower .pred .pred_upper
    ##    <int> <list>                   <dbl> <dbl>       <dbl>
    ##  1     1 <tibble [100 x 2]>       3308. 3456.       3596.
    ##  2     2 <tibble [100 x 2]>       3334. 3535.       3778.
    ##  3     3 <tibble [100 x 2]>       3307. 3596.       3822.
    ##  4     4 <tibble [100 x 2]>       3781. 4139.       4499.
    ##  5     5 <tibble [100 x 2]>       3638. 3854.       4082.
    ##  6     6 <tibble [100 x 2]>       3300. 3493.       3762.
    ##  7     7 <tibble [100 x 2]>       3276. 3432.       3570.
    ##  8     8 <tibble [100 x 2]>       3758. 4036.       4358.
    ##  9     9 <tibble [100 x 2]>       3276. 3452.       3614.
    ## 10    10 <tibble [100 x 2]>       3254. 3407.       3648.
    ## # ... with 74 more rows

### Bug reports/feature requests

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
