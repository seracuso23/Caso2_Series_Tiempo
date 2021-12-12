#https://www.business-science.io/code-tools/2021/03/15/introducing-modeltime-h2o.html

library(tidymodels)
library(modeltime.h2o)
library(tidyverse)
library(timetk)

data_tbl <- walmart_sales_weekly %>%
  select(id, Date, Weekly_Sales)

summarytools::freq(data_tbl$id)

data_tbl %>% 
  group_by(id) %>% 
  plot_time_series(
    .date_var    = Date,
    .value       = Weekly_Sales,
    .facet_ncol  = 2,
    .smooth      = F,
    .interactive = F
  )

splits <- time_series_split(data_tbl, assess = "3 month", cumulative = TRUE)

dim(training(splits))
dim(testing(splits))

recipe_spec <- recipe(Weekly_Sales ~ ., data = training(splits)) %>%
  step_timeseries_signature(Date) 

train_tbl <- training(splits) %>% bake(prep(recipe_spec), .)
test_tbl  <- testing(splits) %>% bake(prep(recipe_spec), .)


h2o.init(
  nthreads = -1,
  ip       = 'localhost',
  port     = 54321
)

# Optional - Turn off progress indicators during training runs
h2o.no_progress()

model_spec <- automl_reg(mode = 'regression') %>%
  set_engine(
    engine                     = 'h2o',
    max_runtime_secs           = 5, 
    max_runtime_secs_per_model = 3,
    max_models                 = 3,
    nfolds                     = 5,
    exclude_algos              = c("DeepLearning"),
    verbosity                  = NULL,
    seed                       = 786
  ) 

model_spec


model_fitted <- model_spec %>%
  fit(Weekly_Sales ~ ., data = train_tbl)

model_fitted

predict(model_fitted, test_tbl)

modeltime_tbl <- modeltime_table(
  model_fitted
) 

modeltime_tbl
modeltime_tbl %>%
  modeltime_calibrate(test_tbl) %>%
  modeltime_forecast(
    new_data    = test_tbl,
    actual_data = data_tbl,
    keep_data   = TRUE
  ) %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .facet_ncol = 2, 
    .interactive = FALSE
  )

data_prepared_tbl <- bind_rows(train_tbl, test_tbl)

future_tbl <- data_prepared_tbl %>%
  group_by(id) %>%
  future_frame(.length_out = "1 year") %>%
  ungroup()

future_prepared_tbl <- bake(prep(recipe_spec), future_tbl)

refit_tbl <- modeltime_tbl %>%
  modeltime_refit(data_prepared_tbl)


refit_tbl %>%
  modeltime_forecast(
    new_data    = future_prepared_tbl,
    actual_data = data_prepared_tbl,
    keep_data   = TRUE
  ) %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .facet_ncol  = 2,
    .interactive = FALSE
  )


# model_fitted %>% 
#   save_h2o_model(path = "../model_fitted", overwrite = TRUE)

#model_h2o <- load_h2o_model(path = "../model_fitted/")