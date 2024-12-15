library(vroom)
library(tidyverse)
library(tidymodels)
library(patchwork)

train = vroom("train.csv")
test = vroom("test.csv")

storeItem = train |> 
  filter(store == 1, item == 1)

p1 = storeItem |> 
  ggplot(mapping = aes(x = date, y = sales)) +
  geom_line() +
  geom_smooth(se = FALSE)

p2 = storeItem |> 
  pull(sales) |> 
  forecast::ggAcf(lag.max = 30)

p3 = storeItem |> 
  pull(sales) |> 
  forecast::ggAcf(lag.max = 2 * 365)

storeItem2 = train |> 
  filter(store == 5, item == 10)

p4 = storeItem2 |> 
  ggplot(mapping = aes(x = date, y = sales)) +
  geom_line() +
  geom_smooth(se = FALSE)

p5 = storeItem2 |> 
  pull(sales) |> 
  forecast::ggAcf(lag.max = 30)

p6 = storeItem2 |> 
  pull(sales) |> 
  forecast::ggAcf(lag.max = 2 * 365)

(p1 + p2 + p3) / (p4 + p5 + p6)








# Modeling

my_recipe = recipe(sales ~ ., storeItem) |> 
  step_date(date, features="dow") |> 
  step_date(date, features="month") |> 
  step_date(date, features="year") |> 
  step_date(date, features="doy") |> 
  step_date(date, features="decimal") |> 
  step_rm(date) |> 
  step_rm(store) |> 
  step_rm(item) |> 
  step_dummy(all_nominal_predictors())

prepped_recipe = prep(my_recipe)
baked = bake(prepped_recipe, new_data = storeItem)

baked

model = linear_reg(penalty = tune(), mixture = tune()) |> 
  set_engine("glmnet")

wf = workflow() |> 
  add_recipe(my_recipe) |> 
  add_model(model)

tuning_grid = grid_regular(penalty(), mixture(), levels = 10)

folds = vfold_cv(storeItem, v = 10, repeats = 1)

CV_results = wf |> 
  tune_grid(resamples = folds,
            grid = tuning_grid,
            metrics = metric_set(smape),
            control=control_grid(verbose=TRUE))

bestTune = CV_results |> 
  select_best()

CV_results |> 
  show_best(n = 1, metric = "smape")













