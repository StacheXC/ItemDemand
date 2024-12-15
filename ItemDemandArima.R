library(vroom)
library(tidymodels)
library(modeltime)
library(timetk)
library(patchwork)

train = vroom("train.csv")
test = vroom("test.csv")

storeItemTrain = train |> 
  filter(store == 5, item == 5)

storeItemTest = test |> 
  filter(store == 5, item == 5)

cv_split = time_series_split(storeItemTrain, assess = "3 months", cumulative = TRUE)

cv_split |> 
  tk_time_series_cv_plan() |> 
  plot_time_series_cv_plan(date, sales, .interactive = FALSE)

my_recipe = recipe(sales ~ ., storeItemTrain) |> 
  step_date(date, features="dow") |> 
  step_date(date, features="month") |> 
  step_date(date, features="year") |> 
  step_date(date, features="doy") |> 
  step_date(date, features="decimal") |> 
  step_rm(store) |> 
  step_rm(item) |> 
  step_dummy(all_nominal_predictors())

prepped_recipe = prep(my_recipe)
baked = bake(prepped_recipe, new_data = storeItemTrain)

arima_model <- arima_reg(non_seasonal_ar=5,
                         non_seasonal_ma=5,
                         seasonal_ar=2,
                         seasonal_ma=2,
                         non_seasonal_differences=2,
                         seasonal_differences=2) |> 
set_engine("auto_arima")

arima_wf <- workflow() |> 
  add_recipe(my_recipe) |> 
  add_model(arima_model) |> 
  fit(data=training(cv_split))

cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))

p3 = cv_results |> 
  modeltime_forecast(new_data = testing(cv_split),
                     actual_data = training(cv_split)) |> 
  plot_modeltime_forecast(.interactive=FALSE)

fullfit <- cv_results |> 
  modeltime_refit(data=storeItemTrain)

p4 = fullfit |> 
  modeltime_forecast(new_data = storeItemTest,
                     actual_data = storeItemTrain) |> 
  plot_modeltime_forecast(.interactive=FALSE)

(p1 + p3) / (p2 + p4)









