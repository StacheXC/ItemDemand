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

prophet_model = prophet_reg() |> 
  set_engine(engine = "prophet") |> 
  fit(sales ~ date, data = training(cv_split))

cv_results <- modeltime_calibrate(prophet_model,
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









