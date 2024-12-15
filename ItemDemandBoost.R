library(vroom)
library(tidymodels)
library(embed)
library(bonsai)
library(lightgbm)

train = vroom("train.csv")
test = vroom("test.csv")

nStores <- max(train$store)
nItems <- max(train$item)

for(s in 1:2){
  for(i in 1:2){
    
    storeItemTrain = train |> 
      filter(store == s, item == i)
    
    storeItemTest = test |> 
      filter(store == s, item == i)
    
    # Fit
    
    my_recipe = recipe(sales ~ ., storeItemTrain) |> 
      step_date(date, features = c("year", "month", "doy", "dow")) |> 
      step_range(date_doy, min = 0, max = pi) |> 
      step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) |> 
      step_lencode_mixed(all_nominal_predictors(),
                         outcome = vars(sales)) |> 
      step_rm(date, item, store) |> 
      step_normalize(all_numeric_predictors())
    
    my_model = boost_tree(tree_depth = 2,
                          trees = 1000,
                          learn_rate = 0.01) |> 
      set_engine("lightgbm") |> 
      set_mode("regression")
    
    my_wf = workflow() |> 
      add_recipe(my_recipe) |> 
      add_model(my_model) |> 
      fit(storeItemTrain)
    
    preds = predict(my_wf, new_data = storeItemTest)
    
    if(s==1 & i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds, preds)
    }
    
    
  }
}

kaggle_submission = all_preds |> 
  bind_cols(test[1:360,]) |> 
  rename(sales = .pred) |> 
  select(id, sales)

vroom_write(all_preds, file="/kaggle/working/submission.csv", delim=",")



