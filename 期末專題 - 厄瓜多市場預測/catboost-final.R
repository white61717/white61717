cat("Setting up Enviornment\n")
library(data.table)
library(catboost)
library(dplyr)
#load("E:\\Desktop\\R code\\期末專題 - 厄瓜多市場預測\\catboost.RData")
cat("Loading test and train")
train <- fread("E:\\Desktop\\R code\\期末專題 - 厄瓜多市場預測\\train.csv", sep=",", na.strings="", 
               #skip=110000000,
               col.names=c("id","date","store_nbr","item_nbr","unit_sales","onpromotion"))
test <- fread("E:\\Desktop\\R code\\期末專題 - 厄瓜多市場預測\\test.csv", sep=",", na.strings = "")

cat("Setting onpromotion to integer")

#把onpromotion取log1p  sale轉integer  (:= 增加欄位(data.table專用) http://blog.fens.me/r-data-table/ )
train[, ":=" ( onpromotion = as.integer(onpromotion)
               ,unit_sales = log1p(ifelse(unit_sales>0, unit_sales, 0)) )]
test[, onpromotion := as.integer(onpromotion)]
#df_2017為2017-05-31
df_2017 <- train[date >= "2017-01-31"]

# Making wide table of onpromotion information
vars = c("item_nbr", "store_nbr", "date","onpromotion")
#取df_2017的vars欄位   然後把date當column   store_nbr+item_nbr當row   onpromotion當值
promo_2017_train <- dcast(df_2017[,vars,with=F], store_nbr+item_nbr~date, value.var="onpromotion")
promo_2017_test <- dcast(test[,vars,with=F], store_nbr+item_nbr~date, value.var="onpromotion")
#把train和test並在一起 空值補0
promo_2017 <- left_join(promo_2017_train, promo_2017_test,by=c("store_nbr","item_nbr")) %>% as.data.table
promo_2017[is.na(promo_2017)] = 0
rm(promo_2017_train, promo_2017_test)

# Make wide table of unit_sales information
df_2017 <- dcast(df_2017, store_nbr+item_nbr ~ date, value.var = "unit_sales")
df_2017[is.na(df_2017)] = 0

# - Functions
prepare_dataset <- function(t2017, is_train = T) {
  #把日期存起來
  m3_names = as.character(seq.Date(t2017-3, by = "days", length.out = 3))
  m7_names = as.character(seq.Date(t2017-7, by = "days", length.out = 7))
  m14_names = as.character(seq.Date(t2017-14, by = "days", length.out = 14))
  m140_names = as.character(seq.Date(t2017-140, by = "days", length.out = 140))
  p14_names = as.character(seq.Date(t2017-14, by = "days", length.out = 14))
  p140_names = as.character(seq.Date(t2017-140, by = "days", length.out = 140))
  
  X <- data.table( store_nbr = df_2017[, store_nbr]
                   ,item_nbr = df_2017[, item_nbr]
                   ,mean_3_2017 = df_2017[, rowMeans(.SD), .SDcols = m3_names]    #根據.SDcols的日期去取移動平均   https://stackoverflow.com/questions/10945703/calculate-row-means-on-subset-of-columns/10945896
                   ,mean_7_2017 = df_2017[,rowMeans(.SD), .SDcols = m7_names]
                   ,mean_14_2017 = df_2017[,rowMeans(.SD), .SDcols = m14_names]
                   ,mean_140_2017 = df_2017[,rowMeans(.SD), .SDcols = m140_names]
                   ,promo_14_2017 = promo_2017[,rowSums(.SD), .SDcols = p14_names]
                   ,promo_140_2017 = promo_2017[,rowSums(.SD), .SDcols = p140_names]
  )
  # Get onpromotion information for t2017 and the following 15 days.
  for(i in 0:15) {
    new_var = paste0("promo_",i)
    var = as.character(t2017+i) #從2017-06-21往後16天
    X[, (new_var) := promo_2017[, var, with=F]]     #新增new_var 數值是 promo_2017[, var, with=F]
  }
  # Get unit_sales information for t2017 and following 15 days
  if(is_train) { #只有train才做     #2017-06-21往後16天
    y_dates = as.character(seq.Date(t2017, by="days", length.out = 16))
    y = df_2017[,c("store_nbr", "item_nbr", y_dates), with=F]
    colnames(y) = c("store_nbr", "item_nbr", "y_1","y_2","y_3","y_4","y_5","y_6","y_7","y_8",
                    "y_9","y_10","y_11","y_12","y_13","y_14","y_15","y_16")
    return(list(X = X, y = y))
  } else {
    return(list(X = X))
  }
}

cat("Making X_train and y_train\n")
t2017 = as.Date("2017-6-21")
# Four 'sets' of train data. With i=0, we have:
# t2017 = 2017-06-21   第一個set
# X will have unit_sales info from 2017-06-07 --> 2017-06-20
# X will have onpromotion info through 2017-07-06
# Y will have unit_sales info from 2017-06-21 --> 2017-07-06   
# 因為到時候要預測16天 所以拿16天的y
for(i in 0:3) {
  # Make X_tmp and Y_tmp
  delta <- 7*i
  results <- prepare_dataset(t2017+delta)
  X_tmp = results$X
  y_tmp = results$y
  # Concatenating X_l
  if(i == 0) {
    X_train <- X_tmp
    y_train <- y_tmp
  } else {
    X_train = rbindlist(list(X_train, X_tmp))
    y_train = rbindlist(list(y_train, y_tmp))          #16萬*4
  }
}
val_date <- as.Date("2017-07-26")         # 7/26~8/11 當 validation set
results = prepare_dataset(val_date)
X_val = results$X
y_val = results$y
# Can't produce results$y since that is what we are predicting
results = prepare_dataset(as.Date("2017-08-16"), is_train = F)
X_test = results$X
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Preparing data for model
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
cat("Ordering all data.tables")
# Train set
X_train <- X_train[order(store_nbr, item_nbr)]
y_train <- y_train[order(store_nbr, item_nbr)]
# Val set
X_val <- X_val[order(store_nbr, item_nbr)]
y_val <- y_val[order(store_nbr, item_nbr)]
# Test set
X_test_in = as.matrix(X_test[,-c("store_nbr","item_nbr"), with=F])
X_test <- X_test[order(store_nbr,item_nbr)]

cat("Initializing prediction tables")
val_pred <- X_val[,.(store_nbr,item_nbr)]
test_pred <- X_test[,.(store_nbr,item_nbr)]

cat("Setting up X and Y matrices")
X_train_in = as.matrix(X_train[,-c("store_nbr", "item_nbr"), with=F])
X_val_in = as.matrix(X_val[,-c("store_nbr", "item_nbr"), with=F])

cat("Cleaning up before building model")
rm(train, df_2017, promo_2017); gc()
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Build Model and Getting Predictions
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
fit_params <- list(iterations = 500,
#                   loss_function = 'Logloss',
                   task_type = 'GPU',
                   learning_rate = 0.05,
#                  l2_leaf_reg = 0.1,
                   depth = 8
#                   bootstrap_type = "Bernoulli"
                   )
#save.image("E:\\Desktop\\R code\\期末專題 - 厄瓜多市場預測\\catboost.RData")
cat("Building Model")
dtest <- catboost.load_pool(X_test_in)

for(i in 0:15) {
  cat("Round:",i,"of 15\n")
  y_col = paste0("y_",i+1)
  dtrain <- catboost.load_pool(X_train_in, label = as.matrix(y_train[, y_col,with=F]))
  dval <- catboost.load_pool(X_val_in, label = as.matrix(y_val[, y_col,with=F]))
  bst <- catboost.train(dtrain,params = fit_params)
  # Make predictions
  val_pred[, eval(y_col) := catboost.predict(bst,dval,prediction_type = 'RawFormulaVal')]
  test_pred[, eval(y_col) := catboost.predict(bst,dtest,prediction_type = 'RawFormulaVal')]
}



# colnames are originally y_1, y_2, ... y_16. Switching to test dates for submission
colnames(test_pred) <- c("store_nbr", "item_nbr", sort(unique(test$date)))
# Make wide table long (columns are: store_nbr, item_nbr, date, unit_sales)
test_pred_long <- melt(test_pred, measure.vars = sort(unique(test$date)),
                       variable.name="date", value.name="unit_sales")
test_pred_long[, dates := as.Date(date)]
test[, unit_sales := 0]
# Merge with original test set
test[test_pred_long, unit_sales := i.unit_sales, on=c("store_nbr","item_nbr", "date")]
test[unit_sales<0, unit_sales:=0]
test[, unit_sales := expm1(unit_sales)]

cat("Making submission")


final <- test[,.(id, unit_sales)]
final$unit_sales[final$unit_sales>10000]
fwrite(test[,.(id, unit_sales)], "catboost_sub4.csv", sep=",", dec=".", quote=FALSE, row.names=FALSE)
