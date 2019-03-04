cat("Setting up Enviornment\n")
library(data.table)
library(catboost)
library(dplyr)
library(RFunctionsSN)
library(matrixStats)
#load("E:\\Desktop\\R code\\期末專題 - 厄瓜多市場預測\\catboost.RData")
cat("Loading test and train")
train <- fread("C:\\Users\\Student\\Desktop\\厄瓜多data\\train.csv", sep=",", na.strings="", 
               #skip=110000000,
               col.names=c("id","date","store_nbr","item_nbr","unit_sales","onpromotion"))
test <- fread("C:\\Users\\Student\\Desktop\\厄瓜多data\\test.csv", sep=",", na.strings = "")

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
  m1_names = as.character(seq.Date(t2017-1, by = "days", length.out = 1))
  m3_names = as.character(seq.Date(t2017-3, by = "days", length.out = 3))
  m5_names = as.character(seq.Date(t2017-5, by = "days", length.out = 5))
  m7_names = as.character(seq.Date(t2017-7, by = "days", length.out = 7))
  m14_names = as.character(seq.Date(t2017-14, by = "days", length.out = 14))
  m30_names = as.character(seq.Date(t2017-30, by = "days", length.out = 30))
  m60_names = as.character(seq.Date(t2017-60, by = "days", length.out = 60))
  m140_names = as.character(seq.Date(t2017-140, by = "days", length.out = 140))
  diff3_names = as.character(seq.Date(t2017-3, by = "days", length.out = 3))
  diff5_names = as.character(seq.Date(t2017-5, by = "days", length.out = 5))
  diff7_names = as.character(seq.Date(t2017-7, by = "days", length.out = 7))
  diff14_names = as.character(seq.Date(t2017-14, by = "days", length.out = 14))
  diff30_names = as.character(seq.Date(t2017-30, by = "days", length.out = 30))
  diff60_names = as.character(seq.Date(t2017-60, by = "days", length.out = 60))
  diff140_names = as.character(seq.Date(t2017-140, by = "days", length.out = 140))
  median3_names = as.character(seq.Date(t2017-3, by = "days", length.out = 3))
  median5_names = as.character(seq.Date(t2017-5, by = "days", length.out = 5))
  median7_names = as.character(seq.Date(t2017-7, by = "days", length.out = 7))
  median14_names = as.character(seq.Date(t2017-14, by = "days", length.out = 14))
  median30_names = as.character(seq.Date(t2017-30, by = "days", length.out = 30))
  median60_names = as.character(seq.Date(t2017-60, by = "days", length.out = 60))
  median140_names = as.character(seq.Date(t2017-140, by = "days", length.out = 140))
  min3_names = as.character(seq.Date(t2017-3, by = "days", length.out = 3))
  min5_names = as.character(seq.Date(t2017-5, by = "days", length.out = 5))
  min7_names = as.character(seq.Date(t2017-7, by = "days", length.out = 7))
  min14_names = as.character(seq.Date(t2017-14, by = "days", length.out = 14))
  min30_names = as.character(seq.Date(t2017-30, by = "days", length.out = 30))
  min60_names = as.character(seq.Date(t2017-60, by = "days", length.out = 60))
  min140_names = as.character(seq.Date(t2017-140, by = "days", length.out = 140))
  max3_names = as.character(seq.Date(t2017-3, by = "days", length.out = 3))
  max5_names = as.character(seq.Date(t2017-5, by = "days", length.out = 5))
  max7_names = as.character(seq.Date(t2017-7, by = "days", length.out = 7))
  max14_names = as.character(seq.Date(t2017-14, by = "days", length.out = 14))
  max30_names = as.character(seq.Date(t2017-30, by = "days", length.out = 30))
  max60_names = as.character(seq.Date(t2017-60, by = "days", length.out = 60))
  max140_names = as.character(seq.Date(t2017-140, by = "days", length.out = 140))
  std3_names = as.character(seq.Date(t2017-3, by = "days", length.out = 3))
  std5_names = as.character(seq.Date(t2017-5, by = "days", length.out = 5))
  std7_names = as.character(seq.Date(t2017-7, by = "days", length.out = 7))
  std14_names = as.character(seq.Date(t2017-14, by = "days", length.out = 14))
  std30_names = as.character(seq.Date(t2017-30, by = "days", length.out = 30))
  std60_names = as.character(seq.Date(t2017-60, by = "days", length.out = 60))
  std140_names = as.character(seq.Date(t2017-140, by = "days", length.out = 140))
  
  p3_names = as.character(seq.Date(t2017-3, by = "days", length.out = 3))
  p5_names = as.character(seq.Date(t2017-5, by = "days", length.out = 5))
  p7_names = as.character(seq.Date(t2017-7, by = "days", length.out = 7))
  p14_names = as.character(seq.Date(t2017-14, by = "days", length.out = 14))
  p60_names = as.character(seq.Date(t2017-60, by = "days", length.out = 60))
  p140_names = as.character(seq.Date(t2017-140, by = "days", length.out = 140))
  p3_names_after = as.character(seq.Date(t2017, by = "days", length.out = 3))
  p7_names_after = as.character(seq.Date(t2017, by = "days", length.out = 7))
  p14_names_after = as.character(seq.Date(t2017, by = "days", length.out = 14))

  X <- data.table( store_nbr = df_2017[, store_nbr]
                   ,item_nbr = df_2017[, item_nbr]
                   ,mean_1_2017 = df_2017[, rowMeans(.SD), .SDcols = m1_names]    #根據.SDcols的日期去取移動平均   https://stackoverflow.com/questions/10945703/calculate-row-means-on-subset-of-columns/10945896
                   ,mean_3_2017 = df_2017[, rowMeans(.SD), .SDcols = m3_names]    #根據.SDcols的日期去取移動平均   https://stackoverflow.com/questions/10945703/calculate-row-means-on-subset-of-columns/10945896
                   ,mean_5_2017 = df_2017[,rowMeans(.SD), .SDcols = m5_names]
                   ,mean_7_2017 = df_2017[,rowMeans(.SD), .SDcols = m7_names]
                   ,mean_14_2017 = df_2017[,rowMeans(.SD), .SDcols = m14_names]
                   ,mean_30_2017 = df_2017[,rowMeans(.SD), .SDcols = m30_names]
                   ,mean_60_2017 = df_2017[,rowMeans(.SD), .SDcols = m60_names]
                   ,mean_140_2017 = df_2017[,rowMeans(.SD), .SDcols = m140_names]
                   ,diff3_mean = rowMeans( diff(  as.matrix(  df_2017[,(.SD), .SDcols = diff3_names]  )  )  )
                   ,diff5_mean = rowMeans( diff(  as.matrix(  df_2017[,(.SD), .SDcols = diff5_names]  )  )  )
                   ,diff7_mean = rowMeans( diff(  as.matrix(  df_2017[,(.SD), .SDcols = diff7_names]  )  )  )
                   ,diff14_mean = rowMeans( diff(  as.matrix(  df_2017[,(.SD), .SDcols = diff14_names]  )  )  )
                   ,diff30_mean = rowMeans( diff(  as.matrix(  df_2017[,(.SD), .SDcols = diff30_names]  )  )  )
                   ,diff60_mean = rowMeans( diff(  as.matrix(  df_2017[,(.SD), .SDcols = diff60_names]  )  )  )
                   ,diff140_mean = rowMeans( diff(  as.matrix(  df_2017[,(.SD), .SDcols = diff140_names]  )  )  )
                   ,median_3_2017 = rowMedians(  as.matrix(  df_2017[,(.SD), .SDcols = diff3_names]  )  )
                   ,median_5_2017 = rowMedians(  as.matrix(  df_2017[,(.SD), .SDcols = diff5_names]  )  )
                   ,median_7_2017 = rowMedians(  as.matrix(  df_2017[,(.SD), .SDcols = diff7_names]  )  )
                   ,median_14_2017 = rowMedians(  as.matrix(  df_2017[,(.SD), .SDcols = diff14_names]  )  )
                   ,median_30_2017 = rowMedians(  as.matrix(  df_2017[,(.SD), .SDcols = diff30_names]  )  )
                   ,median_60_2017 = rowMedians(  as.matrix(  df_2017[,(.SD), .SDcols = diff60_names]  )  )
                   ,median_140_2017 = rowMedians(  as.matrix(  df_2017[,(.SD), .SDcols = diff140_names]  )  ) 
                   ,min_3_2017 = df_2017[, rowMin(.SD), .SDcols = min3_names]   #錯了
                   ,min_5_2017 = df_2017[,rowMin(.SD), .SDcols = min5_names]
                   ,min_7_2017 = df_2017[,rowMin(.SD), .SDcols = min7_names]
                   ,min_14_2017 = df_2017[,rowMin(.SD), .SDcols = min14_names]
                   ,min_30_2017 = df_2017[,rowMin(.SD), .SDcols = min30_names]
                   ,min_60_2017 = df_2017[,rowMin(.SD), .SDcols = min60_names]
                   ,min_140_2017 = df_2017[,rowMin(.SD), .SDcols = min140_names]  
                   ,max_3_2017 = df_2017[, rowMax(.SD), .SDcols = max3_names]   
                   ,max_5_2017 = df_2017[,rowMax(.SD), .SDcols = max5_names]
                   ,max_7_2017 = df_2017[,rowMax(.SD), .SDcols = max7_names]
                   ,max_14_2017 = df_2017[,rowMax(.SD), .SDcols = max14_names]
                   ,max_30_2017 = df_2017[,rowMax(.SD), .SDcols = max30_names]
                   ,max_60_2017 = df_2017[,rowMax(.SD), .SDcols = max60_names]
                   ,max_140_2017 = df_2017[,rowMax(.SD), .SDcols = max140_names]  
                   ,std_3_2017 = rowSds(  as.matrix(  df_2017[,(.SD), .SDcols = std3_names]  )  )   
                   ,std_5_2017 = rowSds(  as.matrix(  df_2017[,(.SD), .SDcols = std5_names]  )  )
                   ,std_7_2017 = rowSds(  as.matrix(  df_2017[,(.SD), .SDcols = std7_names]  )  )
                   ,std_14_2017 = rowSds(  as.matrix(  df_2017[,(.SD), .SDcols = std14_names]  )  )
                   ,std_30_2017 = rowSds(  as.matrix(  df_2017[,(.SD), .SDcols = std30_names]  )  )
                   ,std_60_2017 = rowSds(  as.matrix(  df_2017[,(.SD), .SDcols = std60_names]  )  )
                   ,std_140_2017 = rowSds(  as.matrix(  df_2017[,(.SD), .SDcols = std140_names]  )  )
                   
                   ,promo_3_2017 = promo_2017[,rowSums(.SD), .SDcols = p3_names]
                   ,promo_5_2017 = promo_2017[,rowSums(.SD), .SDcols = p5_names]
                   ,promo_7_2017 = promo_2017[,rowSums(.SD), .SDcols = p7_names]
                   ,promo_14_2017 = promo_2017[,rowSums(.SD), .SDcols = p14_names]
                   ,promo_60_2017 = promo_2017[,rowSums(.SD), .SDcols = p60_names]
                   ,promo_140_2017 = promo_2017[,rowSums(.SD), .SDcols = p140_names]
                   ,promo_3_2017_after = promo_2017[,rowSums(.SD), .SDcols = p3_names_after]
                   ,promo_7_2017_after = promo_2017[,rowSums(.SD), .SDcols = p7_names_after]
                   ,promo_14_2017_after = promo_2017[,rowSums(.SD), .SDcols = p14_names_after]
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
# Y will have unit_sales info from 2017-06-21 --> 2017-07-06   #因為到時候要預測16天 所以拿16天的y
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
ncol(X_train_in)
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
