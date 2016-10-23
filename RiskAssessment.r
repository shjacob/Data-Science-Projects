library(xgboost)
library(MASS)#for ridge reg
library(Metrics) # calculate errors
library(ggplot2) #for plotting
library(cowplot) #to arrange (grid) plots
library(parallel)
#install.packages("snow")
library(snow)
#install.packages("NMOF")
library(NMOF)
library(plyr)
library(gridExtra)#grid arrange, for plots

#set seed, in case needed
set.seed(1)

##Read in data
src_train_data = read.csv(paste(getwd(), "train.csv", sep="//"))
src_test_data = read.csv(paste(getwd(), "test.csv", sep="//"))

nrow(train_data)
#[1] 59381
nrow(test_data)
#[1] 19765 
head(src_train_data)
head(src_test_data)


#### Perform EDA ####
#'Age' variable derived out of date of birth as a feature; DOB variable dropped
#Primary_Loan_Owner name dropped due to too many categories in it.
#Signin_name dropped
#Remaining_EMI imputed with 0 (median) as only 100 values were missing
#Interest_Rate_Missing created which is 1 if Interest_Rate was missing else 0 | Original variable Interest_Rate dropped
#Lead_Creation_Date dropped because made little intuitive impact on outcome
#Actual_Loan_Amt_Appld, Loan_Amt_Tenure_Appl (part of prev history) imputed with median values
#Loan_Amt_Sub_Missing created which is 1 if Loan_Amt_Sub was missing else 0; Original variable Loan_Amt_Sub dropped
#Income (Employment_Info_x) tranformed (Currency - AUD) - log transformation


##########  Split the categorical,continous,discrete variables from the dataset for better analysis  ##########
#src_train_data_cat <- as.data.frame(lapply(src_train_data[, c(paste("Product_Info_", c(1:3,5:7), sep=""), paste("Employment_Info_", c(1,2,3), sep=""))], factor))
#src_train_data_cont <- src_train_data[, c("ProductInfo4", "Age", "Ht", "Wt", "BMI", "Employment_Info_1", "Employment_Info_4", "Employment_Info_6", "Insurance_History_5")]
src_train_data_cat <- as.data.frame(lapply(src_train_data[, c("Citizen_status", paste("Product_Info_", c(1:3,5:7), sep=""), paste("Loan_Info_", c(1:3), sep=""),
					paste("Employment_Info_", c(1,2,3), sep=""),
                   paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
                   "Crime_History", paste("Owner_History_", c(2:5), sep=""), "Marital_Status")], factor))
src_train_data_cont <- src_train_data[, c("Product_Info_4", "Outstanding_Loan_1", "Employment_Info_5", "Employment_Info_6", 
                    "Employment_Info_7", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", 
                    "Family_Hist_5", paste("Prev_Loan_History_", c(14, 16:23), sep=""))]
src_train_data_disc <- src_train_data[, c("Age","Loan_History_4", "Loan_History_5", "Loan_History_6", "Employment_Info_4", paste("Loan_Keyword_", 1:4, sep=""))]

src_test_data_cat <- as.data.frame(lapply(src_test_data[, c("Citizen_status", paste("Product_Info_", c(1:3,5:7), sep=""), paste("Loan_Info_", c(1:3), sep=""),
					paste("Employment_Info_", c(1,2,3), sep=""),
                   paste("InsuredInfo_", 1:7, sep=""), paste("Insurance_History_", c(1:4,7:9), sep=""), 
                   "Crime_History", paste("Owner_History_", c(2:5), sep=""), "Marital_Status")], factor))
src_test_data_cont <- src_test_data[, c("Product_Info_4", "Outstanding_Loan_1", "Employment_Info_5", "Employment_Info_6", 
                    "Employment_Info_7", "Insurance_History_5", "Family_Hist_2", "Family_Hist_3", "Family_Hist_4", 
                    "Family_Hist_5", paste("Prev_Loan_History_", c(14, 16:23), sep=""))]
src_test_data_disc <- src_test_data[, c("Age","Loan_History_4", "Loan_History_5", "Loan_History_6", "Employment_Info_4", paste("Loan_Keyword_", 1:4, sep=""))]




#######   Looking at the structure of the data and summary stats  #############
str(src_train_data_cont)
str(src_train_data_disc)
str(src_train_data_cat)
`str(src_test_data_cont)
str(src_test_data_disc)
str(src_test_data_cat)

summary(src_train_data_cont)
summary(src_train_data_disc)
summary(src_train_data_cat)

summary(src_test_data_cont)
summary(src_test_data_disc)
summary(src_test_data_cat)

##Fraction of data missing? Duplicate? Constant columns?
src_train_data.const <- sapply(src_train_data, function(x) { length(unique(x)) == 1 })
cat("### TRAINING SET ### : \nNo. of NAs : ", paste("\t", paste(names(src_train_data), sapply(src_train_data, function(x) { sum(is.na(x)) }), sep = (" : ")), sep = "\n"), "\n\nFraction of missing data = ", sum(is.na(src_train_data)) / (nrow(src_train_data) * ncol(src_train_data)), "\n\nDuplicates? :", nrow(src_train_data) - nrow(unique(src_train_data)), "\n\nConstants? : ", sum(src_train_data.const))


src_test_data.const <- sapply(src_test_data, function(x) { length(unique(x)) == 1 })
cat("### TEST SET ### : \nNo. of NAs : ", paste("\t", paste(names(src_test_data), sapply(src_test_data, function(x) { sum(is.na(x)) }), sep = (" : ")), sep = "\n"), "\n\nFraction of missing data = ", sum(is.na(src_test_data)) / (nrow(src_test_data) * ncol(src_test_data)), "\n\nDuplicates? :", nrow(src_test_data) - nrow(unique(src_test_data)), "\n\nConstants? : ", sum(src_test_data.const))

########## Plot the CATEGORICAL features  ##########
catPlot <- function(i) {
	pl <- ggplot(src_train_data, aes(src_train_data[,i], fill=src_train_data[,i]))
	# by using geom_bar, it will automatically create a new column called "count" (available in an internal, transformed data frame).
	pl <- pl + geom_bar(width = 1, colour = "white")
	pl <- pl + geom_text(aes(y = ..count.., label = ..count..),
					   stat = "count", color = "white",
					   hjust = 1.0, size = 3)# geom_text, could then access that computed var with the ..count.. 
	pl <- pl + theme(legend.position = "none")
	pl <- pl + coord_flip()
	pl <- pl +  labs(x = names(src_train_data)[i])
	pl
}

catPlot(3)
p <- lapply(ind, function(x) {catPlot(x)})
par(mfrow=c(3,3))
marrangeGrob(p, nrow=3,ncol=3) ## multi-paneled display of histograms for easy analysis #change ncol and nrow as required for the panel split


########## Densities of the CONTINUOUS features  ##########
densityPlot <- function(i) {
	data <- data.frame(x = src_train_data[,i], Response = src_train_data$Response)
	pl <- ggplot(data) + geom_line(aes(x = x), stat = "density", size = 1, alpha = 1.0) +
		xlab(colnames(src_train_data)[i]) + theme_light()
	pl
}

densityPlot(9)
#densityPlot(18)
#densityPlot(35)
#####
pl <- lapply(ind, function(x) {densityPlot(x)})
par(mfrow=c(3,3))
marrangeGrob(pl, nrow=3,ncol=3) ## multi-paneled display of histograms for easy analysis #change ncol and nrow as required for the panel split

########## Boxplots of the CONTINUOUS features (depending on Response) ##########
boxPlt <- function(i) {
	data <- data.frame(y=src_train_data[,i], Response = src_train_data$Response)
	pl <- ggplot(data, aes(x = factor(Response), y = y)) + geom_boxplot() + ylab(colnames(src_train_data)[i]) + theme_light()
	return(pl)
}

boxPlt(18)
#boxPlt(35)

##Take a look at the response
ggplot(src_train_data) + geom_histogram(aes(factor(Response))) + xlab("Response") + theme_light()

#XGBoost needs every feature vector to contain only digits
for (j in names(src_train_data)) {
  if (class(src_train_data[[j]]) == "character") {
    #levels <- unique(c(src_train_data[[j]], src_test_data[[j]]))
	levels <- unique(src_train_data[[j]])
    src_train_data[[j]] <- as.integer(factor(src_train_data[[j]], levels=levels))
    #src_test_data[[j]]  <- as.integer(factor(src_train_data[[j]],  levels=levels))
  }
}

##Missing value imputation
missing_impute <- function(dtafrme) {
	dtafrme <- apply(dtafrme, 2, function(x) {if(is.numeric(x)) { x[is.na(x)] <- median(x[!is.na(x)])}})
	dtafrme
}


##----Reducing the number of features via ridge regression
#Making a new dataframe with no Id, and only numeric variables, then using ridge regression
#to remove some variables. The cutoff was optimized via cross-validation.
train_data <- missing_impute(src_train_data[,-c(1,3,98)])
gc()
mod_rdge <- lm.ridge(Response ~ ., data = train_data, lambda = 0.5)
train_data <- train_data[,(abs(mod_rdge$coef) > quantile(abs(mod_rdge$coef), 0.382))] #only "train_datartant" variables left
feature.names <- names(train_data) #their names

#####Grid search
# Form the Parameter_grid - to search
# set xgboost task_parameters
nrounds <- 1000
folds <- 10
obj <- 'reg:linear'
eval <- 'rmse'
params <- list(eval_metric = eval,
  objective = obj,
  eta = c(0.1,0.01),
  max_depth = c(4,6,8,10),
  max_delta_step = c(0,1),
  subsample = 1, scale_pos_weight = 1
)

# Table - performance tacking (for each worker node)
output <- data.frame()

#set the label (target variable)
label <- src_src_train_data$Response

# Cross_validate xgboost (xgb.cv invoked) training function.. (returns the minimum error - for the entire grid search)
xgb_CV <- function (params) {
	crossval <- xgb.cv(data = data.matrix(src_train_data), 
						label = label, 
						param = params,
						missing = NA, 
						nfold = folds, 
						prediction = FALSE,
						early.stop.round = 50,
						maximize = FALSE,
						nrounds = nrounds
					)
  rounds <- nrow(crossval)
  metric = paste('test.',eval,'.mean',sep='')
  index <- which.min(crossval[,crossval[[metric]]]) 
  val <- crossval[index,][[metric]]
  output <<- rbind(output,c(index,val,rounds))
  colnames(output) <<- c('index','val','rounds')
  return(val)
}

# To find the minimal testing error - perform parallel
# gridSearch - from the library 'NMOF'
#detectCores - from the library 'parallel'
#makeCluster - from the library 'snow'
#Note: I am passing in cluster ‘cl’ for method ‘snow’: otherwise it would use method ‘loop’
cl <- makeCluster(round(detectCores()/2)) #form cluster for parallel grid search, effective.. else go for random search
print(cl)
#socket cluster with 2 nodes on host ‘localhost’

clusterExport(cl, c("xgb.cv",'src_train_data','label','nrounds','res','eval','folds'))
#perform grid search - parallel
res_gs <- gridSearch(
  fun = xgb_CV,
  levels = params,
  method = 'snow',
  cl = cl,
  keepNames = TRUE,
  asList = TRUE)

# Combine the results of all models
comb=clusterEvalQ(cl,output)
results <- ldply(comb,data.frame)
stopCluster(cl)

# Now Train model using the parameters figured out using xgb.cv
#increase nrounds (possibly...!!)
paramtrs <- c(res_gs$minlevels, objective = obj, eval_metric = eval)
#train with these parameters and see how it performs. else go for a subtest_accuracy method.
xgbModel <- xgboost( data = xgb.DMatrix(data.matrix(train),missing=NaN, label = trainLabel),
			param = paramtrs, nrounds = results[which.min(results[,2]),1]
			)

#################			


#Setting up - for Xgboost CV using the Subtest_Accuracy method
trainSet <- train_data[1:floor(nrow(train_data)*0.1),]
splits <- floor(nrow(trainSet) / (50+1))#splitting like so

eta_val <- 0.3#default
depth <- 10
rounds <- 70

# lets recordthe elapsed time  
proc_time <- proc.time()

#Xgboost CV using the Subtest_Accuracy method
for (eta_val in seq(0.02, 0.4, 0.08)) {
	for (depth in seq(5, 10, 1)) {
		for (rounds in seq(25, 90, 10)) {
			#for (threshold in seq(0.1, 0.3, 0.05)) {
                prevError <- 100
                indexCount <- 1
                for (portion in seq(1:50)) {
                        #data to test
						dataTest <- trainSet[c((portion * splits):(portion * splits + splits)),]#Index to split to train and test 
                        #to train, rest of the rows
						dataTrain <- trainSet[-c((portion * splits):(portion * splits + splits)),]
                        
                        mod <- xgboost(data = as.matrix(dataTrain[,-c(ncol(dataTrain))]),#The last column holds the target variable
                                       label = dataTrain[,c(ncol(dataTrain))],
                                       eta = eta_val,#added
									   #eval_metric = "rmse",#added
									   #max.depth=depth, 
									   #threshold, 
									   #accuracy, 
									   #ams_sum
									   nround=rounds,
                                       objective = "reg:linear", verbose=T)
									   
                        pred <- predict(mod, as.matrix(dataTest[,-c(ncol(dataTest))]), outputmargin = TRUE)
                        #check error
                        currErr <- rmse(as.numeric(dataTest[,c(ncol(dataTrain))]), as.numeric(pred))
                     }
					 
				params_vec = c(eta, depth, nround
				#, threshold, accuracy, ams_sum
				)
				params_df = rbind(params_df, params_vec) 
                if (currErr < prevError) {
						prevError = currErr
                        print(paste("Got better : ",depth, rounds, eta_val, currErr))
                }  
				else {
						print(paste("Didn't get better : ", depth, rounds, eta_val, currErr))
				}
			}
		}
	}
}

elapsed_time <- proc.time() - proc_time

names(params_df) = c("eta", "max_depth", "nrounds"#, "threshold", "accuracy", "ams_sum"
					)
write.csv(params_df, "xgb_CrossValidationResults.csv", row.names = FALSE)  #save cross validation results into a file

modTrain <- xgboost(data = as.matrix(src_train_data[,-c(ncol(src_train_data))]),#The last column holds the target variable
                                       label = src_train_data[,c(ncol(src_train_data))],
                                       eta = eta_val,#added
									   #eval_metric = "rmse",#added
									   #max.depth=depth, 
									   nround=rounds,
                                       objective = "reg:linear", verbose=T)
									   
predTest <- predict(modTrain, as.matrix(src_test_data[,-c(ncol(src_test_data))]), outputmargin = TRUE)
Err <- rmse(as.numeric(dataTest[,c(ncol(dataTrain))]), as.numeric(pred))

print(paste("Test Prediction Error : ", Err))

