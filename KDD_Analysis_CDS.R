# ==============================================================================
# LIBRARIES
# ==============================================================================
setwd('~')
library(dplyr)
#library(readr) readr unavailable..
library(data.table) #switching to data.table
library(lubridate)
library(magrittr)
library(ggplot2)
library(ROCR)
library(stringr)
library(glmnet)
library(foreach)
library(doMC)
library(igraph)
#library(plyr)
source('~/data_science_functions/library.R')

# ==============================================================================
# LOADING DATA
# ==============================================================================
# changing load function from read_csv to data.table

# data.table = FALSE so that left_join works in create_detail function
# since "Error: Data table joins must be on same key"

enroll_df <- fread("kdd_cup/enrollment_train.csv", data.table = FALSE)
log_df <- fread("kdd_cup/log_train.csv", data.table = FALSE)
object_df <- fread("kdd_cup/object.csv", data.table = FALSE)
label_df <- fread("kdd_cup/truth_train.csv", data.table = FALSE)
setnames(label_df, c("enrollment_id", "dropout"))

# Don't have label for enroll id 139669. Remove it from data
log_df %<>% filter(enrollment_id != 139669)
enroll_df %<>% filter(enrollment_id != 139669)

# Add labels to enroll_df
enroll_df <- inner_join(enroll_df, label_df)

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# create a function to clean/add features to object_df
clean_object = function(object_df) {
  
  # removing duplicate records
  object_df %<>% unique(.)
  object_df$num_children <- nchar(gsub(' ', '', object_df$children))/32
  object_df %<>%
    group_by(module_id) %>%
    summarise(num_children = max(num_children)) %>%
    inner_join(., object_df)
  print('duplicates removed ...')
  
  # reformat date field
  ncores = 30 # set the number of cores to use
  registerDoMC(cores=ncores) # register cores
  object_df %<>% mutate(start = ymd_hms(gsub("T", " ", start)),
                        module_release_dt = as.Date(start))
  print('dates reformatted ...')
  
  parent <- foreach(i=object_df$module_id) %dopar% which(grepl(i, object_df$children))
  object_df$parent <- unlist(lapply(parent, function(x) paste(unique(object_df$module_id[x]), collapse = ' ')))
  rm(parent)
  print('parents found ...')
  
  # getting depth for all objects
  y <- 0 # while
  z <- 1 # loop
  d <- 0 # parameters
  n <- nrow(object_df)
  object_df$depth <- ifelse(object_df$parent == '', 0, NA) # set depth for modules w/o parents
  c <- colnames(object_df) # get original colnames
  while (y < n & z > 0) {
    # add on depth for next level down
    object_df %<>%
      filter(depth == d) %>%
      select(module_id, depth) %>%
      mutate(depth = depth+1) %>%
      left_join(object_df, ., by = c("parent" = "module_id")) %>%
      mutate(depth = ifelse(is.na(depth.x), depth.y, depth.x))
    object_df <- object_df[,c] # select original columns
    d <- d+1 # move down to next level
    y <- sum(!is.na(object_df$depth)) # check how populated depth is
    z <- sum(object_df$depth == d, na.rm = TRUE) # check than new depth records were recorded
  }
  print('depth created ...')
  
  object_df %<>% mutate(Ochapter = ifelse(category == 'chapter', module_id, NA),
                        Oproblem = ifelse(category == 'problem', module_id, NA),
                        Osequential = ifelse(category == 'sequential', module_id, NA),
                        Ovideo = ifelse(category == 'video', module_id, NA))
  print('Done!')
  
  return(object_df)
}

# create a function to clean/add features to log_df
clean_log = function(log_df) {
  
  # sample for testing
  #log_df <- filter(log_df, enrollment_id < 10)
  
  # Change spelling error nagivate to navigate!
  log_df[log_df$event == "nagivate",]$event = "navigate"
  print('spelling corrected ...')
  
  # New column for each event type
  log_df %<>% mutate(time = ymd_hms(gsub("T", " ", time)),
                     event_date = as.Date(time),
                     access_date = as.Date('1970-01-01') + ifelse(event == 'access', event_date, NA),
                     access = ifelse(event == 'access', object, NA),
                     discussion = ifelse(event == 'discussion', object, NA),
                     navigate = ifelse(event == 'navigate', object, NA),
                     page_close = ifelse(event == 'page_close', object, NA),
                     problem = ifelse(event == 'problem', object, NA),
                     video = ifelse(event == 'video', object, NA),
                     wiki = ifelse(event == 'wiki', object, NA))
  print('dates reformatted & columns added ...')
  
  # Get course start and end date for each course
  log_df %<>%
    group_by(course_id) %>%
    summarise(course_strt_dt = as.Date(min(time)),
              course_end_dt = as.Date(max(time))) %>%
    inner_join(log_df, .)
  print('self join performed ...')
  
  # turn into data table for rankover
  log_df <- data.table(log_df, key = "enrollment_id")
  
  # rank each record for each enrollment
  log_df <- log_df[,transform(.SD, order = rank(time, ties.method = "first")), by = enrollment_id]
  log_df <- as.data.frame(log_df)
  
  return(log_df)
}

# function to compute pageranks
pagerank = function(dt, threshold = 30) {
  
  # select necessary columns
  dt %<>% select(enrollment_id, object, time, order)
  dt2 <- dt
  
  # create order2 for self join
  dt$order2 <- dt$order - 1
  dt <- as.data.frame(dt)
  
  # self join
  dt %<>%
    select(enrollment_id, object, time, order2) %>%
    inner_join(dt, ., by = c("enrollment_id" = "enrollment_id", "order" = "order2"))
  
  # clean records and create spent feature
  dt <- dt[,c( 'enrollment_id', 'object.x', 'object.y', 'time.x', 'time.y', 'order', 'order2')]
  dt %<>% mutate(spent = as.numeric(time.y - time.x),
                 order2 = order2+2)
  
  # function to define each session
  sessions = function(dat, thresh) {
    n <- nrow(dat)
    x <- 1
    s <- rep(0, n)
    t <- dat$spent
    for (i in 1:n) {
      x <- ifelse(t[i] < thresh, x, x+1)
      s[i] <- x
    }
    dat$session <- s
    return(dat)
  }
  enrolls <- unique(dt$enrollment_id)
  
  # define sessions in parallel
  registerDoMC(cores=35)
  dt <- foreach(i = enrolls) %dopar% sessions(filter(dt, enrollment_id == i), 60*threshold)
  
  # rbind list together
  library(plyr)
  dt <- rbind.fill(dt)
  detach("package:plyr", unload=TRUE, force = TRUE)
  library(dplyr)
  
  # fill in missing sessions
  dt2 %<>% left_join(., dt, by = c('enrollment_id' = 'enrollment_id', 'object' = 'object.y', 'time' = 'time.y', 'order' = 'order2')) %>%
    select(enrollment_id, object, time, order, session) %>%
    left_join(., dt, by = c('enrollment_id' = 'enrollment_id', 'object' = 'object.x', 'time' = 'time.x', 'order' = 'order')) %>%
    mutate(session = ifelse(is.na(session.x), session.y, session.x)) %>%
    select(enrollment_id, object, time, order, session, spent)
  dt2 %<>% group_by(enrollment_id, session) %>%
    summarise(order = max(order),
              last = 'Y') %>%
    left_join(dt2, ., by = c('enrollment_id' = 'enrollment_id', 'session' = 'session', 'order' = 'order'))
  dt2$last[is.na(dt2$last)] <- 'N'
  dt2 %<>% mutate(esess = paste(enrollment_id, session, sep = '_'))
  
  dt2 <- data.table(dt2, key = 'esess')
  dt2 <- dt2[,transform(.SD, session_order = rank(time, ties.method = "first")), by = esess]
  dt2 %<>% mutate(session_order2 = session_order - 1)
  dt2 <- as.data.frame(dt2)
  dt2 %<>%
    select(esess, session_order2, object) %>%
    left_join(dt2, ., by = c("esess" = "esess", "session_order" = "session_order2")) %>%
    select(object.x, object.y, enrollment_id, session, time, spent, last, session_order, order)
  
  links <- dt2 %>%
    filter(!is.na(object.y)) %>%
    select(object.x, object.y) %>%
    graph.data.frame(.) %>%
    page.rank(.)
  
  links <- data.frame(object.x = names(links$vector), pagerank = links$vector)
  rownames(links) <- 1:nrow(links)
  dt2 %<>% left_join(., links)
  
  return(dt2)
  
}

# Create a function to make detail df
create_detail <- function(log_df, object_df) {
  
  # Join log_df to pagerank table
  log_df <- inner_join(log_df, pagerank(log_df), by = c('enrollment_id' = 'enrollment_id',
                                                        'order' = 'order'))
  print('pagerank joined ...')
  # Join log_df to object_df
  detail_df <- left_join(log_df, object_df, by = c("course_id" = "course_id",
                                                   "object.x" = "module_id"))
  print('joined to object_df ...')
  
  detail_df$time.y <- NULL
  colnames(detail_df)[colnames(detail_df) == 'time.x'] <- 'time'
  detail_df$spent[detail_df$last == 'Y'] <- 60*30
  gc()
  print('Done!')
  return(detail_df)
}

# Create a function to make course table
create_course = function(object_df, log_df) {
  course_df1 <-
    object_df %>%
    group_by(course_id) %>%
    summarise(cobjects = n(),
              cchapters = sum(category=='chapter'),
              cproblems = sum(category=='problem'),
              csequentials = sum(category=='sequential'),
              cvideos = sum(category == 'video'),
              min_start = min(start, na.rm = T),
              max_start = max(start, na.rm = T),
              depth0 = sum(depth == 0),
              depth1 = sum(depth == 1),
              depth2 = sum(depth == 2),
              depth3 = sum(depth == 3),
              depth4 = sum(depth == 4),
              mean_kids = mean(num_children),
              median_kids = median(num_children),
              sum_kids = sum(num_children),
              max_kids = max(num_children))
  course_df2 <-
    log_df %>%
    group_by(course_id) %>%
    summarise(students = n_distinct(username),
              naccess = sum(event == 'access'),
              ndiscussion = sum(event == 'discussion'),
              nnavigate = sum(event == 'navigate'),
              npage_close = sum(event == 'page_close'),
              nproblem = sum(event == 'problem'),
              nvideo = sum(event == 'video'),
              nwiki = sum(event == 'wiki'),
              ndaccess = n_distinct(access),
              nddiscussion = n_distinct(discussion),
              ndnavigate = n_distinct(navigate),
              ndpage_close = n_distinct(page_close),
              ndproblem = n_distinct(problem),
              ndvideo = n_distinct(video),
              ndwiki = n_distinct(wiki))
  course_df <- merge(course_df1, course_df2)
  print('Done!')
  return(course_df)
}

# Create a function to make some summary features
create_summary <- function(df) {
  
  summary_df <- 
    df %>%
    group_by(enrollment_id, username, course_id) %>%
    summarise(num_video = sum(event == "video"),
              num_navigate = sum(event == "navigate"),
              num_access = sum(event == "access"),
              num_problem = sum(event == "problem"),
              num_page_close = sum(event == "page_close"),
              num_discussion = sum(event == "discussion"),
              num_wiki = sum(event == "wiki"),
              numd_video = n_distinct(video),
              numd_navigate = n_distinct(navigate),
              numd_access = n_distinct(access),
              numd_problem = n_distinct(problem),
              numd_page_close = n_distinct(page_close),
              numd_discussion = n_distinct(discussion),
              numd_wiki = n_distinct(wiki),
              num_server = sum(source == 'server'),
              num_browser = sum(source == 'browser'),
              mean_pagerank = mean(pagerank),
              high_pr = sum(pagerank > 0.1),
              low_pr = sum(pagerank < 0.1),
              num_sessions = n_distinct(session),
              num_events = n(),
              lst_wk_strt_date = max(course_end_dt) - 7,
              lst_2wk_strt_date = max(course_end_dt) - 14,
              num_events_lst_wk = sum(event_date >= lst_wk_strt_date),
              num_access_lst_wk = sum(event_date >= lst_wk_strt_date & event == "access"),
              num_events_lst2_wk = sum(event_date >= lst_2wk_strt_date),
              num_access_lst2_wk = sum(event_date >= lst_2wk_strt_date & event == "access"),
              days_course_strt_access1 = as.numeric(min(access_date, na.rm = T) - max(course_strt_dt)),
              days_course_end_access_lst = as.numeric(max(course_end_dt) - max(access_date, na.rm = T)),
              unique_days_accessed = n_distinct(event_date),
              unique_access_days = n_distinct(access_date),
              num_Ochapters = sum(!is.na(Ochapter)),
              num_Oproblems = sum(!is.na(Oproblem)),
              num_Osequentials = sum(!is.na(Osequential)),
              num_Ovideos = sum(!is.na(Ovideo)),
              numd_Ochapters = n_distinct(Ochapter),
              numd_Oproblems = n_distinct(Oproblem),
              numd_Osequentials = n_distinct(Osequential),
              numd_Ovideos = n_distinct(Ovideo),
              num_depth0 = sum(depth == 0, na.rm = T),
              num_depth1 = sum(depth == 1, na.rm = T),
              num_depth2 = sum(depth == 2, na.rm = T),
              num_depth3 = sum(depth == 3, na.rm = T),
              num_depth4 = sum(depth == 4, na.rm = T),
              mean_depth = mean(depth, na.rm = T),
              max_depth = max(depth, na.rm = T),
              min_time = min(time),
              max_time = max(time),
              mid_time = max_time - (max_time - min_time)/2,
              num_before_mid = sum(time <= mid_time),
              num_after_mid = sum(time > mid_time),
              access_before_mid = sum(time <= mid_time & event == 'access'),
              access_after_mid = sum(time > mid_time & event == 'access'),
              problem_before_mid = sum(time <= mid_time & event == 'problem'),
              problem_after_mid = sum(time > mid_time & event == 'problem')) %>%
    ungroup
  print('summary created ...')
  
  # Get features at module level like the median days between a module release
  # and the first access by user
  summary_df <- 
    detail_df %>%
    group_by(enrollment_id, username, course_id, object) %>%
    summarise(days_acs1_mod_rls = as.numeric(
      min(access_date, na.rm = T) - max(module_release_dt, na.rm = T)),
      days_acslst_mod_rls = as.numeric(
        max(access_date, na.rm = T) - max(module_release_dt, na.rm = T))) %>%
    ungroup %>%
    group_by(enrollment_id, username, course_id) %>%
    summarise(median_days_acs1_mod_rls = median(days_acs1_mod_rls, na.rm = T),
              median_days_acslst_mod_rls = median(days_acslst_mod_rls, na.rm = T)) %>%
    ungroup %>%
    left_join(summary_df, .)
  print('self join performed ...')
  
  summary_df$mean_depth <- ifelse(is.na(summary_df$mean_depth), 0, summary_df$mean_depth)
  summary_df$max_depth <- ifelse(is.na(summary_df$max_depth), 0, summary_df$max_depth)
  summary_df$days_course_strt_access1 <- ifelse(is.na(summary_df$days_course_strt_access1), 9999, summary_df$days_course_strt_access1)
  summary_df$days_course_end_access_lst <- ifelse(is.na(summary_df$days_course_end_access_lst), 9999, summary_df$days_course_end_access_lst)
  summary_df$median_days_acs1_mod_rls <- ifelse(is.na(summary_df$median_days_acs1_mod_rls), 9999, summary_df$median_days_acs1_mod_rls)
  summary_df$median_days_acslst_mod_rls <- ifelse(is.na(summary_df$median_days_acslst_mod_rls), 9999, summary_df$median_days_acslst_mod_rls)
  print('replaced nulls ...')
  gc()
  print('Done!')
  return(summary_df)
}

# create a function to join course data on
create_all <- function(summary_df, course_df) {
  all_df <- merge(summary_df, course_df, all.x = TRUE)
  all_df %<>% mutate(video_ratio = num_video/(nvideo/students),
                     navigate_ratio = num_navigate/(nnavigate/students),
                     access_ratio = num_access/(naccess/students),
                     problem_ratio = num_problem/(nproblem/students),
                     page_close_ratio = num_page_close/(npage_close/students),
                     discussion_ratio = num_discussion/(ndiscussion/students),
                     wiki_ratio = num_wiki/(nwiki/students),
                     p_video = numd_video/ndvideo,
                     p_navigate = numd_navigate/ndnavigate,
                     p_access = numd_access/ndaccess,
                     p_problem = numd_problem/ndproblem,
                     p_page_close = numd_page_close/npage_close,
                     p_discussion = numd_discussion/ndiscussion,
                     p_wiki = numd_wiki/ndwiki,
                     p_Ochapters = numd_Ochapters/cchapters,
                     p_Oproblems = numd_Oproblems/cproblems,
                     p_Osequentials = numd_Osequentials/csequentials,
                     p_Ovideos = numd_Ovideos/cvideos,
                     server_rate = num_server/num_events,
                     browser_rate = num_browser/num_events,
                   start_lag = as.numeric(min_time - min_start),
                   end_lag = as.numeric(max_start - max_time),
                   half_ratio = num_after_mid/num_before_mid,
                   first_half_ratio = num_before_mid/(num_after_mid+num_before_mid),
                   second_half_ratio = num_after_mid/(num_after_mid+num_before_mid),
                   access_first_half = access_before_mid/(access_before_mid+access_after_mid),
                   access_second_half = access_after_mid/(access_before_mid+access_after_mid),
                   problem_first_half = problem_before_mid/(problem_before_mid+problem_after_mid),
                   problem_second_half = problem_after_mid/(problem_before_mid+problem_after_mid),
                   access_per_day = num_access/unique_days_accessed,
                   events_per_session = num_events/num_sessions,
                   sessions_per_dat = num_sessions/unique_days_accessed,
                   high_low_pr = high_pr/(high_pr + low_pr))
  all_df[is.na(all_df)] <- 0
  return(all_df)
}
           
# compute stats function
compute_stats <- function(pred, test) {
  print(table(pred, test$dropout))
  message(paste("The AUC is:", calcAUC(pred, test$dropout)))
}

# Create a function to compute the AUC
calcAUC <- function(predcol, outcol) {
  perf <- performance(prediction(predcol, outcol == 1), "auc")
  as.numeric(perf@y.values)
}

# ==============================================================================
# TRAIN TEST SPLIT
# ==============================================================================

# clean up object & log tables
object_df <- clean_object(object_df)
log_df <- clean_log(log_df)

# Create detail df
detail_df <- create_detail(log_df, object_df)

# make course table
course_df <- create_course(object_df, log_df)

# Free up some memory by removing unneeded objects
rm(log_df)
gc()

# Create summary df
summary_df <- create_summary(detail_df)
all_df <- create_all(summary_df, course_df)
all_df %<>% inner_join(label_df)

# Save detail_df and summary_df dataframes
saveRDS(detail_df, file = "kdd_cup/Data_Files/detail_df.rds")
saveRDS(summary_df, file = "kdd_cup/Data_Files/summary_df.rds")

# Split into test and train
#set.seed(729375)
set.seed(630491)
all_df$rgroup <- runif(nrow(all_df))
train <- all_df %>% filter(rgroup <= 0.8)
test <- all_df %>% filter(rgroup > 0.8)
train$rgroup <- NULL
test$rgroup <- NULL

train %<>% select(num_video:num_events, num_events_lst_wk:num_depth2, num_depth4:max_depth,
                  num_before_mid:median_days_acslst_mod_rls, video_ratio:dropout)

# ==============================================================================
# VISUALIZATIONS
# ==============================================================================

# Let's see the trend of dropouts with unique number of days MOOC was accessed
ggplot(train, aes(x = unique_days_accessed, y = as.numeric(dropout))) +
    geom_point(position = position_jitter(w = 0.05, h = 0.05)) +
    geom_smooth() +
    xlab("Number of unique days MOOC was accessed") +
    ylab("Dropout") +
    ggtitle("Trend of dropouts with unique number of days accessed")

# ==============================================================================
# MODELS
# ==============================================================================
# ------------------------------------------------------------------------------
# Decision Tree
# ------------------------------------------------------------------------------

library(rpart)
library(rpart.plot)

tree.model <- rpart(as.factor(dropout) ~ ., data = train,
                    control = rpart.control(maxdepth = 7))
prp(tree.model)

# Predict
pred <- predict(tree.model, newdata = test)
pred <- ifelse(pred[, 2] >= 0.5, 1, 0)
compute_stats(pred, test)

# ------------------------------------------------------------------------------
# Random Forest
# ------------------------------------------------------------------------------

library(randomForest)

formula <- as.formula(as.factor(dropout) ~ .) 
rf.model <- randomForest(formula, data = train, do.trace = TRUE)

# Predict
pred <- predict(rf.model, newdata = test)
compute_stats(as.integer(as.character(pred)), test)

# ------------------------------------------------------------------------------
# GBM
# ------------------------------------------------------------------------------

library(gbm)
library(pROC)

train$dropout <- as.numeric(train$dropout == 1) ## what is this for?
gbm.model <- gbm(dropout ~ ., data = train, n.trees = 500,
                 distribution = "bernoulli", bag.fraction = 0.75, cv.folds = 5,
                 interaction.depth = 3, verbose = TRUE)

gbm_perf <- gbm.perf(gbm.model, method = "cv")
pred <- predict(gbm.model, newdata = test, n.trees = gbm_perf,
                type = "response")

roc.curve <- roc(test$dropout, pred)
plot(roc.curve, print.thres = "best",
     print.thres.best.method = "closest.topleft")

result.coords <- coords(roc.curve, "best", best.method="closest.topleft",
                        ret = c("threshold", "accuracy"))
result.coords
glmnet::auc(as.numeric(test$dropout == 1), pred)

# ------------------------------------------------------------------------------
# GBM with cross-validation and hyperparameter tuning using caret
# ------------------------------------------------------------------------------

library(caret)
library(e1071)

train_gbm <- train
train_gbm$dropout <- ifelse(train_gbm$dropout == 1, "Dropout", "NoDropout")

# create trainControl & grid for cross validation and grid to be searched
objControl <- trainControl(method = "cv", number = 3, returnResamp = "none", 
                           summaryFunction = twoClassSummary, classProbs = TRUE)
gbmGrid <- expand.grid(interaction.depth = c(7, 9, 11),
                    n.trees = seq(1500, 5000, 100),
                    shrinkage = c(0.01, 0.001, 0.0001))

# Run Model
registerDoMC(cores=38)
gbmModel <- train(train_gbm[,-ncol(train_gbm)], as.factor(train_gbm$dropout), 
                  method = "gbm", 
                  trControl = objControl,
                  tuneGrid = gbmGrid,
                  metric = "ROC",
                  preProc = c("center", "scale"))

# Find out variable importance
summary(gbmModel)

# Find out model details
gbmModel

# Predict probabilites 
library(pROC)
test_gbm <- test[,colnames(train_gbm)]
predictions <- predict(gbmModel, newdata = test_gbm[,-ncol(test_gbm)], type = "prob")
head(predictions)

# Compute AUC
glmnet::auc(as.numeric(test_gbm$dropout == 1), predictions[[1]])

## model evaluation
detach("package:caret", unload=TRUE)
detach("package:plyr", unload=TRUE)
detach("package:pROC", unload=TRUE)

eval <- data.frame(enrollment_id = test$enrollment_id, predictions, actual = test_gbm[,ncol(test_gbm)])
falsepos <- filter(eval, Dropout>NoDropout & actual == 0)

# ==============================================================================
# SUBMISSIONS
# ==============================================================================

# Load actual test data
enroll_test_df <- fread("~/kdd_cup/enrollment_test.csv", data.table = FALSE)
log_test_df <- fread("~/kdd_cup/log_test.csv", data.table = FALSE)

detach("package:plyr", unload=TRUE)

log_test_df <- clean_log(log_test_df)

# Create detail df
detail_test_df <- create_detail(log_test_df, object_df)

# make course table
course_test_df <- create_course(object_df, log_test_df)

# Free up some memory by removing unneeded objects
rm(log_test_df, object_df)
gc()

# Create summary df
summary_test_df <- create_summary(detail_test_df)
all_test_df <- create_all(summary_test_df, course_test_df)
all_test_df %<>% left_join(enroll_test_df, .)

# Predict using decision tree
pred <- predict(tree.model, newdata = summary_test_df)
pred <- ifelse(pred[, 2] >= 0.5, 1, 0)

# Predict using random forest 500 trees
pred <- predict(rf.model, newdata = summary_test_df)
pred <- as.integer(as.character(pred))
pred <- ifelse(is.na(pred), 1, pred)

# Predict using gbm
test_df <- all_test_df[,colnames(train_gbm)[-ncol(train_gbm)]]
pred <- predict(gbmModel, newdata = test_df, type = "prob")
detach("package:plyr", unload=TRUE)
# Create submission data frame
submit_df <- data.frame(enroll_id = enroll_test_df$enrollment_id, 
                        prediction = pred[, 1])

# Write results to file
write.table(submit_df, "~/kdd_cup/submissions/submission2_gbmcaret_May28.csv",
          col.names = F, row.names = F, quote = F, sep = ',')
