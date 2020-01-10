## ----load our packages, echo = TRUE, message= FALSE, warning = FALSE-----
repo_url <- "http://cran.us.r-project.org"
if(!require(kableExtra)) 
  install.packages("kableExtra", repos = repo_url)
if(!require(tidyverse))
  install.packages("tidyverse", repos = repo_url)
if(!require(data.table)) 
  install.packages("data.table", repos = repo_url)
if(!require(caret)) 
  install.packages("caret", repos = repo_url)


## ----some helper functions-----------------------------------------------
cor_prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

flatten_square_matrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]], j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut], p=m[ut])
}

order_by_pvalue <- function(fit.data, selected) {
  d1 <- data.frame(summary(fit.data)$coeff)
  dta <- cbind(variable=rownames(d1), d1)
  dta <-dta[selected, ]
  names(dta) <- c('Variable', 'Estimate', 'Std.Error', 'z.value', 'pvalue')
  selected.ds <- dta %>% 
    filter(Variable != '(Intercept)') %>% 
    arrange(pvalue)
  names(selected.ds)[5] <- 'p-value'
  return(selected.ds)
}

separate.variables <- function(fit.data, alpha.level) {
  signif.idx <- summary(fit.data)$coeff[,4] < alpha.level
  signif.idx[0] <- TRUE
  result <- list(significant = order_by_pvalue(fit.data, signif.idx),
                 nonsignificant = order_by_pvalue(fit.data, !signif.idx))
  return(result)
}


## ------------------------------------------------------------------------
load_clean_data <- function (ds_filename) {
  temp <- read.table(ds_filename, sep=",", header=TRUE)
  temp['i_pmf_7d_after_sub'] <- temp['i_pmfcan_7d_after_sub'] + temp['i_pmfcus_7d_after_sub']
  temp_tb <- as_tibble(temp) %>%
    filter(duration == 1 & gender == 1 & iswinback==0) %>%
    select(
      cancel_07,
      i_fr_7d_after_sub, i_ch_7d_after_sub, i_fl_7d_after_sub,
      i_pv_7d_after_sub, i_wr_7d_after_sub, i_fl_from_blocked_7d_after_sub,
      i_wi_from_blocked_7d_after_sub,
      o_wi_7d_after_sub, o_fr_7d_after_sub, o_ch_7d_after_sub, o_fl_7d_after_sub,
      o_pv_7d_after_sub,
      o_ciy_7d_after_sub, o_caru_7d_after_sub, o_za_zm_7d_after_sub, 
      o_zd_7d_after_sub,  o_fl_blocked_7d_after_sub,
      o_wi_blocked_7d_after_sub, photo_ver_7d_after_sub,
      d2d2_blocked_7d_after_sub,
      i_fl_pm_7d_after_sub, i_gmf_7d_after_sub, i_pmf_7d_after_sub,
      o_gmf_7d_after_sub, o_pmfcan_7d_after_sub, o_pmfcus_7d_after_sub,
      o_cin_7d_after_sub,
      invi_7d_after_sub)
  # model1_ds <- na.omit(model1_ds)
  temp_tb
}

# Loading the data ---------------------------
work_dir <- '/tmp/churn'
train_filename <- paste(work_dir, '/', 'train_data.csv', sep = '')
test_filename <- paste(work_dir,  '/', 'test_data.csv', sep = '')
train_tb <- load_clean_data(train_filename) 
test_tb <- load_clean_data(test_filename) 


## ------------------------------------------------------------------------
str(train_tb)


## ---- warning=FALSE------------------------------------------------------
cor_results <- flatten_square_matrix(cor_prob(train_tb))
cor_table <- cor_results %>% 
  select(i,j,cor)  %>% 
  filter(abs(cor) > 0.80) %>% 
  arrange(i,desc(cor))
names(cor_table) <- c('Variable 1', 'Variable 2', 'Correlation Coef.')
kable(cor_table) %>% 
  kable_styling(position = "center", full_width = F)


## ------------------------------------------------------------------------
train_tb_v1 <- train_tb %>% select (-c(o_ch_7d_after_sub))


## ----warning=FALSE-------------------------------------------------------
logit_model_v1 <- glm(formula = cancel_07 ~ ., family = binomial, data = train_tb_v1)
summary(logit_model_v1)


## ------------------------------------------------------------------------
model_v1_vars <- separate.variables(logit_model_v1, 0.05)
kable(model_v1_vars$significant) %>% 
  kable_styling(position = "center", full_width = F)


## ------------------------------------------------------------------------
kable(model_v1_vars$nonsignificant) %>% 
  kable_styling(position = "center", full_width = F)


## ------------------------------------------------------------------------
extract_ds_v2 <- function(ds) {
  ds %>% 
    select(cancel_07,
           o_pv_7d_after_sub, i_pv_7d_after_sub, o_za_zm_7d_after_sub,
           o_fl_7d_after_sub, i_gmf_7d_after_sub, o_ciy_7d_after_sub,
           i_ch_7d_after_sub, photo_ver_7d_after_sub, i_fl_pm_7d_after_sub,
           i_wi_from_blocked_7d_after_sub, o_zd_7d_after_sub, o_wi_7d_after_sub)
}
model_v1_vars$significant$Variable
train_tb_v2 <- extract_ds_v2(train_tb_v1)
test_tb_v2 <- extract_ds_v2(test_tb)


## ----warning=FALSE-------------------------------------------------------
logit_model_v2 <- glm(formula = cancel_07 ~ ., family = binomial, data = train_tb_v2)
summary(logit_model_v2)


## ------------------------------------------------------------------------
test_input <- test_tb_v2
test_input$pred_prob <- predict(logit_model_v2, test_tb_v2, type = "response")


## ------------------------------------------------------------------------
test_input <- test_input  %>% 
  mutate(model_pred = 1 * (pred_prob > 0.53) + 0)
test_input$model_pred[is.na(test_input$model_pred)] <- 0
test_input <- test_input %>% mutate(accurate = 1 * (model_pred == cancel_07) + 0)
model_accuracy_pct <- sum(test_input$accurate)/nrow(test_input) * 100

accuracy_df <- data.frame(model = "Simple Model (Avg Rating)", rmse = round(model_accuracy_pct,digits=3))
names(accuracy_df)  <- c("Metric", "Value")
kable(accuracy_df) %>% 
    kable_styling(position = "center", full_width = F)

