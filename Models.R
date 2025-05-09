libs = c("tidyverse", "lubridate", "here", "zoo", "quantmod", "TTR", "nnet", "caret", "rpart")
invisible(lapply(libs, library, character.only = TRUE))

mainFolder = here()
setwd(mainFolder)
setwd("Data")

table = read_csv("AXISBANK.csv")
table$Date <- ymd(table$Date)

table <- table %>%
  mutate(
    Signal = case_when(
      lead(Close, 1) >= Close * 1.005 ~ "Buy",
      lead(Close, 1) <= Close * 0.995 ~ "Sell",
      TRUE ~ "Hold"
    )
  )

# for Supertrend feature
atr_values <- ATR(HLC = table[, c("High", "Low", "Close")], n = 10)
table$ATR <- atr_values[, "atr"]
multiplier <- 3
table$Upper_Band <- table$`Prev Close` + (multiplier * table$ATR)
table$Lower_Band <- table$`Prev Close` - (multiplier * table$ATR)

table$SMA_5 <- rollmean(table$Close, k = 5, fill = NA, align = "right")
table$SMA_20 <- rollmean(table$Close, k = 20, fill = NA, align = "right")

table$ROC_10 <- ROC(table$Close, n = 10)
table$RSI_14 <- RSI(table$Close, n = 14)
table$SMA_Slope <- table$SMA_5 - table$SMA_20
table$Return_1d <- (table$Close / lag(table$Close, 1)) - 1
macd <- MACD(table$Close, nFast = 12, nSlow = 26, nSig = 9)
table$MACD_Line <- macd[, 1]

adx <- ADX(HLC(table), n = 14)
table$ADX <- adx[, "ADX"]

table$Volume_Spike <- table$Volume / SMA(table$Volume, n = 20)

n <- nrow(table)
table$Supertrend <- NA
table$Trend <- TRUE  # TRUE for uptrend, FALSE for downtrend

for (i in 2:n) {
  prev_close <- table$`Prev Close`[i]
  curr_close <- table$Close[i]
  
  # Carry forward the final upper/lower bands
  final_ub <- table$Upper_Band[i]
  final_lb <- table$Lower_Band[i]
  
  # Update final bands to prevent shrinking during a strong trend
  if (!is.na(table$Supertrend[i - 1])) {
    if (curr_close > table$Upper_Band[i - 1]) {
      table$Trend[i] <- TRUE  # Uptrend
    } else if (curr_close < table$Lower_Band[i - 1]) {
      table$Trend[i] <- FALSE # Downtrend
    } else {
      table$Trend[i] <- table$Trend[i - 1]  # Continue trend
      if (table$Trend[i]) {
        if (final_lb < table$Lower_Band[i - 1]) {
          final_lb <- table$Lower_Band[i - 1]
        }
      } else {
        if (final_ub > table$Upper_Band[i - 1]) {
          final_ub <- table$Upper_Band[i - 1]
        }
      }
    }
  }
  
  # Assign supertrend value based on trend
  if (table$Trend[i]) {
    table$Supertrend[i] <- final_lb
  } else {
    table$Supertrend[i] <- final_ub
  }
}
table$ST_Dist <- table$Close - table$Supertrend

table = table %>% select(where(~ sum(is.na(.)) <= 500))
table = na.omit(table)
cor(subset(table, select = c("ROC_10", "RSI_14", "SMA_Slope", "Return_1d", "MACD_Line", "ST_Dist", "ADX", "Volume_Spike")), use = "complete.obs")

# Normalizing features
features_to_scale <- c("ROC_10", "RSI_14", "SMA_Slope", "Return_1d", "MACD_Line", "ST_Dist", "ADX", "Volume_Spike")
# Scale only on rows with no NA to avoid carrying NA through
table[features_to_scale] <- scale(table[features_to_scale])

View(table)

table$Signal <- as.factor(table$Signal)
signal_levels <- levels(table$Signal)

window_size <- 200
n <- nrow(table)
predictions <- c()
true_labels <- c()

for (i in (window_size + 1):n) {
  train_data <- table[(i - window_size):(i - 1), ]
  test_data  <- table[i, ]
  
  # For decision tree model 
  model <- rpart(Signal ~ ROC_10 + ADX + RSI_14 + Return_1d + ST_Dist + Volume_Spike, data = train_data)
  # For neural network model
  #model <- multinom(Signal ~ ROC_10 + ADX + RSI_14 + Return_1d + ST_Dist + Volume_Spike, data = train_data)
  
  # Predict for the next day
  pred <- predict(model, newdata = test_data, type = "class")
  
  # Save results
  predictions <- c(predictions, pred)
  true_labels <- c(true_labels, test_data$Signal)
}

accuracy <- mean(predictions == true_labels)
print(paste("Rolling window accuracy:", round(accuracy * 100, 2), "%"))

predictions <- signal_levels[unlist(predictions)]
true_labels <- signal_levels[as.numeric(true_labels)]

true_factor <- factor(true_labels, levels = c("Buy", "Hold", "Sell"))
pred_factor <- factor(predictions, levels = c("Buy", "Hold", "Sell"))

confusionMatrix(pred_factor, true_factor)