# ETF-Analysis-on-R-
An analysis of JP Morgan's Premium US Equity ETF


```{r}

# STEP 1: Installing useful packages:

library(tidyverse)
library(stringr)

install.packages("e1071")
library(e1071)
install.packages("anytime")
library(anytime)

install.packages("lubridate")
library(lubridate)

install.packages("ggplot2")
library(ggplot2)


```


```{r}

# STEP 2:Opening and reading the data file:


# JP Morgan Premium Equity ETF: 

df_etf <- read.csv('/Users/GiovanniGentile/Desktop/JPMorgan_Equity Premium_Income_ETF3.csv')


# Goldman Sachs' US Large Cap Equity ETF: 

df_goldman <- read.csv('/Users/GiovanniGentile/Desktop/JUST.csv')


# S&P 500 ETF Trust: 

df_spy <- read.csv('/Users/GiovanniGentile/Desktop/SPY.csv')


# Checking all is good:

df_etf
df_goldman
df_spy
```

```{r}

# STEP 3: Now I will be working on the JP Morgan dataset and generating some insights


# Converting date column to date format = dd/mm/yyyy:

df_etf$Date <- anydate(df_etf$Date)


# Checking all is good:

df_etf

```


```{r}

# Now, converting all other columns to numeric

df_etf[, -1] <- sapply(df_etf[, -1], as.numeric)


# Checking all is good:

df_etf
```

```{r}

# Creating a new column on the data frame with Returns (absolute returns):

df_etf$Returns <- (df_etf$Close - df_etf$Open)


# Checking all is good:

df_etf
```


```{r}

# Creating a new column on the dataframe with returns in percentages:

df_etf$Percentage_Returns <- (df_etf$Returns / df_etf$Open) 


# Checking all is good:

df_etf

```


```{r}

# Graph of Price over time:

ggplot(df_etf, aes(x = Date, y = Close)) +
  geom_area(fill = "#54301a", alpha = 0.2) +
  geom_line(size = 0.7, color = "#54301a") +
  labs(x = "Date", y = "Closing Price") +
  ggtitle("Price of JP Morgan's Equity Premium ETF Over Time") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"))

# This is interesting. The graph clearly shows reduced volatility with a decrease in the end of 2022.

```


```{r}

# Graph of Price over time with moving averages:

install.packages("zoo")
library(zoo)
library(dplyr)

# Create 10-day, 30-day, and 50-day moving averages for volume
df_etf_ma <- df_etf %>%
  mutate(ma_10d = zoo::rollmean(Close, k = 10, fill = NA, align = "right"),
         ma_30d = zoo::rollmean(Close, k = 30, fill = NA, align = "right"),
         ma_50d = zoo::rollmean(Close, k = 50, fill = NA, align = "right"))

# Plot the volume with all three moving averages
ggplot(df_etf_ma, aes(x = Date)) +
  geom_area(aes(y = Close), fill = "#54301a", alpha = 0.3) +
  geom_line(aes(y = ma_10d), size = 1, color = "red") +
  geom_line(aes(y = ma_30d), size = 1, color = "#FFD700") +
  geom_line(aes(y = ma_50d), size = 1, color = "blue") +
  labs(x = "Date", y = "Price") +
  ggtitle("Price of JP Morgan's Equity Premium ETF Over Time") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        legend.position = "right") +
  scale_color_manual(name = "Moving Averages",
                     values = c("red", "#FFD700", "blue"),
                     labels = c("10-day MA", "30-day MA", "50-day MA"))


```

```{r}

# Graph of volume over time:

ggplot(df_etf, aes(x = Date, y = Volume)) +
  geom_area(fill = "#54301a", alpha = 0.2) +
  geom_line(size = 0.7, color = "#54301a") +
  labs(x = "Date", y = "Volume") +
  ggtitle("Volume of JP Morgan's Equity Premium ETF Over Time") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"))

# This is interesting, volume has increased recently but it very volatile. Much more volatile than Price.
```



```{r}

# Let's graph volume with moving averages:

install.packages("zoo")
library(zoo)
library(dplyr)

# Create 10-day, 30-day, and 50-day moving averages for volume
df_etf_ma <- df_etf %>%
  mutate(ma_10d = zoo::rollmean(Volume, k = 10, fill = NA, align = "right"),
         ma_30d = zoo::rollmean(Volume, k = 30, fill = NA, align = "right"),
         ma_50d = zoo::rollmean(Volume, k = 50, fill = NA, align = "right"))

# Plot the volume with all three moving averages
ggplot(df_etf_ma, aes(x = Date)) +
  geom_area(aes(y = Volume), fill = "#54301a", alpha = 0.8) +
  geom_line(aes(y = ma_10d), size = 1, color = "red") +
  geom_line(aes(y = ma_30d), size = 1, color = "#FFD700") +
  geom_line(aes(y = ma_50d), size = 1, color = "blue") +
  labs(x = "Date", y = "Trading Volume") +
  ggtitle("Trading of JP Morgan's Equity Premium ETF Over Time") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"),
        legend.position = "right") +
  scale_color_manual(name = "Moving Averages",
                     values = c("red", "#FFD700", "blue"),
                     labels = c("10-day MA", "30-day MA", "50-day MA"))

```




```{r}
# Now, let's perform some descriptive analysis: 

# Average Closing price of the ETF and other important statistics of the variable: 
summary(df_etf$Close)
skewness(df_etf$Close, type = 2)
hist(df_etf$Close)

# Average return of the ETF and other important statistics of the variable: 
summary(df_etf$Percentage_Returns)
skewness(df_etf$Percentage_Returns, type = 2)
hist(df_etf$Percentage_Returns)

# Average of volume traded over time and summary statistics of the variable:
summary(df_etf$Volume)
skewness(df_etf$Volume, type = 2)
hist(df_etf$Volume)


# As we have observed in the previous four graphs, volume seems to be more volatile than price. Let's check:

# Standard deviation:
sd(df_etf$Volume) > sd(df_etf$Close)
# Variation:
var(df_etf$Volume) > var(df_etf$Close)


```


```{r}

# It is very important to adjust the price and returns based on trading volume. This is why we will create this new variable.

# Let's now create a variable which adjusts return according to trading volume:

df_etf$Percentage_volume_return <- (df_etf$Percentage_Returns) * df_etf$Volume # This multiplies percentage returns with the volume
df_etf$Percentage_of_Total_Volume = (df_etf$Percentage_volume_return / sum(df_etf$Volume)) # It then divides it by the total sum of trading volume 

# It basically 'weighted' percentage returns' by their daily trading volume.

# Let's check:
df_etf

```


```{r}
# Let's graph returns weighted by trading volume:

ggplot(df_etf, aes(x = Date, y = Percentage_of_Total_Volume)) +
  geom_line(size = 0.7, color = "#54301a") +
  labs(x = "Date", y = "Volume-weighted Returns") +
  ggtitle("Returns of JP Morgan's Equity Premium ETF Over Time, weighted by trading volume") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"))

# Now, this is really insightful. Trading volume increased from 2022 on a steady path. This means that when you look at volume-weighted returns, they are much more volatile in 2022 and 2023 because of how volume 'amplified' risk.
```


```{r}

# Let's now consider volatility.Volatiliy is usually calculated with standard deviation.

# Checking for volatility: 


# Calculating volatility on percentage returns: 
sd(df_etf$Percentage_Returns)
var(df_etf$Percentage_Returns)

# Calculating volatility on volume-weighted returns:
sd(df_etf$Percentage_of_Total_Volume)
var(df_etf$Percentage_of_Total_Volume)

# This might seem counter-intuitive. However, it is due to the fact that returns weighted for volume have been steady for 2 years before increasing and being steady at a higher level for another 2 years. So, it is understandable.

sd(df_etf$Percentage_Returns) > sd(df_etf$Percentage_of_Total_Volume)
var(df_etf$Percentage_Returns) > var(df_etf$Percentage_of_Total_Volume)

```

```{r}
# STEP 4

# Now that we have concluded analysis on the JP Morgan's ETF dataset, let's introduce two more variables and proceed with benchmarking analysis. 

# First, I'm turning goldman dataset variables and S&P 500 dataset variables to numeric:
df_goldman[, -1] <- sapply(df_goldman[, -1], as.numeric)
df_spy[, -1] <- sapply(df_spy[, -1], as.numeric)

```


```{r}

# I then calculate absolute returns for both dataframes:

df_goldman$Returns <- (df_goldman$Close - df_goldman$Open)
df_spy$Returns <- (df_spy$Close - df_spy$Open)


# Checking it's all good:
df_goldman
df_spy
```


```{r}
# Similarly to the JP Morgan dataset, I will now proceed to calculate returns for both Goldman Sachs and the S&P 500, expressed in percentages:

df_goldman$Percentage_Returns <- (df_goldman$Returns / df_goldman$Open) 
df_spy$Percentage_Returns <- (df_spy$Returns / df_spy$Open) 


# Checking it's all good:

df_goldman
df_spy
```

```{r}


# This graph is just for reference, and is nothing more than percentage returns for the Goldman Sachs ETF over time. 

ggplot(df_etf, aes(x = Date, y = Percentage_Returns)) +
  geom_hline(yintercept = 0, size = 0.5, color = "black") +
  geom_line(size = 0.5, color = "#54301a") +
  labs(x = "Date", y = "Percentage Returns") +
  ggtitle("Returns of JP Morgan's Equity Premium ETF Over Time") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"))
```





```{r}

# Now converting dates to date types to have no errors when making graphs:
df_goldman$Date <- anydate(df_goldman$Date)


# This graph is just for reference, and is nothing more than percentage returns for the Goldman Sachs ETF over time. 

ggplot(df_goldman, aes(x = Date, y = Percentage_Returns)) +
  geom_hline(yintercept = 0, size = 0.5, color = "black") +
  geom_line(size = 0.7, color = "#6B96C3") +
  labs(x = "Date", y = "Percentage Returns") +
  ggtitle("Returns of Goldman Sachs' Equity Large Cap ETF Over Time") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"))

# It clearly follows a random walk. Not too insightful
```


```{r}

# Turning the date column to date type

df_spy$Date <- anydate(df_spy$Date)


# Doing the exact same thing as I did with Goldman's ETF. Plotting returns against time.

ggplot(df_spy, aes(x = Date, y = Percentage_Returns)) +
  geom_hline(yintercept = 0, size = 0.5, color = "black") +
  geom_line(size = 0.7, color = "#425951") +
  labs(x = "Date", y = "Percentage Returns") +
  ggtitle("Returns of S&P 500 ETF Trust Over Time") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black"))

# Again, nothing too insightful
```

```{r}

# Here I create the same variable I previously created for JP's data when calculating returns adjusted to volumes. 

# It looks long because they have to be calculated in two steps

df_goldman$Percentage_volume_return <- (df_goldman$Percentage_Returns) * df_goldman$Volume
df_goldman$Percentage_of_Total_Volume = (df_goldman$Percentage_volume_return / sum(df_goldman$Volume))

# Checking it's all good:
df_goldman

df_spy$Percentage_volume_return <- (df_spy$Percentage_Returns) * df_spy$Volume
df_spy$Percentage_of_Total_Volume = (df_spy$Percentage_volume_return / sum(df_spy$Volume))

# Checking it's all good:
df_spy
```


```{r}

# Here is an important step.

# To proceed with benchmark analysis, I am creating a new dataframe, called 'df_benchmark'. 

# In this dataframe, I am matching, for JP, Goldman and S&P 500, their closing price, their returns (as percentages), their volume and their returns adjusted for volumes based on dates. 

# In other words, I have created a dataset where all the info I need will be displayed only if they exist in all three datasets under the same date.


library(dplyr)

df_benchmark <- inner_join(df_etf, df_goldman, by = "Date") %>% 
                inner_join(df_spy, by = "Date") %>%
                select(Date, 
                       Close_jp = Close.x, 
                       Close_goldman = Close.y, 
                       Close_spy = Close,
                       Volume_jp = Volume.x,
                       Volume_goldman = Volume.y,
                       Volume_spy = Volume,
                       Percentage_Returns_jp = Percentage_Returns.x,
                       Percentage_Returns_goldman = Percentage_Returns.y,
                       Percentage_Returns_spy = Percentage_Returns,
                       Percentage_of_Total_Volume_jp = Percentage_of_Total_Volume.x,
                       Percentage_of_Total_Volume_goldman = Percentage_of_Total_Volume.y,
                       Percentage_of_Total_Volume_spy = Percentage_of_Total_Volume)

df_benchmark


```


```{r}

# This is a graph of prices against time for all three assets. Could be done on returns as well, but wouldn't be interesting, they're three random walks.

df_benchmark$Date <- anydate(df_benchmark$Date)

ggplot(df_benchmark, aes(x = Date)) +
  geom_line(aes(y = Close_jp, color = "#54301a")) +
  geom_line(aes(y = Close_goldman, color = "#6B96C3")) +
  scale_y_continuous(name = "JP Morgan & Goldman Sachs ETF Closing Prices", 
                     sec.axis = sec_axis(~./10, name = "S&P 500 ETF Closing Price")) +
  geom_line(aes(y = Close_spy/10, color = "#425951")) +
  labs(title = "ETF Close Prices",
       x = "Date",
       y = "JP Morgan ETF & Goldman Sachs ETF Closing Prices",
       color = "ETFs") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "bottom",
        legend.box.background = element_rect(color = "black", fill = "white")) +
  scale_color_manual(values = c("#54301a", "#6B96C3", "#425951"), 
                     labels = c("JP Morgan", "Goldman Sachs", "S&P 500"))

```


```{r}
# Here I am creating and adding to the df_benchmark dataframe 3 colums for Price, Percentage Returns and Volume where the average accross the three assets is calculated and displayed.

library(dplyr)

df_benchmark <- df_benchmark %>%
  rowwise() %>%
  mutate(`Avg. Price` = mean(c(Close_jp, Close_goldman, Close_spy)),
         `Avg Volume` = mean(c(Volume_jp, Volume_goldman, Volume_spy)),
         `Avg Percentage_Returns` = mean(c(Percentage_Returns_jp, Percentage_Returns_goldman, Percentage_Returns_spy)),
         `Avg Percentage_of_Total_Volume` = mean(c(Percentage_of_Total_Volume_jp, Percentage_of_Total_Volume_goldman, Percentage_of_Total_Volume_spy)))

df_benchmark
```


```{r}

# This is a graph of JP Morgan's Closing Price against the average, with their spread highlighted in blue.

library(ggplot2)

# Calculate the average close
df_benchmark$avg_close <- rowMeans(df_benchmark[, c("Close_jp", "Close_goldman", "Close_spy")], na.rm = TRUE)

# Create the graph
ggplot(df_benchmark, aes(x = Date)) +
  geom_line(aes(y = Close_jp), size = 1, color = "#54301a") +
  geom_line(aes(y = avg_close), size = 1, color = "#FFD700") +
  labs(x = "Date", y = "Price") +
  ggtitle("JP Close vs. Average Close") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black")) +
  geom_ribbon(aes(ymin = pmin(Close_jp, avg_close), ymax = pmax(Close_jp, avg_close)), 
              fill = "blue", alpha = 0.3)


# Not very interesting, they're just on two different price levels.
```


```{r}
# This is a graph of JP Morgan's Closing Volume against the average, with their spread highlighted in blue.

# Calculate the average close
df_benchmark$avg_volume <- rowMeans(df_benchmark[, c("Volume_jp", "Volume_goldman", "Volume_spy")], na.rm = TRUE)

# Create the graph
ggplot(df_benchmark, aes(x = Date)) +
  geom_line(aes(y = Volume_jp), size = 1, color = "#54301a") +
  geom_line(aes(y = avg_volume), size = 1, color = "#FFD700") +
  labs(x = "Date", y = "Trading Volume") +
  ggtitle("JP Volume vs. Average Volume") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black")) +
  geom_ribbon(aes(ymin = pmin(Volume_jp, avg_volume), ymax = pmax(Volume_jp, avg_volume)), 
              fill = "blue", alpha = 0.3)

# JP Morgan's ETF trades at much lower volumes than average, even though this is probably raised by the S&P 500 which trades much larger volumes than both JP and Goldman.
```


```{r}
# This is a graph of JP Morgan's returns against the average, with their spread highlighted in blue.


# Calculate the average close
df_benchmark$avg_percentage_returns <- rowMeans(df_benchmark[, c("Percentage_Returns_jp", "Percentage_Returns_goldman", "Percentage_Returns_spy")], na.rm = TRUE)

# Create the graph
ggplot(df_benchmark, aes(x = Date)) +
  geom_line(aes(y = Percentage_Returns_jp), size = 0.8, color = "#54301a") +
  geom_line(aes(y = avg_percentage_returns), size = 0.3, color = "red", linetype = "dotted") +
  labs(x = "Date", y = "Percentage Returns") +
  ggtitle("JP Returns vs. Average Returns") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black")) +
  geom_ribbon(aes(ymin = pmin(Percentage_Returns_jp, avg_percentage_returns), ymax = pmax(Percentage_Returns_jp, avg_percentage_returns)), 
              fill = "blue", alpha = 0.3)


# They are very similar, the spread is very low.
# Dotted line and thinner and change title 
```

```{r}

# This is a graph of JP Morgan's returns weighted with volume against the average, with their spread highlighted in blue.

# Calculate average percentage of total volume
df_benchmark$avg_percentage_of_total_volume <- rowMeans(df_benchmark[, c("Percentage_of_Total_Volume_jp", "Percentage_of_Total_Volume_goldman", "Percentage_of_Total_Volume_spy")], na.rm = TRUE)

# Create the graph
ggplot(df_benchmark, aes(x = Date)) +
  geom_line(aes(y = Percentage_of_Total_Volume_jp), size = 1, color = "#54301a") +
  geom_line(aes(y = avg_percentage_of_total_volume), size = 1, color = "#FFD700") +
  labs(x = "Date", y = "Percentage of Total Volume") +
  ggtitle("JP Percentage of Total Volume vs. Average Percentage of Total Volume") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        axis.line = element_line(color = "black"),
        axis.text = element_text(color = "black")) +
  geom_ribbon(aes(ymin = pmin(Percentage_of_Total_Volume_jp, avg_percentage_of_total_volume), ymax = pmax(Percentage_of_Total_Volume_jp, avg_percentage_of_total_volume)), 
              fill = "blue", alpha = 0.3)
              
# They are very similar, the spread is very low.

```


```{r}

# Here I am calculating the spread. 

# The spread is the PERCENTAGE DIFFERENCE between Goldman Sachs's ETF and JP Morgan's ETF. It is a new column calculated in two steps and added to the dataframe df_benchmark.

# Benchmarking returns 
df_benchmark$goldman_jp_price_spread <- (df_benchmark$Close_jp) - (df_benchmark$Close_goldman) 
df_benchmark$price_spread <- df_benchmark$goldman_jp_price_spread / df_benchmark$Close_jp

df_benchmark$goldman_jp_volume_spread <- (df_benchmark$Volume_jp) - (df_benchmark$Volume_goldman)
df_benchmark$volume_spread <- df_benchmark$goldman_jp_volume_spread / df_benchmark$Volume_jp

df_benchmark$goldman_jp_price_PercentageReturns <- (df_benchmark$Percentage_Returns_jp) - (df_benchmark$Percentage_Returns_goldman) 
df_benchmark$returns_spread <- df_benchmark$goldman_jp_price_PercentageReturns / df_benchmark$Percentage_Returns_jp

# The spread has been calculated for Price, Volume and Percentage Returns 


# Let's check it's all good:
                                                    
df_benchmark


```


```{r}

# I can now plot all of the spreads with cool graphs:


# Plotting price_spread against Date
ggplot(df_benchmark, aes(x = Date, y = price_spread)) + 
  geom_ribbon(aes(ymin = 0, ymax = price_spread), fill = "#54301a", alpha = 0.5) +
  geom_line(size = 0.7, color = "#54301a") + 
  labs(title = "Price Spread Over Time", y = "Price Spread", x = "Date") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(min(df_benchmark$price_spread), max(df_benchmark$price_spread)))

# Plotting volume_spread against Date
ggplot(df_benchmark, aes(x = Date, y = volume_spread)) + 
  geom_ribbon(aes(ymin = 0, ymax = volume_spread), fill = "#54301a", alpha = 0.5) +
  geom_line(size = 1.0, color = "#54301a") + 
  labs(title = "Volume Spread Over Time", y = "Volume Spread", x = "Date") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(min(df_benchmark$volume_spread), max(df_benchmark$volume_spread)))

# Plotting returns_spread against Date
ggplot(df_benchmark, aes(x = Date, y = returns_spread)) + 
  geom_ribbon(aes(ymin = 0, ymax = returns_spread), fill = "#54301a", alpha = 0.5) +
  geom_line(size = 0.7, color = "#54301a") + 
  labs(title = "Return Spread Over Time", y = "Return Spread", x = "Date") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
```


```{r}

# I can plot a 3D scatter plot with returns accross the three assets too.

install.packages("scatterplot3d")

library(scatterplot3d)

# create a data frame with the variables of interest
df <- data.frame(
  jp_returns = df_benchmark$Percentage_Returns_jp,
  goldman_returns = df_benchmark$Percentage_Returns_goldman,
  spy_returns = df_benchmark$Percentage_Returns_spy
)

# create a 3D scatterplot
scatterplot3d(df, color = "#54301a", pch = 20, main = "3D Scatter Plot")

# fit a linear regression model
fit <- lm(jp_returns ~ goldman_returns + spy_returns, data = df)

# A cool insight here is that they all rise together and have positive correlation, as expected.

```


```{r}

# Finally, a correlation matrix:

library(ggplot2)
install.packages("reshape2")
library(reshape)


# create a data frame with the variables of interest
df <- data.frame(
  jp_returns = df_benchmark$Percentage_Returns_jp,
  goldman_returns = df_benchmark$Percentage_Returns_goldman,
  spy_returns = df_benchmark$Percentage_Returns_spy,
  jp_volume = df_benchmark$Volume_jp,
  goldman_volume = df_benchmark$Volume_goldman,
  spy_volume = df_benchmark$Volume_spy,
  jp_close = df_benchmark$Close_jp,
  goldman_close = df_benchmark$Close_goldman,
  spy_close = df_benchmark$Close_spy
)

# calculate the correlation matrix
corr <- cor(df)

# create a data frame with the correlation matrix values and labels
corr_df <- reshape2::melt(corr)
corr_df$Var1 <- gsub("_", " ", corr_df$Var1)
corr_df$Var2 <- gsub("_", " ", corr_df$Var2)

# create a ggplot with the correlation matrix and color-coded tiles
ggplot(corr_df, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#FFFFFF", mid = "#FFCC99", high = "#54301a", 
                       midpoint = 0.8, guide = "colorbar", na.value = "#CCCCCC") +
  labs(title = "Correlation Matrix") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
  coord_fixed() +
  guides(fill = guide_colorbar(title = "Correlation", barwidth = 10, barheight = 0.5,
                               ticks = FALSE, direction = "horizontal",
                               label.position = "bottom")) +
  theme(legend.title = element_blank())
```

```{r}
df_benchmark
```


```{r}


# Creating table 2: column with min and max values for each ETF and corresponding date.

library(dplyr)

df_benchmark %>%
  mutate(year = lubridate::year(Date)) %>%
  group_by(year) %>%
  summarize(
    jp_max = max(Close_jp),
    jp_max_date = Date[which.max(Close_jp)],
    jp_min = min(Close_jp),
    jp_min_date = Date[which.min(Close_jp)],
    goldman_max = max(Close_goldman),
    goldman_max_date = Date[which.max(Close_goldman)],
    goldman_min = min(Close_goldman),
    goldman_min_date = Date[which.min(Close_goldman)],
    spy_max = max(Close_spy),
    spy_max_date = Date[which.max(Close_spy)],
    spy_min = min(Close_spy),
    spy_min_date = Date[which.min(Close_spy)]
  ) %>%
  select(year, jp_max, jp_max_date, jp_min, jp_min_date, goldman_max, goldman_max_date, goldman_min, goldman_min_date, spy_max, spy_max_date, spy_min, spy_min_date)


```


```{r}

# Creating new dfs to come up with the values for table 1.

# Extract relevant columns
df_jp1 <- df_benchmark[, c("Quarter", "avg_percentage_returns", "Percentage_Returns_jp")]

# Create spread column
df_jp1$spread <- df_jp1$Percentage_Returns_jp - df_jp1$avg_percentage_returns

df_jp1

# Extract relevant columns
df_goldman1 <- df_benchmark[, c("Quarter", "avg_percentage_returns", "Percentage_Returns_goldman")]

# Create spread column
df_goldman1$spread <- df_goldman1$Percentage_Returns_goldman - df_goldman1$avg_percentage_returns

df_goldman1


# Extract relevant columns
df_spy1 <- df_benchmark[, c("Quarter", "avg_percentage_returns", "Percentage_Returns_spy")]

# Create spread column
df_spy1$spread <- df_spy1$Percentage_Returns_spy - df_spy1$avg_percentage_returns

df_spy1

```

```{r}

# Creating percentage returns spread for each quarter to add to the table (table 1)

jpQ12022 <- (sum(df_jp1$spread[df_jp1$Quarter == "2022 Q1"])) / sum(df_jp1$Quarter == '2022 Q1')

jpQ22022 <- (sum(df_jp1$spread[df_jp1$Quarter == "2022 Q2"])) / sum(df_jp1$Quarter == '2022 Q2')

jpQ32022 <- (sum(df_jp1$spread[df_jp1$Quarter == "2022 Q3"])) / sum(df_jp1$Quarter == '2022 Q3')

jpQ42022 <- (sum(df_jp1$spread[df_jp1$Quarter == "2022 Q4"])) / sum(df_jp1$Quarter == '2022 Q4')

jpQ12023 <- (sum(df_jp1$spread[df_jp1$Quarter == "2023 Q1"])) / sum(df_jp1$Quarter == '2023 Q1')



goldmanQ12022 <- (sum(df_goldman1$spread[df_goldman1$Quarter == "2022 Q1"])) / sum(df_goldman1$Quarter == '2022 Q1')

goldmanQ22022 <- (sum(df_goldman1$spread[df_goldman1$Quarter == "2022 Q2"])) / sum(df_goldman1$Quarter == '2022 Q2')

goldmanQ32022 <- (sum(df_goldman1$spread[df_goldman1$Quarter == "2022 Q3"])) / sum(df_goldman1$Quarter == '2022 Q3')

goldmanQ42022 <- (sum(df_goldman1$spread[df_goldman1$Quarter == "2022 Q4"])) / sum(df_goldman1$Quarter == '2022 Q4')

goldmanQ12023 <- (sum(df_goldman1$spread[df_goldman1$Quarter == "2023 Q1"])) / sum(df_goldman1$Quarter == '2023 Q1')


spyQ12022 <- (sum(df_spy1$spread[df_spy1$Quarter == "2022 Q1"])) / sum(df_spy1$Quarter == '2022 Q1')

spyQ22022 <- (sum(df_spy1$spread[df_spy1$Quarter == "2022 Q2"])) / sum(df_spy1$Quarter == '2022 Q2')

spyQ32022 <- (sum(df_spy1$spread[df_spy1$Quarter == "2022 Q3"])) / sum(df_spy1$Quarter == '2022 Q3')

spyQ42022 <- (sum(df_spy1$spread[df_spy1$Quarter == "2022 Q4"])) / sum(df_spy1$Quarter == '2022 Q4')

spyQ12023 <- (sum(df_spy1$spread[df_spy1$Quarter == "2023 Q1"])) / sum(df_spy1$Quarter == '2023 Q1')



jpQ12022 <- paste0(round(jpQ12022 * 100, 2), "%")
jpQ22022 <- paste0(round(jpQ22022 * 100, 2), "%")
jpQ32022 <- paste0(round(jpQ32022 * 100, 2), "%")
jpQ42022 <- paste0(round(jpQ42022 * 100, 2), "%")
jpQ12023 <- paste0(round(jpQ12023 * 100, 2), "%")

goldmanQ12022 <- paste0(round(goldmanQ12022 * 100, 2), "%")
goldmanQ22022 <- paste0(round(goldmanQ22022 * 100, 2), "%")
goldmanQ32022 <- paste0(round(goldmanQ32022 * 100, 2), "%")
goldmanQ42022 <- paste0(round(goldmanQ42022 * 100, 2), "%")
goldmanQ12023 <- paste0(round(goldmanQ12023 * 100, 2), "%")

spyQ12022 <- paste0(round(spyQ12022 * 100, 2), "%")
spyQ22022 <- paste0(round(spyQ22022 * 100, 2), "%")
spyQ32022 <- paste0(round(spyQ32022 * 100, 2), "%")
spyQ42022 <- paste0(round(spyQ42022 * 100, 2), "%")
spyQ12023 <- paste0(round(spyQ12023 * 100, 2), "%")


```

```{r}

#  Creating a column with min and max values for each ETF and corresponding date, but with a different format to make it visually more appealing. (Table 2)

library(dplyr)
library(lubridate)
library(kableExtra)

df_benchmark %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarize(
    jp_max = max(Close_jp),
    jp_max_date = Date[which.max(Close_jp)],
    jp_min = min(Close_jp),
    jp_min_date = Date[which.min(Close_jp)],
    goldman_max = max(Close_goldman),
    goldman_max_date = Date[which.max(Close_goldman)],
    goldman_min = min(Close_goldman),
    goldman_min_date = Date[which.min(Close_goldman)],
    spy_max = max(Close_spy),
    spy_max_date = Date[which.max(Close_spy)],
    spy_min = min(Close_spy),
    spy_min_date = Date[which.min(Close_spy)]
  ) %>%
  select(year, jp_max, jp_max_date, jp_min, jp_min_date, goldman_max, goldman_max_date, goldman_min, goldman_min_date, spy_max, spy_max_date, spy_min, spy_min_date) %>%
  kable(format = "html", caption = "Summary of Maximum and Minimum Close Prices by Year and ETF") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  row_spec(1, bold = TRUE) %>%
  column_spec(2:5, bold = TRUE, border_left = TRUE, color = "#54301a") %>%
  column_spec(6:9, bold = TRUE, border_left = TRUE, color = "#6B96C3") %>%
  column_spec(9:12, bold = TRUE, border_left = TRUE, color = "#425951") %>%
  column_spec(ncol(.)-1, bold = TRUE, border_right = TRUE) %>%
  add_header_above(c(" " = 1, "JP Morgan ETF" = 4, "Goldman Sachs ETF" = 4, "S&P 500 ETF" = 4)) %>%
  add_footnote("Source: Yahoo Finance")
```


```{r}

# Creating a table with quarterly spreads for each ETF(Table 1)

install.packages('knitr')
library(knitr)
library(kableExtra)

# Create a matrix with the values
table_data <- matrix(c(jpQ12022, goldmanQ12022, spyQ12022,
                       jpQ22022, goldmanQ22022, spyQ22022,
                       jpQ32022, goldmanQ32022, spyQ32022,
                       jpQ42022, goldmanQ42022, spyQ42022,
                       jpQ12023, goldmanQ12023, spyQ12023),
                     nrow = 3, byrow = TRUE)

# Add row and column names
colnames(table_data) <- c('2022 Q1', '2022 Q2', '2022 Q3', '2022 Q4', '2023 Q1')
rownames(table_data) <- c('JP Morgan ETF', 'Goldman Sachs ETF', 'S&P 500 ETF')

# Create the table using kable
kable(table_data, format = "html", caption = "Spread averages by quarter and ETF") %>%
  kable_styling(full_width = FALSE, bootstrap_options = "striped") %>%
  column_spec(1:5, bold = TRUE) %>%
  row_spec(1:3, bold = TRUE) %>%
  add_header_above(c(" " = 1, "2022" = 4, "2023" = 1)) %>%
  row_spec(1:3, extra_css = "border-style: solid; border-width: 1px;")

```

```{r}

# Creating a table with quarterly spreads for each ETF, but with a different format to make it visually more appealing. (Table 1)

# Install and load kableExtra package
install.packages('kableExtra')
library(kableExtra)

library(knitr)
library(kableExtra)

# Create a matrix with the values
table_data <- matrix(c(jpQ12022, goldmanQ12022, spyQ12022,
                       jpQ22022, goldmanQ22022, spyQ22022,
                       jpQ32022, goldmanQ32022, spyQ32022,
                       jpQ42022, goldmanQ42022, spyQ42022,
                       jpQ12023, goldmanQ12023, spyQ12023),
                     nrow = 3, byrow = TRUE)

# Add row and column names
colnames(table_data) <- c('Q1', 'Q2', 'Q3', 'Q4', 'Q1')
rownames(table_data) <- c('JP Morgan ETF', 'Goldman Sachs ETF', 'S&P 500 ETF')

# Create the table using kable and format the cell colors and styles
kable(table_data, format = "html", caption = "Spread averages by quarter and ETF") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  row_spec(1, bold = TRUE, color = "#54301a") %>%
  row_spec(2, bold = TRUE, color = "#6B96C3") %>%
  row_spec(3, bold = TRUE, color = "#425951") %>%
  column_spec(1, border_left = TRUE, border_right = TRUE) %>%
  column_spec(2:ncol(table_data) - 1, border_right = TRUE) %>%
  column_spec(ncol(table_data), border_left = TRUE, border_right = TRUE) %>%
  add_header_above(c(" " = 1, "2022" = 4, "2023" = 1)) %>%
  add_footnote("Source: yahoo Finance")
  
  
  
