## LSE Data Analytics Online Career Accelerator 

# Turtle Games.

###############################################################################

## As a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - what is the impact on sales per product?
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis).
## - if there is any possible relationship(s) in sales between North America,
##   Europe, and global sales.

################################################################################

# What is the impact on sales per product?


# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Import library needed - Tidyverse.
library(tidyverse)

# Import  the data set sales.
# If you are in the working directory, use this line.
sales <- read.csv("turtle_sales.csv", header = TRUE)
# If you have to browse for the file, use this line.
sales <- read.csv(file.choose(), header=TRUE)

# View the data frame sales and sense check the data.
head(sales)
as_tibble(sales)
str(sales)
dim(sales)
## Notes: 352 observations and 9 variables dataframe. Check Product for data type.

# Check for missing values in the data frame.
sum(is.na(sales))

# Look for missing values column.
sum(is.na(sales$Ranking))
sum(is.na(sales$Product))
sum(is.na(sales$Platform))
sum(is.na(sales$Year))

# Replace NA value in Year column with Median value.
sales$Year[is.na(sales$Year)] <- median(sales$Year, na.rm=TRUE)

# Check again for missing values.
sum(is.na(sales$Year))

# Convert Product column to factor (categorical value). 
sales <- mutate(sales, Product = as.factor(Product))

# Check the descriptive statistics.
summary(sales)

# View the dataframe.
View(sales)

# Create a new data frame (sales1) from the sales data frame.
# Remove unnecessary columns.
sales1 <- subset(sales, select = -c(Ranking, Year, Genre, Publisher))

# View the data frame.
View(sales1)
dim(sales1)
## Notes: 352 observations and 5 variables left.

# Check for NA values in the data frame.
sum(is.na(sales1))

# View the descriptive statistics for sales1.
summary(sales1)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots:
# North America Sales.
qplot(y=NA_Sales, data=sales1,  ylab = "NA Sales in millions of GBP")

# NA Sales by platform.
ggplot(sales1, aes(x=Platform, y=NA_Sales, col=Platform)) +
  geom_point() +
  labs(y="North America Sales in milliones of GBP",
       x="Platform",
       title="North America Sales by Platform", 
       caption="Source: Turtle Games")

# Europe Sales scatterplot.
qplot(y=EU_Sales, data=sales1, ylab = "Europe Sales in millions of GBP")

# EU Sales by platform.
ggplot(sales1, aes(x=Platform, y=EU_Sales, col=Platform)) + 
  geom_point() +
  labs(y="Europe Sales in milliones of GBP",
       x="Platform",
       title="Europe Sales by Platform", 
       caption="Source: Turtle Games")

# Global Sales scatterplot.
qplot(y=Global_Sales, data = sales1, ylab = "Global Sales in millions of GBP")

# Global Sales by platform.
ggplot(sales1, aes(x=Platform, y=Global_Sales, col=Platform)) + 
  geom_point() +
  labs(y="Global Sales in milliones of GBP",
       x="Platform",
       title="Global Sales by Platform", 
       caption="Source: Turtle Games")


## 2b) Histograms.
# Create histograms:
# North America Sales.
qplot(NA_Sales, data = sales1, colour=I('blue'), xlab = "NA Sales in millions of GBP")

# North America Sales histogram with density.
ggplot(sales1, aes(x=NA_Sales)) + 
  geom_histogram(bins = 20, aes(y=..density..), color="blue", fill="white") +
  geom_density(alpha=.2, fill="blue") +
  labs(x="North America Sales in milliones of GBP",
       y="Density",
       title="North America Sales Distribution", 
       caption="Source: Turtle Games")


# Europe Sales histogram.
qplot(EU_Sales, data = sales1, colour=I('red'), xlab = "Europe Sales in millions of GBP")

# Europe Sales histogram with density.
ggplot(sales1, aes(x=EU_Sales)) + 
  geom_histogram(bins = 20, aes(y=..density..), color="red", fill="white") +
  geom_density(alpha=.2, fill="red") +
  labs(x="Europe Sales in milliones of GBP",
       y="Density",
       title="Europe Sales Distribution", 
       caption="Source: Turtle Games")

# Global Sales histogram.
qplot(Global_Sales, data = sales1, colour=I('green'), xlab = "Global Sales in millions of GBP")

# Global Sales histogram with density.
ggplot(sales1, aes(x=Global_Sales)) + 
  geom_histogram(bins = 20, aes(y=..density..), color="green", fill="white") +
  geom_density(alpha=.2, fill="green") +
  labs(x="Global Sales in milliones of GBP",
       y="Density",
       title="Global Sales Distribution", 
       caption="Source: Turtle Games")

## 2c) Boxplots
# Create boxplots.
# Quick boxplot to compare NA and Europe Sales.
boxplot(sales1$NA_Sales, sales1$EU_Sales,   
        main = "NA and EU Sales",
        names = c("NA Sales", "EU Sales"),
  ylab = "Sales in millions of GBP")

# North America Sales by Platform boxplot.
qplot(Platform, NA_Sales, data=sales1, geom='boxplot', fill = Platform,
      ylab = "North America Sales in millions of GBP")

# North America Sales boxplot.
ggplot(sales1, aes(y=NA_Sales)) + 
  geom_boxplot(fill = 'blue', notch = TRUE, outlier.color = 'black') +
  scale_x_discrete( ) +
  ylim(c(0, 40)) +
  theme_minimal() +
  labs(title = "North America Sales",
       y = "NA Sales in millions of GBP",
       caption = "source: Turtle Games") +
  coord_flip()

# Europe Sales by Platform boxplot.
qplot(Platform, EU_Sales, data=sales1, geom='boxplot', fill = Platform,
      ylab = "Europe Sales in millions of GBP")

# Europe Sales boxplot.
ggplot(sales1, aes(y=EU_Sales)) + 
  geom_boxplot(fill = 'red', notch = TRUE, outlier.color = 'black') +
  scale_x_discrete( ) +
  ylim(c(0, 25)) +
  theme_minimal() +
  labs(title = "Europe Sales",
       y = "EU Sales in millions of GBP",
       caption = "source: Turtle Games") +
  coord_flip()

# Global Sales by Platform.
qplot(Platform, Global_Sales, data=sales1, geom='boxplot', fill = Platform, 
      ylab = "Global Sales in millions of GBP")

# Global Sales boxplot.
ggplot(sales1, aes(y=Global_Sales)) + 
  geom_boxplot(fill = 'green', notch = TRUE, outlier.color = 'black') +
  scale_x_discrete( ) +
  ylim(c(0, 70)) +
  theme_minimal() +
  labs(title = "Global Sales",
       y = "Global Sales in millions of GBP",
       caption = "source: Turtle Games") +
  coord_flip()
###############################################################################

# 3. Save the file with the data frame used in your wd.
write_csv(sales1, file='sales_updated.csv')
###############################################################################

# 4. Observations and insights
## Initial dataset dimension is 352 X 9. After deleting not needed columns, no missing
## data found. Size is 352 X 5.
## Scatterplot observations: most of sales i both NA and Europe are concentrated 
## in the region between 0-5 mil. and 0-2.5 mil. respectively. There are clear outliers 
## in both regions with sales much higher than average. After investigating by 
## Platform, in NA, Europe and globally we can see X360, Wii, PS3. The outlier
## game is on Wii platform,there are also on NES and GB stands out.
##  Histogram shows the right-skewed distribution of sales, later to be checked
## with QQ plot. If confirms, might need to use log of Sales data. Boxplot shows
## average sales for NA below 5mil with lot's of outliers, confirming
## not normal distribution. Europe has similar picture: scatterplot shows
## most of the sales in the region below 5 mil. Looking by Platform, we can 
## highlight Wii, X360, PS3 with clear outliers visible. Histogram confirms
## most probably not normal, right_skewed distribution. Boxplot for EU shows average
## sales below 2.5 mil. and lots of outliers. Comparing NA ans EU boxplots,
## we can say that NA has larger ouliers and greater variability. Global Sales 
## shows the same tendency, as it is a sum of NA, EU and the rest markets.

###############################################################################
###############################################################################


# The reliability of the data (e.g. normal distribution, Skewness, Kurtosis).

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include insights and observations.

################################################################################

# 1. Load and explore the data.
# If you are in the working directory, use this line.
#sales1 <- read.csv("sales_updated.csv", header = TRUE)
# If you have to browse for the file, use this line.
#sales1 <- read.csv(file.choose(), header=TRUE)

# View data frame created before.
head(sales1)
str(sales1)

# Check output: Determine the min, max, and mean values.
# For NA Sales.
summary(sales1$NA_Sales)
# For EU Sales.
summary(sales1$EU_Sales)
# For Global Sales.
summary(sales1$Global_Sales)

# View the descriptive statistics.
summary(sales1)
###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
product_sales <- sales1 %>% group_by(Product)%>%
                            summarise(NA_ProductSales = sum(NA_Sales),
                            EU_ProductSales = sum(EU_Sales),
                            Global_ProductSales = sum(Global_Sales),
                            .groups = 'drop')

# View the data frame.
head(product_sales)

# Explore the data frame.
dim(product_sales)
as_tibble(product_sales)
summary(product_sales)
## Notes: product sales has 175 observations and 4 variables.

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
# NA Sales by Product.
ggplot(product_sales, aes(x=Product, y=NA_ProductSales)) +
  geom_point() +
 scale_x_discrete(guide = guide_axis(angle = 90)) +
 theme(axis.text.x = element_text(size=5)) +
  labs(y="North America Sales in milliones of GBP",
       x="Product",
       title="North America Sales by Product", 
       caption="Source: Turtle Games")

# EU Sales by Product.
ggplot(product_sales, aes(x=Product, y=EU_ProductSales)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(axis.text.x = element_text(size=5)) +
  labs(y="Europe Sales in milliones of GBP",
       x="Product",
       title="Europe Sales by Product", 
       caption="Source: Turtle Games")

# Global Sales by Product.
ggplot(product_sales, aes(x=Product, y=Global_ProductSales)) +
  geom_point() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(axis.text.x = element_text(size=5)) +
  labs(y="Global Sales in milliones of GBP",
       x="Product",
       title="Global Sales by Product", 
       caption="Source: Turtle Games")

# Create histograms.
# NA Sales by Product distribution.
ggplot(product_sales, aes(x=NA_ProductSales)) +
  geom_histogram(binwidth=1, color="blue", fill="white") +
  labs(y="Frequency",
       x="North America Sales by product in milliones of GBP",
       title="North America Sales by Product Distribution", 
       caption="Source: Turtle Games")

# EU Sales by Product distribution.
ggplot(product_sales, aes(x=EU_ProductSales)) +
  geom_histogram(binwidth=1, color="red", fill="white") +
  labs(y="Frequency",
       x="Europe Sales by product in milliones of GBP",
       title="Europe Sales by Product Distribution", 
       caption="Source: Turtle Games")

# Global Sales by Product distribution.
ggplot(product_sales, aes(x=Global_ProductSales)) +
  geom_histogram(binwidth=1, color="green", fill="white") +
  labs(y="Frequency",
       x="Global Sales by product in milliones of GBP",
       title="Global Sales by Product Distribution", 
       caption="Source: Turtle Games")

# Create boxplots.
# NA Sales by Product.
ggplot(product_sales, aes(y=NA_ProductSales)) + 
  geom_boxplot(fill = 'blue', notch = TRUE, outlier.color = 'black') +
  scale_x_discrete( ) +
  ylim(c(0, 40)) +
  theme_minimal() +
  labs(title = "North America Sales by Product",
       y = "NA Sales per Product in millions of GBP",
       caption = "source: Turtle Games") +
  coord_flip()

# Europe Sales by Product.
ggplot(product_sales, aes(y=EU_ProductSales)) + 
  geom_boxplot(fill = 'red', notch = TRUE, outlier.color = 'black') +
  scale_x_discrete( ) +
  ylim(c(0, 40)) +
  theme_minimal() +
  labs(title = "Europe Sales by Product",
       y = "Europe Sales per Product in millions of GBP",
       caption = "source: Turtle Games") +
  coord_flip()

# Global Sales by Product.
ggplot(product_sales, aes(y=Global_ProductSales)) + 
  geom_boxplot(fill = 'green', notch = TRUE, outlier.color = 'black') +
  scale_x_discrete( ) +
  ylim(c(0, 70)) +
  theme_minimal() +
  labs(title = "Global Sales by Product",
       y = "Global Sales per Product in millions of GBP",
       caption = "source: Turtle Games") +
  coord_flip()
###############################################################################

# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots.
# NA Sales by Product.
qqnorm(product_sales$NA_ProductSales, col = 'blue')
qqline(product_sales$NA_ProductSales, col='black')

# Europe Saeles by Product.
qqnorm(product_sales$EU_ProductSales, col = 'red')
qqline(product_sales$EU_ProductSales, col='black')

# Global Sales by Product.
qqnorm(product_sales$Global_ProductSales, col = 'green')
qqline(product_sales$Global_ProductSales, col='black')

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.
# NA Sales by Product
shapiro.test((product_sales$NA_ProductSales))
# As p-value is <0.05, and we can conclude that the sample data is not 
# normally distribution.

# Europe Sales by Product.
shapiro.test((product_sales$EU_ProductSales))
# As p-value is <0.05, and we can conclude that the sample data is not 
# normally distribution.

# Global Sales by Product.
shapiro.test((product_sales$Global_ProductSales))
# As p-value is <0.05, and we can conclude that the sample data is not 
# normally distribution.

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
# NA Sales by Product.
skewness(product_sales$NA_ProductSales)
kurtosis(product_sales$NA_ProductSales)

# Europe Sales by Product.
skewness(product_sales$EU_ProductSales)
kurtosis(product_sales$EU_ProductSales)

# Global Sales by Product.
skewness(product_sales$Global_ProductSales)
kurtosis(product_sales$Global_ProductSales)

## 3d) Determine correlation
# Determine correlation.
cor(product_sales$NA_ProductSales, product_sales$EU_ProductSales)
cor(product_sales$NA_ProductSales, product_sales$Global_ProductSales)
cor(product_sales$EU_ProductSales, product_sales$Global_ProductSales)
###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# NA and Europe Sales by product scatterplot.
# Which games sales are better in NA and EU?
ggplot(product_sales, aes(x=EU_ProductSales, y=NA_ProductSales, col=Product)) + 
  geom_point() +
  labs(y="NA Sales in milliones of GBP",
       x="EU Sales in milliones of GBP",
       title="NA and EU Sales by Product", 
       caption="Source: Turtle Games") +
  theme_minimal()

# NA market top 10 products by Sales.
NA_Top10 <- product_sales %>% group_by(Product) %>% 
  summarise(NA_ProductSales,
            .groups = 'drop') %>%
  arrange(desc(NA_ProductSales)) %>%
  head(10)
# View the data
NA_Top10

# View NA Top 10 game Platform.
NA10_join <- left_join(NA_Top10,sales1)
NA10_join

# Plot Top 10 NA market products with platform. 
ggplot(NA10_join ,aes(x = reorder(Product, -NA_ProductSales),
                      y = NA_Sales, fill=Platform))+
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(size = 5)) +
  theme(axis.title.y = element_text(size = 8)) +
  theme_minimal() +
  labs(title = "Top 10 Products by NA Sales",
       y = "Sales in millions of GBP",
       x = "Product",
       caption = "source: Turtle Games") +
  theme(legend.position="right")


# Plot top 10 NA games by sale.
ggplot(NA_Top10 ,aes(x = reorder(Product, -NA_ProductSales),
                         y = NA_ProductSales, fill=Product))+
  geom_bar(stat = "identity", fill='#ADD8E6', color="darkblue") +
  theme(axis.text.x = element_text(size = 5)) +
  theme(axis.title.y = element_text(size = 8)) +
  theme_minimal() +
  labs(title = "Top 10 Products by NA Sales",
       y = "Sales in millions of GBP",
       x = "Product",
       caption = "source: Turtle Games") +
  theme(legend.position="none") +
  geom_text(aes(label = NA_ProductSales), size = 4, color = "black",
            fontface = "bold", hjust = 1.5) +
  coord_flip() 

# EU market top 10 products by Sales.
EU_Top10 <- product_sales %>% group_by(Product) %>% 
  summarise(EU_ProductSales,
            .groups = 'drop') %>%
  arrange(desc(EU_ProductSales)) %>%
  head(10)
EU_Top10

# View EU Top 10 fames Platform.
EU10_join <- left_join(EU_Top10,sales1)
EU10_join

# Plot Top 10 EU market products with platform. 
ggplot(EU10_join ,aes(x = reorder(Product, -EU_ProductSales),
                      y = EU_Sales, fill=Platform))+
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(size = 5)) +
  theme(axis.title.y = element_text(size = 8)) +
  theme_minimal() +
  labs(title = "Top 10 Products by EU Sales",
       y = "Sales in millions of GBP",
       x = "Product",
       caption = "source: Turtle Games") +
  theme(legend.position="right")


# Plot top 10 EU games by sale.
ggplot(EU_Top10 ,aes(x = reorder(Product, -EU_ProductSales),
                         y = EU_ProductSales, fill=Product))+
  geom_bar(stat = "identity", fill='#FFB6C1', color='darkred') +
  theme(axis.text.x = element_text(size = 5)) +
  theme(axis.title.y = element_text(size = 8)) +
  theme_minimal() +
  labs(title = "Top 10 Products by Europe Sales",
       y = "Sales in millions of GBP",
       x = "Product",
       caption = "source: Turtle Games") +
  theme(legend.position="none") +
  geom_text(aes(label = EU_ProductSales), size = 4, color = "black",
            fontface = "bold", hjust = 1.5) +
  coord_flip()

# Global market top 10 products by Sales.
Global_Top10 <- product_sales %>% group_by(Product) %>% 
  summarise(Global_ProductSales,
            .groups = 'drop') %>%
  arrange(desc(Global_ProductSales)) %>%
  head(10)
Global_Top10

# View Global Top 10 games Platform.
Global10_join <- left_join(Global_Top10,sales1)
Global10_join

# Plot Top 10 Global market products with platform. 
ggplot(Global10_join ,aes(x = reorder(Product, -Global_ProductSales),
                      y = Global_Sales, fill=Platform))+
  geom_bar(stat = "identity") +
#  scale_fill_brewer(palette="Paired") +
  theme(axis.text.x = element_text(size = 5)) +
  theme(axis.title.y = element_text(size = 8)) +
  theme_minimal() +
  labs(title = "Top 10 Products by Global Sales",
       y = "Sales in millions of GBP",
       x = "Product",
       caption = "source: Turtle Games") +
  theme(legend.position="right")

# Plot top 10 games globally by sales.
ggplot(Global_Top10 ,aes(x = reorder(Product, -Global_ProductSales),
                         y = Global_ProductSales, fill=Product))+
  geom_bar(stat = "identity", fill = '#98FB98', color='darkgreen') +
  theme(axis.text.x = element_text(size = 5)) +
  theme(axis.title.y = element_text(size = 8)) +
  theme_minimal() +
  labs(title = "Top 10 Products by Global Sales",
       y = "Sales in millions of GBP",
       x = "Product",
       caption = "source: Turtle Games") +
  theme(legend.position="none") +
  geom_text(aes(label = Global_ProductSales), size = 4, color = "black",
            fontface = "bold", hjust = 1.5) +
  coord_flip()


# Platforms rating by sales on global market, in NA and Europe.
PlatformSales <- sales %>% group_by(Platform) %>% 
  summarise(GlobalSales_Sum = sum(Global_Sales),
            NASales_Sum = sum(NA_Sales),
            EUSales_Sum = sum(EU_Sales),
            .groups = 'drop') %>% 
  arrange(desc(GlobalSales_Sum)) %>%
  mutate(Global_percent = GlobalSales_Sum/sum(GlobalSales_Sum)*100) %>%
  mutate(NA_percent = NASales_Sum/sum(NASales_Sum)*100) %>%
  mutate(EU_percent = EUSales_Sum/sum(EUSales_Sum)*100)
# View the results.
View(PlatformSales)

#Top 10 of Platforms.
Top10PlatformSales <- PlatformSales[1:10,]
Top10PlatformSales

# Barplot: Top 10 Platforms by Global Sales.
ggplot(Top10PlatformSales,aes(x=reorder(Platform, -GlobalSales_Sum),
                                        y=GlobalSales_Sum, fill=Platform))+
  geom_bar(stat = "identity",fill='#ADD8E6', color="darkblue") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(axis.text.x = element_text(size = 5)) +
  theme(axis.title.y = element_text(size = 8)) +
  theme_minimal() +
  labs(title = "Top 10 Platforms by Global Sales",
       x = "Platform",
       y = "Global Sales in millions of GBP",
       caption = "source: Turtle Games") +
  geom_text(aes(label = GlobalSales_Sum), size = 4, color = "black",
            fontface = "bold", vjust = 1.5) +
  theme(legend.position="none")

# Barplot: Top 10 Platforms by NA Sales.
ggplot(Top10PlatformSales,aes(x=reorder(Platform, -NASales_Sum),
                              y=NASales_Sum, fill=Platform))+
  geom_bar(stat = "identity", fill='#ADD8E6', color="darkblue") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(axis.text.x = element_text(size = 5)) +
  theme(axis.title.y = element_text(size = 8)) +
  theme_minimal() +
  labs(title = "Top 10 Platforms by NA Sales",
       x = "Platform",
       y = "NA Sales in millions of GBP",
       caption = "source: Turtle Games") +
  geom_text(aes(label = NASales_Sum), size = 4, color = "black",
            fontface = "bold", vjust = 1.5) +
  theme(legend.position="none")

# Barplot: Top 10 Platforms by EU Sales.
ggplot(Top10PlatformSales,aes(x=reorder(Platform, -EUSales_Sum),
                              y=EUSales_Sum, fill=Platform))+
  geom_bar(stat = "identity", fill='#ADD8E6', color="darkblue") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(axis.text.x = element_text(size = 5)) +
  theme(axis.title.y = element_text(size = 8)) +
  theme_minimal() +
  labs(title = "Top 10 Platforms by EU Sales",
       x = "Platform",
       y = "EU Sales in millions of GBP",
       caption = "source: Turtle Games") +
  geom_text(aes(label = EUSales_Sum), size = 4, color = "black",
            fontface = "bold", vjust = 1.5) +
  theme(legend.position="none")

# Publishers rating by sales on global market.
PublisherSales <- sales %>% group_by(Publisher) %>% 
  summarise(GlobalSales_Sum = sum(Global_Sales),
            NASales_Sum = sum(NA_Sales),
            EUSales_Sum = sum(EU_Sales),
           .groups = 'drop') %>% 
  arrange(desc(GlobalSales_Sum))
head(PublisherSales)

#Top 10 of Publishers.
Top10PublisherSales <- PublisherSales[1:10,]
Top10PublisherSales

# Barplot: Top 10 publishers by Global Sales.
ggplot(Top10PublisherSales,aes(x = reorder(Publisher, -GlobalSales_Sum),
                               y = GlobalSales_Sum, fill=Publisher))+
  geom_bar(stat = "identity") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(axis.text.x = element_text(size = 5)) +
  theme(axis.title.x = element_text(size = 5)) +
  theme_minimal() +
  labs(title = "Top 10 Publishers by Global Sales",
       x = "Publisher",
       y = "Global Sales in millions of GBP",
       caption = "source: Turtle Games") +
  geom_text(aes(label = GlobalSales_Sum), size = 4, color = "black",
            fontface = "bold", vjust = -0.5) +
  theme(legend.position="none")

# Barplot: Top 10 publishers by NA Sales.
ggplot(Top10PublisherSales,aes(x = reorder(Publisher, -NASales_Sum),
                               y = NASales_Sum, fill=Publisher))+
  geom_bar(stat = "identity") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(axis.text.x = element_text(size = 5)) +
  theme(axis.title.x = element_text(size = 5)) +
  theme_minimal() +
  labs(title = "Top 10 Publishers by NA Sales",
       x = "Publisher",
       y = "NA Sales in millions of GBP",
       caption = "source: Turtle Games") +
  geom_text(aes(label = NASales_Sum), size = 4, color = "black",
            fontface = "bold", vjust = -0.5) +
  theme(legend.position="none")

# Barplot: Top 10 publishers by EU Sales.
ggplot(Top10PublisherSales,aes(x = reorder(Publisher, -EUSales_Sum),
                               y = EUSales_Sum, fill=Publisher))+
  geom_bar(stat = "identity") +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme(axis.text.x = element_text(size = 5)) +
  theme(axis.title.x = element_text(size = 5)) +
  theme_minimal() +
  labs(title = "Top 10 Publishers by EU Sales",
       x = "Publisher",
       y = "EU Sales in millions of GBP",
       caption = "source: Turtle Games") +
  geom_text(aes(label = EUSales_Sum), size = 4, color = "black",
            fontface = "bold", vjust = -0.5) +
  theme(legend.position="none")
###############################################################################

# 5. Save the file with the data frame

write_csv(product_sales, file='product_sales.csv')
###############################################################################

# 6. Observations and insights
## Average Global sales are £5.34m. with maximum of £ 67.85m. in one game sales.
##North America has higher average sales compared to Europe £2.52m. 
## European average sales are £1.64m.
## Scatterplots show clear bestsellers. Most revenue by product under 5m. for NA and
## under 2.5m for EU. Histograms: not normal, right skewed distributions, but not 
## too much information. Boxplots: show not normal distribution and outliers with 50%
## of data between 0.4 and 3.12 for NA, 0.4 and 2.16 for EU and Globally 
## between 1.12 and 6.44.
## Based on QQ and Shapiro Wilk: not normal, right skewed, heavy tails distr.
## Positive correlation: GL~NA 0.92, GL~EU 0.85 and NA~EU 0.62
## Scatterplots are nest fit:
## NA and EU = almost 80% of Global sales
## NA: X360, Wii, PS3 (NES higher sales than EU)
## EU: Wii, PS3, X360 (NES and GB not as popular as US. )
## Global: Wii, X360, PS3
## Nintendo - leader. EU:Electronic Art(#2) - FIFA more popular in EU
## Globally: Activision (Call of Duty?).
## Wii 107 = bestseller, NA - NES games 123 and 326 more popular
## EU: 195 Wii, 515(mostly for PSs)

###############################################################################
###############################################################################

##  Looking for relationship(s) in sales between North America,
##  Europe, and global sales.

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Investigate
## any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis, you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explore the data
# If you are in the working directory, use this line.
#product_sales <- read.csv("product_sales.csv", header = TRUE)
# If you have to browse for the file, use this line.
#product_sales <- read.csv(file.choose(), header=TRUE)

# View data frame created in Week 5.
## Explore the data set.
head(product_sales)
str(product_sales)

# Determine a summary of the data frame.
summary(product_sales)

###############################################################################
# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
cor_num_data <- product_sales[, sapply(product_sales, is.numeric)]
cor(cor_num_data)
# Install the psych package.
install.packages('psych')
# Import the psych package.
library(psych)

# Build correlation matrix.
corPlot(cor_num_data, cex=2)

# Plot the relationships between sales.
## NA and EU product sales.
plot(product_sales$NA_ProductSales, product_sales$EU_ProductSales)
## Global and NA product sales.
plot(product_sales$Global_ProductSales, product_sales$NA_ProductSales)
## Global and EU product sales.
plot(product_sales$Global_ProductSales, product_sales$EU_ProductSales)

# Create a linear regression model on the original data.
## NA Product Sales and EU Product Sales: how NA sales can predict EU sales?
NA_EU_model <- lm(EU_ProductSales~NA_ProductSales, data = product_sales)
## Global Product Sales and NA Product Sales: how NA sales can predict Global sales?
GL_NA_model <- lm(Global_ProductSales~NA_ProductSales, data = product_sales)
## Global Product Sales and EU Product Sales: how EU sales can predict Global sales?
GL_EU_model <- lm(Global_ProductSales~EU_ProductSales, data = product_sales)

# View the models.
NA_EU_model
GL_NA_model
GL_EU_model

# View more outputs for the model - the full regression table.
summary(NA_EU_model)
summary(GL_NA_model)
summary(GL_EU_model)

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
# View residuals on a plot for NA_EU model.
plot(NA_EU_model$residuals)
# Plot the relationship NA and EU product sales.
plot(product_sales$NA_ProductSales, product_sales$EU_ProductSales)
# Add line-of-best-fit.
abline(coefficients(NA_EU_model))

# View residuals on a plot for GL_NA model.
plot(GL_NA_model$residuals)
# Plot the relationship Global and NA product sales.
plot(product_sales$NA_ProductSales, product_sales$Global_ProductSales)
# Add line-of-best-fit.
abline(coefficients(GL_NA_model))

# View residuals on a plot for GL_EU model.       
plot(GL_EU_model$residuals)
# Plot the relationship Global and EU product sales.
plot(product_sales$EU_ProductSales, product_sales$Global_ProductSales)
# Add line-of-best-fit.
abline(coefficients(GL_EU_model))
###############################################################################

# 3. Create a multiple linear regression model
# Multiple linear regression model.
# Create a new object, specify the lm function and the variables.
model_mlr <- lm(Global_ProductSales~NA_ProductSales+EU_ProductSales, data=product_sales)
# Print the summary statistics.
summary(model_mlr)

###############################################################################
# 4. Predictions based on given values
# Compare with observed values for a number of records.
# Create a new object and specify the predict function.
model_mlrForecast <- data.frame(NA_ProductSales=c(34.02, 3.93, 2.73, 2.26, 22.08),
                                EU_ProductSales=c(23.80, 1.56, 0.65, 0.97, 0.52))
model_mlrForecast

# Predict Global product sales for given values of NA and EU sales.
predict(model_mlr,
        newdata=model_mlrForecast, interval = 'confidence')

# Find the actual data in the original dataframe.
data <- subset(product_sales, (NA_ProductSales == 34.02 & EU_ProductSales == 23.80) |
                 (NA_ProductSales == 3.93 & EU_ProductSales == 1.56) |
                 (NA_ProductSales == 2.73 & EU_ProductSales == 0.65) |
                 (NA_ProductSales == 2.26 & EU_ProductSales == 0.97) |
                 (NA_ProductSales == 22.08 & EU_ProductSales == 0.52))
# View the results and compare the sales. 
View(data)
###############################################################################
# 5. Try log of sales to check if it improves the prediction:
# Log transformation of sales data.
log_sales <- mutate(product_sales, logGlobal_ProductSales = log(Global_ProductSales),
                       logNA_ProductSales = log(NA_ProductSales),
                       logEU_ProductSales = log(EU_ProductSales))

# View how many Inf(log of 0 sales)
log_sales[is.na(log_sales) | log_sales=="Inf" | log_sales=="-Inf"]
# Replace "-Inf" with 0.
log_sales[log_sales=="-Inf"] = 0

# Create a new object with log data.
log_model_mlr <- lm(logGlobal_ProductSales~logNA_ProductSales+logEU_ProductSales, 
                   data=log_sales)

# Print the summary statistics.
summary(log_model_mlr)

# Predictions based on given values
# Compare with observed values for a number of records.
# Create a new object and specify the predict function.
log_model_mlrForecast <- data.frame(logNA_ProductSales=c(1.53, 0.59, 0.44, 0.35, 1.34),
                                logEU_ProductSales=c(1.38, 0.19, -0.18, -0.013, -0.28))
log_model_mlrForecast

# Predict Log Global product sales for given log values of NA and EU sales
log_predict <- predict(log_model_mlr,
        newdata=log_model_mlrForecast,interval='confidence')

# Find the actual data in the original dataframe.
data <- subset(sales, (NA_Sales == 34.02 & EU_Sales == 23.80) |
                 (NA_Sales == 3.93 & EU_Sales == 1.56) |
                 (NA_Sales == 2.73 & EU_Sales == 0.65) |
                 (NA_Sales == 2.26 & EU_Sales == 0.97) |
                 (NA_Sales == 22.08 & EU_Sales == 0.52))
data <-mutate(data, logGlobal_Sales = log(Global_Sales))
View(data)

###############################################################

# 5. Observations and insights
## 3 Simple regressions used:
## EU~NA:R2 is only 38.56%(NA  Sales explains 38.56% of variability of EU Sales), 
## P value very small and slope is significant. EU product sales
## go us 0.42 when NA changes 1 unit. Best fit line is not ideal. Large number of outliers.
## 
## GL~NA: R2 =83.95%. Statistically significant slope: Global sales go up 1.63
## when NA sale changes 1 unit.
## GL~EU: R2=72.01%. Slope is statistically significant. Global sales go up 
## 2.24 when EU sale changes 1 unit.
## MLR: GL~NA+EU AdjR2= 96.64%, all slopes are statistically significant. 
## Predict values vs actual:
##     fit       lwr       upr      ACTUAL
##  68.056548 66.429787 69.683310   67.85
##  7.356754  7.099418  7.614090   no data found in product_sales
## 4.908353  4.614521  5.202185     4.32
##  4.761039  4.478855  5.043223   No data found in product_sales
## 26.625558 25.367353 27.883763   23.21
## Tried to improve the fit with log of sales, but it gives worse predictions.
## and has lower adjusted R2 as well.
###############################################################################
###############################################################################




