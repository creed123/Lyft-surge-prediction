# %% [code]
library(dplyr)
library('fastDummies')
install.packages("caTools")
library(caTools)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
library(caret)

data = read.csv('/kaggle/input/ops-data/Ops_data.csv')

#Filter out Uber cabs since no surge in Uber
data = data[data$cab_type == 'Lyft', ]

#Group by cab type and find avg distance travelled by each cab type
  overall_grp = data %>% group_by(name, cab_type) %>%
    summarise(avg_dist = median(distance),
              .groups = 'drop')
  View(overall_grp)


#Filter out only those trips which are made on standard price (surge multiplier = 1)
  filter_df = data %>% filter((surge_multiplier == 1))
  filter_df
  grp_df = filter_df %>% group_by(name, cab_type) %>%
    summarise(avg_price = mean(price),
              .groups = 'drop')
  barplot(grp_df$avg_price, names.arg = grp_df$name)

#Take a sample of 100 points for faster visualisation
  sample = sample_n(filter_df, 100)
  plot(sample$distance, sample$price, xlab = 'Distance', ylab = 'Price', main = '100 sample trips at surge = 1 for UBER X')


#Filter out only those trips with surge > 1
  filter_df = data%>% filter(surge_multiplier > 1)
  filter_df
  grp_df = filter_df %>% group_by(hour) %>%
    summarise(count = n())
  grp_df

#Plot of number of trips with surge>1 by hour
  barplot(names.arg = grp_df$hour, grp_df$count)

#Filter out only those trips with surge = 1
  filter_df = data%>% filter(surge_multiplier > 1 & cab_type == 'Lyft')
  filter_df
  grp_prob_df = filter_df %>% group_by(hour) %>%
    summarise(count = n())
  grp_prob_df$prob = grp_df$count/(grp_df$count+grp_prob_df$count)
  barplot(names.arg = grp_prob_df$hour, grp_prob_df$prob, xlab = 'hour', ylab = 'Probability of surge', main = 'Probability of surge by hour')

#Take a sample of 100 points for faster visualisation
  sample = sample_n(filter_df, 100)
  plot(sample$distance, sample$price, xlab = 'Distance', ylab = 'Price', main = '100 sample trips at surge = 1 for UBER X')



#New column surge indicator when there is a surge in trip price
  data$surge_indicator = data$surge_multiplier > 1
  summary(data$surge_indicator)


#Time series analysis. Trying to understand probability of surge on an hourly basis.
  filter_df = data[data$cab_type == 'Lyft', ] %>% group_by(month, day, hour) %>%
        summarise(prob_surge = sum(surge_indicator)/n(),
                                   .groups = 'drop')

#Ordering the probability in decreasing order of hour
  filter_df= filter_df[order(filter_df$prob_surge, decreasing = TRUE),]


# Convert distances into quantiles
  data$distance_quantile = as.factor(
    ifelse(data$distance >= 2.920, '4Q', 
           ifelse(data$distance < 2.920 & data$distance > 2.189, '3Q', 
                  ifelse(data$distance < 2.189 & data$distance >= 1.280, '2Q', 
                         '1Q'))))

# Surge probability increases as trip distance increases. 4Q distances have the highest surge probability. 
 .surge_trips = data%>%group_by(distance_quantile)%>%summarise(prob_surge = sum(surge_indicator)/n())


#Total trips by source
  total_trips_source = data%>%group_by(source)%>%summarise(n())


#Total trips by source, this will give us how many trips made from source and 
# the count of each trip made from source for each of the distance quantiles
  source_distance_counts = data%>%group_by(source, distance_quantile)%>%summarise(n())

# Trips from source with surge probability by source  
  source_distance_surge_prob = data%>%group_by(source)%>%summarise(prob =sum(surge_indicator)/n())

# Just ordering the trips by source in decreasing order of surge probability
  source_distance_surge_prob = source_distance_surge_prob[order(source_distance_surge_prob$prob), decreasing = TRUE]

# Taking only the top 7 sources by surge probability
  source_distance_surge_prob = source_distance_surge_prob[1:7, ]


# Final approach is to consider the top sources with highest probability of surge as the features,
# distance Quantiles, and apply PCA to group weather related features together.

#create new column to encode top 3 sources with highest probability as a separate categorical column. 
  data$source_indicator = as.factor(
  ifelse(data$source == 'Beacon Hill', 'BeaconHill', 
         ifelse(data$source == 'South Station', 'SouthStation', 
                ifelse(data$source == 'Financial District', 'FinancialDistrict', 
                       'Other'))))

#Converts those categorical variables into columns
  data = dummy_cols(data, select_columns = 'source_indicator')

#scale the data for PCA analysis
  data[, c('wind_source', 'humidity_source', 'temp_source', 'clouds_source', 'pressure_source')] = scale(data[, c('wind_source', 'humidity_source', 'temp_source', 'clouds_source', 'pressure_source')])

#PCA analysis
  pc = prcomp(data[, c('wind_source', 'humidity_source', 'temp_source', 'clouds_source', 'pressure_source')], center = TRUE, scale = TRUE)

  model_df = data.frame(pc$x[, 1:4], data$source ,data$surge_indicator, data$distance_quantile, data$source_indicator_BeaconHill, data$source_indicator_FinancialDistrict, data$source_indicator_BeaconHill, data$X)

  model_df$id = seq.int(nrow(model_df))

  # One way to look at the problem is train different models for each location. Since each location has some inherent characteristics which are hard to determine. Running a separate model for each location helps our model learn those characteristics better. 
  model_df = model_df[model_df$data.source == 'South Station', ]
#Predict based on the results of pca analysis
  set.seed(1234)
  split = sample.split(model_df$id, SplitRatio = 0.5)
  Train = subset(model_df, split==TRUE)
  Test = subset(model_df, split==FALSE)

# Adjusting the cost function to give us a greater penalty when we predict no surge when surge happens. Assuimng greater cost for not being able to predict surge accurately.
  StevensTree = rpart(data.surge_indicator ~ PC1 + PC2 + PC3 + PC4 + data.distance_quantile  + data.source_indicator_BeaconHill + data.source_indicator_FinancialDistrict, method="class", data=Train, parms=list(loss=matrix(c(0,1,20,0), byrow=TRUE, nrow=2)))
  prp(StevensTree)
  StevensPredict = predict(StevensTree, newdata=Test)
  
# Giving us 51% accuracy for predicting surge for Beacon Hill area.
# Baseline model giving us 90% overall accuracy but accuracy on predicting surge up by 50%.  
# Similar 35% accuracy found for South Station area. Able to predict surge by 83% accuracy. 
  table(Test$data.surge_indicator, StevensPredict[,2]>=0.075)
  
# Next steps find an accurate threshold by running a random forest/ XG boost classifier model for each of the top 3 areas by highest probability of surge.
