
##################### Final Project ############################

getwd()
setwd("Documents/Boston/")
getwd()
df = read.csv("AB_NYC_2019.csv")
head(df)
colnames(df)
na.cols = c()
# This is to check weather we have any columns with NA
for (i in colnames(df)) {
  if(any(is.na(df[i]))){
    na.cols=c(i)
    }
}
na.cols
# It gives one col that has NA's in it and it is reviews_per_month, which we can compute it 
#   from reviews by dividing with 12
df$reviews_per_month = df$number_of_reviews/12
any(is.na(df$reviews_per_month))
# Now it has no NA's

# Hence we filtered our data with NA's
# Now by looking at data we can filter columns so lets make data set just for columns we need
col2keep = c("name","host_id","host_name","neighbourhood_group","neighbourhood",
              "latitude","longitude","room_type","price","minimum_nights",
                "number_of_reviews","reviews_per_month")
df = df[,col2keep]
head(df)


### Descreptive analysis 
# We will analyze the nighbourhood group as our categorieal varible
x = table(df$neighbourhood_group)
x

# Lets see their respective percentage and as seen we expect people use Airbnb more in Manhattan
prop.table(x)

# It looks Brooklyn is not laggig behind with much wheare as Staten Island is out of game
# Let describe with pie chart
y = paste(names(x),paste(round(prop.table(x)*100,2),"%",sep = ""))
pie(x, main = "NYC Borough Airbnb Rentals Percentage Coverage",labels = y, col=c("Orange","Yellow","Blue","Green","Red"))


# SO let also see what are most types of properties are in Airbnb in respective neighbour hood group
y = table(df$room_type,df$neighbourhood_group)
y.prop = prop.table(y)
addmargins(y)
addmargins(y.prop)*100
barplot(y, xlab = "Neighbourhood Group", beside = TRUE, legend.text = TRUE,ylim = c(0,15000),
        main = "Room Type Over Neighbourhood Group",col=c("red", "green", "blue"))

# As we knew Manhatten would have more Airbnb rentals, where as bronx and Staten Island doesn't
# have good enough amount of rentals.
# As expected from the idea of Airbnb people will rent rooms more than a whole apt/house which is
# completely seen in our data, but I am surprised to see different to that in Manhatten. 
# Now we would have thought since Manhattan is so expensive people would rent private room or
# shared room but from our data points which is completely different than our expectations. 
# Although now we know that if someone is going to rent on Airbnb in queens they are likely to go for 
# privet rooom and entire apt/house in Manhattan, we can't say anything sure about Brooklyn.
# However second choice for NYC Airbnb booking will be Brooklyn over Queens for Manhatten




par(mfrow=c(2,1))
# Lets see how would box plot look for the prices per night in nyc


f = fivenum(df$price)
IQR = f[4]-f[2]
IQR
summary(df$price)
# So Mean is 152.7 and Median in 106 and Inter Quaraltile Range is 106

# No of 0's
sum(df$price==0)

boxplot(df$price, horizontal = TRUE, main = "Box Plot for Price in Airbnb dataset",xaxt = "n")
axis(side = 1, at = seq(0,max(df$price)+10,25), labels = TRUE,  las=2)
abline(v=1500,col="RED")

# As it seems there way more outlier in our data and there are many but since there many points
#  together below 1500 and it's darker line till 1500 so we will still considered them valid 

# Also let's see how would box plot look without over 1500
price_below_1500 = subset(df, df$price <= 1500)
boxplot(price_below_1500$price, horizontal = TRUE, xaxt = "n", main = "Price Analysis Box Plot of Airbnb ad post below 1500")
axis(side = 1, at = fivenum(price_below_1500$price), labels = TRUE,  las=2)

#   Over 1500 we need to study more, why are they so high, is the data set wrong?
df_price_1500 = subset(df,df$price>1500)
nrow(df_price_1500)
table(df_price_1500$neighbourhood_group) 
# As it's quite often that manhatten and brooklyn can cost more beacuase of manhatten parties
#   However, staten_island, bronx and queens will be intersting to look
# let's see why they cost more
df_price_1500[df_price_1500$neighbourhood_group %in% c("Bronx","Queens","Staten Island"),]

# As it seems they are entire home/apt we can expect someone will pay more than 1500 if it's 
#  some important occation but it's still highly doubtable about shared room or private room.
# Let also look on top 10 expensive stays 
df_price_1500[order(df_price_1500$price,decreasing = TRUE)[1:10],]
# As it seems they are more premium places but it's stll highly doubtable to pay that much. Though
#   it is intersting to learn that people are posting ads with high prices.


# Conclusion
# Looking at all the calculations, I think there is quite missing or incorrect observation 
# in the data. Although R calculation might say they are outliears, we do not have any evidence
# to tell. Since living in NY I know it is not impossible for someone to rent at 3000 or 5000
# in Manhatten. Hence let's consider this data not corrupt. 



par(mfrow = c(1,2))
hist(df$price,probability = TRUE,main= "Histogram of Price per Night\nFor whole data set",
          xlab = " Price/Night",breaks=100,col = "Magenta")
hist(price_below_1500$price,main= "Histogram of Price per Night\nFor data Price below 1500",
          xlab = "Price/Night", ylim= c(0,0.008),probability = TRUE, breaks = 100,col = "Magenta")

mean.price = mean(df$price)
sd.price = sd(df$price) 

summary(df$price)
sd.price
summary(price_below_1500$price)
sd(price_below_1500$price)


# As it is seen the data is more skewed towards right. Hence it is Right skewed distribution
# That's why mean is greather than median. As for calculation purpose it is better to work with
# median here.



#### Central Limit Theorem
par(mfrow =c(2,2))
samples = 10000
for (size in c(500,1000,1500,2000)){
  xbar <- numeric(samples)
    for (i in 1: samples) {
      xbar[i] <- mean(sample(df$price,size))
    }
  hist(xbar,prob = TRUE, col = "Yellow", ylim = c(0,0.08),xlim = c(130,180),
       main = paste("Sample Size",size), xlab = "Avg Price of each Sample", breaks = 20)
  lines(density(xbar))

  cat("Sample Size = ", size, " Mean = ", mean(xbar),
      " SD = ", sd(xbar), "\n")
}

head(df)


#### Sampling Methods
library(sampling)
library(prob)
name_sample = c()
set.seed(123)

# SRSWOR
# Equal Probability
s = srswor(1000,nrow(df))
sample.1 = df[as.logical(s),]
head(sample.1)
name_sample = c("Equal probability: Simple random sampling without replacement")

# Systematic Sampling
set.seed(151)
N = nrow(df)
n = 1000
k = ceiling(N / n)
r <- sample(k, 1)
r
s = seq(r, by = k, length = n)
sample.2 = df[s, ]
tail(sample.2)
# Hence we got some NA we need to change that 

extra = sample((1:nrow(df))[-s], 2)
# Now we can omit NA nd add last two rows
new_s =  sort(c(s[s<= nrow(df)],extra))
sample.2 = df[new_s,]
tail(sample.2)
name_sample = c(name_sample ,"Equal probability: Systematic sampling")


# UPsystematic
# Using the probabilities of reviews to determine our sample
pik = inclusionprobabilities(df$number_of_reviews, 1000)
length(pik)
sum(pik)
s = UPsystematic(pik)
sample.3 = df[as.logical(s),]
tail(sample.3)
name_sample = c(name_sample ,"Unequal probability: UPSystematic sampling")

## Stratified sampling 
set.seed(146)
df <- df[order(df$neighbourhood_group), ]
st.size = 1000*prop.table(table(df$neighbourhood_group))

st = strata(df, stratanames = c("neighbourhood_group"),
              size = st.size, method = "srswor",
              description = TRUE)
head(st)
sample.4 = getdata(df,st)
head(sample.4)
name_sample = c(name_sample ,"Stratified sampling using method srswor")
prop.table(table(sample.4$neighbourhood_group))
prop.table(table(df$neighbourhood_group))
# As we can see that propotion are same

library(stringr)
ans = str_c("Sample Dataset; Original","   Mean = ",round(mean(df$price),3),"   SD = ", sd(df$price),
            "   Min = ",min(df$price),"   Max = ",max(df$price))

par(mfrow =c(2,2))
#1
hist(sample.1$price,prob = TRUE, col = "Light Blue", main = paste(name_sample[1],"Sample Dataset; sample.1",sep = "\n"), 
     ylim = c(0,0.01),xlim= c(0,1000),xlab = "Price", breaks = 200)
lines(density(sample.1$price))
ans = c(ans,str_c("Sample Dataset; sample.1", "   Mean = ", round(mean(sample.1$price),3),  "   SD = ", sd(sample.1$price),
                  "   Min = ",min(sample.1$price),"   Max = ",max(sample.1$price)))

#2
hist(sample.2$price,prob = TRUE, col = "green", main = paste(name_sample[2],"Sample Dataset; sample.2",sep = "\n"), 
     ylim = c(0,0.01),xlim= c(0,1000),xlab = "Price", breaks = 200)
lines(density(sample.2$price))
ans = c(ans , str_c("Sample Dataset; sample.2", "   Mean = ", round(mean(sample.2$price),3),  "   SD = ", sd(sample.2$price),
                    "   Min = ",min(sample.2$price),"   Max = ",max(sample.2$price)))

#3
hist(sample.3$price,prob = TRUE, col = "Red", main = paste(name_sample[3],"Sample Dataset; sample.3",sep = "\n"), 
     ylim = c(0,0.01),xlim= c(0,1000),xlab = "Price", breaks = 200)
lines(density(sample.3$price))
ans = c(ans , str_c("Sample Dataset; sample.3", "   Mean = ", round(mean(sample.3$price),3),  "   SD = ", sd(sample.3$price),
                    "   Min = ",min(sample.3$price),"   Max = ",max(sample.3$price)))

#4
hist(sample.4$price,prob = TRUE, col = "orange", main = paste(name_sample[4],"Sample Dataset; sample.4",sep = "\n"), 
     ylim = c(0,0.01),xlim= c(0,1000),xlab = "Price", breaks = 200)
lines(density(sample.4$price))
ans = c(ans , str_c("Sample Dataset; sample.4", "   Mean = ", round(mean(sample.4$price),3),  "   SD = ", sd(sample.4$price),
                    "   Min = ",min(sample.4$price),"   Max = ",max(sample.4$price)))

ans[1:5]





########### Extra credit
# Actually it really took me 3 days to figure out this
library(plotly)
library(mvtnorm)
fig <- plot_ly(
  fill = "toself",
  lon = df$longitude,
  lat = df$latitude,
  type = 'scattermapbox',
  text = paste("Price : ",df$price),
  marker = list(size = 2,color = "Light Blue"),
  fillcolor = 'color'
  ) 
fig <- fig %>%
  layout(
    title = "2019 Airbnb Listing's rental precise location with Price",
    mapbox = list(
      style = "stamen-terrain",
      center = list(lon = mean(df$longitude), lat = mean(df$latitude)),
      zoom = 9.5),
    showlegend = TRUE)

fig


# heat map and 2d histogram 
s <- matrix(c(1, -.75, -.75, 1), ncol = 2)
obs <- mvtnorm::rmvnorm(500, sigma = s)
fig <- plot_ly(y = df$latitude, x = df$longitude) %>% 
  layout(title= "Properties Location of Airbnb ad post alongside heatmap in interactive plotly plots")
fig2 <- subplot(
  fig %>% add_markers(alpha = 0.5),
  fig %>% add_histogram2d(colorscale = 'YlOrRd')
)

fig2

