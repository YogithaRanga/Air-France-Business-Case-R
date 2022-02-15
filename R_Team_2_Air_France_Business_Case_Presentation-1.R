# STEP 1
#-------------------
# Importing the dataset and understanding the data
library(readxl)
library(minpack.lm)
#reading our data
airf <- read_excel("C:/Users/yogit/Downloads/Air France Case Spreadsheet Supplement.xls", sheet =2)
# View the dataset
# View the dataset
#View(airf)
###############################################
#Understanding the Data
###############################################
colnames(airf)

# Assigning columns 12 to 23 to a new dataframe
af <- airf[c(12:23)]
af <- as.data.frame(af) 
# View the dataframe
#View(af)

###############################################
#Massaging the Data
###############################################
# Fill unknown for null values
af["" == af] <- "unknown"

#Change colnames

names(af)[1]  <- "search_engine_bid"
names(af)[2]  <- "clicks"
names(af)[3]  <- "click_charges"
names(af)[4]  <- "avg_cpc"
names(af)[5]  <- "impressions"
names(af)[6]  <- "ctr"
names(af)[7]  <- "avg_pos"
names(af)[8]  <- "tcr"
names(af)[9]  <- "total_costpertrans"
names(af)[10] <- "amount"
names(af)[11] <- "total_cost"
names(af)[12] <- "total_bookings"
# add roas(return on ad spend) to the dataset
# roas = amount - cost of ad/cost of ad col no 13
a <- af$total_cost
b <- af$amount-af$total_cost
af$roas <- (ifelse(a==0,0,b/a))
table(af$roas)


# viewing the updated dataframe
View(af)

###############################################
#Descriptive Statistics
###############################################
#For loop to generate descriptive statistics for all variables in the dataframe
for(i in 1:ncol(af)){
  my_mean <- try(mean(af[,i], na.rm=TRUE))
  my_sd   <- try(sd(af[,i], na.rm=TRUE))
  my_min  <- try(min(af[,i], na.rm=TRUE))
  my_max  <- try(max(af[,i], na.rm=TRUE))
  print(c(my_min, my_mean, my_sd, my_max))
}#closing for loop


###############################################
#Preparing the data for predictive modelling
###############################################
# Feature Engineering
#Normalization UDF
normalize <- function(x) {
  my_min     <- min(x, na.rm=TRUE)
  my_max     <- max(x, na.rm=TRUE)
  normalized <- (x - my_min)/(my_max - my_min)
  return(normalized)
}

# call normalize UDF to normalize variables with different units
af$search_engine_bid_norm   <-normalize(x=af$search_engine_bid)
af$clicks_norm              <-normalize(x=af$clicks)
af$click_charges_norm       <-normalize(x=af$click_charges)
af$avg_cpc_norm             <-normalize(x=af$avg_cpc)
af$impressions_norm         <-normalize(x=af$impressions)
af$ctr_norm                 <-normalize(x=af$ctr)
af$avg_pos_norm             <-normalize(x=af$avg_pos)
af$tcr_norm                 <-normalize(x=af$tcr)
af$total_costpertrans_norm  <-normalize(x=af$total_costpertrans)
af$amount_norm              <-normalize(x=af$amount)
af$total_cost_norm          <-normalize(x=af$total_cost)
af$total_bookings_norm      <-normalize(x=af$total_bookings)
af$roas_norm                <-normalize(x=af$roas)

###############################################################################
#Predictive Modelling with Non-Linear Least Squares Levenberg-Marquardt algorithm
###############################################################################
# check correlation
cor.test(af$roas,af$search_engine_bid)
cor.test(af$roas,af$clicks) 
cor.test(af$roas,af$click_charges)
cor.test(af$roas,af$avg_cpc)
cor.test(af$roas,af$impressions)
cor.test(af$roas,af$ctr)
cor.test(af$roas,af$avg_pos)
cor.test(af$roas,af$tcr)
cor.test(af$roas,af$total_costpertrans)
cor.test(af$roas,af$amount)
cor.test(af$roas,af$total_cost)
cor.test(af$roas,af$total_bookings)


#nlsLM
#Defining the variables for our nlsLM
var1 <- af$search_engine_bid_norm   #x
var2 <- af$clicks_norm              #y - behavioral
var3 <- af$click_charges_norm       #z - behavioral
var4 <- af$avg_cpc_norm             #w
var5 <- af$impressions_norm         #t - behavioral
var6 <- af$ctr_norm                 #f - behavioral
var7 <- af$avg_pos_norm             #g - behavioral
var8 <- af$tcr_norm                 #h - behavioral
var9 <- af$amount_norm              #j 
var10<- af$total_cost_norm          #k
var11<- af$total_bookings_norm      #l - behavioral
var12<- af$roas_norm                #m


nls_data <- af[14:26]
View(nls_data)
my_function_1 <- function(x,y,z,w,t,f,g,h,j,k,l,m){
  p <- (x*var1) + (y*var2) + (z*var3) + (w*var4) +
       (t*var5) + (f*var6) + (g*var7) + (h*var8) + 
       (j*var9) + (k*var10) + (l*var11) + (m*var12)
  return(c(mean(p),median(p)))
}#closing my func

#what is p: unitless combination of social impact(customer behavior) and(+) financials
#Defining the lower, upper and actuals vectors for 3 different nlsLM models

lower_1   <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
upper_1   <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
actuals_1 <- c(0.5,0.5)

#nlsLM
nlsLM(actuals_1 ~ my_function_1(x,y,z,w,t,f,g,h,j,k,l,m),
      lower=lower_1,
      upper = upper_1,
      start = list(x = 0, y = 0, z = 0, w = 0,
                   t = 0, f = 0, g = 0, h = 0,
                   j = 0, k = 0, l = 0, m = 0
      ))
warnings()

###############################################
#Data Visualization
###############################################
# Text Analysis
#install.packages("qdap",dependencies = TRUE)
#install.packages("tm")
#install.packages(rJava)
#install.packages("RColorBrewer")
library(RColorBrewer)
library(rJava)
library(qdap)
library(tm)
#Creating dataset with top 10 words in keywords
wfreq <- data.frame(freq_terms(text.var=words, top = 80, at.least = 3, stopwords =NULL))
wvec <- c()
#For loop for determining average click trough % for each keyword
for (val in 1:nrow(wfreq)){
  wvec <- grep(pattern=wfreq$WORD[val],x=airf$Keyword,value = FALSE)
  wfreq$avg_ctr[val] <- round(mean(airf$`Engine Click Thru %`[wvec]),2)
}#end of for loop
#Bubble chart
ggplot(data=wfreq[1:10,], aes(x=WORD, y=FREQ, size = avg_ctr, col=avg_ctr)) +
  geom_point(alpha=10) +
  ylim(300,800) +
  scale_size_continuous(range = c(5, 15))


# word cloud
#install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(data = wfreq[,1:2], minSize = 7.5, color=brewer.pal(8, "Pastel1"), backgroundColor = "dimgrey")

View(af)
# CHART 1 - SALES PER PUBLISHER
# assigning publisher names to the new dataframe
af[,27] <- airf[,2]
# rename
names(af)[27]<- "publisher_name"

# getting unique publisher names
unique_publisher_name <- unique(af$publisher_name)
print(unique_publisher_name)

# summing the sales for each publisher
sales_per_publisher <- c()
publisher_names <- c()
totalcost_per_publisher <- c()
for (val in 1:length(unique_publisher_name)) {
  totalcost_per_publisher <- c(totalcost_per_publisher,sum(af$total_cost[which(af[,27] == unique_publisher_name[val])]))
  sales_per_publisher <- c(sales_per_publisher,sum(af$amount[which(af[,27] == unique_publisher_name[val])]))
  publisher_names <- c(publisher_names,unique_publisher_name[val])
}
# Appending Kayak info to the vector
publisher_names <- append(publisher_names, "Kayak", 8)
sales_per_publisher <- append(sales_per_publisher,233694.0, 8)

print(publisher_names)
print(sales_per_publisher)


library(plotly)
# plotting graph for sales per publisher using plotly
pub_graph <- publisher_names
sales_graph<- sales_per_publisher
fig1 <- plot_ly(
  x = reorder(pub_graph, +sales_graph),
  y = sales_graph,
  name = "Publisher Sales",
  type = "bar",
  marker = list(color = c('rgba(49,130,189,0.4)', 'rgba(204,204,204,1)',
                          'rgba(49,130,189,0.5)', 'rgba(204,204,204,1)',
                          'rgba(49,130,189,0.6)','rgba(204,204,204,1)',
                          'rgba(204,204,204,1)','rgba(204,204,204,1)'))
)
# Adding title to the chart
fig1 <- fig1 %>% layout(title = "Sales per publisher",
                        xaxis = list(title = "Publishers",gridcolor = 'ffff'),
                        yaxis = list(title = "Sales",gridcolor = 'ffff'))

fig1

# CHART 2 - ROAS PER PUBLISHER
# calculating ROAS and total cost for each publisher
roas_per_publisher <- c()
for (val in 1:length(unique_publisher_name)) {
  roas_per_publisher <- c(roas_per_publisher,sum(af$roas[which(af[,27] == unique_publisher_name[val])]))
}

# Append Kayak info
roas_per_publisher <- append(roas_per_publisher,64.5132, 8)
totalcost_per_publisher <- append(totalcost_per_publisher,3567.16, 8)
print(publisher_names)
print(roas_per_publisher)
print(totalcost_per_publisher)

# Plot graph for ROAS vs Total cost for each publisher
pub_graph <- publisher_names
roas_graph<- roas_per_publisher
fig2 <- plot_ly(
  y = reorder(pub_graph, +roas_graph),
  x = roas_graph,
  name = 'Publisher Return on Ad Spent',
  type = "bar",
  marker = list(color = totalcost_per_publisher),
  orientation = 'h'
)
# Adding title for the chart
fig2 <- fig2 %>% layout(title = "ROAS vs. Total Cost ",
                        yaxis = list(title = "Publishers",gridcolor = 'ffff'),
                        xaxis = list(title = "Return on Ad spent",gridcolor = 'ffff'))
fig2






