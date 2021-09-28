################### Forcast Ideology #################
library(forecast)
library(timeSeries)

#****************** Make into Time Series Data ********************

House_mean <- aggregate(nominate.dim1~ year + party_code, data = House_Final, mean)

House_Rep <- subset(House_mean, House_mean$party_code %in% c(200))

#Reorder to most recient 
House_Rep <- House_Rep[order(-House_Rep$year),] 

Trainning <- House_Rep[10:nrow(House_Rep),]
Validate <- House_Rep[1:10,]


Trainning$party_code<- NULL
Trainning$year<- NULL

auto.arima(Trainning, seasonal=FALSE)
fit<-auto.arima(Trainning, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')
fcast <- forecast(fit, h=10)
plot(fcast)


#We used this data:
Trainning <- House_Rep[10:nrow(House_Rep),]

ggplot(Trainning, aes(x = year, y = nominate.dim1))+
  stat_smooth(color = "red", se = F) +
  # scale_color_manual("Party", labels= c("Republicans"), values = "coral")) +
  scale_x_continuous("Year", breaks = seq(min(Trainning$year), max(Trainning$year), 10)) +  
  theme_minimal() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5))+
  labs(y = "DW-Nominate Scores", title = "Training Data - Republican Ideology")


#To Predict our Validate Our Model
Validate <- House_Rep[1:10,]

ggplot(Validate, aes(x = year, y = nominate.dim1))+
  stat_smooth(color = "red", se = F) +
  # scale_color_manual("Party", labels= c("Republicans"), values = "coral")) +
  scale_x_continuous("Year", breaks = seq(min(Validate$year), max(Validate$year), 2)) +  
  theme_minimal() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5))+
  labs(y = "DW-Nominate Scores", title = "Validated Data - Republican Ideology")

