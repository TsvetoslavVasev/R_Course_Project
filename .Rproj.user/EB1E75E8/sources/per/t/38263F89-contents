install.packages("UsingR")
install.packages("StatDA")
library(UsingR)
library(StatDA)
library(scales)

#paths and reading of csv ----
#path <- file.path("C:/Users/vikmk/Desktop/fmi shit/statistika/project/googleplaystore.csv")
#reviewPath <- file.path("C:/Users/vikmk/Desktop/fmi shit/statistika/project/googleplaystore_user_reviews.csv")
#read and view
apps = read.csv("googleplaystore.csv", stringsAsFactors = FALSE)
appsReview = read.csv("googleplaystore_user_reviews.csv", stringsAsFactors = FALSE)
View(apps)
View(appsReview)

#Анализ на данните ----
#App - Качествени
#Category - Качествени
#Rating - Количествени и Дискретни
#Reviews - Количествени и Дискретни
#Size - Качествени и Непрекъснати
#Installs - Количествени и Дискретни
#Type - Качествени
#Price - Количествени и Непрекъснати
#Content.Rating - Качествени
#Genres - Качествени
#Last.Updated - Качествени
#Current.ver - Количествени
#Android.ver - Качествени

#почистване на данните и преработка на полетата
options(scipen = 999)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
apps = apps[apps$Size != "Varies with device" & apps$Rating <= 5 & (apps$Type == "Paid" | apps$Type == "Free"), ]
apps = apps[complete.cases(apps),]
for (row in 1:nrow(apps)) {
  price <- apps[row, "Price"]
  
  price <- gsub('^.{1}', '', price)
  if(price == "") {
    apps[row, "Price"] <- 0
  }
  else{
    apps[row, "Price"] <- as.numeric(price)
  }
  
  size <- apps[row, "Size"]
  size <- gsub('.{1}$', '', size)
  apps[row, "Size"] <- size
  
  lastUpdated <- apps[row, "Last.Updated"]
  lastUpdated <- substrRight(lastUpdated, 4)
  apps[row, "Last.Updated"] <- lastUpdated
  
  installs <- apps[row, "Installs"]
  installs <- gsub('.{1}$', '', installs)
  installs <- gsub(',', '', installs)
  apps[row, "Installs"] <- installs
  
}
apps <- transform(apps, Price = as.numeric(Price))
apps <- transform(apps, Size = as.numeric(Size))
apps <- transform(apps, Last.Updated = as.numeric(Last.Updated))
apps <- transform(apps, Installs = as.numeric(Installs))
#Анализ на колоните от дата фрейма apps
#Type - 92.60% - Paid, 7.38 - Paid
cbind(round(prop.table(table(apps$Type))*100, 2))
#Category
cbind(round(prop.table(table(apps$Category))*100, 2))
#Genres
cbind(round(prop.table(table(apps$Genres))*100, 2))
#Content.Rating - 80.38% - Everyone, 11.14% - Teen, ...
cbind(round(prop.table(table(apps$Content.Rating))*100, 2))
#Rating
cbind(round(prop.table(table(apps$Rating))*100, 2))
#Installs
cbind(round(prop.table(table(apps$Installs))*100, 2))
#Android.version
cbind(round(prop.table(table(apps$Android.Ver))*100, 2))

#Summaries
summary(apps$Rating)
summary(apps$Installs)
summary(apps$Type)
summary(apps$Content.Rating)
summary(apps$Last.Updated)

#първите 10 наблюдения
head(apps,10)

#Ще разгледаме типът на приложенията
apps <- apps[is.na(apps$Type)==FALSE, ]
table(apps$Type)
table(apps$Type)["Free"]
table(apps$Type)["Paid"]
barplot(table(apps$Type),
        beside=TRUE,
        col = c("red","green"),
        legend.text = TRUE,
        main = "Type")
#Забелязжаме, че безплатните приложения са значително повече от платените

#Всички приложения с Rating != NA и Sentiment != NA
apps <- apps[is.na(apps$Rating)==FALSE, ]
appsReview <- appsReview[is.na(appsReview$Sentiment)==FALSE,]
appsReview <- appsReview[is.na(appsReview$Sentiment_Polarity)==FALSE, ]
appsReview <- appsReview[is.na(appsReviewHead$Sentiment_Subjectivity)==FALSE, ]

#таблици ----
table(apps$Category)
table(apps$Category)>80
prop.table(table(apps$Category))
pie(prop.table(table(apps$Category)>80))
#Забелязваме че има най-много приложения от категории Dating,Education,Health_And_Fitnes,Entertainment и Finance

sum(apps$Category=="ART_AND_DESIGN",as.numeric(apps$Reviews),na.rm=TRUE)
sum(apps$Category=="SPORTS",as.numeric(apps$Reviews),na.rm=TRUE)
sum(apps$Category=="GAME",as.numeric(apps$Reviews),na.rm=TRUE)
sum(apps$Category=="FAMILY",as.numeric(apps$Reviews),na.rm=TRUE)
sum(apps$Category=="TOOLS",as.numeric(apps$Reviews),na.rm=TRUE)
sum(apps$Category=="FINANCE",as.numeric(apps$Reviews),na.rm=TRUE)

#Ще анализираме връзкате между Rating, Reviews и типа на приложението
ggplot(apps, aes(x=Reviews, y=Rating)) +
  geom_point(aes(col=Type)) +
  labs(title="Android App Ratings vs Number of Reviews", subtitle="Google Playstore Dataset", y="Rating from 1 to 5 stars", x="Number of Reviews") +
  theme_linedraw()
cor(apps$Rating,as.numeric(apps$Reviews))
#Забелязваме, че повечето безплатни приложения имат рейтинг между 4 и 5,както и че има ясно изразена връзка между Reviews и Rating


#Ще разгледаме само платените тъй като базплатните са значително повече и не можем да получим нова информация от тях
Paid<-apps$Price[apps$Price>0]
summary(Paid)
hist(Paid,
     main = "Price of paid apps")
qqnorm(Paid)
qqline(Paid)
shapiro.test(Paid)
t.test(Paid)
#От хистограмата и шапиро теста забелязваме, че платените приложения са с нормално разпределение и, 
#яе 95% от данните за цените на платените приложения 
 #имат доверителен интервал [4.17, 5.80]

#
#piе и честотна таблица за Content.Rating
pie(table(apps$Content.Rating),
    main="Content Rating")
table(apps$Content.Rating)
prop.table(table(apps$Content.Rating))
#Oт графиката виждаме, че рейтинга на съдржанието на повечето приложения е подходящ за всички
hist(apps$Last.Updated,
     main = "Last updated",
     col = "purple")
#Забелязваме, че повечето приложениш са последно обновени през 2018, както и че разпределението е експоненциално
wilcox.test(as.numeric(lastUpdated),conf.int =TRUE)

#най-високо оценено приложение за Dating ----
datingApps=apps[apps$Category == "DATING", ]
head(datingApps[datingApps$Rating == max(datingApps$Rating),],1)

#най-ниско оценено приложение за Dating ----
head(datingApps[datingApps$Rating == min(datingApps$Rating),],1)

#търсим приложение, което го няма и приложение, което го има
datingApps[grepl("Tinder", datingApps$App), ]
datingApps[grepl("Daddy", datingApps$App), ]


#Броят на игрите между 1 и 10 милиона изтегляния ----
#и извадка в графика на техния рейтинг
autoApps <-apps[apps$Category=="AUTO_AND_VEHICLES", ]
autoApps = autoApps[autoApps$Installs >= 1000000 | autoApps$Installs <= 5000000,]
hist(autoApps$Rating, main="Auto & Vehicles Rating", xlab="Rating", ylab="Count",col="purple")
shapiro.test(autoApps$Rating)
t.test(autoApps$Rating)
#Забелязваме, че рейтинга на приложенията от категория Auto & Vehicles имат нормално разпределение и са 
#доверителен интервал [4.13,4.38] 

#Ще разгледаме по-подбробно информацията за рейтинг на п всички риложенията
hist(apps$Rating)
shapiro.test(apps$Rating)
qqnorm(apps$Rating,col = 'pink')
qqline(apps$Rating,col = 'purple')
wilcox.test(apps$Rating,conf.int = TRUE)
#Забелязваме, че 95% от данните за рейтинга на приложенията имат доверител интервал [4.29,4.3]


#Хистограма и графика на плътността на размерите на всички приложения ----
#стойностите са конвертирани от string в numeric
hist(apps$Size, col="purple", xlab="Size of apps", main="Histogram of application sizes" )
plot(density(table(apps$Size)), col="purple")
shapiro.test(apps$Size)
wilcox.test(apps$Size,conf.int = TRUE)
#След проверка забелязваме, че 95% от данните за размера на приложеништа попадат в интервала [17.5,19.9]

#Ще разгледаме данните за инсталации на приложения
hist(apps$Installs,
     main = "Installs",
     col='purple')
shapiro.test(apps$Installs)
wilcox.test(apps$Installs,conf.int = TRUE)
#след тестовете забелязваме, че инсталациите са с екпоненциално разпределение и имат 
 #дожерителен интервал [750000,1000000]


barplot(table(apps$Type,apps$Content.Rating),
        legend.text = TRUE,
        beside=TRUE,
        col =c("green","red","blue","pink"),
        main = "Type and Rating")

#Ще разгледаме връзката между инсталациите и рейтинга
cor(apps$Rating,apps$Installs)
boxplot(apps$Rating~apps$Installs,
        horizontal=TRUE)
##Набллудаваме, че приложенията с по-висок рейтинг имат повече инсталации



#---------------------------------------->DA SE OPRAVI OTTUK 
#сравнение на непрекъсната и дискретна
boxplot(as.numeric(apps$Reviews) ,apps$Rating,
        beside= TRUE,
        notch =TRUE,
        names=c("Reviews","Rating"),
        main="Reviews and Rating")
#nabludavame che medianite ne se otdalechavat ryazko sledovatelno dannite sa simetrichni

boxplot(apps$Installs~apps$Type,
        main="Installs and Type")
#nabludavame che bezplatnite appove imat poveche izteglyania

plot(apps$Last.Updated~apps$Rating)
#dannite pokazvat che appovete s po visok rating sa updete-vani po skoro

pie(table(appsReview$Sentiment),
    main="Sentiment")
prop.table(table(appsReview$Sentiment))
#Zabelyazvame che povecheto revutio sa s polojitelna ocenka

plot(appsReview$Sentiment_Polarity,appsReview$Sentiment_Subjectivity)
plot(density(appsReview$Sentiment_Polarity,appsReview$Sentiment_Subjectivity),
     col = "purple")
cor(appsReview$Sentiment_Polarity,appsReview$Sentiment_Subjectivity)
#nabludavame che ima silno izrazena korelacia mejdu subektivnite i polyarnite review-ta

boxplot(apps$Price,apps$Rating, name="PRICE & RATING", names = c("Price", "Rating"))
#nabludavame che app-ovete s po-niski ceni imat po-visok rating

#-------------------------------------> DO TUK


#linear regression
#Ще изследваме връзката между reviews и броят изтегляния на приложенията
scatter.smooth(x=apps$Reviews, y=apps$Installs, main="Rating ~ Installs",col = 'purple')  # scatterplot
#Графиката показва че по-високият брой ревюта на приложенията има общо с по-високия брой изтегляния
#Правим боксплот за да проверим за outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(as.numeric(apps$Reviews), main="Reviews")  # box plot for 'reviews'
boxplot(apps$Installs, main="Installs")  # box plot for 'installs'

#density plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(as.numeric(apps$Reviews)), main="Density Plot: Reviews", ylab="Frequency")  # density plot for 'reviews'
polygon(density(as.numeric(apps$Reviews)), col="red")
plot(density(apps$Installs,na.rm=TRUE), main="Density Plot: Installs", ylab="Frequency")  # density plot for 'installs'
polygon(density(apps$Installs,na.rm=TRUE), col="red")
#corelation
apps <- apps[is.na(apps$Installs)==FALSE,]
cor(as.numeric(apps$Reviews), apps$Installs) 
#Стойността на корелацията върви към 1, което означава че има връзка между ревютата и броя изтегляния в права пропорционалност
#linear model
linearMod <- lm(Reviews ~ Installs, data=apps)  # build linear regression model on full data
print(linearMod)
# зависимостта е Installs = Intercept + (β ∗ Reviews)
# Installs =  98946.3399 + (0.2327 * Reviews)
# Сега имаме линеен модел и направихме формула която да предсказва стойността на Installs ако имаме стойност на Reviews
# Но това не е достатъчно, за да можем да използваме този модел. Преди да го използваме трябва да разберем дали е статистически верен
summary(linearMod)
