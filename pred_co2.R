setwd("xx")


library(readxl)
library(psych)
library(ggplot2)
library(lmtest)
library(car)
library(corrplot)
library(stargazer)
library(broom)
library(xtable)
library(dplyr)
library(skedastic)
library(corrplot)

#adatb?zis beolvas?sa
df_full <- read.csv("data.csv")

#tetsz?leges mintav?tel az adatb?zisb?l
set.seed(200)
df <- sample_n(df_full, 200, fac="CO2.Emissions.g.km.", replace = FALSE)
df_remaining <- anti_join(df_full, df) 

#adattiszt?t?s

data_cleaning <- function(df){
  df <- as.data.frame(df)
  df$Fuel.Type <- as.factor(df$Fuel.Type)
  df$Make<- as.factor(df$Make)
  df$Model<- as.factor(df$Model)
  df$Vehicle.Class <- as.factor(df$Vehicle.Class)
  df$Transmission <- as.factor(df$Transmission)
  df$Transmission <- as.factor(ifelse(grepl("^A", df$Transmission),
                                      "Automatic", "Manual"))
  
  df <- df %>%
    mutate(Fuel.Type = case_when(
      Fuel.Type == "X" ~ "Regular Gasoline",
      Fuel.Type == "Z" ~ "Premium Gasoline",
      Fuel.Type == "D" ~ "Diesel",
      Fuel.Type == "E" ~ "Ethanol",
      Fuel.Type == "N" ~ "Natural Gas",
      TRUE ~ "Other"  
    ))
  df$Fuel.Type <- as.factor(df$Fuel.Type)
  
  amerikai <- c("ACURA", "BUICK", "CADILLAC", "CHEVROLET", "CHRYSLER", 
                "DODGE", "FORD", "GMC", "JEEP", "LINCOLN", "RAM")
  df$Make <- as.factor(ifelse(df$Make %in% amerikai,
                              "American", "Non-American"))
  
  cars <- c("COMPACT", "FULL-SIZE", "MID-SIZE", "MINICOMPACT",
            "SUBCOMPACT", "TWO-SEATER")
  station_wagons <- c("STATION WAGON - MID-SIZE", "STATION WAGON - SMALL")
  suvs <- c("SUV - SMALL", "SUV - STANDARD")
  pickup_trucks <- c("PICKUP TRUCK - SMALL", "PICKUP TRUCK - STANDARD")
  vans <- c("MINIVAN", "VAN - CARGO", "VAN - PASSENGER")
  special_purpose <- c("SPECIAL PURPOSE VEHICLE")
  df <- df %>%
    mutate(Vehicle.Class = case_when(
      Vehicle.Class %in% cars ~ "Car",
      Vehicle.Class %in% station_wagons ~ "Station Wagon",
      Vehicle.Class %in% suvs ~ "SUV",
      Vehicle.Class %in% pickup_trucks ~ "Pickup truck",
      Vehicle.Class %in% vans ~ "Van",
      Vehicle.Class %in% special_purpose ~ "Special Purpose Vehicle",
      TRUE ~ "Other"  # Default case
    ))
  
  df$Vehicle.Class <- as.factor(df$Vehicle.Class)
  return (df)
}


#_______________________________
#alapmodellek elk?sz?t?se
df <- data_cleaning(df)
alapmodell <- lm(CO2.Emissions.g.km. ~. - Model, df)
summary(alapmodell)

boxplot(df$Engine.Size.L.)
boxplot(df$Cylinders)
boxplot(df$Fuel.Consumption.City..L.100.km.) 
boxplot(df$Fuel.Consumption.Hwy..L.100.km.)
boxplot(df$Fuel.Consumption.Comb..L.100.km.) #nagyobb mint 25 azt kivessz?k
boxplot(df$Fuel.Consumption.Comb..mpg.)
boxplot(df$CO2.Emissions.g.km.)

df <- df[df$Fuel.Consumption.Comb..L.100.km. < 25,]
df <- df[df$CO2.Emissions.g.km. < 500,]

#fogyaszt?s n?lk?li alapmodell
alapmodell2 <- lm(CO2.Emissions.g.km. ~ . - Model -
                    Fuel.Consumption.City..L.100.km. -  
                    Fuel.Consumption.Hwy..L.100.km. - 
                    Fuel.Consumption.Comb..mpg., df)
summary(alapmodell2)
resettest(alapmodell2) #van m?g mit jav?tani

#heteroszkedaszticit?s kezel?se
white_alapmodell2 <- white(alapmodell2, interactions = TRUE)
white_alapmodell2
bptest(alapmodell2, studentize = TRUE)
ks.test(alapmodell2$residuals, "pnorm")

df$HibatagNegyzet <- alapmodell2$residuals^2
ggplot(df, aes(x=CO2.Emissions.g.km., y=HibatagNegyzet)) + geom_point()
ggplot(df, aes(x=CO2.Emissions.g.km.)) +geom_histogram()
#nem az eredm?nyv?ltoz? eloszl?sa okozza a heteroszkedaszticit?st

#heteroszkedaszticit?s megold?sa White-f?le korr. standard hib?kkal
coeftest(alapmodell2, vcov = hccm(alapmodell2))

#heteroszkedaszticit?s kezel?se GLS becsl?ssel
seged_reg_gls <- lm(log(HibatagNegyzet) ~  Make + Vehicle.Class + Engine.Size.L.
                    + Cylinders + Transmission + Fuel.Type, data = df) 
omega_elemek <- exp(fitted(seged_reg_gls)) 

alapmodell_gls <- lm(CO2.Emissions.g.km. ~ Make + Vehicle.Class + Engine.Size.L.
                     + Cylinders + Transmission + Fuel.Type, weights = 1/omega_elemek, data = df)
summary(alapmodell_gls)

#___________________________________
#modellspecifik?ci? el?k?sz?t? l?p?sei

#numerikus v?ltoz?k hisztogramjai
hist(df$Engine.Size.L.)
hist(df$Cylinders)
hist(df$Fuel.Consumption.Comb..L.100.km.)
hist(df$CO2.Emissions.g.km.)

#interakci?s tagok keres?se
ggplot(data = df, aes(x = Fuel.Consumption.Comb..L.100.km., y = CO2.Emissions.g.km., colour = Fuel.Type)) +
  geom_point() +
  stat_smooth(method=lm)+
  labs(title="A j?rm?vek Co2 kibocs?t?sa ?s fogyaszt?sa k?z?tti kapcsolat ?zemanyag t?pusa szerint", 
       x="J?rm?vek fogyaszt?sa", y="J?rm?vek CO2 kibocs?t?sa")

ggplot(data = df, aes(x = Fuel.Consumption.Comb..L.100.km., y = CO2.Emissions.g.km., colour = Transmission)) +
  geom_point() +
  stat_smooth(method=lm)+
  labs(title="A j?rm?vek Co2 kibocs?t?sa ?s fogyaszt?sa k?z?tti kapcsolat v?lt? t?pusa szerint", 
       x="J?rm?vek fogyaszt?sa", y="J?rm?vek CO2 kibocs?t?sa")


ggplot(data = df, aes(x = Engine.Size.L., y = CO2.Emissions.g.km., colour = Vehicle.Class)) +
  geom_point() +
  stat_smooth(method=lm)+
  labs(title="A j?rm?vek Co2 kibocs?t?sa ?s motor m?rete k?z?tti kapcsolat j?rm? t?pusa szerint", 
       x="J?rm?vek motor m?rete", y="J?rm?vek CO2 kibocs?t?sa")


ggplot(data = df, aes(x = Engine.Size.L., y = CO2.Emissions.g.km., colour = Make)) +
  geom_point() +
  stat_smooth(method=lm)+
  labs(title="A j?rm?vek Co2 kibocs?t?sa ?s motor m?rete k?z?tti kapcsolat j?rm? sz?rmaz?sa szerint", 
       x="J?rm?vek motor m?rete", y="J?rm?vek CO2 kibocs?t?sa")


ggplot(data = df, aes(x = Cylinders, y = CO2.Emissions.g.km., colour = Fuel.Type)) +
  geom_point() +
  stat_smooth(method=lm)+
  labs(title="A j?rm?vek Co2 kibocs?t?sa ?s cilinderek sz?ma k?z?tti kapcsolat j?rm? ?zemanyaga szerint", 
       x="J?rm?vek cilindereinek sz?ma", y="J?rm?vek CO2 kibocs?t?sa")


#n?gyzetes vagy reciprok tag keres?se a numerikus v?ltoz?k k?z?tt
ggplot(data = df, aes(x = Engine.Size.L., y = CO2.Emissions.g.km.)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red")

ggplot(data = df, aes(x = Cylinders, y = CO2.Emissions.g.km.)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red")

ggplot(data = df, aes(x = Fuel.Consumption.Comb..L.100.km., y = CO2.Emissions.g.km.)) +
  geom_point() +
  stat_smooth(method=lm) +
  stat_smooth(color="red")

#____________________________
#modellspecifik?ci? el?k?sz?t?s?nek m?sodik l?p?se (multikollinearit?s vizsg?lata ?s kezel?se)

vif(alapmodell2) #enyh?n indokolt az alapmodell v?ltoz?ira f?komponenseket elk?sz?teni

fokomponens_elemzes <- prcomp(df[,c(4,5,8,9,10,11)], center = TRUE, scale=TRUE)
summary(fokomponens_elemzes)

df2 <- cbind(df, fokomponens_elemzes$x[,1])
colnames(df2)[13] <- "PC1"

Korrel <- cor(df2[,c(4,5,8,9,10,11,13)])
corrplot(Korrel, method="color", tl.cex = 0.8, tl.srt = 45)
corrplot(
  Korrel, 
  method = "color",        
  type = "upper",          
  order = "hclust",        
  addCoef.col = "black",   
  tl.col = "darkblue",     
  tl.cex = 0.8,            
  tl.srt = 45,             
  number.cex = 0.7,        
  col = colorRampPalette(c("red", "white", "blue"))(200), 
  diag = FALSE             
)

#a f?komponensek sz?r?sai alapj?n csak a PC1-et ?rdemes belerakni a modellbe, viszont
#ennek a korrel?ci?ja nehezen ?rtelmezhet?
#nem fogjuk haszn?lni

# f?komponens elemz?s a fogyaszt?st le?r? v?ltoz?kra
fokomponens_elemzes2 <- prcomp(df[,c(8,9,10)], center = TRUE, scale=TRUE)
summary(fokomponens_elemzes2)

df3 <- cbind(df, fokomponens_elemzes$x[,1])
colnames(df3)[13] <- "PC1"

Korrel <- cor(df3[,c(8,9,10,12,13)])
corrplot(Korrel, method="color", tl.cex = 0.8, tl.srt = 45)
#ez egy sok tartalommal b?r? f?komponens, a l?nyeg?ben ugyanolyan fogyaszt?si 
#adatokat egy v?ltoz?ba tudjuk s?r?teni

fokomp_modell <- lm(CO2.Emissions.g.km. ~ Make + Vehicle.Class + Engine.Size.L.
                    + Cylinders + Transmission + Fuel.Type + PC1,
                    data = df3)

summary(fokomp_modell)
summary(alapmodell2)

anova(alapmodell2,fokomp_modell) # nem j?n ki a p-?rt?k
AIC(alapmodell2,fokomp_modell)
BIC(alapmodell2,fokomp_modell)
#Az AIC ?s BIC is az alapmodell2-t prefer?lja, teh?t jobb, ha csak sim?n nem 
#vessz?k bele a fogyaszt?st

#________________________________
#v?gleges modell(ek) elk?sz?t?se
str(df)

#v?gleges model fogyaszt?s n?lk?l interakci?kkal
vegleges_model <- lm(CO2.Emissions.g.km. ~ - Fuel.Consumption.City..L.100.km. - 
                       Fuel.Consumption.Hwy..L.100.km. - Fuel.Consumption.Comb..L.100.km. -
                       Fuel.Consumption.Comb..mpg. - HibatagNegyzet - Cylinders + 
                       Engine.Size.L. * Vehicle.Class  +  Engine.Size.L. * Make, df)

BIC(alapmodell2, vegleges_model)

resettest(vegleges_model)
summary(vegleges_model)
vif(vegleges_model)

fokomponens_cylinder<- prcomp(df[,c("Engine.Size.L.", "Cylinders")], center = TRUE, scale=TRUE)
summary(fokomponens_cylinder)

#v?gleges modell fogyaszt?s f?komponenssel ?s interakci?k n?lk?l
vegleges_model2 <- lm(CO2.Emissions.g.km. ~ Engine.Size.L. + Make + PC1, df2)
summary(vegleges_model2)
BIC(alapmodell2, vegleges_model2)
BIC(vegleges_model, vegleges_model2)

#v?gleges modellek heteroszkedaszticit?s?nak kezel?se
coeftest(vegleges_model, vcov = hccm(vegleges_model))
coeftest(vegleges_model2, vcov = hccm(vegleges_model2))

#____________________________________________
#Kitekint?k?nt elk?sz?tett?k ugyanezt a modell az adatb?zisb?l vett 
#70%-os mint?ra, hogy kipr?b?ljuk, hogy milyen pontoss?ggal tudja 
#megbecs?lni a modell sz?m?ra ismeretlen aut?k Co2 kibocs?t?s?t


df_train <- sample_n(df_full, nrow(df_full)*0.7, fac="CO2.Emissions.g.km.", replace = FALSE)
df_test <- anti_join(df_full, df_train)

df_train <- data_cleaning(df_train)
df_test <- data_cleaning(df_test)

#v?gleges interakci?s modell b?t?k becsl?se a tanul? adatb?zisra
vegleges_model_train <- lm(CO2.Emissions.g.km. ~ - Fuel.Consumption.City..L.100.km. - 
                       Fuel.Consumption.Hwy..L.100.km. - Fuel.Consumption.Comb..L.100.km. -
                       Fuel.Consumption.Comb..mpg. - Cylinders + 
                       Engine.Size.L. * Vehicle.Class  +  Engine.Size.L. * Make, df_train)

#CO2 ?rt?kek becsl?se a teszt adatokra
df_test$Predicted_CO2 <- predict(vegleges_model_train, newdata = df_test)

df_test$diff <- abs(df_test$Predicted_CO2 - df_test$CO2.Emissions.g.km.)
hist(df_test$diff)
count(df_test[df_test$diff <= 20,])

#C02 kibocs?t?s szerint cs?kken? sorrendbe ?ll?t?s
df_test <- df_test %>%
  arrange(desc(CO2.Emissions.g.km.))
df_test$id <- 1:nrow(df_test)

#eredeti ?s predikt?lt ?rt?kek megjelen?t?se grafikonon
ggplot(df_test, aes(x = id)) +
  geom_point(aes(y = CO2.Emissions.g.km., color = "Original"), size = 3, alpha = 0.8) +
  geom_point(aes(y = Predicted_CO2, color = "Predicted"), size = 3, alpha = 0.8) +
  labs(
    title = "Eredeti ?s predikt?lt CO2 kibocs?t?s ?sszevet?se",
    x = "Megfigyel?si egys?gek CO2 kibocs?t?s szerint cs?kken? sorrendben",
    y = "CO2 kibocs?t?s (g/km)",
    color = "Adatok"
  ) +
  scale_color_manual(
    values = c("Original" = "#1f78b4", "Predicted" = "#33a02c"),
    labels = c("Teszt adatok C02 kibocs?t?sa", "Teszt adatok becs?lt CO2 kibocs?t?sa")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

