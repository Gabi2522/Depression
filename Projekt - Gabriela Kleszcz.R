# Tytuł: Depression Dataset - analiza czynników wystąpienia depresji
# File:  depression_data_checked_.csv

#Informacje na temat pliku - Zawarte kolumny z opisami

#Name: The full name of the individual.
#Age: The age of the individual in years.
#Marital Status: The marital status of the individual. Possible values include Single, Married, Divorced, and Widowed.
#Education Level: The highest level of education attained by the individual. Possible values include High School, Associate Degree, Bachelor's Degree, Master's Degree, and PhD.
#Number of Children: The number of children the individual has.
#Smoking Status: Indicates whether the individual is a smoker or not. Possible values are Smoker, Former and Non-smoker.
#Physical Activity Level: The level of physical activity undertaken by the individual. Possible values include Sedentary, Moderate, and Active.
#Employment Status: The employment status of the individual. Possible values include Employed and Unemployed.
#Income: The annual income of the individual in USD.
#Alcohol Consumption: The level of alcohol consumption. Possible values include Low, Moderate, and High.
#Dietary Habits: The dietary habits of the individual. Possible values include Healthy, Moderate, and Unhealthy.
#Sleep Patterns: The quality of sleep. Possible values include Good, Fair, and Poor.
#History of Mental Illness: Whether the individual has a history of mental illness. Possible values are Yes and No.
#History of Substance Abuse: Whether the individual has a history of substance abuse. Possible values are Yes and No.
#Family History of Depression: Indicates if there is a family history of depression. Possible values are Yes and No.
#Chronic Medical Conditions: Whether the individual has chronic medical conditions. Possible values are Yes and No.
#Depression_level: Score value: 1-100

#Problem badawczy: Celem projektu jest znalezienie czynników wpływających na ryzyko wystąpienia depresji?

#Instalacja i ładowanie pakietów
install.packages("readr")
install.packages("tidyverse")
install.packages("mgcv")
install.packages("babynames")
install.packages("ggcorrplot")
install.packages("rsample") 
install.packages("skimr")
suppressWarnings(if(!require("pacman")) install.packages("pacman"))

library(readr)
library(tidyverse)
library(mgcv)
library(babynames)
library(ggcorrplot)
library(rsample)
library(skimr)
pacman::p_load('tidyverse', 'tidymodels', 'glmnet',
               'randomForest', 'xgboost','patchwork',
               'paletteer', 'here', 'doParallel', 'summarytools')

#Przygotuj środowisko
setwd("~/R/R-zajecia")

#Wczytanie pliku
Depression <- read_csv("depression_data_checked_.csv")

#----------------------------Przygotowanie i oczyszczanie danych----------------------------------------------------
-#Przegląd klas danych i zawartość bazy danych
view(Depression)
str(Depression)
glimpse(Depression)
summary(Depression)
skim(Depression)
     
#Dane dla pierwszych 10 wierszy
Depression %>% slice_head(n = 10)

#Zapisz zbiór danych jako tibble
Depression <- Depression %>%
  as_tibble() %>%
  print()

#Sprawdzanie wartości zduplikowanych
Depression_duplicated <- sum(duplicated(Depression)) #Brak wartości zduplikowanych
print(Depression_duplicated)

#Sprawdzenie wartości kategorycznych (character) i faktorów
cat_columns <- Depression %>%
  select(where(is.character)) 
print(cat_columns)


#Zmiana nazw kolumn
Depression <- Depression %>% 
  rename (
    Marital = `Marital Status`,
    Education = `Education Level`,
    Children = `Number of Children`,
    Smoking = `Smoking Status`,
    Activity_Level = `Physical Activity Level`,
    Employment = `Employment Status`,
    Alcohol = `Alcohol Consumption`,
    Dietary = `Dietary Habits`,
    Sleep = `Sleep Patterns`,
    Mental_Illness = `History of Mental Illness`,
    Substance_Abuse = `History of Substance Abuse`, 
    Family_History = `Family History of Depression`, 
    Chronic = `Chronic Medical Conditions`
  )
print(Depression)

#Standaryzacja wartości kategorycznych o nazwach wynikach Yes i No - Zamiana niepoprwanie wprowadzonych wartości (Yes i No) na NA
Depression_character_stand <- Depression %>% 
  mutate(across(c( Mental_Illness, Substance_Abuse, Family_History,Chronic ), 
                ~ ifelse(. %in% c("Yes", "No"), ., NA)))

print(Depression_character_stand)

#Sprawdzanie wartości brakujących
missing_values <- colSums(is.na(Depression_character_stand ))

print(missing_values)

#Brakujące wartości dla Age, Children, Employment, Income, Dietary, Sleep, Mental_Illness, Substance_Abuse, Family_History, Chronic 

#Uzupełnienie brakujących wartości dla zmiennej numerycznej -> Age, Children, Income
cat_columns2 <- Depression_character_stand %>%
  select(where(is.numeric)) 
print(cat_columns2)

Depression <- Depression_character_stand %>% 
  mutate(
    Age = replace_na(Age, mean(Age, na.rm = TRUE)),
    Children = replace_na(Children, mean(Children, na.rm = TRUE)),
    Income  = replace_na( Income, mean( Income, na.rm = TRUE)),
    Depression_Level = replace_na(Depression_Level, mean(Depression_Level, na.rm = TRUE))
    )

print(Depression)

missing_values <- colSums(is.na(Depression))
print(missing_values)

#Pozostają NA wartości dla typów/klas kategorycznych: Employment, Dietary, Sleep, Mental_Illness, Substance_Abuse, Family_History, Chronic

#Uzupełnianie brakujących wartości najczęściej wytępującą odpowiedzią (moda) - funkcja tworzy tabelę częstości z pominięciem NA i zwraca najwyżsą częstość

get_mode <- function(x) {
  ux <- na.omit(x)  
  tab <- table(ux)  
  return(names(tab)[which.max(tab)])
}
Depression <- Depression %>%
  mutate(across(c(Employment, Dietary, Sleep, Mental_Illness, Substance_Abuse, Family_History, Chronic), 
                ~ ifelse(is.na(.), get_mode(.), .)))


missing_values <- colSums(is.na(Depression))
print(missing_values) #Nie ma już brakujących wartości

#Zaokrąglij wartości numeryczne do dwóch miejsc po przecinku
Depression <- Depression %>%
  mutate(across(where(is.numeric), ~ round(., 2)))
print(Depression)

#Podział kolumny Name na Imiona i Nazwiska osobno
Depression <- Depression %>%
  separate(Name, into = c("First_Name", "Last_Name"), sep = " ", extra = "merge")

#Usunięcie kolumny Last_Name
Depression <- Depression %>% 
  select(-Last_Name)

#Próba przypisania płci do imion, stworzenie nowej kolumny : Gender
#install.packages("babynames")
#library(babynames)
#library(tidyverse)

# Pobranie danych z `babynames`
babynames_data <- babynames %>%
  group_by(name) %>%
  summarize(Gender = ifelse(mean(sex == "F") > 0.5, "Female", "Male"))

# Połączenie z Twoją bazą danych
Depression <- Depression %>%
  left_join(babynames_data, by = c("First_Name" = "name"))

missing_values <- colSums(is.na(Depression))
print(missing_values) #Gender ma 8198 NA wartości

Depression <- Depression %>%
  mutate(across(Gender, 
                ~ ifelse(is.na(.), get_mode(.), .)))
summary(Depression) #Brak NA dla Gender

#Dodanie kolumny ID
Depression2 <- Depression %>% mutate(ID = as.numeric(factor(First_Name)))
print(Depression2)

#Zmiana kolejności
Depression <- Depression2 %>% select(First_Name, Gender, ID, everything()) 

#Posortuj Age od najniższej wartości
arrange(Depression, Age) #Dane zebrane dla osób pełnoletnich

#Dodanie nowej kolumny: Wiek produkcyjny i poprodukcyjny w zaleznosci dla płci
#Dla kobiet: wiek produkcyjny  (18 - 59 lat)
#Dla mężczyzn: wiek produkcyjny (18-64 lat)

Depression <- Depression %>% 
  mutate(
    Age_Category = case_when(
    (Gender == "Female" & Age >= 18 & Age <= 59) ~ "Productive",
    (Gender == "Male" & Age >= 18 & Age <= 64) ~ "Productive",
    Age > 59 & Gender == "Female" ~ "Post-productive",
    Age > 64 & Gender == "Male" ~ "Post-productive")
  )
print(Depression)

summary(Depression)

#Income:
#Min.   : 0.41
#1st Qu.: 22075.72
#Median : 39688.69
#Mean   : 50647.89
#3rd Qu.: 72885.13
#Max.   :209995.22 

#Dodanie nowej kolumny: Poziom zarobków
Depression <- Depression %>%
  mutate(Income_Level = case_when(
    Income < 22075 ~ "Low",
    Income >= 22075 & Income < 72885 ~ "Medium",
    Income >= 72885 ~ "High",
    ))

glimpse(Depression)

#-------------------------Wykresy---------------------------------------
##Wykresy - analiza zebranych danych (EDA)##

#Histogram wieku ankietowanych
ggplot(Depression, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Rozkład wieku ankietowanych", x = "Wiek", y = "Liczba ankietowanych")
#Wniosek: Największa liczba pacjentów w wieku 50 lat

#Histogram poziomu depresji
ggplot(Depression, aes(x = Depression_Level)) +
  geom_histogram(binwidth = 5, fill = "palevioletred2", color = "black") +
  theme_minimal() +
  labs(title = "Rozkład poziomu depresji", x = "Poziom depresji", y = "Liczba ankietowanych") +
  geom_vline(xintercept=mean(Depression$Depression_Level), linetype="dashed")
#Wniosek: Największa liczba pacjentów z umiarkowanym poziomem depresji, średnia - 46.57. Większość osób ma poziom depresji w przedziale 25-75, rozkład przypomina normalny.


#Związek między stanem cywilnym a wiekiem
ggplot(Depression, aes(x = Age, fill = Martial)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Rozkład wieku w zależności od stanu cywilnego", x = "Wiek", y = "Gęstość")
#Wniosek: Od 20 do 30 roku życia najwięcej osób stanu wolnego, między 30 a 50 rokiem życia najwięcej osób w związku małżeńskim. 
#Największa liczba rozwodów w przedziale 45-50 lat.Osoby owdowiałe głównie osoby powyżej 60 roku życia.


#Związek między poziomem depresji a wiekiem
ggplot(Depression, aes(Age, Depression_Level)) +
  geom_hex() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Związek między wiekiem a poziomem depresji",
       x = "Wiek", y = "Poziom depresji")

boxplot(Depression_Level ~ cut(Age, breaks=4), data=Depression, 
        main="Poziom depresji a wiek", xlab="Przedziały wiekowe", ylab="Depresja")

#Wniosek: Im starsza grupa, tym wyższy przeciętny poziom depresji.

#Związek między płcią a poziomem depresji
ggplot(Depression, aes(x = Gender, fill = Depression_Level, group = Gender)) +
  geom_bar(position = "fill") +
  labs(title = "Płeć a poziom depresji",
       x = "Płeć",
       y = "Procent osób",
       fill = "Poziom depresji") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal()
#Wniosek: Depresja nie zależy od płci.

#Związek między ilością snu a wiekiem

Depression$AgeGroup <- cut(Depression$Age, 
                           breaks = seq(10, 80, by = 10), 
                           labels = c("10-19", "20-29", "30-39", "40-49", 
                                      "50-59", "60-69", "70-80"))

ggplot(Depression, aes(x = AgeGroup, y = Sleep, fill = AgeGroup)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, color = "black") +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  labs(
    title = "Związek między jakością a wiekiem",
    x = "Grupa wiekowa",
    y = "Liczba godzin snu",
    fill = "Grupa wiekowa"
  ) +
  scale_fill_brewer(palette = "Set3") +  
  theme_minimal()
#Wniosek: Brak wpływu wieku na jakość snu.

#Związek między poziomem aktywności fizycznej a depresją
ggplot(Depression, aes(x = Depression_Level, fill = Activity_Level)) +
  geom_bar(position = "fill") +
  labs(title = "Depresja a aktywność fizyczna", x = "Poziom depresji", y = "Procent") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

boxplot(Depression_Level ~ Activity_Level, data=Depression, 
        main="Poziom depresji a aktywność fizyczna", xlab="Aktywność fizyczna", ylab="Depresja")
#Wniosek: Siedzący tryb życia wpływa na zwiększenie poziomu depresji.Aktywny tryb życia przyczynia się do zmniejszonego występowania depresji.
#Istnieje negatywna korelacja między aktywnością fizyczną a depresją – osoby aktywne mają niższy poziom depresji, a osoby prowadzące siedzący tryb życia są bardziej narażone na wysoki poziom depresji.

#Związek między poziomem wykształcenia a depresją

boxplot(Depression_Level ~ Education, data=Depression, 
        main="Poziom depresji a wykształcenie", xlab="Wykształcenie", ylab="Depresja", las=2)

#Wniosek:Wyższy poziom wykształcenia (zwłaszcza studia magisterskie i doktoranckie) wiąże się tu z niższym poziomem depresji 
#w porównaniu z grupami, które zakończyły edukację na poziomie szkoły średniej lub Associate Degree.

#Związek między poziomem spożycia alkoholu a depresją

boxplot(Depression_Level ~ Alcohol, data=Depression, 
        main="Poziom depresji a spożyciem alkoholu", xlab="Spożycie alkoholu", ylab="Depresja")

#Wnioski: Im większe spożycie alkoholu, tym silniejszy wpływ na wystąpienie depresji. 

#Związek między historią chorób psychicznych a depresją

ggplot(Depression, aes(x = Mental_Illness, 
                       y = Depression_Level, 
                       fill = Mental_Illness)) +
  geom_violin(trim = FALSE, alpha = 0.4, color = NA) +  
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.6) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(
    title = "Poziom depresji a historia chorób psychicznych",
    x = "Historia chorób psychicznych",
    y = "Poziom depresji"
  ) +
  theme_minimal() +
  theme(legend.position = "none") 
  

#Wniosek: Historia chorób psychicznych silnie koreluje z poziomem depresji, co może sugerować, że wcześniejsze problemy psychiczne zwiększają ryzyko cięższej depresji.  
#Wskazuje to na silny związek między historią chorób psychicznych a wyższym poziomem depresji.

#Związek między nawykami żywieniowymi a depresją

ggplot(Depression, aes(x = Dietary, y = Depression_Level, fill = Dietary)) +
  geom_boxplot() +
  labs(title = "Związek między nawykami żywieniowymi a poziomem depresji",
       x = "Nawyki żywieniowe",
       y = "Poziom depresji") +
  theme_minimal()
#Wniosek:Im bardziej niezdrowe nawyki żywieniowe, tym wzrasta poziom depresji. Osoby ze zdrowymi nawykami żywieniowymi mają niższą medianę poziomu depresji.

#Związek między paleniem a depresją
ggplot(Depression, aes(x = Smoking, y = Depression_Level, fill = Smoking)) +
  geom_boxplot() +
  labs(title = "Związek między paleniem a poziomem depresji",
       x = "Status palenia",
       y = "Poziom depresji") +
  theme_minimal()
#Wniosek: W każdej grupie występuje podobny rozkład poziomów depresji. Średni poziom depresji – Mediana poziomu depresji wydaje się być nieco wyższa u osób, 
#które kiedyś paliły (Former) i osób niepalących (Non-smoker) w porównaniu do palaczy (Current).


#Zatrudnienie a poziom depresji
ggplot(Depression, aes(x = Employment, y = Depression_Level, fill = Employment)) +
  geom_boxplot() +
  labs(title = "Związek między statusem zatrudnienia a poziomem depresji",
       x = "Status zatrudnienia",
       y = "Poziom depresji") +
  theme_minimal()
#Wniosek: Poziom depresji jest wyższy w grupie bezrobotnej.Mediana poziomu depresji w grupie osób bezrobotnych (Unemployed) jest wyższa niż wśród osób zatrudnionych (Employed).

#Status małżeński a poziom depresji

Depression <- Depression %>% 
  rename (
    Marital = Martial)

ggplot(Depression, aes(x = Marital, y = Depression_Level, fill = Marital)) +
  geom_boxplot() +
  labs(title = "Związek między statusem małżeńskim a poziomem depresji",
       x = "Status małżeński",
       y = "Poziom depresji") +
  theme_minimal()
#Wniosek: Najniższy poziom depresji u singli, najwyższy natomiast w grupie wdowców. Rozwiedzeni mają poziom depresji zbliżony do osób zamężnych/żonatych.


#Choroby przewlekle,a depresja

ggplot(Depression, aes(x = Chronic, y = Depression_Level, fill = Chronic)) +
  geom_boxplot() +
  labs(title = "Związek między chorobami przewlekłymi a poziomem depresji",
       x = "Choroby przewlekłe",
       y = "Poziom depresji") +
  theme_minimal()
#Wnioski: Rozkład wyników depresji w obu grupach jest dość podobny. Choroby przewlekłe mogą mieć wpływ na poziom depresji, ale nie jest on bardzo wyraźny.


#Liczba dzieci a poziom depresji

ggplot(Depression, aes(x = Children, y = Depression_Level)) +
  geom_point(alpha = 0.5, color = "steelblue1") +
  geom_smooth(method = "lm", color = "thistle2", se = TRUE) +
  labs(title = "Związek między liczbą dzieci a poziomem depresji",
       x = "Liczba dzieci",
       y = "Poziom depresji") +
  theme_minimal()
#Wniosek:Na wykresie widać, że poziom depresji nieznacznie rośnie wraz z liczbą dzieci, co sugeruje pozytywną, choć słabą korelację. 

#Dochód z poziom depresji
ggplot(Depression, aes(x = Income, y = Depression_Level)) +
  geom_point(alpha = 0.5, color = "steelblue1") +
  geom_smooth(method = "lm", color = "thistle2", se = TRUE) +
  labs(title = "Związek między dochodem a poziomem depresji",
       x = "Dochód",
       y = "Poziom depresji") +
  theme_minimal()
#Wniosek: Poziom depresji maleje wraz ze wzrostem dochodu.


#Nadużywanie substancji (Substance Abuse) a poziom depresja
ggplot(Depression, aes(x = Substance_Abuse, y = Depression_Level, fill = Substance_Abuse)) +
  geom_boxplot() +
  labs(title = "Związek między nadużywaniem substancji a poziomem depresji",
       x = "Nadużywanie substancji",
       y = "Poziom depresji") +
  theme_minimal()
#Wniosek: Brak związku między nadużywaniem substancji a poziomem depresji.

#Historia depresji w rodzinie a poziom depresji
ggplot(Depression, aes(x = Family_History, y = Depression_Level, fill = Family_History)) +
  geom_boxplot() +
  labs(title = "Związek między historią depresji w rodzinie a poziomem depresji",
       x = "Historia depresji w rodzinie",
       y = "Poziom depresji") +
  theme_minimal()
#Wniosek: Historia depresji w rodzinie może zwiększać ryzyko wystąpienia depresji u potomków.
#Jednakże średnia wystąpienia na w przypadku rozpoznania histori rodzinnej i jej nie rozpoznania nie odbiega znacząco od siebie.

# Wykresy kolowe:

#A
pie(table(Depression$Sleep), main="Podział jakości snu")

sleep_counts <- table(Depression$Sleep)
sleep_pct <- round(sleep_counts / sum(sleep_counts) * 100, 1)
pie(sleep_counts, labels = paste(names(sleep_counts), sleep_pct, "%"), main="Podział jakości snu")
#Wniosek: Ponad połowa grupy badawczej ocenia swoją jakość snu na poziomie fair (średnim), aż ~30% uważa, że ich jakość snu jest słaba/uboga.

#B.
pie(table(Depression$Activity_Level), main="Podział aktywności fizycznej")

activity_counts <- table(Depression$Activity_Level)
activity_pct <- round(activity_counts / sum(activity_counts) * 100, 1)
pie(activity_counts, labels = paste(names(activity_counts), activity_pct, "%"), main="Podział aktywności fizycznej")
#Wniosek: Aż ~43% ankietowanych przyznaje się do siedzącego trybu życia, o niewielkiem aktywności.

#C.
pie(table(Depression$Education), main="Podział poziomów wykształcenia")

edu_counts <- table(Depression$Education)
edu_pct <- round(edu_counts / sum(edu_counts) * 100, 1)
pie(edu_counts, labels = paste(names(edu_counts), edu_pct, "%"), main="Podział poziomów wykształcenia")
#Wniosek: rozkład pokazuje, że większość osób kończy edukację na poziomie średnim lub licencjackim, a tylko niewielka część zdobywa stopień doktora. 

#D
pie(table(Depression$Employment), main="Podział statusu zatrudnienia")

job_counts <- table(Depression$Employment)
job_pct <- round(job_counts / sum(job_counts) * 100, 1)
pie(job_counts, labels = paste(names(job_counts), job_pct, "%"), main="Podział statusu zatrudnienia")
#Wniosek: Aż 66% grupy wykonuje jest zatrudniona.

#-------------------------Konwersja danych-----------------------------
##Konwersja danych - wartości kategoryczne na numeryczne##
#Zatrudnienie (character)|
#Edukacja (character) |
#Aktywność fizyczna (character) |
#Jakość snu (character) |
#Stan cywilny (character) |
#Palenie (character) |
#Nawyki żywieniowe (character)|
#Choroby (character)
#Wiek (numeric)
#Dochody (numeric)

str(Depression)

##Zamiana character w factor

#Zmiana kolumny Marital na factor
Depression <- Depression %>%
  mutate(Marital = case_when(
     Marital == "Single" ~ 0,
     Marital == "Married" ~ 0.5,
     Marital == "Divorced" ~ 1,
     Marital == "Widowed" ~ 1.5
  ))

#Zmiana kolumny Employment na factor
Depression <- Depression %>%
  mutate(Employment = case_when(
    Employment == "Employed" ~ 1,
    Employment == "Unemployed" ~ 0
  ))

#Zmiana kolumny Education na factor
Depression <- Depression %>%
  mutate(Education = case_when(
    Education == "High School" ~ 0,
    Education == "Associate Degree" ~ 1,
    Education == "Bachelor's Degree" ~ 2,
    Education == "Master's Degree" ~ 3,
    Education == "PhD" ~ 4
  ))

#Zmiana kolumny Activity_Level na factor
Depression <- Depression %>%
  mutate(Activity_Level = case_when(
    Activity_Level == "Sedentary" ~ 0,
    Activity_Level == "Moderate" ~ 1,
    Activity_Level == "Active" ~ 2
  ))

#Zmiana kolumny Sleep na factor
Depression <- Depression %>%
  mutate(Sleep = case_when(
    Sleep == "Good" ~ 2,
    Sleep == "Fair" ~ 1,
    Sleep == "Poor" ~ 0
  ))

#Zmiana kolumny Smoking na factor
Depression <- Depression %>%
  mutate(Smoking = case_when(
    Smoking == "Smoker" ~ 1,
    Smoking == "Former" ~ 0.5,
    Smoking == "Non-smoker" ~ 0
  ))

#Zmiana kolumny Dietary na factor
Depression <- Depression %>%
  mutate(Dietary = case_when(
    Dietary == "Healthy" ~ 2,
    Dietary == "Moderate" ~ 1,
    Dietary == "Unhealthy" ~ 0
  ))

#Zmiana kolumny Family_History na factor
Depression <- Depression %>%
  mutate(Family_History = case_when(
    Family_History == "Yes" ~ 1,
    Family_History == "No" ~ 0))

#Zmiana kolumny Alcohol na factor
Depression <- Depression %>%
  mutate(Alcohol = case_when(
    Alcohol == "Low" ~ 0,
    Alcohol == "Moderate" ~ 1,
    Alcohol == "High" ~ 2
  ))

str(Depression)

#-----------------------------Analiza korelacji--------------------------------------------
#Analiza korelacji ziennych i wybór najlepszych do modelowania

  Depression33 <- Depression %>%
  select_if(is.numeric)

#Sprawdzenie korelacji jednej z powyższych zmiennych np. Wiek
  Depression33%>% summarise(corr_coef = cor(Age, Depression_Level))

cor_matrix <-cor(Depression33, use = "pairwise.complete.obs")

print(cor_matrix)

library(ggcorrplot)
ggcorrplot(cor_matrix, lab = TRUE, colors = c("violet", "white", "skyblue1"),
           title = "Korelacja macierzy Depresji")

#Wybór skorelowanych zmiennych z Depresja: 
#Age,Marital, Education, Children, Activity_Level, Employment, Income, Alcohol, Dietary, Sleep

Depression_cleared <- Depression %>%
  select(c("Age", "Marital", "Education", "Activity_Level", "Employment", "Sleep", "Income", "Depression_Level")) 

str(Depression_cleared)

Depression_cleared <- Depression_cleared  %>%   select_if(is.numeric)

#Sprawdzenie korelacji dla wszystkich zmiennych:
cor_matrix <-cor(Depression_cleared , use = "pairwise.complete.obs")

print(cor_matrix)

ggcorrplot(cor_matrix, lab = TRUE, colors = c("violet", "white", "skyblue1"),
           title = "Korelacja macierzy Depresji oczyszczonej")

#-------------------------Modelowanie------------------------------------
###Przygotowanie danych do modelowania i tworzenie modeli###
##Uczenie nadzorowane##
library(rsample)

#Podział danych na zbiór treningowy i testowy
set.seed(2056)

split <- initial_split(Depression_cleared, prop=0.7)

train_data <- training(split)
test_data <- testing(split)

cat("Training Set", nrow(train_data), "rows",
    "\nTest Set", nrow(test_data), "rows")

#Wynik:
#Training Set 289637 rows 
#Test Set 124131 rows

#------------------Model regresji liniowej--------------------------
#Regresja liniowa
#(załozenie ze liniowość, brak współkorelacji między zmiennymi wejściowymi, zmienne wejsciowe ustandaryzowane, predykcja)
library(tidymodels)
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

model_linear <- lm_spec %>% 
  fit( Depression_Level ~. , data = train_data)

model_linear

pred_train <- model_linear %>% predict(new_data = train_data)
pred_train

pred_test <- model_linear %>% predict(new_data = test_data)
pred_test

results <- test_data %>% 
  bind_cols(model_linear %>% 
              predict(new_data = test_data) %>%
              rename(predictions = .pred))
results

results %>% 
  ggplot(mapping = aes(x = Depression_Level, y = predictions)) +
  geom_point(size = 1.6, color = "steelblue") +
  # Overlay a regression line
  geom_smooth(method = "lm", se = F, color = 'magenta') +
  ggtitle("Poziom Depresji a jego predykcja") +
  xlab("Właściwe") +
  ylab("Oszacowane") +
  theme(plot.title = element_text(hjust = 0.5))

#Ewaluacja:
  eval_metrics <- metric_set(rmse, rsq, mae)

eval_metrics(data = results,
             truth = Depression_Level,
             estimate = predictions)

#Wniosek: Średni błąd predykcji wynosi 9.10, jako że zakres wartości depresji wynosi od 0 do 100, błąd ten jest akceptowalny.
#Średni błąd bezwzględny na poziomie 7.37, wskazuje że na ogół przewidywana wartość różni się od rzeczywistej o około 7 punktów.
#R2 na poziomie 0.641 oznacza ze model tłumaczy 64.1% wariancji poziomu depresji, co jest średnim wynikiem/modelem.

results_train <- train_data %>% 
  bind_cols(model_linear %>% 
              predict(new_data = train_data) %>%
              rename(predictions = .pred))

results_train %>% 
  ggplot(mapping = aes(x = Depression_Level, y = predictions)) +
  geom_point(size = 1.6, color = "steelblue") +
  # Overlay a regression line
  geom_smooth(method = "lm", se = F, color = 'magenta') +
  ggtitle("Poziom Depresji a jego predykcja") +
  xlab("Właściwe") +
  ylab("Oszacowane") +
  theme(plot.title = element_text(hjust = 0.5))

#Ewaluacja:
eval_metrics <- metric_set(rmse, rsq, mae)

eval_metrics(data = results_train,
             truth = Depression_Level,
             estimate = predictions)

#Wniosek: Średni błąd predykcji wynosi 9.07, jako że zakres wartości depresji wynosi od 0 do 100, błąd ten jest akceptowalny.
#Średni błąd bezwzględny na poziomie 7.35, wskazuje że na ogół przewidywana wartość różni się od rzeczywistej o około 7 punktów.
#R2 na poziomie 0.643 oznacza ze model tłumaczy 64.3% wariancji poziomu depresji, co jest średnim wynikiem/modelem.
#Model nie wydaje sie przeuczony, model musi byc bardiej skomplikowany.

#Regresja LASSO:
lasso_spec <- linear_reg(engine = "glmnet", mode = "regression", penalty = 1, mixture =1)
lasso_mod <- lasso_spec %>%
  fit(Depression_Level ~ ., data = train_data)

results_trainLasso <- train_data %>% 
  bind_cols(lasso_mod%>% 
              predict(new_data = train_data) %>%
              rename(predictions = .pred))

lasso_metrics <- eval_metrics(data = results_trainLasso,
             truth = Depression_Level,
             estimate = predictions)

theme_set(theme_light())

lasso_plt <- results_trainLasso %>% 
  ggplot(mapping = aes(x = Depression_Level, y = predictions)) +
  geom_point(size = 1.6, color = 'darkorchid') +
  # overlay regression line
  geom_smooth(method = 'lm', color = 'black', se = F) +
  ggtitle("Poziom Depresji a jego predykcja") +
  xlab("Właściwe") +
  ylab("Oszacowane") +
  theme(plot.title = element_text(hjust = 0.5))

list(lasso_metrics, lasso_plt)

#1 rmse    standard       9.28 
#2 rsq     standard       0.639
#3 mae     standard       7.54 
#Wniosek: Model wyjaśnia około 64% zmienności, co jest średnim wynikiem. RMSE ma dość duży błąd predykcji.Przeciętnie model myli się o 7.5 jednostek.


#-----------------Model drzewa decyzyjnego------------------------------------------------
#Model drzewa decyzyjnego: 
tree_spec <-  decision_tree(
  engine = "rpart",
  mode = "regression")

tree_mode <- tree_spec %>%
  fit(Depression_Level ~. , data = Depression_cleared)

#Predykcja na zbiorze testowym
resultsTREE <- tree_mode %>% 
  augment(new_data = test_data) %>% 
  rename(predictions = .pred)

# Ewaluacja
tree_metrics <- eval_metrics(data = resultsTREE,
                             truth = Depression_Level,
                             estimate = predictions)

tree_plt <- resultsTREE %>% 
  ggplot(mapping = aes(x = Depression_Level, y = predictions)) +
  geom_point(color = 'tomato') +
  # overlay regression line
  geom_smooth(method = 'lm', color = 'steelblue', se = F) +
  ggtitle("Zaleznosci dla depresji a jej przewidywania") +
  xlab("Właściwe") +
  ylab("Oszacowane") +
  theme(plot.title = element_text(hjust = 0.5))

list(tree_metrics, tree_plt)

#.metric .estimator .estimate
#<chr>   <chr>          <dbl>
#1 rmse    standard       8.48
#2 rsq     standard       0.689
#3 mae     standard       6.76 
#Wniosek:  RMSE jest zbliżony do modelu regresji liniowej, ale minimalnie lepszy. MAE jest minimalnie lepszy niż w regresji liniowej.
#Większość punktów leży w okolicach linii diagonalnej, co sugeruje, że model dość dobrze podąża za trendem rosnącego poziomu depresji.

#--------------Random Forest-------------------------
#Model Random Forest
set.seed(2056)
dim(train_data)
library("tidymodels")

#Budowa modelu
rf_spec <- rand_forest() %>% 
  set_engine('randomForest') %>% 
  set_mode('regression')

#Problem z czasem przetwarzania:
train_sample <- train_data %>% sample_n(5000) 

# Trening modelu 
rf_mod <- rf_spec %>% 
  fit(Depression_Level ~ ., data = train_sample)

rf_mod

library(yardstick)

resultsFOR <- rf_mod %>% 
  augment(new_data = train_sample) %>% 
  rename(predictions = .pred)

#Ewaluacja
rf_metrics <- resultsFOR %>%
  metrics(truth = Depression_Level, estimate = predictions)


rf_plt <- resultsFOR %>% 
  ggplot(mapping = aes(x = Depression_Level, y = predictions)) +
  geom_point(color = '#6CBE50FF') +
  # overlay regression line
  geom_smooth(method = 'lm', color = '#2B7FF9FF', se = F) +
  ggtitle("Zaleznosci dla depresji a jej przewidywania") +
  xlab("Właściwe") +
  ylab("Przewidywane") +
  theme(plot.title = element_text(hjust = 0.5))

list(rf_metrics, rf_plt)

# .metric .estimator .estimate
#<chr>   <chr>          <dbl>
#1 rmse    standard       5.11 
#2 rsq     standard       0.892
#3 mae     standard       4.14 
#Wniosek: Lepsza dokladnosci, lepsze dopasowanie i bardzije precyzyjne przewidywania w porównaniu do regresji liniowej.

#------------------------Model XGBoost-------------------------------
#Model XGBoost - maszyna wzmacniania gradientu

set.seed(2056)

boost_spec <- boost_tree() %>% 
  set_engine('xgboost') %>% 
  set_mode('regression')

# Trenowanie xgboost modelu 
boost_mod <- boost_spec %>% 
  fit(Depression_Level ~ ., data = train_data)

boost_mod

# Predykcja dla testowych danych
resultsXGB <- boost_mod %>% 
  augment(new_data = test_data) %>% 
  rename(predictions = .pred)

# Ewaluacja
boost_metrics  <- resultsXGB  %>%
  metrics(truth = Depression_Level, estimate = predictions)

boost_plt <- resultsXGB %>% 
  ggplot(mapping = aes(x = Depression_Level, y = predictions)) +
  geom_point(color = '#4D3161FF') +
  # overlay regression line
  geom_smooth(method = 'lm', color = 'black', se = F) +
  ggtitle("Zaleznosci dla depresji a jej przewidywania") +
  xlab("Actual Labels") +
  ylab("Predicted Labels") +
  theme(plot.title = element_text(hjust = 0.5))

list(boost_metrics, boost_plt)

#.metric .estimator .estimate
#<chr>   <chr>          <dbl>
#1 rmse    standard       6.16 
#2 rsq     standard       0.836
#3 mae     standard       4.99 

#Wnioski: Model XGBoost znacznie lepiej przewiduje poziom depresji.Model wyjaśnia aż 83.6% wariancji depresji,
#dodatkowo MAE spadło do 4.99, co wskazuje ze przeciętny błąd predykcji jest o 2.38 jednostki mniejszy niż w drzewach decyzyjnych.
#XGBoost i Random Forest dają dokładnie takie same wyniki w tej konfiguracji.


#-------Regresja logistyczna--------------------------

#Model regresji logistyczna

# Specyfikacja modelu
logreg_spec <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

logreg_spec

train_data <- train_data %>%
  mutate(Depression_Level = as.factor(Depression_Level))

#Trenowanie modelu
logreg_fit <- logreg_spec %>% 
  fit(Depression_Level ~ ., data = train_data)

logreg_fit

pred_train <- predict(logreg_fit, new_data = train_data, type = "class") %>% pull(.pred_class)
pred_train

pred_test <- predict(logreg_fit, new_data = test_data, type = "class") %>% pull(.pred_class)
pred_test

resultsLOG <- test_data %>%
  select(Depression_Level) %>% 
  bind_cols(predict(logreg_fit, new_data = test_data, type = "class") %>%
              rename(predictions = .pred_class))
resultsLOG

resultsLOG %>% 
  ggplot(mapping = aes(x = Depression_Level, y = predictions)) +
  geom_point(size = 1.6, color = "steelblue") +
  # Overlay a regression line
  geom_smooth(method = "lm", se = F, color = 'magenta') +
  ggtitle("Poziom Depresji a jego predykcja") +
  xlab("Właściwe") +
  ylab("Oszacowane") +
  theme(plot.title = element_text(hjust = 0.5))

#Wniosek: Model logistyczny odrzucony, ponieważ wykres sugeruje, że regresja logistyczna nie jest tutaj odpowiednia.
#---------------Ocena moodeli-------------------
# Ocena modeli
library(yardstick)

#macierz pomyłek, accuracy, precision, recall i F1-score nie mają zastosowani w modelu regresyjnym, tylko w klasyfikacji

#Finalne wnioski:
#Po przeprowadzeniu analizy pięciu modeli regresyjnych (Regresja Liniowa, regresja logistyczna, LASSO, Drzewo Decyzyjne, Random Forest, XGBoost) pod kątem przewidywania poziomu depresji, 
#widzimy że model XGBoost daje najlepsze wyniki. Model Random Forest osiągnął najlepsze wyniki, jendak przy mniejszej próbie.
#Model ten najlepiej dopasował się do danych, tłumacząc 89.2% zmienności poziomu depresji.
#W porównaniu do innych modeli, miał najniższy błąd predykcji (RMSE), co oznacza, że jego prognozy były najbardziej precyzyjne.
#Model XGBoost również uzyskał dobre wyniki, choć nieco słabsze niż Random Forest.
#Modele liniowe nie poradziły sobie tak dobrze, co sugeruje, że zależności w danych nie są w pełni liniowe.


---------#Zapisanie pliku------------------
#Zapisanie

write_csv(Depression_cleared, "Depression_cleared.csv")
write_csv(results_train, "Depression_results.csv")
write_csv(results_trainLasso, "Depression_resultsLasso.csv")
write_csv(resultsTREE, "Depression_resultsTREE.csv")
write_csv(resultsFOR, "Depression_resultsFOR.csv")
write_csv(resultsXGB, "Depression_resultsXGB.csv")

#------Czyszczenie----------
#Czyszczenie

# Usunięcie wszystkich obiektów
rm(list = ls()) 

# Usunięcie pakietów
detach("package:datasets", unload = T) 
p_unload(all) 

# Usunięcie wykresów
graphics.off()  

# Wyszczenie konsoli
cat("\014")  

