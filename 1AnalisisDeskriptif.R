library(readxl)
library(dplyr)
library(car)
library(tidyverse)
library(ggplot2)
library(wordcloud2)
library(xlsx)

# Data
df <- read_excel("Data/Data - 2KS3.xlsx")
df <- as.data.frame(df)

# Pembersihan Missing Values
any(is.na(df))
nrow(df)
df <- na.omit(df) 
nrow(df)

# Pebgambilan Data Numerik
df_kipi <- df[,c(7:22)]
df_kipi <- as.data.frame(df_kipi)

# Jumlah Gejala Yang Dialami
sum_kipi <- df_kipi %>%
  summarize_if(is.numeric, sum, na.rm=TRUE)
sum_kipi <- as.data.frame(sum_kipi)
sum_kipi <- t(sum_kipi)
sum_kipi <- as.data.frame(sum_kipi)
sum_kipi$KIPI <- rownames(sum_kipi)
colnames(sum_kipi) <- c('Jumlah','Gejala')
rownames(sum_kipi) <- NULL
sum_kipi$proporsi <- sum_kipi$Jumlah/nrow(df)

ggplot(data=sum_kipi, aes(y=Gejala, x=Jumlah, fill=Gejala)) +
  geom_bar(colour="black", stat="identity") +
  ggtitle("Jumlah Gejala KIPI Yang Dirasakan") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="none")


# Jumlah Gejala Per Orang
sum_orang <- rowSums(df_kipi)
sum_orang
df_sumgejala <- data.frame('ID'=df$ID, 'Banyak Gejala'= sum_orang)
df_sumgejala2$Banyak.Gejala <- as.factor(df_sumgejala2$Banyak.Gejala)
df_sumgejala2 <- df_sumgejala %>% group_by(Banyak.Gejala) %>% summarise('Banyak.Orang' = n())
df_sumgejala2 <- df_sumgejala2 %>%
  arrange(desc(Banyak.Gejala)) %>%
  mutate(prop = Banyak.Orang / sum(df_sumgejala2$Banyak.Orang) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

ggplot(df_sumgejala2, aes(x="", y=prop, fill=as.factor(Banyak.Gejala))) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  scale_fill_brewer(palette="Set1") + 
  ggtitle("Jumlah Gejala Yang Dirasakan") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(y = ypos, label = round(prop,2)), color = "white", size=3)
ambil <- c(c(7,11,12,13,14))
gender <- df %>% group_by(`Jenis Kelamin`) %>%  summarise(across(ambil, sum))
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")
as.data.frame(gender)
library(xlsx)
library(readxl)
gender$Jumlah
gender <- read_excel("gender.xlsx")
ggplot(gender, aes(fill=`Jenis Kelamin`, x=Gejala, y=Jumlah)) + 
  geom_bar(position="dodge", stat="identity")

wilayah <- df %>% group_by(`Tempat Tinggal`) %>%  summarise(across(ambil, sum))
wilayah
library(xlsx)
write.xlsx(wilayah,"wilayah.xlsx")
wilay

pendidikan <- df %>% group_by(Pendidikan) %>%  summarise(across(ambil, sum))
write.xlsx(pendidikan,"pendidikan.xlsx")


wilayah <- read_excel("wilayah.xlsx")
wilayahbar <- ggplot(wilayah, aes(fill=Wilayah, x=Gejala, y=Banyak)) + 
  geom_bar(position="dodge", stat="identity")
genderbar
# 
# pendidikan <- df %>% group_by(Usia) %>%  summarise(across(ambil, sum))
# 
# df<-mutate(df, umur = case_when(
#   Usia == '< 20' ~ "<50",
#   Usia == '20-25' ~ "<50",
#   Usia == '26-30' ~ "<50",
#   Usia == '31-35' ~ "<50",
#   Usia == '36-40' ~ "<50",
#   Usia == '41-45' ~ "<50",
#   Usia == '46-50' ~ "<50",
#   Usia == '51-55' ~ ">50",
#   Usia == '56-60' ~ ">50",
#   Usia == '> 60' ~ ">50",
# ))
# 
# umur <- df %>% group_by(Usia) %>%  summarise(across(ambil, sum))
# write.xlsx(pendidikan,"umur.xlsx")
