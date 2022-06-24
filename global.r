library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)
library(wordcloud2)
library(plotly)

word <- read_excel("word.xlsx")
inferensia <- read_excel("Inferensia.xlsx")
as.data.frame(word)
as.data.frame(inferensia)


gender <- read_excel("gender.xlsx")
genderbar <- ggplot(gender, aes(fill=`Jenis Kelamin`, x=Gejala, y=Jumlah)) + 
  geom_bar(position="dodge", stat="identity")
ggplotly(genderbar)

wilayah <- read_excel("wilayah.xlsx")
wilayahbar <- ggplot(wilayah, aes(fill=Wilayah, x=Gejala, y=Banyak)) + 
  geom_bar(position="dodge", stat="identity")
ggplotly(wilayahbar)

pendidikan <- read_excel("pendidikan.xlsx")
pendidikanbar <- ggplot(pendidikan, aes(fill=Pendidikan, x=Gejala, y=Jumlah)) + 
  geom_bar(position="dodge", stat="identity")
ggplotly(pendidikanbar)


usia <- read_excel("umur.xlsx")
usiabar <- ggplot(usia, aes(fill=Usia, x=Gejala, y=Jumlah)) +
  geom_bar(position="dodge", stat="identity")
ggplotly(usiabar)



cont <- read_excel("contingenct.xlsx")
a1 <- cont %>% filter(Gejala == "Pembengkakan")
a1
a1g <- ggplot(a1, aes(fill=`Jenis Kelamin`, x=Tipe, y=Nilai)) +
  geom_bar(position="dodge", stat="identity")+
   ggtitle("Tabel Kontingensi")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
a1g<-ggplotly(a1g)



a2 <- cont %>% filter(Gejala == "Kemerahan")

a2g <- ggplot(a2, aes(fill=`Jenis Kelamin`, x=Tipe, y=Nilai)) +
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Tabel Kontingensi")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
a2g<-ggplotly(a2g)

a3 <- cont %>% filter(Gejala == "Demam")

a3g <- ggplot(a3, aes(fill=`Jenis Kelamin`, x=Tipe, y=Nilai)) +
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Tabel Kontingensi")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
a3g<-ggplotly(a3g)



a4 <- cont %>% filter(Gejala == "Sakit kepala")

a4g <- ggplot(a4, aes(fill=`Jenis Kelamin`, x=Tipe, y=Nilai)) +
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Tabel Kontingensi")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
a4g<-ggplotly(a4g)

a5 <- cont %>% filter(Gejala == "Nyeri Otot")

a5g <- ggplot(a5, aes(fill=`Jenis Kelamin`, x=Tipe, y=Nilai)) +
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Tabel Kontingensi")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
a5g<-ggplotly(a5g)


a6 <- cont %>% filter(Gejala == "Kelelahan")

a6g <- ggplot(a6, aes(fill=`Jenis Kelamin`, x=Tipe, y=Nilai)) +
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Tabel Kontingensi")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
a6g<-ggplotly(a6g)
a7 <- cont %>% filter(Gejala == "Batuk")

a7g <- ggplot(a7, aes(fill=`Jenis Kelamin`, x=Tipe, y=Nilai)) +
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Tabel Kontingensi")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
a7g<-ggplotly(a7g)
a8 <- cont %>% filter(Gejala == "Diare")

a8g <- ggplot(a8, aes(fill=`Jenis Kelamin`, x=Tipe, y=Nilai)) +
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Tabel Kontingensi")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
a8g<-ggplotly(a8g)
a9 <- cont %>% filter(Gejala == "Mual dan muntah")

a9g <- ggplot(a9, aes(fill=`Jenis Kelamin`, x=Tipe, y=Nilai)) +
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Tabel Kontingensi")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
a9g<-ggplotly(a9g)
a10 <- cont %>% filter(Gejala == "Sesak napas")

a10g <- ggplot(a10, aes(fill=`Jenis Kelamin`, x=Tipe, y=Nilai)) +
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Tabel Kontingensi")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
a10g<-ggplotly(a10g)

a12 <- cont %>% filter(Gejala == "Pingsan")

a12g <- ggplot(a12, aes(fill=`Jenis Kelamin`, x=Tipe, y=Nilai)) +
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Tabel Kontingensi")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
a12g<-ggplotly(a12g)
a13 <- cont %>% filter(Gejala == "Reaksi Anafilaksis")

a13g <- ggplot(a13, aes(fill=`Jenis Kelamin`, x=Tipe, y=Nilai)) +
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Tabel Kontingensi")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
a13g<-ggplotly(a13g)
a14 <- cont %>% filter(Gejala == "Kesemutan")

a14g <- ggplot(a14, aes(fill=`Jenis Kelamin`, x=Tipe, y=Nilai)) +
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Tabel Kontingensi")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
a14g<-ggplotly(a14g)
a15 <- cont %>% filter(Gejala == "Pembengkakan Kelenjar Getah Bening")

a15g <- ggplot(a15, aes(fill=`Jenis Kelamin`, x=Tipe, y=Nilai)) +
  geom_bar(position="dodge", stat="identity")+
  ggtitle("Bar Kontingensi")+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))
a15g<-ggplotly(a15g)


