library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Analisis KIPI"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Eksplorasi Dan Visualisasi", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Analisis Deskriptif", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Analisis Inferensia", tabName = "inferensia", icon = icon("th")),
      menuItem("Source Code",icon = icon("file-text-o"),
               menuSubItem("Analisis Deskriptif", tabName = "AnalisisDeskripsi", icon = icon("angle-right")),
               menuSubItem("Analisis Inferensia", tabName = "AnalisisInferensia", icon = icon("angle-right"))),
      menuItem("About", tabName = "readme", icon=icon("mortar-board"))
      )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              
              fluidRow(
                box(title ="Gejala Yang Sering Dialami Pasca Imunisasi",  solidHeader = TRUE,status = "primary",  align = "center",  width = 12,wordcloud2Output("word_cloud"))
              ),
              fluidRow(
                box( width = 4,
                  selectInput("kategori","Pilih Kategori",
                              c("Jenis Kelamin", "Wilayah","Pendidikan","Usia"
                              )
                  )
                ),
                box(width=8,uiOutput("uidesk"))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "inferensia",
              fluidRow(
                box(width = 6, solidHeader = TRUE, status = "info", title = "Uji Beda Proporsi",
                  column(width =12, selectInput("input_type", "Pilih Gejala",
                                                c("All",
                                                  "Pembengkakan",
                                                  "Kemerahan",
                                                  "Demam",
                                                  "Sakit kepala",
                                                  "Nyeri Otot",
                                                  "Kelelahan",
                                                  "Batuk",
                                                  "Diare",
                                                  "Mual dan muntah",
                                                  "Sesak napas",
                                                  "Nyeri Sendi",
                                                  "Pingsan",
                                                  "Reaksi Anafilaksis",
                                                  "Kesemutan",
                                                  "Pembengkakan Kelenjar Getah Bening"
                                                  
                                                ))),
                  column(width =12, 
                         
                         h6("H0 : Proporsi Gejala Yang Dirasakan Antara Laki-laki dan Perempuan Sama")
                         ,
                         h6("H0 : Proporsi Gejala Yang Dirasakan Antara Laki-laki dan Perempuan Tidak Sama"),
                         h6("Tingkat Signifikansi 5%"),
                         uiOutput("uidesc")
                  )),
                column(width = 6,
                  box(width = 12, status = "info",align= "center", uiOutput("ui")),
                  uiOutput("contingenct")
                )
                
              )
      ),
      tabItem(tabName = "AnalisisDeskripsi",
              box(width = NULL, status = "primary", solidHeader = TRUE, title= "Analisis Deskriptif",

                  pre(includeText("1AnalisisDeskriptif.R"))
              )
      ),
      tabItem(tabName = "AnalisisInferensia",
              box(width = NULL, status = "primary", solidHeader = TRUE, title= "Analisis Inferensia",
                  pre(includeText("2AnalisisInferensia.R"))
              )
      ),
      tabItem(tabName = "readme",
              fluidPage(
                box( title = "", solidHeader = TRUE, status = "info",
                  align = "center",
                  width = 12,
                  h1("nubisub"),
                  h4("2KStress - 66666666666")
                )
              )
              )
    )
  )
)