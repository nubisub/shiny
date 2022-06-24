#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
  })
  output$word_cloud = renderWordcloud2({
    wordcloud2(word,minRotation = 0, maxRotation = 0, rotateRatio = 0,color='random-dark')
  })
  output$table <- renderTable(inferensia)
  output$ui <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "All" = renderTable(inferensia),
           "Pembengkakan" = renderTable(inferensia[1,]),
           "Kemerahan" = renderTable(inferensia[2,]),
           "Demam" = renderTable(inferensia[3,]),
           "Sakit kepala" = renderTable(inferensia[4,]),
           "Nyeri Otot" = renderTable(inferensia[5,]),
           "Kelelahan" = renderTable(inferensia[6,]),
           "Batuk" = renderTable(inferensia[7,]),
           "Diare" = renderTable(inferensia[8,]),
           "Mual dan muntah" = renderTable(inferensia[9,]),
           "Sesak napas" = renderTable(inferensia[10,]),
           "Nyeri Sendi" = renderTable(inferensia[11,]),
           "Pingsan" = renderTable(inferensia[12,]),
           "Reaksi Anafilaksis" = renderTable(inferensia[13,]),
           "Kesemutan" = renderTable(inferensia[14,]),
           "Pembengkakan Kelenjar Getah Bening" = renderTable(inferensia[15,])
    )
  })
  
  output$uidesc <- renderUI({
    if (is.null(input$input_type))
      return()
    if ((input$input_type == "All"))
      return()
    switch(input$input_type,
           "All" = h6("Pilih Kategori !!"),
           "Pembengkakan" = h6(align="justify", "Dapat disimpulkan bahwa pada tingkat signifikansi 5 persen dan jumlah sampel
yang digunakan, belum cukup bukti untuk menyatakan bahwa proporsi gejala Pembengkakan
antara Laki-laki dan Perempuan Berbeda."),
           "Kemerahan" = h6(align="justify","Dapat disimpulkan bahwa pada tingkat signifikansi 5 persen dan jumlah sampel
yang digunakan, belum cukup bukti untuk menyatakan bahwa proporsi gejala Kemerahan
antara Laki-laki dan Perempuan Berbeda."),
           "Demam" =h6(align="justify", "Dapat disimpulkan bahwa pada tingkat signifikansi 5 persen dan jumlah sampel
yang digunakan, belum cukup bukti untuk menyatakan bahwa proporsi gejala Demam
antara Laki-laki dan Perempuan Berbeda."),
           "Sakit kepala" = h6(align="justify","Dapat disimpulkan bahwa pada tingkat signifikansi 5 persen dan jumlah sampel
yang digunakan, belum cukup bukti untuk menyatakan bahwa proporsi gejala Sakit Kepala
antara Laki-laki dan Perempuan Sama."),
           "Nyeri Otot" = h6(align="justify","Dapat disimpulkan bahwa pada tingkat signifikansi 5 persen dan jumlah sampel
yang digunakan, belum cukup bukti untuk menyatakan bahwa proporsi gejala Nyeri Otot
antara Laki-laki dan Perempuan Sama."),
           "Kelelahan" = h6(align="justify","Dapat disimpulkan bahwa pada tingkat signifikansi 5 persen dan jumlah sampel
yang digunakan, belum cukup bukti untuk menyatakan bahwa proporsi gejala kelelahan
antara Laki-laki dan Perempuan Berbeda."),
           "Batuk" = h6(align="justify","Dapat disimpulkan bahwa pada tingkat signifikansi 5 persen dan jumlah sampel
yang digunakan, belum cukup bukti untuk menyatakan bahwa proporsi gejala Batuk
antara Laki-laki dan Perempuan Berbeda."),
           "Diare" = h6(align="justify","Dapat disimpulkan bahwa pada tingkat signifikansi 5 persen dan jumlah sampel
yang digunakan, belum cukup bukti untuk menyatakan bahwa proporsi gejala Diare
antara Laki-laki dan Perempuan Berbeda."),
           "Mual dan muntah" = h6(align="justify","Dapat disimpulkan bahwa pada tingkat signifikansi 5 persen dan jumlah sampel
yang digunakan, belum cukup bukti untuk menyatakan bahwa proporsi gejala Mual dan Muntah
antara Laki-laki dan Perempuan Berbeda."),
           "Sesak napas" = h6(align="justify","Dapat disimpulkan bahwa pada tingkat signifikansi 5 persen dan jumlah sampel
yang digunakan, belum cukup bukti untuk menyatakan bahwa proporsi gejala Sesak napas
antara Laki-laki dan Perempuan Berbeda."),
           "Nyeri Sendi" = h6(align="justify","Dapat disimpulkan bahwa pada tingkat signifikansi 5 persen dan jumlah sampel
yang digunakan, belum cukup bukti untuk menyatakan bahwa proporsi gejala Nyeri Sendi
antara Laki-laki dan Perempuan Berbeda."),
           "Pingsan" = h6(align="justify","Dapat disimpulkan bahwa pada tingkat signifikansi 5 persen dan jumlah sampel
yang digunakan, belum cukup bukti untuk menyatakan bahwa proporsi gejala Pingsan
antara Laki-laki dan Perempuan Berbeda."),
           "Reaksi Anafilaksis" = h6(align="justify","Dapat disimpulkan bahwa pada tingkat signifikansi 5 persen dan jumlah sampel
yang digunakan, belum cukup bukti untuk menyatakan bahwa proporsi gejala Reaksi Anafilaksis
antara Laki-laki dan Perempuan Berbeda."),
           "Kesemutan" = h6(align="justify","Dapat disimpulkan bahwa pada tingkat signifikansi 5 persen dan jumlah sampel
yang digunakan, belum cukup bukti untuk menyatakan bahwa proporsi gejala Kesemutan
antara Laki-laki dan Perempuan Berbeda."),
           " Pembengkakan Kelenjar Getah Bening" = h6(align="justify","Dapat disimpulkan bahwa pada tingkat signifikansi 5 persen dan jumlah sampel
yang digunakan, belum cukup bukti untuk menyatakan bahwa proporsi gejala Pembengkakan Kelenjar Getah Bening
antara Laki-laki dan Perempuan Berbeda.")
    )
    
  })
  
  output$dynamic_value <- renderPrint({
    str(input$dynamic)
  })
  output$uidesk <- renderUI({
    if (is.null(input$kategori))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$kategori,
           "Jenis Kelamin" = renderPlotly(genderbar),
           "Wilayah" = renderPlotly(wilayahbar),
           "Pendidikan" = renderPlotly(pendidikanbar),
           "Usia" = renderPlotly(usiabar)
    )
  })
  
  output$contingenct <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "Pembengkakan" = box(width = 12,renderPlotly(a1g)),
           "Kemerahan" = box(width = 12,renderPlotly(a2g)),
            "Demam"=box(width = 12,renderPlotly(a3g)),
           "Sakit kepala"=box(width = 12,renderPlotly(a4g)),
           "Nyeri Otot"=box(width = 12,renderPlotly(a5g)),
           "Batuk"=box(width = 12,renderPlotly(a6g)),
           "Diare"=box(width = 12,renderPlotly(a7g)),
           "Mual dan muntah"=box(width = 12,renderPlotly(a8g)),
           "Sesak napas"=box(width = 12,renderPlotly(a9g)),
           "Nyeri Sendi"=box(width = 12,renderPlotly(a10g)),
           "Pingsan"=box(width = 12,renderPlotly(a11g)),
           "Reaksi Anafilaksis"=box(width = 12,renderPlotly(a12g)),
           "Kesemutan"=box(width = 12,renderPlotly(a13g)),
           "Kelelahan"=box(width = 12,renderPlotly(a14g)),
           "Pembengkakan Kelenjar Getah Bening"=box(width = 12,renderPlotly(a15g))
           )
  })
  
})
