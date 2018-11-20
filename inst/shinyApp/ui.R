ui <- fluidPage(
  fluidRow(
    sliderInput("Slider_1",
                h3(tr("#NR")),
                min = 1,
                max = 999,
                value = 1,
                width="200%")
  ),

  fluidRow(
    column(1, align="center", style = "margin-top: 35px;",
           actionButton("prev_1", label =  "",icon = icon("arrow-left", "fa-5x"))
    ),
    column(1, align="center", style = "margin-top: 35px;",
           actionButton("next_1", label =  "",icon = icon("arrow-right", "fa-5x"))
    ),
    column(1,
           dateInput("date1", label = h3(tr("Beg")), value = NULL,format = "dd.mm.yyyy")
    ),
    column(1,
           dateInput("date2", label = h3(tr("End")), value = NULL ,format ="dd.mm.yyyy")

    ),
    column(2,
           radioButtons("HW", label = h3(tr("HW")),
                        choices =
                        {l=list(a =1,b= 2);
                        names(l)<-c(tr("HW"),tr("noHW"));
                        l},
                        selected = 1) ,
           textInput("Text_correct", label =  h3(tr("Ann")),value = NULL)
    ),
    column(2,
           radioButtons("URM", label = h3(tr("Un")),
                        choices =
                        {l=list(a =1,b= 2,c=3,d=4,e=5);
                        names(l)<-c(tr("Un1"),tr("Un2"),tr("Un3"),tr("Un4"),tr("Un5"));
                        l},
                        selected = NULL)
    ),
    column(2, style = "margin-top: +20px;",
           htmlOutput("Gebiet_text"),
           numericInput("buff",
                        h3("Buffer"),
                        value = 10)
    ),
    column(2,
           fileInput("file1", label = h3(tr("csv")),
                     accept = ".csv"),
           downloadButton("downloadData", h3(tr("down")))
    )
  ),
  br(),
  mainPanel(
    plotOutput("distPlot", width = "1500px",height = "650px"),
    br(),
    br()
  )
)
