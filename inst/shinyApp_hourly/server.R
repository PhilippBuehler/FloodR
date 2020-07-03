server <- function(input,output,session) {
  Sys.setenv(TZ='UTC')
  Reactive_DF <- reactiveValues()

  observeEvent({input$file1
    input$run
    Reactive_DF},{
      if(!is.null(input$file1)){
        inFile=input$file1

        w_str=as.numeric(regexpr("_", inFile$name))
        w_str=ifelse(w_str==-1,(nchar(inFile$name)-3),w_str)

        Reactive_DF$lab=substr(inFile$name,1,(w_str-1))
        Reactive_DF$S <- read.table(inFile$datapath,sep=";", header = TRUE)
        Reactive_DF$S$Begin <- as.POSIXct(Reactive_DF$S$Begin)
        Reactive_DF$S$End <- as.POSIXct(Reactive_DF$S$End)
        Reactive_DF$S$Peak_date <- as.POSIXct(Reactive_DF$S$Peak_date)


        Reactive_DF$Q = Discharge[[which(names(Discharge)==Reactive_DF$lab)]]

        if(is.null(Precipitation)) Precipitation <- Discharge
        for(iii in 1:length(Precipitation)) Precipitation[[iii]][,2] <- 0

        Reactive_DF$N = Precipitation[[which(names(Precipitation)==Reactive_DF$lab)]]

        if(is.null(Catchment_Properties)) Catchment_Properties <- data.frame(Name=names(Discharge),rep(NA,length(Discharge)))
        Reactive_DF$I=Catchment_Properties[grep(Reactive_DF$lab, Catchment_Properties$Name),]

      }else{
        Reactive_DF$S = Dummy$Floods
        Reactive_DF$Q = Dummy$Q
        Reactive_DF$N = Dummy$N
        Reactive_DF$lab = "Dummy_Catchment"
        Reactive_DF$I = data.frame(Name=Reactive_DF$lab)
      }


      if(is.null(Reactive_DF$I$Area)) Reactive_DF$I$Area <- NA
      Reactive_DF$MQ=mean(Reactive_DF$Q[,2],na.rm = TRUE)
      Reactive_DF$L=nrow(Reactive_DF$S)

      #Create Colums if not already there
      str_col=c("Not_a_Floodevent","Irregularity","more_Comments","Modified","Comment","Slope_of_Baseflow","Volume","dir_Volume",
        "baseflow_peak","baseflow_begin","baseflow_end" ,"HQ_dir", "TQDir")
      for (new_col in 1:length(str_col)){
        Reactive_DF$S=fncols(Reactive_DF$S,str_col[new_col])
      }



      if(!is.null(Reactive_DF$S)){
        updateSliderInput(session, "Slider_1", value = 1,
          min = 1, max = nrow(Reactive_DF$S), step = 1)

        d1=Reactive_DF$S[input$Slider_1,"Begin"]
        d2=Reactive_DF$S[input$Slider_1,"End"]
        updateDateInput(session, "date1",
          label = NULL,
          value = as.Date(d1),
          min   = NULL,
          max   = NULL)
        updateDateInput(session, "date2",
          label = NULL,
          value = as.Date(d2),
          min   = NULL,
          max   = NULL)
      }

      updateSliderInput(session, "hour1", value = as.numeric(format(d1,"%H")),
        min = 0, max = 23, step = 1)

      updateSliderInput(session, "hour2", value =as.numeric(format(d2,"%H")),
        min = 0, max = 23, step = 1)
    })


  observeEvent(input$HW,{
    if(!is.null(Reactive_DF$S)){
      temp=Reactive_DF$S
      if(input$HW==1){
        temp[input$Slider_1,"Not_a_Floodevent"]=""
      }
      if(input$HW==2){
        temp[input$Slider_1,"Not_a_Floodevent"]="X"
      }
      Reactive_DF$S=temp
    }
  })

  observeEvent(input$URM,{
    if(!is.null(Reactive_DF$S)){
      temp=Reactive_DF$S
      if(input$URM==2){
        temp[input$Slider_1,"Irregularity"]=tr("Un2")
      }
      if(input$URM==3){
        temp[input$Slider_1,"Irregularity"]=tr("Un3")
      }
      if(input$URM==4){
        temp[input$Slider_1,"Irregularity"]=tr("Un4")
      }
      if(input$URM==5){
        temp[input$Slider_1,"Irregularity"]=tr("Un5")
      }
      Reactive_DF$S=temp
    }
  })


  observeEvent(input$Text_correct,{
    if(!is.null(Reactive_DF$S)){
      temp=Reactive_DF$S
      if(input$Text_correct!=""){
        temp=Reactive_DF$S
        temp[input$Slider_1,"more_Comments"]=input$Text_correct
      }
      Reactive_DF$S=temp
    }
  })



  observeEvent({
    input$date1
    input$hour1
  },{#observe date to update begin
    if(!is.null(Reactive_DF$S)){
      temp=Reactive_DF$S
      cond1 = temp[input$Slider_1,"Begin"] != as.POSIXct(paste0(input$date1, " ",sprintf("%02d", input$hour1),":00:00"))
      cond2 = as.Date(temp[input$Slider_1,"Begin"]) == as.Date("2099-09-09")

      if(cond1 && cond2){
        temp[input$Slider_1,"Begin"]=as.POSIXct(paste0(input$date1, " ",sprintf("%02d", input$hour1),":00:00"))
        temp[input$Slider_1,"Modified"]="X"
        Reactive_DF$S=temp
      }
    }
  })

  observeEvent({
    input$date2
    input$hour2
  },{#observe date to update end
    if(!is.null(Reactive_DF$S)){
      temp=Reactive_DF$S
      cond1 = temp[input$Slider_1,"End"] != as.POSIXct(paste0(input$date2, " ",sprintf("%02d", input$hour2),":00:00"))
      cond2 = as.Date(temp[input$Slider_1,"End"]) == as.Date("2099-09-09")

      if(cond1 && cond2){
        temp[input$Slider_1,"End"]= as.POSIXct(paste0(input$date2, " ",sprintf("%02d", input$hour2),":00:00"))
        temp[input$Slider_1,"Modified"]="X"
        Reactive_DF$S=temp
      }
    }
  })



  observeEvent(input$next_1, {#update slider value
    updateSliderInput(session, "Slider_1", value = (input$Slider_1+1), min = NULL, max = NULL, step = 1)
  })

  observeEvent(input$prev_1, {#update slider value
    updateSliderInput(session, "Slider_1", value = (input$Slider_1-1), min = NULL, max = NULL, step = 1)
  })


  observeEvent({input$Slider_1},{
    if(!is.null(Reactive_DF$S)){
      #update comments
      if (Reactive_DF$S[input$Slider_1,"more_Comments"]==""){
        updateTextInput(session, "Text_correct", label = NULL, value = "",placeholder = NULL)
      }else{
        updateTextInput(session, "Text_correct", label = NULL, value =Reactive_DF$S[input$Slider_1,"more_Comments"] ,
          placeholder = NULL)
      }


      if(Reactive_DF$S[input$Slider_1,"Not_a_Floodevent"]!="X"){
        updateRadioButtons(session, "HW",selected = 1)
      }
      if (Reactive_DF$S[input$Slider_1,"Not_a_Floodevent"]=="X"){
        updateRadioButtons(session, "HW",selected = 2)
      }

      if(Reactive_DF$S[input$Slider_1,"Irregularity"]==""){
        updateRadioButtons(session, "URM",selected = 1)
      }
      if(Reactive_DF$S[input$Slider_1,"Irregularity"]==tr("Un2")){
        updateRadioButtons(session, "URM",selected = 2)
      }
      if(Reactive_DF$S[input$Slider_1,"Irregularity"]==tr("Un3")){
        updateRadioButtons(session, "URM",selected = 3)
      }
      if(Reactive_DF$S[input$Slider_1,"Irregularity"]==tr("Un4")){
        updateRadioButtons(session, "URM",selected = 4)
      }
      if(Reactive_DF$S[input$Slider_1,"Irregularity"]==tr("Un5")){
        updateRadioButtons(session, "URM",selected = 5)
      }

      #Then change date_box to new date
      d1=Reactive_DF$S[input$Slider_1,"Begin"]
      d2=Reactive_DF$S[input$Slider_1,"End"]
      updateDateInput(session, "date1",
        label = NULL,
        value = as.Date(d1),
        min   = NULL,
        max   = NULL)
      updateDateInput(session, "date2",
        label = NULL,
        value = as.Date(d2),
        min   = NULL,
        max   = NULL)

      # and hourly Slider
      updateSliderInput(session, "hour1", value = as.numeric(format(d1,"%H")), min = NULL, max = NULL, step = 1)

      updateSliderInput(session, "hour2", value =as.numeric(format(d2,"%H")), min = NULL, max = NULL, step = 1)


      #See if it has multiple peaks
      if(input$Slider_1 %in% c(1, Reactive_DF$L)){
        Reactive_DF$Mult=""
      }else{
        if (Reactive_DF$S[input$Slider_1,"Comment"]==" " & Reactive_DF$S[(input$Slider_1+1),"Comment"]=="overlaid"){
          Reactive_DF$Mult=paste("Big flood with",rle(A$Kommentar[(input$Slider_1+1):nrow(A)])$length[1],"overlaid waves")

        }else if (Reactive_DF$S[(input$Slider_1),"Comment"]=="overlaid" ){
          temp=min(which(rev(A$Kommentar[1:(input$Slider_1-1)])==" "))
          Reactive_DF$Mult=paste(temp,". overlaid flood",sep="")

        }else if (Reactive_DF$S[input$Slider_1,"Comment"]==" " & Reactive_DF$S[(input$Slider_1+1),"Comment"]=="First wave"){
          Reactive_DF$Mult=paste("Big flood containing",(min(which(A$Kommentar[(input$Slider_1+1):nrow(A)]==" "))-1),"waves")

        }else if (length(agrep("wave",Reactive_DF$S[(input$Slider_1),"Comment"]))==1){
          temp=min(which(rev(A$Kommentar[1:(input$Slider_1-1)])==" "))
          Reactive_DF$Mult=paste(temp,". wave",sep="")

        }else{
          Reactive_DF$Mult=""
        }
      }
    }
  })


  output$distPlot <- renderPlot({
    if(!is.null(Reactive_DF$S)){
      buff=input$buff

      a_Q=which(Reactive_DF$S[input$Slider_1,"Begin"]==Reactive_DF$Q[,1])
      b_Q=which(Reactive_DF$S[input$Slider_1,"End"]==Reactive_DF$Q[,1])
      c_Q=which(Reactive_DF$S[input$Slider_1,"Peak_date"]==Reactive_DF$Q[,1])
      a_N=which(Reactive_DF$S[input$Slider_1,"Begin"]==Reactive_DF$N[,1])
      b_N=which(Reactive_DF$S[input$Slider_1,"End"]==Reactive_DF$N[,1])


      if((a_Q-buff)<1){buff=120}

      plot_data=data.frame(x=Reactive_DF$Q[(a_Q-buff):(b_Q+buff),1],Q=Reactive_DF$Q[(a_Q-buff):(b_Q+buff),2],
        N=Reactive_DF$N[(a_N-buff):(b_N+buff),2])
      Qb_steigung=(plot_data$Q[nrow(plot_data)-buff]-plot_data$Q[buff+1])/(b_Q-a_Q)
      # Reactive_DF$S[input$Slider_1,"Slope_of_Baseflow"]=round(Qb_steigung,3)

      if(a_Q!=b_Q){
        dd=calc_stats(Reactive_DF$S[input$Slider_1,],Reactive_DF$Q)
      }else{
        dd=data.frame(dir_Volume=NaN)
        dd$dir_Volume=NaN
      }


      Title_1=paste(Reactive_DF$Mult," ",format(Reactive_DF$Q[c_Q,1],"%d.%m.%Y"),"- Qb:",round(Qb_steigung,2),"m\U00B3/s/h\n",
        "Direct-Vol.:",dd$dir_Volume,"Mio. m³ - Direct-HQ:",dd$HQ_dir,"m³/s")

      par(xaxs="i", yaxs="i", mar=c(5,5,5,5))
      ylimz_N <- max(plot_data$N) * 2
      if(ylimz_N==0) ylimz_N=1
      plot(plot_data$x, plot_data$N, type="h", ylim=c(ylimz_N,0),
        axes=FALSE, xlab=NA, ylab=NA, col="cornflowerblue",
        lwd=40, lend="square")
      axis(4,cex.axis=1.5)
      mtext(paste(tr("N"), "[mm/d]"), side=4, line=3,cex=1.5)

      par(new=TRUE)
      plot(plot_data$x,plot_data$Q,type="o",xlab = NA,ylab=paste(tr("Q"), "[m\U00B3/s]"),
        cex=1,  cex.axis=1.5,cex.lab=1.5,cex.main=2,ylim=c(0, max(plot_data$Q)*1.5),
        main=Title_1)
      grid((length(plot_data$x)-1),NULL)
      lines(c(plot_data$x[buff+1],plot_data$x[nrow(plot_data)-buff]),c(plot_data$Q[buff+1] ,plot_data$Q[nrow(plot_data)-buff]),col="grey50")
      points(plot_data$x[buff+1],plot_data$Q[buff+1],cex=2,col="red",pch = 16)
      points(plot_data$x[nrow(plot_data)-buff],plot_data$Q[nrow(plot_data)-buff],cex=2,col="red",pch = 16)
    }
  })


  output$downloadData <- downloadHandler(
    filename = function() {
      paste(Reactive_DF$lab,"_", tr("mod"),".csv", sep = "")
    },
    content = function(file) {
      temp=Reactive_DF$S
      for (jj in 1:nrow(temp)){
        temp[jj,]=calc_stats(temp[jj,],Reactive_DF$Q)
      }
      Reactive_DF$S=temp
      write.table(Reactive_DF$S, file,sep = ";", row.names = FALSE,col.names = TRUE)
    }
  )

  output$Gebiet_text <- renderUI({
    str1 <- paste(tr("E"),Reactive_DF$lab)
    str2 <- paste(tr("F"),Reactive_DF$I$Area,"km\U00B2")
    str3 <- paste("MQ:",round(Reactive_DF$MQ,2),"m\U00B3/s")
    HTML(paste("<font size=4>","<b>",str1,'<br/>', str2,'<br/>',str3,'<br/>',"</font>", sep = ''))
  })
}
