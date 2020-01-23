eventsep<-function(dailyMQ,monthlyHQ,dvar=3,theta=0.25, ddur=40,Kappa=0.4, eta=0.1, delta=0.2, usemed=FALSE, medbf=0.5){
  
  
  daten<-dailyMQ[,1:2]
  daten[,1]<-as.Date(daten[,1], format="%d.%m.%Y")
  monthlyHQ[,1]<-as.Date(monthlyHQ[,1], format="%d.%m.%Y")
  
  MQ<-mean(daten[,2], na.rm=TRUE)
  
  
  diffs<-diff(daten[,2], lag=1)
  
  diffs<-c(0,diffs)
  
  #claculate the cumulative Lag 1 differences
  
  if(any(is.na(diffs))){
    cumdiffs<-numeric(length(diffs))
    cumdiffs[1]<-diffs[1]
    for(k in 2:length(diffs)){
      if(is.na(diffs[k])){
        cumdiffs[k]<-0
      }else{
        cumdiffs[k]<-cumdiffs[k-1]+diffs[k]
      }
    }
  }else{
    cumdiffs<-cumsum(diffs)}
  
  
  #calculate the window-variance with window length dvar
  var3d<-numeric()
  for(i in dvar:length(daten[,1])){
    var3d[i]<-var(daten[(i-(dvar-1)):i,2],na.rm = TRUE)
    
  }
  
  var3d[1:(dvar-1)]<-0
  
  #calculate the theshold for the variance
  thvar<-mean(var3d[dvar:length(var3d)], na.rm = TRUE)+theta*sd(var3d[dvar:length(var3d)], na.rm = TRUE)
  
  
  n2<-0
  
  #start iterating the days until the variance threshold is exceeded
  events<-NULL
  i<-0
  while(i <= length(var3d)){
    old_start<-0
    i<-i+1
    if(is.na(var3d[i])){}else{
      if(var3d[i]>thvar){
        var3dpart<-var3d[(i):length(var3d)]
        endvar<-which(var3dpart<thvar)[1]
        peak_ind<-i+which.max(var3dpart[1:endvar])-1
        while(daten[peak_ind-1,2]>daten[peak_ind,2]){
          peak_ind<-peak_ind-1
        }
        #chosse the event start as the days where the lag 1  differences are negative the last time
        pos_start<-max(which(diffs[1:(peak_ind-1)]<0))
        
        #modify start according to the assumptions
        while(((daten[pos_start+1,2]-daten[pos_start,2])< ((daten[peak_ind,2]-daten[pos_start,2])*eta)) && (pos_start<=peak_ind)) {
          pos_start<-pos_start+1
        }
        
        if(((daten[pos_start-1,2]-daten[pos_start-2,2])>((daten[peak_ind,2]-daten[pos_start,2])*Kappa)) && (daten[pos_start-2,2]<daten[peak_ind,2])){
          pos_start<-pos_start-2
        }
        
        if(daten[pos_start,2]>daten[pos_start+1,2]){pos_start<-pos_start+1}
        
        while(daten[peak_ind,2]<daten[peak_ind+1,2]){
          peak_ind<-peak_ind+1
        }
        
        #calculate the sum of the rising limb
        incsum<- sum(diffs[(pos_start+1):peak_ind])
        
        #define the end of the event
        pos_end<-peak_ind+1
        
        
        basefl1<-approxfun(c(daten[pos_start,1],daten[pos_end,1]),c(daten[pos_start,2],daten[pos_end,2]))
        base_diff<-(basefl1(daten[pos_start:pos_end,1])-daten[pos_start:pos_end,2])
        base_rel<-(basefl1(daten[pos_end,1])-basefl1(daten[pos_start,1]))/(pos_end-pos_start)
        
        #use median baseflow difference?
        if(usemed){
          while( (((((sum(diffs[(peak_ind+1):(pos_end+2)], na.rm = TRUE)-sum(diffs[(peak_ind+1):(pos_end)], na.rm = TRUE))/sum(diffs[(peak_ind+1):(pos_end)], na.rm = TRUE))>(1+delta))|| any(base_diff>0) || (base_rel>(2*medbf)) || ((incsum+sum(diffs[(peak_ind+1):pos_end], na.rm = TRUE))>(1*daten[pos_start,2])))) ){
            if(is.na(cumdiffs[pos_end+1])){break}
            pos_end<-pos_end+1
            
            basefl1<-approxfun(c(daten[pos_start,1],daten[pos_end,1]),c(daten[pos_start,2],daten[pos_end,2]))
            base_diff<-(basefl1(daten[pos_start:pos_end,1])-daten[pos_start:pos_end,2])
            base_rel<-(basefl1(daten[pos_end,1])-basefl1(daten[pos_start,1]))/(pos_end-pos_start)
          }
        }else{
          while( (((((sum(diffs[(peak_ind+1):(pos_end+2)], na.rm = TRUE)-sum(diffs[(peak_ind+1):(pos_end)], na.rm = TRUE))/sum(diffs[(peak_ind+1):(pos_end)], na.rm = TRUE))>(1+delta))|| any(base_diff>0)  || ((incsum+sum(diffs[(peak_ind+1):pos_end], na.rm = TRUE))>(1*daten[pos_start,2])))) ){
            if(is.na(cumdiffs[pos_end+1])){break}
            pos_end<-pos_end+1

            basefl1<-approxfun(c(daten[pos_start,1],daten[pos_end,1]),c(daten[pos_start,2],daten[pos_end,2]))
            base_diff<-(basefl1(daten[pos_start:pos_end,1])-daten[pos_start:pos_end,2])
            base_rel<-(basefl1(daten[pos_end,1])-basefl1(daten[pos_start,1]))/(pos_end-pos_start)
          }
        }
        if(pos_end<length(daten[,1])){
          pos_end<-pos_end+1
        }
        if(daten[pos_end,2]>=daten[pos_end-1,2]){pos_end<-pos_end-1}
        
        while(daten[pos_end,2]<daten[pos_start,2]){pos_end<-pos_end-1}
        
        No_peaks_all<-sum(diff(sign(diff(daten[pos_start:pos_end,2]))) < -1)
        
        no_max<-diff(sign(diff(var3d[pos_start:pos_end])))
        no_peaks<-max(1,sum((no_max< -1) & (var3d[(pos_start+1):(pos_end-1)]>thvar) & (diff(sign(diff(daten[pos_start:pos_end,2])))< -1)))
        
        peak_ind<-which.max(daten[pos_start:pos_end,2])+pos_start-1
        
        #create output for event
        if((peak_ind == pos_start) || (No_peaks_all==0) || is.element(as.character(as.numeric(daten[peak_ind,1])), events[,3])){}else{
          
          basefl<-approxfun(c(daten[pos_start,1],daten[pos_end,1]),c(daten[pos_start,2],daten[pos_end,2]))
          start<-daten[pos_start,1]
          end<-daten[pos_end,1]
          peak_MQ<-max(daten[(pos_start):(pos_end),2])
          peak_date<-daten[peak_ind,1]
          Volume<-sum(daten[(pos_start):(pos_end),2])*24*60*60/1000000
          Baseflow_peak<-basefl(peak_date)
          Base_vol<-integrate(basefl, lower=daten[pos_start,1], upper=daten[pos_end,1])$value*24*60*60/1000000
          Vol_dir<-Volume-Base_vol
          base_start<-basefl(start)
          base_end<-basefl(end)
          HQ<-monthlyHQ[which((monthlyHQ[,1]>=(peak_date-1)) & (monthlyHQ[,1]<=(peak_date+1))),2 ]
          if(length(HQ)<1){HQ<-NA}else{HQ<-max(HQ)}
          if(!is.na(HQ) && (HQ<peak_MQ)){HQ<-NA}
          HQ_dir<-HQ-Baseflow_peak
          comm<-" "
          if(any(end<as.Date(as.numeric(events[,2]),origin ="1970-01-01"))){comm<-"aufgesetzt"}
          events<-rbind(events, c(start, end, peak_date,peak_MQ, Volume, Vol_dir, Baseflow_peak,base_start,base_end,no_peaks,HQ, HQ_dir, comm))
          
          
        }
        
        #multiple peak event?
        if(No_peaks_all>1){
          
          #possible double-peaked event
          if((pos_end-pos_start+1)<ddur){
            peaks<-pos_start+which(diff(sign(diffs[pos_start:pos_end]) ) < -1)-1
            
            old_start<-pos_start
            old_end<-pos_end
            
            maxis<-mapply(function(x,y){max(daten[x,2],daten[y,2])},x=peaks[1:(length(peaks)-1)],y=peaks[2:length(peaks)])
            minmax<-mapply(function(x,y){min(daten[x,2],daten[y,2])},x=peaks[1:(length(peaks)-1)],y=peaks[2:length(peaks)])
            
            minis<-mapply(function(x,y){min(daten[x:y,2])},x=peaks[1:(length(peaks)-1)],y=peaks[2:length(peaks)])
            
            #test the condition for double-peaked events
            if(any(((0.4*maxis)>= minis) & ((maxis*0.2)<=minmax) & (minis <= (0.7*minmax)))){ ####FallÜberlagerung
              
              max_diff<-minmax-minis
              
              ind_diff<-which(((0.4*maxis)>= minis) & ((maxis*0.2)<=minmax) & (minis <= (0.7*minmax)))
              
              ind2<-which.max(max_diff[ind_diff])
              
              peak_ind<-peaks[ind_diff[ind2]]
              
              pos_start<-old_start
              
              pos_end<-which.min(daten[peak_ind:peaks[ind_diff[ind2]+1],2])+peak_ind-1
              
              ##construct first wave
              wave<-daten[pos_start:pos_end,]
              
              n1<-length(wave[,1])
              n_save<-n1
              
              while(daten[old_end,2]<wave[n1,2]){
                
                next_ind<-which(daten[(pos_start+n1+1):old_end,2]< wave[n1,2])[1]+pos_start+n1 ##positiv!!!
                
                wave<-rbind(wave, daten[next_ind,])
                
                n1<-length(wave[,1])
                
              }
              
              no_max<-diff(sign(diff(var3d[old_start:(old_start+n1-1)])))
              no_peaks<-max(1,sum((no_max< -1) & (var3d[(old_start+1):(old_start+n1-2)]>thvar) & (diff(sign(diff(daten[old_start:(old_start+n1-1),2])))< -1)))
              
              peak_ind<-which.max(wave[,2])
              #characteristics of first wave
              if((peak_ind == 1)){}else{
                
                basefl<-approxfun(c(1,n1),c(wave[1,2],wave[n1,2]))
                start<-wave[1,1]
                end<-start+n1-1
                peak_MQ<-max(wave[,2])
                peak_date<-wave[peak_ind,1]
                Volume<-sum(wave[,2])*24*60*60/1000000
                Baseflow_peak<-basefl(peak_ind)
                Base_vol<-integrate(basefl, lower=1, upper=n1)$value*24*60*60/1000000
                Vol_dir<-Volume-Base_vol
                base_start<-basefl(1)
                base_end<-basefl(n1)
                HQ<-monthlyHQ[which((monthlyHQ[,1]>=(peak_date-1)) & (monthlyHQ[,1]<=(peak_date+1))),2 ]
                if(length(HQ)<1){HQ<-NA}else{HQ<-max(HQ)}
                if(!is.na(HQ) && (HQ<peak_MQ)){HQ<-NA}
                HQ_dir<-HQ-Baseflow_peak
                comm<-"first wave"
                events<-rbind(events, c(start, end, peak_date,peak_MQ, Volume, Vol_dir, Baseflow_peak,base_start,base_end,no_peaks,HQ, HQ_dir, comm))
                
                
              }
              
              # construct second wave
              
              sec_wave<-daten[pos_end:old_end,]
              sec_wave[1:(n1-n_save+1),2]<-sec_wave[1:(n1-n_save+1),2]-wave[(n_save):n1,2]
              
              n2<-length(sec_wave[,2])
              
              
              no_max<-diff(sign(diff(var3d[(old_start+n1-1):old_end])))
              no_peaks<-max(1,sum((no_max< -1) & (var3d[(old_start+n1):(old_end-1)]>thvar) & (diff(sign(diff(daten[(old_start+n1-1):old_end,2])))< -1)))
              
              
              peak_ind<-which.max(daten[pos_end:old_end,2])
              
              #characteristics of second wave
              if((peak_ind == 1) ){}else{
                basefl<-approxfun(c(1,n2),c(sec_wave[1,2],sec_wave[n2,2]))
                start<-sec_wave[1,1]
                end<-start+n2-1
                peak_MQ<-max(sec_wave[,2])
                peak_date<-sec_wave[peak_ind,1]
                Volume<-sum(sec_wave[,2])*24*60*60/1000000
                Baseflow_peak<-basefl(peak_ind)
                Base_vol<-integrate(basefl, lower=1, upper=n2)$value*24*60*60/1000000
                Vol_dir<-Volume-Base_vol
                base_start<-basefl(1)
                base_end<-basefl(n2)
                HQ<-monthlyHQ[which((monthlyHQ[,1]>=(peak_date-1)) & (monthlyHQ[,1]<=(peak_date+1))),2 ]
                if(length(HQ)<1){HQ<-NA}else{HQ<-max(HQ)}
                if(!is.na(HQ) && (HQ<peak_MQ)){HQ<-NA}
                HQ_dir<-HQ-Baseflow_peak
                comm<-"second wave"
                events<-rbind(events, c(start, end, peak_date,peak_MQ, Volume, Vol_dir, Baseflow_peak,base_start,base_end,no_peaks,HQ, HQ_dir, comm))
                
                
              }
              
              
            }
            #possible overlaid event
          }else{
            
            peaks<-pos_start+which(diff(sign(diffs[pos_start:pos_end])) < -1)-1
            real_peaks<-peaks[daten[peaks,2]>=(max(daten[peaks,2])*0.2)]
            old_start<-pos_start
            old_end<-pos_end
            #check the assumptions on overlaid events
            if(any(diff(daten[real_peaks,1])>7)){
              ind_ev<-match(real_peaks[which(diff(daten[real_peaks,1])>7)], peaks)
              #
              #
              #split the overlaid waves from the main event
              for(j in 1:length(ind_ev)){
                if((0.5*max(daten[peaks[ind_ev[j]],2],daten[peaks[ind_ev[j]+1],2]))>= min(daten[peaks[ind_ev[j]]:(peaks[ind_ev[j]+1]),2])){
                  
                  pos_end<-peaks[ind_ev[j]]+1
                  
                  while(diffs[pos_end]<=0){
                    if(is.na(diffs[pos_end])){break}
                    pos_end<-pos_end+1
                    
                  }
                  
                  
                  if(daten[pos_end,2]>daten[pos_end-1,2]){pos_end<-pos_end-1}
                  peak_ind<-max(peaks[max(ind_ev[j-1]+1,1)])
                  
                  pos_start<-peak_ind-1
                  while(diffs[pos_start]>=0){
                    if(is.na(diffs[pos_start])){break}
                    pos_start<-pos_start-1
                    
                  }
                  while(daten[pos_start,2]<daten[pos_end,2]){
                    pos_start<-pos_start+1
                    
                  }
                  pos_start<-pos_start-1
                  
                  if(daten[pos_start,2]>=daten[pos_start+1,2]){pos_start<-pos_start+1}
                  
                  if(any(daten[(pos_start+1):peaks[ind_ev[j]],2]<daten[pos_start,2])){
                    pos_start<-max(which(daten[(pos_start+1):peaks[ind_ev[j]],2]==min(daten[(pos_start+1):peaks[ind_ev[j]],2])))+pos_start
                  }
                  
                  
                  no_max<-diff(sign(diff(var3d[pos_start:pos_end])))
                  no_peaks<-max(1,sum((no_max< -1) & (var3d[(pos_start+1):(pos_end-1)]>thvar) & (diff(sign(diff(daten[pos_start:pos_end,2])))< -1)))
                  
                  
                  peak_ind<-which.max(daten[pos_start:pos_end,2])+pos_start-1
                  
                  if((peak_ind == pos_start)){}else{
                    
                    basefl<-approxfun(c(daten[pos_start,1],daten[pos_end,1]),c(daten[pos_start,2],daten[pos_end,2]))
                    start<-daten[pos_start,1]
                    end<-daten[pos_end,1]
                    peak_MQ<-max(daten[(pos_start):(pos_end),2])
                    peak_date<-daten[peak_ind,1]
                    Volume<-sum(daten[(pos_start):(pos_end),2])*24*60*60/1000000
                    Baseflow_peak<-basefl(peak_date)
                    Base_vol<-integrate(basefl, lower=daten[pos_start,1], upper=daten[pos_end,1])$value*24*60*60/1000000
                    Vol_dir<-Volume-Base_vol
                    base_start<-basefl(start)
                    base_end<-basefl(end)
                    HQ<-monthlyHQ[which((monthlyHQ[,1]>=(peak_date-1)) & (monthlyHQ[,1]<=(peak_date+1))),2 ]
                    if(length(HQ)<1){HQ<-NA}else{HQ<-max(HQ)}
                    if(!is.na(HQ) && (HQ<peak_MQ)){HQ<-NA}
                    HQ_dir<-HQ-Baseflow_peak
                    comm<-"overlaid"
                    events<-rbind(events, c(start, end, peak_date,peak_MQ, Volume, Vol_dir, Baseflow_peak,base_start,base_end,no_peaks,HQ, HQ_dir, comm))
                    
                    
                  }
                  
                }
              }
              #remaining part of overlaid wave
              if(any((peaks> pos_end) & (peaks>(peak_ind+7)) & (daten[peaks,2]>0.2*max(daten[peaks,2])))){
                peak_ind<-which((peaks> pos_end) & (peaks>(peak_ind+7)) & (daten[peaks,2]>0.2*max(daten[peaks,2])))
                peak_ind<-peaks[peak_ind[which.max(daten[peak_ind,2])]]
                
                pos_end<-peaks[length(peaks)]+1
                
                while(diffs[pos_end]<=0){
                  if(is.na(diffs[pos_end+1])){break}
                  pos_end<-pos_end+1
                  
                }
                
                
                if(daten[pos_end,2]>daten[pos_end-1,2]){pos_end<-pos_end-1}
                if(pos_end>old_end){pos_end<-old_end}
                
                pos_start<-peaks[ind_ev[length(ind_ev)]+1]
                while(diffs[pos_start]>=0){
                  if(is.na(diffs[pos_start])){break}
                  pos_start<-pos_start-1
                  
                }
                while(daten[pos_start,2]<daten[pos_end,2]){
                  pos_start<-pos_start+1
                  
                }
                pos_start<-pos_start-1
                
                if(daten[pos_start,2]>=daten[pos_start+1,2]){pos_start<-pos_start+1}
                
                if(any(daten[(pos_start+1):(which.max(daten[pos_start:pos_end,2])+pos_start-1),2]<daten[pos_start,2])){
                  pos_start<-max(which(daten[(pos_start+1):(which.max(daten[pos_start:pos_end,2])+pos_start-1),2]==min(daten[(pos_start+1):(which.max(daten[pos_start:pos_end,2])+pos_start-1),2])))+pos_start
                }
                
                
                no_max<-diff(sign(diff(var3d[pos_start:pos_end])))
                no_peaks<-max(1,sum((no_max< -1) & (var3d[(pos_start+1):(pos_end-1)]>thvar) & (diff(sign(diff(daten[pos_start:pos_end,2])))< -1)))
                
                
                peak_ind<-which.max(daten[pos_start:pos_end,2])+pos_start-1
                
                if((peak_ind == pos_start)){}else{
                  
                  basefl<-approxfun(c(daten[pos_start,1],daten[pos_end,1]),c(daten[pos_start,2],daten[pos_end,2]))
                  start<-daten[pos_start,1]
                  end<-daten[pos_end,1]
                  peak_MQ<-max(daten[(pos_start):(pos_end),2])
                  peak_date<-daten[peak_ind,1]
                  Volume<-sum(daten[(pos_start):(pos_end),2])*24*60*60/1000000
                  Baseflow_peak<-basefl(peak_date)
                  Base_vol<-integrate(basefl, lower=daten[pos_start,1], upper=daten[pos_end,1])$value*24*60*60/1000000
                  Vol_dir<-Volume-Base_vol
                  base_start<-basefl(start)
                  base_end<-basefl(end)
                  HQ<-monthlyHQ[which((monthlyHQ[,1]>=(peak_date-1)) & (monthlyHQ[,1]<=(peak_date+1))),2 ]
                  if(length(HQ)<1){HQ<-NA}else{HQ<-max(HQ)}
                  if(!is.na(HQ) && (HQ<peak_MQ)){HQ<-NA}
                  HQ_dir<-HQ-Baseflow_peak
                  comm<-"overlaid"
                  events<-rbind(events, c(start, end, peak_date,peak_MQ, Volume, Vol_dir, Baseflow_peak,base_start,base_end,no_peaks,HQ, HQ_dir, comm))
                  
                  
                }
                
                
              }
              
            }
          }
        }
        
        
        i<-max(pos_end, i+endvar, old_start+n2-1)
        
      }
    }
  }
  
  events2<-cbind(as.character(as.Date(as.numeric(events[,1]),origin ="1970-01-01")),as.character(as.Date(as.numeric(events[,2]),origin ="1970-01-01")),as.character(as.Date(as.numeric(events[,3]),origin ="1970-01-01")), events[,4], events[,5], events[,6], events[,7], events[,8],events[,9], events[,10], events[,11],events[,12],events[,13])
  
  events2<-as.data.frame(events2)
  
  names(events2)<-c("Begin", "End", "Peak_date", "DailyMQ", "Volume", "dir_Volume", "baseflow_peak","baseflow_begin","baseflow_end", "No_Peaks", "HQ", "HQ_dir", "Comments")
  
  return(events2)
  
}
