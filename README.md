finding_continuity<-function(a,b){
  continuous<-data.frame()
  L<-as.vector(unique(a$idts))
  k<-c(1,2,3,4)
  p<-c(NA,NA,NA,NA)
  G<-data.frame(rbind(k,p))
  colnames(G)<-c("idts","delivery_year","delivery_month","order_cost_mile")
  row.names(G)<-c(1,2)
  conti<-data.frame()
  for(i in 1:length(L)){
    data<-subset(a,a$idts %in% L[i])
    if(min(data$delivery_year)== (2011 ||2012||2013||2014)){
    data<-data[-c(which(data$delivery_year == min(data$delivery_year))),]
    }
    else{
      data<-subset(a,a$idts %in% L[i])
    }
    for(j in 1:nrow(b)){     
      if(data$idts[1] == b[[1]][j]){
        if(b[[2]][j]== 2010 && nrow(data)== 64){
          K= 3 
          break
        }
        else if(b[[2]][j]== 2011 && nrow(data)==52){
          K= 3
          break
        }
        else if(b[[2]][j]== 2012 && nrow(data)==40){
          K= 3
          break
        }
        else if(b[[2]][j]== 2013 && nrow(data)==28){
          K= 3
          break
        }
        else if(b[[2]][j]== 2014 && nrow(data)==16){
          K= 3
          break
        }
        else if(b[[2]][j]== 2015 && nrow(data)==4){
          K= 3
          break
        }
        else if(b[[2]][j]== 2016 && nrow(data)== 4){
          K= 3
          break
        }
        else{
          K=4
          }
      } 
      else{
        next
      }
    }
    if(K==3){
      route<-c(data$idts[1])
      compleroute<-c(data[[1]][1])
      conti<-data.frame(cbind(route,compleroute))
      continuous<-data.frame(rbind(continuous,conti))
    }
    else if(K==4){
      route<-c(data$idts[1])
      compleroute<-c(G[[1]][1])
      conti<-data.frame(cbind(route,compleroute))
      continuous<-data.frame(rbind(continuous,conti))
    }
    
  }
  return(continuous)
}
