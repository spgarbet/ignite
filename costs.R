####### Costs & QALYs
#If run after the main run, no need to reload inputs & counters below.

###########
options("scipen"=100, "digits"=6)
annual_discount_rate <- 0.03

cont_discount_rate <- -log(1-annual_discount_rate) # Yearly Time Scale

discount_value <- function(value,ar=annual_discount_rate,A,B)
{
  r <- (1 + ar)^(1/365)-1
  (value/r)*(exp(-r*A)-exp(-r*B))
}

discount = function(value,ar=annual_discount_rate,A) value / (1+ar)^(A/365.25)

cost.qaly <- function(arrivals,inputs) 
{
  # Make all resources a factor (this allows for null events to still get summaries)
  arrivals$resource <- factor(arrivals$resource, counters)
  
  # Adjust all event end times from the inputs$durations
  mapply(function(value, name){
    arrivals[arrivals$resource == name,]$end_time <<-arrivals[arrivals$resource == name,]$start_time + value
  }, value=inputs$durations, name=names(inputs$durations) )
  
  # Truncate to end of study or life
  end_times <- arrivals[arrivals$resource == 'time_in_model',]
  arrivals$end_time <- pmin(arrivals$end_time, 
                            (plyr::join(arrivals[,c("name","end_time")], end_times[,c("name","end_time")], by="name", match="first"))$end_time)
  
  # Compute total activity times
  arrivals$activity_time <- arrivals$end_time - arrivals$start_time
  
  
  # Computes discounted rate of time
  arrivals$discounted_time <- discount_value(value=1,A=arrivals$start_time,B=arrivals$end_time)
  
  # Compute Event base cost map
  idx <- function(str) {as.numeric(factor(str, levels=levels(arrivals$resource)))}
  base_cost_map <- rep(0, nlevels(arrivals$resource))
  sapply(names(inputs$costs), FUN=function(name){
    base_cost_map[idx(name)] <<- inputs$costs[[name]]    
  })
  
  # Compute Disutility cost map
  base_disutility_map <- rep(0, nlevels(arrivals$resource))
  sapply(names(inputs$disutilities), FUN=function(name){
    base_disutility_map[idx(name)] <<- inputs$disutilities[[name]]    
  })
  names(base_disutility_map) = levels(arrivals$resource)
  
  arrivals$cost <- base_cost_map[as.numeric(arrivals$resource)]
  arrivals$discounted_cost <- ifelse(arrivals$activity_time>0,
                                     discount_value(value=arrivals$cost,A=arrivals$start_time,B=arrivals$end_time),
                                     discount(value = arrivals$cost,A=arrivals$start_time))
  
  arrivals$disutility = base_disutility_map[arrivals$resource]
  
  type <- data.frame(resource=names(inputs$type),type=unlist(inputs$type),row.names=NULL)
  qaly1 <- arrivals %>% group_by(name) %>% 
    arrange(start_time,desc(end_time)) %>% dplyr::mutate(utility = ifelse(row_number()==1,1,NA)) %>% filter(disutility>0 | utility>0) %>% #cross out events that have no impact on utility
    dplyr::select(name,resource,start_time,end_time,activity_time,disutility) %>%
    merge(type,by="resource",all.x=TRUE) %>% #attach type of events: temp vs. permanent disutility
    dplyr::mutate(us=disutility,ue=disutility*(-type)) %>%  #us/ue stand for disutility at start/end time: temp event will add back disutility at end time
    dplyr::select(name,start_time,end_time,us,ue,resource,type) %>% melt(id.vars=c("name","resource","us","ue","type")) %>% arrange(value) %>% #separate and spread start/end time
    dplyr::mutate(disutility=ifelse(variable=="start_time",us,ue)) %>% arrange(name,value,desc(variable)) %>% #match disutility with start/end time
    group_by(name) %>% mutate(time=lead(value)) %>% dplyr::mutate(dtime=ifelse(row_number()>1,time-lag(time),time)) %>% filter(!is.na(dtime)) %>% 
    filter(!(type==0 & dtime==0)) #For events that permanently reduce utility, this deletes double counts of the event and prevent double counting of disutility 
  #For temp event, we need to keep two records (start & end) in the datasets in order to adding back disutility at end time
  
  qaly2 <- qaly1 %>% mutate(cum1=ifelse(type==1 | is.na(type),0,disutility)) %>% #For permanent events (type==0), pass disutility to accumulate
    group_by(name) %>% mutate(temp_u=1-cumsum(cum1)) %>% 
    dplyr::mutate(cum2=ifelse(type==0 | is.na(type),0,disutility)) %>% mutate(utility=temp_u-cumsum(cum2)) %>% #For temp events, deduct accumulative disutility from temp_u
    filter(utility>0) #do not count negative/zero utility in qaly computation
  
  qaly.i <- qaly2 %>% dplyr::select(name, value, time, utility) %>%
    dplyr::mutate(qaly.d = discount_value(utility,A=value,B=time)) #discounted QALY for each period of time 
  
  QALY = qaly.i %>% group_by(name) %>% dplyr::summarise(dQALY = sum(qaly.d)/365.25)
  COST = arrivals %>% filter(discounted_cost>0) %>% group_by(name) %>% dplyr::summarise(dCOST = sum(discounted_cost))
  out <- QALY %>% left_join(COST,by="name") %>% replace_na(list(dQALY=0,dCOST=0))
  
  return(out)
  
  c(dQALY=mean(out$dQALY), dCost=mean(out$dCOST))
}
