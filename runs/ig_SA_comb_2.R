

###raw results
print("raw")
result <- NULL
for(i in 0:9) {
  load(file=paste0("/gpfs23/data/h_imph/gravesj/right/raw_ignite_SA_",i,"_p.rda"))
  results <- results %>%  mutate(name = paste0(name,"_b",i))
  if(is.null(result)) { result <- results } else  {result <- rbind(result, results)}
  rm(results)
}
results <- result
rm(result)
save(results,file="/gpfs23/data/h_imph/gravesj/right/raw_ignite10_p.rda")

###Costs
print("cost")
source("./costs_ICER.R")
inputs$vN <- 200000
out <- NULL
  for(i in 0:4) {
    s <- cost.qaly(subset(results,strategy==i),inputs) %>% mutate(strategy=i)
    if(is.null(out)) { out <- s } else  {out <- rbind(out,s)}
}
save(out,file="/gpfs23/data/h_imph/gravesj/right/cost_ignite10_p.rda")

###event summary
print("event1")
DT <- data.table(results)
DT[, .N, by = list(resource, strategy)]
summ <- DT[, .N, by = list(resource, strategy)]
save(summ,file="/gpfs23/data/h_imph/gravesj/right/sum_ignite10_p.rda")

print("event2")
ael <- c("mi_event","st_event","dapt_stroke","revasc_event","bleed_event",
         "secular_death","bleed_fatal","cardio_death")
dt <- results %>% filter(resource == "dapt_start") %>% mutate(dapt_start=start_time)
dt2 <- results %>% filter(resource %in% ael) %>% select(name,start_time,resource,strategy) %>%
  inner_join(dt %>% select(name,dapt_start,strategy),by=c("name","strategy")) %>%
  mutate(event_time=start_time-dapt_start)

dt30 <- dt2 %>% filter(event_time<=30) %>% group_by(strategy,resource) %>% dplyr::summarise(N=n())
dt365 <- dt2 %>% filter(event_time<=365 & event_time>30) %>% group_by(strategy,resource) %>% dplyr::summarise(N=n())
save(dt30,file="/gpfs23/data/h_imph/gravesj/right/sum_ign_30d_p.rda")
save(dt365,file="/gpfs23/data/h_imph/gravesj/right/sum_ign_365d_p.rda")

###op
###more measures
var <- c("LOF","clopidogrel","ticagrelor","dapt_stroke","dapt_start","mi_event","secular_death",
         "st_event","bleed_fatal","cardio_death")
ael <- c("dapt_stroke","mi_event","st_event")

funsum <- function(dt,nam) {
  sum(dt[,nam])/nrow(dt)
}

#patients are categorized into: Clo - LOF, Alt - LOF, NonLOF
op <- NULL
for(i in 0:4) {
  r1 <- results %>% filter(strategy==i) %>% filter(resource %in% var)
  temp <- as.vector(r1[r1$resource=="LOF",]$name)
  mace <- r1 %>% filter(resource %in% ael) %>% select(name) %>% unique() %>% unlist()
  ### MI includes ST and non-ST here
  mi <- r1 %>% filter(resource %in% c("mi_event","st_event")) %>% select(name) %>% unique() %>% unlist()
  stroke <- r1 %>% filter(resource=="dapt_stroke") %>% select(name) %>% unique() %>% unlist()
  death <- r1 %>% filter(resource=="cardio_death") %>% select(name) %>% unique() %>% unlist()
  clop <- r1 %>% filter(resource=="clopidogrel") %>% select(name) %>% unique() %>% unlist()
  alte <- r1 %>% filter(resource=="ticagrelor") %>% select(name) %>% unique() %>% unlist()
  
  r2 <- r1 %>% select(name) %>% unique() %>%
    mutate(MACE=ifelse(name %in% mace,1,0),
           MI=ifelse(name %in% mi,1,0),
           Stroke=ifelse(name %in% stroke,1,0),
           Death=ifelse(name %in% death,1,0),
           LOF=ifelse(name %in% temp,1,0),
           Clo=ifelse(name %in% clop,1,0),
           Alt=ifelse(name %in% alte,1,0))
  
  if(i==0) {
    #strategy 0: None / Clo - LOF, Clo - NonLOF
    s0_1 <- r2 %>% filter(LOF==1) #Clo - LOF
    s0_2 <- r2 %>% filter(LOF==0) #Clo - NonLOF
    re <- data.frame(pop=c("total","Clo - LOF","NonLOF"),
                     MACE=sapply(list(r2,s0_1,s0_2),funsum,nam="MACE"),
                     MI=sapply(list(r2,s0_1,s0_2),funsum,nam="MI"),
                     Stroke=sapply(list(r2,s0_1,s0_2),funsum,nam="Stroke"),
                     Death=sapply(list(r2,s0_1,s0_2),funsum,nam="Death")) %>%
      mutate(strategy=i)
  } else if(i==1) {
    #strategy 1: Alt No genotyping / Alt - LOF, Alt - NonLOF
    s1_1 <- r2 %>% filter(LOF==1) #Alt - LOF
    s1_2 <- r2 %>% filter(LOF==0) #Alt - NonLOF
    re <- data.frame(pop=c("total","Alt - LOF","NonLOF"),
                     MACE=sapply(list(r2,s1_1,s1_2),funsum,nam="MACE"),
                     MI=sapply(list(r2,s1_1,s1_2),funsum,nam="MI"),
                     Stroke=sapply(list(r2,s1_1,s1_2),funsum,nam="Stroke"),
                     Death=sapply(list(r2,s1_1,s1_2),funsum,nam="Death")) %>%
      mutate(strategy=i)
  } else if(i==2) {
    #strategy 2: Alt30d No genotyping (all switch)/ A2C - LOF, NonLOF
    s2_1 <- r2 %>% filter(LOF==1) #A2C - LOF
    s2_2 <- r2 %>% filter(LOF==0) #A2C - NonLOF
    re <- data.frame(pop=c("total","A2C - LOF","NonLOF"),
                     MACE=sapply(list(r2,s2_1,s2_2),funsum,nam="MACE"),
                     MI=sapply(list(r2,s2_1,s2_2),funsum,nam="MI"),
                     Stroke=sapply(list(r2,s2_1,s2_2),funsum,nam="Stroke"),
                     Death=sapply(list(r2,s2_1,s2_2),funsum,nam="Death")) %>%
      mutate(strategy=i)
  } else if(i==3) {
    #strategy 3: GenotypeIdeal / Alt - LOF, NonLOF
    s5_1 <- r2 %>% filter(LOF==1) #Alt - LOF
    s5_2 <- r2 %>% filter(LOF==0) #Clo - NonLOF
    re <- data.frame(pop=c("total","Alt - LOF","NonLOF"),
                     MACE=sapply(list(r2,s5_1,s5_2),funsum,nam="MACE"),
                     MI=sapply(list(r2,s5_1,s5_2),funsum,nam="MI"),
                     Stroke=sapply(list(r2,s5_1,s5_2),funsum,nam="Stroke"),
                     Death=sapply(list(r2,s5_1,s5_2),funsum,nam="Death")) %>%
      mutate(strategy=i)  
  } else if(i==4) {
    #strategy 4: Genotype30dIdeal / Alt - LOF, NonLOF
    s6_1 <- r2 %>% filter(LOF==1) #Alt - LOF
    s6_2 <- r2 %>% filter(LOF==0) #A2C - NonLOF
    re <- data.frame(pop=c("total","Alt - LOF","NonLOF"),
                     MACE=sapply(list(r2,s6_1,s6_2),funsum,nam="MACE"),
                     MI=sapply(list(r2,s6_1,s6_2),funsum,nam="MI"),
                     Stroke=sapply(list(r2,s6_1,s6_2),funsum,nam="Stroke"),
                     Death=sapply(list(r2,s6_1,s6_2),funsum,nam="Death")) %>%
      mutate(strategy=i)      
  }
  
  if(is.null(op)) {op <- re} else {op <- rbind(op, re)}
}
save(op,file="/gpfs23/data/h_imph/gravesj/right/pct_ignite10_new_p.rda")





