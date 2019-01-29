
#individual cost
costi <- NULL
for(i in 0:9) {
        load(file=paste0("/gpfs23/data/h_imph/gravesj/right/cost_ignite10_SA_test",i,"_p.rda"))
        if(is.null(costi)) { costi <- out } else  {costi <- rbind(costi, out)}
        rm(out)
}

sum_costsi <- costi %>% data.frame() %>% 
        replace(is.na(.), 0) %>%
        group_by(strategy,tcost) %>%
        summarise(QALY=mean(dQALY),total=mean(total),test=mean(test),drug=mean(drug),event=mean(event)) %>%
        arrange(strategy) %>%
        mutate(ICER = (total-total[1])/(QALY-QALY[1]))

save(sum_costsi,file="/gpfs23/data/h_imph/gravesj/right/icer_ignite10_SA_test_p.rda")
