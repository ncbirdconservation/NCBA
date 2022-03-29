# Code for parsing block data
#
#
plot_spp_accumulation <- function(block_recs) {
  # TODO - list highest behavior code by spp
  # TODO - collecct data for categories
  #  C1 – Observed; C2 – Possible; C3 – Probable; C4 – Confirmed
  #  observed can be "" or "C1"

  # spp_acc <- data.frame(matrix(ncol=2, nrow=0))
  # colnames(spp_acc) <- c('min','ntot')
  spp_acc <- data.frame(matrix(ncol=6, nrow=0))
  colnames(spp_acc) <- c('min','all','c1','c2','c3','c4')
  # spp_unique <- vector(mode="list", length=0)

  #keep track of unique spp and their breeding_categories
  spp_unique <- data.frame(matrix(ncol=3,nrow=0))
  colnames(spp_unique) <- c("spp","bcat","bcat_num")

  obs_min <- 0
  curr_checklist <- "" #placeholder for current SAMPLING_EVENT_IDENTIFIER
  # spp_check_unique <- 0 #checklist unique spp count

  for (i in 1:nrow(block_recs)){
    #check to see if this is a new checklist

      if (curr_checklist != block_recs$SAMPLING_EVENT_IDENTIFIER[i]) {
        # save a new row in the dataset
        #get current category count
        if (i > 1) {
          c1 <- length(spp_unique["spp"][spp_unique["bcat"] == "C1"])
          c2 <- length(spp_unique["spp"][spp_unique["bcat"] == "C2"])
          c3 <- length(spp_unique["spp"][spp_unique["bcat"] == "C3"])
          c4 <- length(spp_unique["spp"][spp_unique["bcat"] == "C4"])
          # print(c(c1,c2,c3,c4))
          # print(c(obs_min,length(spp_unique$spp),c1,c2, c3, c4))
          spp_acc[nrow(spp_acc)+1,]<-c(obs_min,length(spp_unique$spp),c1,c2, c3, c4)
        }

        # new checklist, add minutes to total
        obs_min <- obs_min + block_recs$DURATION_MINUTES[i]
        curr_checklist <- block_recs$SAMPLING_EVENT_IDENTIFIER[i]

        # print(curr_checklist)

        # # were any new spp found in this checklist? if so, add to df
        # if (spp_check_unique > 0 ){
        #   spp_acc[nrow(spp_acc) + 1,] <- c(obs_min, length(spp_unique))
        # }
        # #reset checklist unique spp count
        # spp_check_unique <- 0

      } #end new checklist

      #set values for spp name and breeding category
      curr_spp <- block_recs$COMMON_NAME[i]
      curr_bcat <- block_recs$BREEDING_CATEGORY[i]
      if (curr_bcat == "") { curr_bcat <- "C1"} # "observed" stored as either "" or "C1"

      curr_bcat_num <- strtoi(substr(curr_bcat,2,2))

      # capture ntot counts
      if (!(curr_spp %in% spp_unique$spp)) {
        #found a new species!
        spp_unique[nrow(spp_unique)+1,] <- c(curr_spp, curr_bcat, curr_bcat_num)
      } else {
        # spp already observed in the block, check to see if the bcat is better
        exist_spp_row <-spp_unique[][spp_unique["spp"] == curr_spp] #get current spp row data

        #check if bcat is better than previous observation
        if (curr_bcat_num > exist_spp_row[3]) {
          #better bcat
          spp_unique["bcat"][spp_unique["spp"] == curr_spp] <- curr_bcat
          spp_unique["bcat_num"][spp_unique["spp"] == curr_spp] <- curr_bcat_num
        }

      } #end counting breeding codes
  } #end loop


  #add final data point
  c1 <- length(spp_unique["spp"][spp_unique["bcat"] == "C1"])
  c2 <- length(spp_unique["bcat"][spp_unique["bcat"] == "C2"])
  c3 <- length(spp_unique["bcat"][spp_unique["bcat"] == "C3"])
  c4 <- length(spp_unique["bcat"][spp_unique["bcat"] == "C4"])

  # print(c(obs_min,length(spp_unique$spp),c1,c2, c3, c4))
  spp_acc[nrow(spp_acc)+1,]<-c(obs_min,length(spp_unique$spp),c1,c2, c3, c4)

  spp_tot <- nrow(spp_unique)
  spp_tot_half <- spp_tot * 0.5
  hrs_total = obs_min * 0.0166666666666

  #plot the data
  plot_response <- ggplot(data=spp_acc,aes((min), all)) +
    geom_hline(aes(yintercept=(spp_tot_half), colour = "50% total")) +
    geom_text(aes(0,spp_tot_half), label = "50% total spp", vjust = -1) +
    geom_smooth(method = 'loess', formula = 'y ~ x', aes(y = all, colour="all")) +
    geom_smooth(method = 'loess', formula = 'y ~ x', aes(y = c4, colour="confirmed")) +
    geom_smooth(method = 'loess', formula = 'y ~ x', aes(y = c3, color="probable")) +
    geom_smooth(method = 'loess', formula = 'y ~ x', aes(y = c2, color="possible")) +
    scale_colour_manual(name="", values = c("#444444", "#2a3b4d", "#ff1a1a", "#ffbf00", "#ccccff")) +
    ylab("# Species") + xlab("Observation Time") + xlim(c(0,obs_min))
    # geom_line() +

  #figure out how to provide multiple return data
  # plot = accumulation plot output
  # spp_uniques = list of species found
  # spp_acc + Number of unique spp found
  response <- list("plot" = plot_response, "spp_unique" = spp_unique, "spp_acc_data" = spp_acc, "hrs_total" = hrs_total)
  # print(test_spp)

  return(response)

}
