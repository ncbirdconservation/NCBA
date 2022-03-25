# Code for parsing block data
#
#
plot_spp_accumulation <- function(block) {

  # make sure to get onlyl species (no spp, or slash)
  tquery <- str_interp('{"ID_NCBA_BLOCK":"${block}", "OBSERVATIONS.CATEGORY":"species"}')
  tfilter <- '{"SAMPLING_EVENT_IDENTIFIER":1, "OBSERVATION_DATE":1, "DURATION_MINUTES":1, "OBSERVATIONS.BREEDING_CODE":1, "OBSERVATIONS.BREEDING_CATEGORY":1, "OBSERVATIONS.COMMON_NAME":1}'


  block_recs <- get_ebd_data(tquery, tfilter)

  # spp_acc <- data.frame(matrix(ncol=6, nrow=0))
  spp_acc <- data.frame(matrix(ncol=2, nrow=0))
  spp_unique <- vector(mode="list", length=0)

  obs_min <- 0

  # colnames(spp_acc) <- c('min','ntot','no','npo','npr','nco')
  colnames(spp_acc) <- c('min','ntot')

  curr_checklist <- "" #placeholder for current SAMPLING_EVENT_IDENTIFIER
  spp_check_unique <- 0 #checklist unique spp count

  for (i in 1:nrow(block_recs)){

    #check to see if this is a new checklist
    if (curr_checklist != block_recs$SAMPLING_EVENT_IDENTIFIER[i]) {
      # new checklist, add minutes to total
      obs_min <- obs_min + block_recs$DURATION_MINUTES[i]
      curr_checklist <- block_recs$SAMPLING_EVENT_IDENTIFIER[i]
      # print(curr_checklist)

      # were any new spp found in this checklist? if so, add to df
      if (spp_check_unique > 0 ){
        print("new spp_acc row")
        spp_acc[nrow(spp_acc) + 1,] <- c(obs_min, length(spp_unique))
      }
      #reset checklist unique spp count
      spp_check_unique <- 0

    }

    # capture ntot counts
    if (!(block_recs$COMMON_NAME[i] %in% spp_unique)) {
      #found a new species!

      spp_unique <- c(spp_unique, block_recs$COMMON_NAME[i])
      spp_check_unique <- spp_check_unique + 1
      print(spp_check_unique)
    }

  }
  #add final data point
  spp_acc[nrow(spp_acc) + 1,] <- c(obs_min, length(spp_unique))


  #plot the data

  plot_response <- ggplot(data=spp_acc,aes(min, ntot)) +
    geom_smooth(aes(y = ntot), color="#2a3b4d") +
    ylab("# Species") + xlab("Observation Time")

  #figure out how to provide multiple return data
  # response <- c(plot_response, spp_unique, spp_acc)

  return(plot_response)

}
