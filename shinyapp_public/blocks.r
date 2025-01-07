# Code for parsing block data
# Functions:
#   plot_spp_accumulation(block_recs, spp_bcs)
#		modified by S. Pearson, 2023-12-14
#
plot_spp_accumulation <- function(block_recs, spp_bcs) {
  # TODO - list highest behavior code by spp
  # TODO - collecct data for categories
  #  C1 – Observed; C2 – Possible; C3 – Probable; C4 – Confirmed
  #  observed can be "" or "C1"
  # print(head(block_recs))
  # spp_acc <- data.frame(matrix(ncol=2, nrow=0))
  # colnames(spp_acc) <- c('min','ntot')
  # Create data frame to store graph data
  spp_acc <- data.frame(matrix(ncol=6, nrow=0))
  colnames(spp_acc) <- c('min','all','c1','c2','c3','c4')
  # spp_unique <- vector(mode="list", length=0)

  #keep track of unique spp and their breeding_categories
  spp_unique <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(spp_unique) <- c("spp", "bcat", "bcat_num")

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
        spp_acc[nrow(spp_acc) + 1, ] <- c(
          obs_min,
          length(spp_unique$spp),
          c1,
          c2,
          c3,
          c4)
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
    # "observed" stored as either "" or "C1"
    if (curr_bcat == "") {curr_bcat <- "C1"}

    curr_bcat_num <- strtoi(substr(curr_bcat,2,2))

    # capture ntot counts
    if (!(curr_spp %in% spp_unique$spp)) {
      #found a new species!
      spp_unique[nrow(spp_unique) + 1, ] <- c(
        curr_spp,
        curr_bcat,
        curr_bcat_num)
    } else {
      # spp already observed in the block, check to see if the bcat is better
      #get current spp row data
      exist_spp_row <- spp_unique[][spp_unique["spp"] == curr_spp]

      #check if bcat is better than previous observation
      if (curr_bcat_num > exist_spp_row[3]) {
        #better bcat
        spp_unique["bcat"][spp_unique["spp"] == curr_spp] <- 
          curr_bcat
        spp_unique["bcat_num"][spp_unique["spp"] == curr_spp] <- 
          curr_bcat_num
      }

    } #end counting breeding codes
  } #end loop


  #add final data point
  c1 <- length(spp_unique["spp"][spp_unique["bcat"] == "C1"])
  c2 <- length(spp_unique["bcat"][spp_unique["bcat"] == "C2"])
  c3 <- length(spp_unique["bcat"][spp_unique["bcat"] == "C3"])
  c4 <- length(spp_unique["bcat"][spp_unique["bcat"] == "C4"])
  # print(c(obs_min,length(spp_unique$spp),c1,c2, c3, c4))
  spp_acc[nrow(spp_acc) + 1, ] <- c(
    obs_min,
    length(spp_unique$spp),
    c1,
    c2,
    c3,
    c4
  )

  spp_tot <- nrow(spp_unique)
  # spp_tot_half <- spp_tot * 0.5
  hrs_convert <- 60.0
  hrs_total = obs_min / hrs_convert

  ### Revised code by S. Pearson starts here ###
  # add field (c2_4) 'coded spp' = sum of C2, C3, C4
  spp_acc$c2_4 <- apply(spp_acc[, 4:6], 1, sum)
  # 'coded_spp_25perc' = y position of horizontal line for
  #      25% threshold of 'coded spp' (with codes)
  # print(sprintf("coded gt 55 %d", max(spp_acc$c2_4) > 55)) # TESTING
  if (max(spp_acc$c2_4) > 55) {
    coded_spp_25perc <- trunc(max(spp_acc$c2_4) * 0.25)
  } else {
    coded_spp_25perc <- 14
  }	# close if else

  # set y-axis maximum to be >=60
  ymax <- max(spp_acc$all)	# set to maximum number of species
  if (ymax < 60){
    ymax <- 60
  }	# unless <60
  # print(sprintf("max poss %i", coded_spp_25perc)) #TESTING
  # plot the data (lines without smoothing)
  plot_response <- ggplot(data = spp_acc, aes((min), all)) +
    geom_hline(yintercept = (55), linetype = 2) +			# min coded spp
    geom_hline(yintercept = (coded_spp_25perc), linetype = 2) +	# 25% coded spp
    geom_line(aes(y = all, color = "a"), linewidth = 1.2) +
    geom_line(aes(y = c2_4, color = "b"), linewidth = 1.2) +
    geom_line(aes(y = c4, color = "c"), linewidth = 1.2) +
    geom_line(aes(y = c3, color = "d"), linewidth = 1.2) +
    geom_line(aes(y = c2, color = "e"), linewidth = 1.2) +
    scale_colour_manual(
      name = "Breeding status",
      values = c(
        "a" = "#aaaaaa",
        "b" = "#2a3b4d",
        "c" = "#ff1a1a",
        "d" = "#ffbf00",
        "e" = "#ccccff"
      ),
      labels = c(
        "All Species",
        "Coded Species",
        "Confirmed",
        "Probable",
        "Possible"
      )
    ) +
    geom_text(
      aes(x = 5, y = 18,
      label = sprintf("25 pct goal (%i spp)", 18)
      # aes(5, coded_spp_25perc),
      # label = sprintf("25 pct goal (%i spp)", coded_spp_25perc),
      # vjust = -0.5,
      # hjust = +0.00
      )
    ) +
    geom_text(
      aes(5, 55), 
      label = "Minimum coded species",
      vjust = -0.5,
      hjust=+0.00
    ) +
    xlim(0, obs_min) +
    ylim(0, ymax) +
    ylab("Count of Species") +
    xlab("Observation Time")
  ### end of revised code from S. Pearson ###

  #figure out how to provide multiple return data
  # plot = accumulation plot output
  # spp_uniques = list of species found
  # spp_acc + Number of unique spp found
  response <- list(
    "plot" = plot_response,
    "spp_unique" = spp_unique,
    "spp_acc_data" = spp_acc[, 1:6],		# excludes spp_acc$c2_4, SMP
    "hrs_total" = hrs_total
  )

  return(response)

}
