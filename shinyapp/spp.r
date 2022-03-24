#species functions


breeding_codes_key <- read.csv("input_data/breeding_codes.csv")
print(head(breeding_codes_key))
# ------------------------------------------------------------------------------
# FROM - ADD CREDIT HERE!
breeding_boxplot <- function(species, ebird, pallet, out_pdf, no_plot_codes,
                             lump, drop, cex.x.axis = 0.9, cex.y.axis = 0.8) {

 # print(ebird)
  # Produces a boxplot of breeding codes over calendar day.
  # derived from
  #
  # Description:
  #   Produces a boxplot of breeding codes with some customization options.
  #     Copied from https://github.com/ngwalton/wbba_tools.
  #
  # Arguments:
  # species -- common name of the species
  # data -- data frame of ebird or NCBA data
  # pallet -- choose a named RColorBrewer pallet (multiple colors), or a single color (name
  #   or hex); see brewer.pal.info for list and display.brewer.all() to view all
  #   pallets
  # out_pdf -- path and name where to save an output pdf.  Set to NULL if you
  #   don't want to save output.
  # no_plot_codes -- a vector of evidence codes not be plotted. For example,
  #   c("PE", "UN")
  # lump -- a list of named vectors where the vector name is used to place all
  #   codes in the corresponding vector (e.g. 'S = c("S", "S7", "M")' replaces all
  #   "S", "S7", and "M" with "S"). Note that any code that is not already in
  #   variable "codelevels" in function "chronplot" (below) will need to be added
  #   there.
  # drop -- TRUE or FALSE whether to include unreported codes in the plot


  # Data prep
  # put all dates within the same year -- ignores leap year
  ebird$OBSERVATION_DATE <- sub("^20\\d\\d", "2016", ebird$OBSERVATION_DATE)

  # remove white space from evidence codes
  ebird$BREEDING_CODE <- trimws(ebird$BREEDING_CODE)

  # lump evidence codes if lump has been set
  if (is.null("lump") == FALSE) {
    for (i in seq_along(lump)) {
      indx <- ebird$BREEDING_CODE %in% lump[[i]]
      ebird[indx, "BREEDING_CODE"] <- names(lump)[i]
    }
  }

  # remove unneeded evidence codes
  if (is.null("no_plot_codes") == FALSE) {
    ebird <- ebird[! ebird$BREEDING_CODE %in% no_plot_codes, ]
  }

  # rename columns because ebird names are long
  cols <- c("COMMON_NAME", "BREEDING_CODE", "OBSERVATION_DATE")
  newnames <- c("name", "code", "obsdate")
  ebird <- ebird[ebird$COMMON_NAME == species, cols]
  names(ebird) <- newnames

  # make obsdate a date object
  ebird$obsdate <- as.Date(ebird$obsdate, "%Y-%m-%d")

  # set order that box plots will be plotted.
  # http://stackoverflow.com/questions/19681586/ordering-bars-in-barplot
  # this will be the order that codes are plotted in.
  # this vector will need updating if any new codes are introduced via "lump".
  codelevels <- c("H", "S", "S7", "M", "T", "P", "C", "B", "CN", "NB", "A", "N",
                  "DD", "ON", "NE", "FS", "CF", "NY", "FY", "FL", "PE", "UN",
                  "F", "", "O", "NC")

  if (! all(ebird$code %in% codelevels)) {
    warn <- paste("Not all eBird codes (BREEDING_CODE) for",
                  species, "are in codelevels")
    warning(warn)
  }

  # associate colors with codelevels
  if (pallet %in% rownames(brewer.pal.info)) {
    n <- brewer.pal.info[pallet, "maxcolors"]
    codecolors <- colorRampPalette(brewer.pal(n, pallet))(length(codelevels))
  } else {
    codecolors <- rep(pallet, length(codelevels))
  }

  names(codecolors) <- codelevels

  # used droplevels so that codes that where not observed are not plotted;
  # remove droplevels if you'd like unobserved codes to be included on the plot
  if (drop == TRUE) {
    ebird$code <- droplevels(factor(ebird$code, levels = codelevels,
                                    ordered = TRUE))
  }

  # plot "empty" box plot
  boxplot(obsdate ~ code, horizontal = TRUE, cex.axis = cex.y.axis, xaxt = "n",
          data = ebird, border = "white", main = species, las = 2,
          xlab = "Date", ylab = "Breeding Codes", show.names = TRUE)

  date0 <- round_date(min(ebird$obsdate), "month")
  date1 <- round_date(max(ebird$obsdate), "month")
  labels <- seq(from = date0, to = date1, by = "month")

  if (length(unique(month(ebird$obsdate))) == 1) {
    labels <- c(min(ebird$obsdate), max(ebird$obsdate))
    labels <- unique(labels)  # in case there's only one obs
  } else {
    # limit labels to those within observed range
    int <- interval(min(ebird$obsdate), max(ebird$obsdate))
    labels <- labels[labels %within% int]

    if (nrow(ebird) > 1 && length(labels) == 1) {
      labels <- unique(c(min(ebird$obsdate), max(ebird$obsdate)))
    }
  }

  # use format "%m/%d" for e.g. 06/01
  # use format "%b %d" for e.g. "Aug 23"
  names(labels) <- format(labels, "%b %d")

  vps <- baseViewports()
  pushViewport(vps$inner, vps$figure, vps$plot)

  # label x axis; set font size in gpar(cex = relative_fontsize);
  # grid.text is can be hard to follow but allows for arbitrary rotation of
  # x labels
  grid.text(names(labels), x = unit(labels, "native"), y = unit(-0.7, "lines"),
            just = "right", rot = 65, gp = gpar(cex = cex.x.axis))
  popViewport(3)

  # add tick marks
  axis(1, labels, labels = FALSE)

  # uncomment this to label the x axis a second time for sanity check
  # because grid.text can be difficult to understand
  # axis(1, labels, format(labels, "%m/%d"), col.axis = "red", las = 2)

  # select colors for stripchart
  # should be able to use "codecolors[levels(ebird$code)]",  but
  # that's giving an issue matching the empty string...
  col <- codecolors[names(codecolors) %in% levels(ebird$code)]

  stripchart(obsdate ~ code, data = ebird, vertical = FALSE, method = "jitter",
             pch = 16, col = col, add = TRUE)

  #set boxplot color and partial transparency (where alpha is opacity)
  #run mycol to get the color code, then paste it into the next line
  #mycol <- rgb(245, 245, 245, max = 255, alpha = 0, names = "ltgrayclear")
  #mycol

  # plot
  boxplot(obsdate ~ code, horizontal = TRUE,  col = "#F5F5F500", yaxt = "n",
          xaxt = "n", data = ebird, add = TRUE)
}
