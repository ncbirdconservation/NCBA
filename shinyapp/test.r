

if(!require(ggplot2)) install.packages(
  "ggplot2", repos = "http://cran.us.r-project.org")


if(!require(waffle)) install.packages(
  "waffle", repos = "https://cinc.rud.is")

if(!require(ggwaffle)) install.packages(
  "ggwaffle", repos = "http://cran.us.r-project.org")


#Lake Lure SE example - 2023 Region 9 Report
num_c = 40
num_r = 0
num_p = 16
num_coded = num_c + num_r + num_p
num_o = 21
pct_c = (num_c/num_coded)*100
pct_r = (num_r/num_coded)*100
pct_p = (num_p/num_coded)*100
print(pct_c)
print(pct_r)
print(pct_p)
leg_p = paste0('Possible (', pct_p , '%)')
leg_r = paste0('Probable (', pct_r , '%)')
leg_c = paste0('Confirmed (', pct_c , '%)')
coded_to_go = 55-num_coded
if (coded_to_go > 0) {
  coded_to_go = 0
}
print(coded_to_go)


num_breed_confirm <- paste(
  "Confirmed (C4):", num_c,  " (", format(pct_c, digits=2), "%)")
num_breed_prob <- paste(
  "Probable (C3):", num_r, " (", pct_r , "%)")
num_breed_poss <- paste(
  "Possible (C2):", num_p, " (", pct_p, "%)")

print(num_breed_confirm)

spp_crp <- c(
      leg_p = num_p,
      leg_r = num_r,
      leg_c = num_c
    )
    
# spp_crp <- data.frame(
#     Status = c('Possible', 'Probable', 'Confirmed'),
#     count = c(num_p, num_r, num_c)
# )
# ggplot(
#     data = spp_crp,
#     aes(
#         fill = Status,
#         values = count
#     )
# ) + 
# geom_waffle(
#     n_rows = 5,
#     size = 0.5,
#     colour = '#fffffff'
# ) + 
# scale_fill_manual(values = c('#BF78EB','#7E2AB3','#300C56')) +
# labs(
#     title = "Species Breeding Status",
#     subtitle = paste0( leg_p, leg_r, leg_c)
# )


spp_crp <- c('Possible' = num_p, 'Probable' = num_r, 'Confirmed' = num_c)
waffle(
    spp_crp,
    rows = 5,
    size = 1,
    pad = coded_to_go,
    colors = c('#BF78EB',alpha('#7E2AB3', 1/3),'#300C56'),
    title = "Species Breeding Status",
    legend_pos = "bottom"
) + 
geom_segment(
  aes(
    x = 11.5,
    y = 0,
    xend = 11.5,
    yend = 6,
    ),
  colour = "#2a3b4d",
  size = 1
) + 
annotate(
  geom = "text",
  x = 11.5,
  y = 0,
  label = "minimum coded",
  size = 4,
  hjust = 0
) +
theme(
  plot.margin = unit(c(10,10,10,10),"pt"),
  panel.background = element_rect(
    fill = NA,
    colour = "#2a3b4d",
    linewidth = 1
  ) 
)