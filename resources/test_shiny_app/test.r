
library("ggplot2")

ncba_blue <- "#2a3b4d"
ncba_half_blue <- "#B8C5D3"
checklists_found <- 0
ncba_failed <- "#dd0000"
ncba_success <- "#43AA8B"
ncba_white <- "#ffffff"
ncba_gray <- "#aaaaaa"


    criteria <- data.frame(
      type = c("Num Coded", "Pct Confirmed", "Pct Possible", "Hours"),
      pct = c(1.1, 0.75, 1.2, 1.1),
      pct_labels = c("60 Coded", "19% Confirmed", "30% Possible", "22 Hours")
    )
    criteria$type <- factor(criteria$type, levels = criteria$type)
    # criteria$pct_labels <- factor(criteria$pct_labels, levels = criteria$pct_labels)
    ggplot(
      data = criteria,
      aes( 
        x = type,
        y = pct,
        fill = as.factor(pct)
        )
    ) +
    geom_bar(
      stat = "identity",
      width = 1
      ) +
    scale_fill_manual(
      values = c("#dd0000", "#2a3b4d", "#dd0000", "#2a3b4d")
    ) +
    theme(
      legend.position = "none",
      panel.background = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_blank()
      # axis.text.x = element_text(size = 10)
      ) +
    scale_y_continuous(
      expand = expansion(mult = c(0 , 0))
    ) +
    geom_text(
      # data = criteria,
      # aes(x = factor(type)),
      aes(

        label = criteria$pct_labels,
        y = 0.5
      ),
      angle = 90,
      color = "white"
    ) +
    geom_hline(yintercept = 1, col = ncba_gray)

    # ggsave(g, height = 1, width = 1)


