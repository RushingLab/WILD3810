WILD3810_colors <- data.frame(name = c("primary", "secondary", "success", "info", "warning", "danger", "light", "dark"),
                              value = c("#446E9B", "#999999", "#3CB521", "#3399F3", "#D47500", "#CD0200", "#eeeeee", "#333333"))
WILD3810_colors$value <- as.character(WILD3810_colors$value)
usethis::use_data(WILD3810_colors, overwrite = TRUE)
