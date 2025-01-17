prettify_anova = function(raw, cap="title"){
  init = raw$ANOVA %>% 
    select(1:6) %>% 
    rename(sig=`p<.05`) %>%
    mutate(
      p = format(round(p,3), nsmall=3),
      F = format(round(F,2), nsmall=2) 
    ) %>% 
    mutate(
      sig=case_when(
        p < 0.001 ~ "***",
        p < 0.01  ~ "**",
        p < 0.05  ~ "*",
        TRUE      ~ ""
      )
    )
  gt(init) %>% 
    tab_header(cap) %>% 
    tab_options(table.align = "left")
}

prettify_sphericity = function(raw, cap="title"){
  init = raw$ANOVA %>% 
    select(1:6) %>%  
    rename(sig=`p<.05`) %>%
    mutate(
      p = format(round(p,3), nsmall=3),
      F = format(round(F,2), nsmall=2) 
    ) %>% 
    mutate(
      sig=case_when(
        p < 0.001 ~ "***",
        p < 0.01  ~ "**",
        p < 0.05  ~ "*",
        TRUE      ~ ""
      )
    )
  gt(init) %>% 
    fmt_number(rows=c(2,3),columns=c(2,3),decimals=c(2)) %>% 
    fmt_number(columns=c(4),decimals=2) %>% 
    tab_header(cap) %>% 
    tab_options(table.align = "left")
}

prettify_sphericity_3_way = function(raw, cap="title"){
  init = raw$ANOVA %>% 
    select(1:6) %>%  
    rename(sig=`p<.05`) %>%
    mutate(
      p = format(round(p,3), nsmall=3),
      F = format(round(F,2), nsmall=2) 
    ) %>% 
    mutate(
      sig=case_when(
        p < 0.001 ~ "***",
        p < 0.01  ~ "**",
        p < 0.05  ~ "*",
        TRUE      ~ ""
      )
    )
  gt(init) %>% 
    fmt_number(rows=contains("fr",TRUE, Effect), columns=c(2,3), decimals=1) %>% 
    tab_header(cap) %>% 
    tab_options(table.align = "left")
}

prettify_means = function(raw, cap="title"){
  gt(raw) %>% 
    fmt_number(decimals=2) %>% 
    tab_header(cap) %>% 
    tab_options(table.align = "left")
}
