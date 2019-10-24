library(rio)
library(magrittr)
library(purrr)

df_stata <- import("D:/ldvl/tmp/ldvl_2017.dta") %>% rename_all(tolower)
df_r = ldvl_2017[, 99:140]

ktra <- function(varname = "agegroup5") {
  print(paste("Processing:", varname))
  
  tbl <- table(df_r[[varname]], df_stata[[varname]]) %>% 
    as.data.frame() %>% 
    filter(Freq != 0)
  
}

ktra_x <- partial(ktra, df_r = ldvl_2017[, 99:140],  df_stata)


name_r <- df_r %>% select_if(is.character) %>% names() %>% tolower()
name_stata <- df_stata %>% names() %>% tolower()

same_names <- name1 %>% intersect(name2)

all_tbl <- map_dfr(same_names, ktra)

export(all_tbl, "check_tbl.xlsx")
