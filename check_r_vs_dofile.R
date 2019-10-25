library(rio)
library(magrittr)
library(purrr)

df_stata <- import("D:/ldvl/tmp/ldvl_2017.dta") %>% rename_all(tolower)
df_r = ldvl_2017[, 99:140]
name_r <- df_r %>% select_if(is.character) %>% names() %>% tolower()
name_stata <- df_stata %>% names() %>% tolower()
same_names <- name_r %>% intersect(name_stata)
names(same_names) <- same_names


ktra <- function(varname = "agegroup5") {
  print(paste("Processing:", varname))
  
  tbl <- table(df_r[[varname]], df_stata[[varname]]) %>% 
    as.data.frame() %>% 
    filter(Freq != 0)
  
}

# export 
check_tbl <- map_dfr(same_names, ktra, .id = 'varname') 

check_tbl %>% export("check_tbl.xlsx")

#=============================================================================

table(df_r$train0, df_r$train) %>% as.data.frame() %>% filter(Freq != 0) -> x
table(df_stata$underemployment, df_r$underemployment) %>% as.data.frame() %>% filter(Freq != 0) -> xx

#=============================================================================

ktra_x <- partial(ktra, df_r = ldvl_2017[, 99:140],  df_stata)

df1$prev_nganh_n_c_d %>% table() %>% length()
df2$prev_nganh_n_c_d %>% table() %>% length()
df_stata$prev_nganh_N_C_D %>% table() %>% length()


x <- ldvl_2017 %>% select(prev_occup1, prev_occup2)
y <- df_stata %>% select(prev_occup1, prev_occup2) %>% set_names(paste0(names(.), "2"))
z <- cbind(x, y)

z %>% filter(prev_occup1 != prev_occup12)
