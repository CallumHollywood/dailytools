


params_in <- fread('app_data/params_symbols.csv')


date_from_3_cols <- function(df, date_col){
  
  # takes a data frame, colname (string) to parse
  # combines colname_year, _month, _date 
  # mutate newcol to paste to ymd
  # drops colname_year, _month, _date 
  
  date_year <- df %>% 
    select(paste0(date_col, '_year'))
  
  # print(date_year)
  
  date_month <- df %>% 
    select(paste0(date_col, '_month')) %>% 
    rename(month = 1) %>% 
    mutate(month = ifelse(nchar(month) == 1, paste0('0', month), month))
  
  names(date_month) <- paste0(date_col, '_month')
  
  # print(date_month)
  
  date_date <- df %>% 
    select(paste0(date_col, '_date')) %>% 
    rename(date = 1) %>% 
    mutate(date = ifelse(nchar(date) == 1, paste0('0', date), date))
  
  names(date_date) <- paste0(date_col, '_date')
    
  # print(date_date)
  
  date_col_ymd <- cbind(date_year, date_month, date_date) %>%
    rename(
      date_col_year    = 1
      , date_col_month = 2
      , date_col_date  = 3
    ) %>%
      mutate(date_col = paste0(
        date_col_year
        , "-"
        , date_col_month
        , "-"
        , date_col_date
        )) %>%
    select(date_col)

    colnames(date_col_ymd) <- date_col

    date_col_ymd

  df %>%
    select_if(!str_detect(colnames(.), paste0(date_col))) %>%
    bind_cols(date_col_ymd)
  
  
}


date_from_3_cols(params_in, 'end_date')







