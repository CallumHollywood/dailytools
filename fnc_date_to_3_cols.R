



date_to_3_cols <- function(df, col){

  # takes a dataframe, a (date) column
  # splits the column into separate year month date named with prefix
  # drop the original date column

  #NB - THis function assumes (without test) that date = yyyy/mm/dd

  df %>%
    mutate('{{col}}_year'  := str_sub({{col}}, 1, 4)) %>%
    mutate('{{col}}_month' := str_sub({{col}}, 6, 7)) %>%
    mutate('{{col}}_date'  := str_sub({{col}}, 9, 10)) %>%
    select(-{{col}})

}


