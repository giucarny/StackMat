
# Prior recode of Q25 # - - - - - - - - - - - - - - - - - -


data.frame(party_name = c('Democratic Party', 'Northern League', 'Five Star Movement'),
           Q7 = c(1501, 1503, 1504),
           Q25 = c(1502, 1504, 1505)) %>%
  as_tibble()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Title: Script for StackMat (Explanation of recoding procedure, various code chunks) 
# Author: G.Carteny
# last update: 2021-06-01
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# Explanation of Party Identification Variables Recoding # ============================================

Q25_Q26_df <- 
  tibble(respid = c(1:5), 
             Q25 = c(138, 139, 140, 0, NA),
             Q26 = c(3,2,1,0, NA))


# Explain Q25 - - - - - - - - - - - - - - - - - - - - - - - -
Q25_rec_df <- cbind(Q25_Q26_df,
      data.frame(Q25_138=c(1, 0, 0, 0, NA),
                 Q25_139=c(0, 1, 0, 0, NA),
                 Q25_140=c(0, 0, 1, 0, NA))) %>%
  as_tibble()


Q25_rec_df_st <- 
  genstacks(idvar='respid', data = Q25_rec_df, stubs = 'Q25_', keepvar = 'Q25') %>%
  dplyr::mutate(Q25_stacked = Q25_) %>%
  dplyr::select(respid, stack, Q25, Q25_stacked) 

# Explain Q26 - - - - - - - - - - - - - - - - - - - - - - - -
Q26_rec_df <- cbind(Q25_Q26_df,
      data.frame(Q26_138=c(3, 0, 0, 0, NA),
                 Q26_139=c(0, 2, 0, 0, NA),
                 Q26_140=c(0, 0, 1, 0, NA))) %>%
  as_tibble()


Q26_rec_df_st <- 
  genstacks(idvar='respid', data = Q26_rec_df, stubs = 'Q26_', keepvar = 'Q26') %>%
  dplyr::mutate(Q26_stacked = Q26_) %>%
  dplyr::select(respid, stack, Q26, Q26_stacked) 

