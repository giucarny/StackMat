# Chunks of code for explaining the dataset - - - - - - -


EES2019_it_stacked %>% 
  dplyr::select(respid, party, 
                pid, pid_str, 
                stacked_pid, stacked_pid_str)