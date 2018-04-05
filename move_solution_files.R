
setwd('//nrelqnap01d/plexos/projects/im3/PLEXOS_database/Solutions/IM3_2010_db_7.3_R04_batch/')

folders = list.files()

# Move solution zip files
for ( i in folders ) {
  
  run = gsub('DA_IM3_year_', 'Model DA_IM3_year_', i)
  run = paste0(run, ' Solution')

  from = paste0('//nrelqnap01d/plexos/projects/im3/PLEXOS_database/Solutions/IM3_2010_db_7.3_R04_batch/', i, '/', run)
  from = paste0(from, '/', run, '.zip')
  
  to = paste0('//nrelqnap01d/plexos/projects/im3/PLEXOS_database/Solutions/IM3_2010_db_7.3_R04_batch/', run, '.zip')
  
  file.rename(from, to)  
    
  print(i)
  
}

# Move solution log files
for ( i in folders ) {
  
  run = gsub('DA_IM3_year_', 'Model DA_IM3_year_', i)
  run = paste0(run, ' Solution')
  
  from = paste0('//nrelqnap01d/plexos/projects/im3/PLEXOS_database/Solutions/IM3_2010_db_7.3_R04_batch/', i, '/', run)
  from = paste0(from, '/', grep('Log.txt', list.files(from), value=TRUE))
  
  to = paste0('//nrelqnap01d/plexos/projects/im3/PLEXOS_database/Solutions/IM3_2010_db_7.3_R04_batch/', run, '_Log.txt')
  
  file.rename(from, to)  
  
  print(i)
  
}
