
# This file will move your .db solution files (created from PLEXOS solution .zip files using process_folder() in the rplexos library)
# It will move individual or partitioned files that start in one directory into individual folders containing all files making up each annual run.
# This format is required for the query scripts to successfully query partitions that make up a year and stitch the data together into one annual run of data. 
# This script is useful if you have several partitioned runs making up each annual solution. In that case, all partitioned solutions must be inside their own individual 
# folder to use the query script. However, it is easiest to start with all PLEXOS .zip solution files in one location so the process_folder() function of the rplexos library
# can be used to creat the .db SQLite solution databases. After that, this script will then move the partitioned files of each annual fun automatically into the correct location,
# while creating the necessary folders at the same time. 

# Set working directory where solution files are contained
setwd('//nrelqnap01d/plexos/projects/IM3/PLEXOS_database/Solutions/')

# Either point folders to empty ('') if all solutions are in the working directory. If there are multiple scenarios (folders), that each contain partitioned solutions for many years for that 
# respective scenario, folders can then point to the individual folder to process. In other words, if all solution files are contained in a folder called "Solutions", either the working 
# directory can be set to "Solutions", or it can be set one level up, and then folders can be set to = "Solutions"
folders = 'Fuel Price Solutions'
folders = ''

for ( i in folders) {
  files = grep('*.db', list.files(i), value=T)
  files.trunc = unique(gsub('_6mo.*', '', files))

  for ( j in files.trunc ) {
    dir.create(paste0(i, '/', j))
    idx = grep(j, files)
    
    for (k in idx) {
      file.rename(paste0(i, '/', files[k]), paste0(i, '/', j, '/', files[k]))
    }
  }
}
