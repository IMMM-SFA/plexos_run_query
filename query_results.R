
# IM3 Run Analysis
# This file queries the partitioned solutions and saves an RData file with all the necessary data for producing the HTML file plots. 

# Load r libraries (most of these are unneccessary when you load data.table fyi)
library(plyr)
library(dplyr)
library(rplexos)
library(reshape2)
library(ggplot2)
library(scales)
library(lubridate)
library(data.table)

# Set working directory
setwd('//nrelqnap02/plexos/projects/im3/PLEXOS_database/Solutions/future_climate/ccsm_rcp45')

# Set locations for PLEXOS solution DB file and CSV Generator Info File
csvName = '//nrelqnap02/plexos/projects/im3/Run Results/gen_name_mapping_aggregated_gas.csv'
csvFileName = read.csv(csvName)

# Load solution db files into R

solution.folders = list.files()
runs = plexos_open(solution.folders)

# Make sure class of solution files has all necessary types for querying (used to not always be the case in previous versions)
attributes(runs)$class = c("rplexos","data.frame","tbl_df")
# runs = runs[1:6,]

# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Set query time range
# Example
# August would be c('2010-08-01', '2010-08-31 23:00:00')
# July through September would be c('2010-07-01', '2010-09-30 23:00:00')

# query.time.range = c("2010-07-01", "2010-08-31 23:00:00") # July and August
# query.time.range = c("2010-07-01", "2010-09-30 23:00:00") # July - September
query.time.range = c('2010-01-01', '2010-12-30 23:00:00') # Annual run
# query.time.range = c('2010-09-06', '2010-09-06 23:00:00') # 1 day test

# Assign filename for RData file that will be saved at the end
rData.filename = 'IM3_future_ccsm_rcp45.RData'
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %


# # % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# # Loop Functions for Annual Runs (non-partitioned runs, when all .db solution files are in the same folder) ----
# # % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# 
# # Adds each row of run results on top of one another. The first column will be the scenario names
# loop_row_join = function(runs, input_function) {
#   
#   for ( i in 1:nrow(runs)) {
#     currentRun = runs[i,]
#     if ( i == 1 ) {
#       totalResults = input_function(currentRun)
#     } else {
#       results = input_function(currentRun)
#       totalResults = rbind(totalResults, results, fill=TRUE)
#     }
#     
#     print(paste('Run: ', i, sep='')) 
#   }
#   return(totalResults)
# }
# 
# # Adds columns of run data next to each other, only adding the run data and not duplicating things like time, name, etc.
# # The column names will be the scenario names
# loop_column_join = function(runs, input_function) {
#   
#   for ( i in 1:nrow(runs)) {
#     currentRun = runs[i,]
#     if ( i == 1 ) {
#       totalResults = input_function(currentRun)
#     } else {
#       results = input_function(currentRun)
#       totalResults = merge(totalResults, results, all=TRUE)
#     }
#     
#     print(paste('Run: ', i, sep='')) 
#   }
#   return(totalResults)
# }

# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Loop Functions for 6 month partitioned runs (each set of partitioned runs are contained in their own folder) ----
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Adds each row of run results on top of one another. The first column will be the scenario names
loop_row_join = function(runs, input_function) {
  
  for ( i in unique(runs$scenario) ) {
    currentRun = runs[runs$scenario == i,]
    if ( i == unique(runs$scenario)[1] ) {
      totalResults = input_function(currentRun)
    } else {
      results = input_function(currentRun)
      totalResults = rbind(totalResults, results, fill=TRUE)
    }
    
    print(paste('Run: ', i, sep='')) 
  }
  return(totalResults)
}

# Adds columns of run data next to each other, only adding the run data and not duplicating things like time, name, etc.
# # The column names will be the scenario names
loop_column_join = function(runs, input_function) {
  
  for ( i in unique(runs$scenario) ) {
    currentRun = runs[runs$scenario == i,]
    if ( i == unique(runs$scenario)[1] ) {
      totalResults = input_function(currentRun)
    } else {
      results = input_function(currentRun)
      totalResults = merge(totalResults, results, all=TRUE)
    }
    
    print(paste('Run: ', i, sep='')) 
  }
  return(totalResults)
}



# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Query functions ----
# This section start the functions that perform queries of the solution databases
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %


# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Annual Constraint Violations --------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

im3_run_violations = function(database) {
  
  # Assign scenario names
  scenario = data.table(Scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario))))
  
  # Calculate unserved energy and # of hours with unserved energy
  unserved.energy = data.table(query_interval(database, 'Zone', 'Unserved Energy', time.range = query.time.range ))[, .(time, name, value)]
  unserved.energy = unserved.energy[, .(value = sum(value)), by=time]
  hours.ue = nrow(unserved.energy[value>0,])
  unserved.energy = sum(unserved.energy$value)
  
  # Calculate Hydro monthly energy violation (> 1 kW to account for weird rounding errors)
  constraint.violation = data.table(query_interval(database, 'Constraint', 'Violation', time.range = query.time.range ))[, .(time, name, value)]
  constraint.violation = constraint.violation[grep('GenMaxEne_', constraint.violation$name), ]
  constraint.violation[value < 0.000001, value := 0 ]                             
  constraint.violation = constraint.violation[ , .(value = sum(value)), by=time]
  hours.hydro.violation = nrow(constraint.violation[value>0,])   
  constraint.violation = sum(constraint.violation$value)

  # Calculate total line flow violations and # of hours with line flow violations
  line.flow.violation = data.table(query_interval(database, 'Line', 'Violation', time.range = query.time.range ))[, .(time, name, value)]
  line.flow.violation[, value := abs(value)]
  line.flow.violation = line.flow.violation[, .(value = sum(value)), by=time]
  hours.line.flow.violation = nrow(line.flow.violation[value>0,])
  line.flow.violation = sum(line.flow.violation$value)
  
  # Calculate total reserve shortage and the number of hours with reserve shortges (MWh)
  reserve.shortage = data.table(query_interval(database, 'Reserve', 'Shortage', time.range = query.time.range ))[, .(time, name, value)]
  reserve.shortage = reserve.shortage[, .(value = sum(value)), by=time]
  hours.reserve.shortage = nrow(reserve.shortage[value>0,])
  reserve.shortage = sum(reserve.shortage$value)

  # Calculate total violation costs for each of these constraints
  hydro.cost = data.table(sum_month(database, 'Constraint', 'Penalty Cost', time.range = query.time.range ))[, .(name, value)]
  hydro.cost = hydro.cost[grep('GenMaxEne_', hydro.cost$name), ]
  hydro.cost = sum(hydro.cost$value)

  # Millions of dollars
  line.flow.cost = data.table(sum_month(database, 'Line', 'Violation Cost', time.range = query.time.range, columns='property'))[, .(value)]

  # Thousands of dollars
  reserve.shortage.cost = data.table(sum_month(database, 'Reserve', 'Shortage', columns='property', time.range = query.time.range ))$value * 500
  
  # Dollars  
  unserved.energy.cost = unserved.energy*100000

  # Construct final row of data
  violations = cbind(scenario, unserved.energy, hours.ue, constraint.violation, hours.hydro.violation, line.flow.violation, hours.line.flow.violation, 
                     reserve.shortage, hours.reserve.shortage, hydro.cost, line.flow.cost, reserve.shortage.cost, unserved.energy.cost)
  # Set column names
  names(violations) = c('Scenario', 'Unserved Energy (MWh)', 'Hours with Unserved Energy', 'Hydro Energy Violation (kWh)', 'Hours with Hydro Violation',  
                        'Line Flow Violation (MWh)', 'Hours with Line Flow Violation', 'Reserve Shortage (MWh)', 'Hours with reserve shortage', 'Hydro violation cost (M$)',
                        'Line Flow Violation Cost (M$)', 'Reserve Shortage Cost (M$)', 'Unserved Energy Cost ($)')
  # Return results
  return(violations)

}

# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Annual results ---------------------------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

im3_annual_results = function(database) {
  
  # Assign scenario name
  scenario = data.table(Scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario))))
  
  #  % % % % % % % % % % % % % % % % % % % % % % % % % %
  # Total Generation all of WECC 
  totalGen = sum_month(database, 'Generator', 'Generation', columns='property', time.range = query.time.range )$value
  
  #  % % % % % % % % % % % % % % % % % % % % % % % % % %
  # Total production cost -------------------------------------
  emissionsCost = sum_month(database, col = 'Generator', prop = 'Emissions Cost', columns = 'property', time.range = query.time.range )$value * 1000
  fuelCost = sum_month(database, col = 'Generator', prop = 'Fuel Cost', columns = 'property', time.range = query.time.range )$value * 1000
  sandsCost = sum_month(database, col = 'Generator', prop = 'Start & Shutdown Cost', columns = 'property', time.range = query.time.range )$value * 1000
  VOMCost = sum_month(database, col = 'Generator', prop = 'VO&M Cost', columns = 'property', time.range = query.time.range )$value * 1000
  totalCost = emissionsCost + fuelCost + sandsCost + VOMCost
  
  #  % % % % % % % % % % % % % % % % % % % % % % % % % %
  # Total Load 
  load_im3 = sum(sum_month(database, 'Zone', 'Load', time.range = query.time.range )$value) - sum(sum_month(database, 'Zone', 'Pump Load', time.range = query.time.range )$value)
  
  #  % % % % % % % % % % % % % % % % % % % % % % % % % %
  # Total Curtailment
  available = data.table(sum_month(database, 'Generator', 'Available Energy', time.range = query.time.range ))
  available = merge(available, csvFileName, by = 'name')
  available = available[Type %in% c('PV', 'Wind'), .(value = sum(value)), by=Type]
  
  generation = data.table(sum_month(database, 'Generator', 'Generation', time.range = query.time.range ))
  generation = merge(generation, csvFileName, by='name')
  generation = generation[Type %in% c('PV', 'Wind'), .(value = sum(value)), by=Type]
  
  curtailed = available[,value] - generation[,value]
  curtailed_sum = sum(curtailed)
  
  curtailed_percent = curtailed / load_im3 * 100
  curtailed_percent_total = curtailed_sum / load_im3 * 100
  
  #  % % % % % % % % % % % % % % % % % % % % % % % % % %
  # Total line congestion 
  congestion = data.table(sum_month(database, 'Line', 'Hours Congested', time.range = query.time.range ))[, .(name, value)]
  congestion = congestion[value > 0, .(name, value)]
  congested.lines = nrow(congestion)
  
  #  % % % % % % % % % % % % % % % % % % % % % % % % % %
  # Total reserve provision and shortage
  reserve.provision = data.table(sum_month(database, 'Reserve', 'Provision', columns='property', time.range = query.time.range ))$value
  reserve.shortage = data.table(sum_month(database, 'Reserve', 'Shortage', columns='property', time.range = query.time.range ))$value
  
  # % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
  # Fuel Offtake 
  fuel = data.table(sum_month(database, 'Fuel', 'Offtake', time.range = query.time.range ))
  fuel.coal = fuel[grep('Coal_', fuel$name), .(Coal.Offtake = sum(value))]
  fuel.ng   = fuel[grep('NG_', fuel$name), .(NG.Offtake = sum(value))]
  
  # Construct row of all queried data
  annual_results = cbind(scenario, totalCost, load_im3, totalGen, curtailed_sum, curtailed_percent_total, 
                         congested.lines, reserve.provision, reserve.shortage, fuel.coal, fuel.ng)
  # Set column names
  names(annual_results) = c('Scenario', 'Cost ($)', 'Load (GWh)', 'Generation (GWh)', 'Curtailment (GWh)', 'Curtailment %',
                            '# Congested Lines', 'Reserve Provision (GWh)', 'Reserve Shortage (GWh)', 'Coal Offtake (GBtu)', 'NG Offtake (GBtu)')
  # Return results
  return(annual_results)
}


# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Gen by type stats (percent of total generation by each type) ----------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Query generation, map generator type to generator name, sum generation by type, figure out percentage of overall generation by each type
gen_by_type = function(database) {
  
  # Scenario name
  name = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  # query and data manipulation
  yearlyGen = data.table(sum_month(database, 'Generator', 'Generation', time.range = query.time.range ))[, .(name, value)]
  yearlyGen = merge(yearlyGen, csvFileName, by = 'name')
  total_gen = yearlyGen[, .(value = sum(value)), by=Type]
  names(total_gen) = c('Type', 'Gen')
  total_gen[, Gen := Gen / sum(Gen) * 100]
  total_gen$Scenario = name
  total_gen = dcast(total_gen, Scenario~Type, value.var='Gen')
  
  # Return results
  return(total_gen)
}


# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Capacity Factor Stats ---------------------------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Query installed capacity and generation. Figure out percentage generation of total possible generation, for each type.
cap_factor = function(database) {
  
  # Scenario name
  name = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  # query and data manipulation
  cap.factor = data.table(query_month(database, 'Generator', c('Installed Capacity', 'Generation'), time.range = query.time.range ))[, .(name, time, value, property)]
  cap.factor = merge(cap.factor, csvFileName, by = 'name')
  cap.factor$Month = month(cap.factor$time)
  hours.per.month = data.table(Month = 1:12, hours = c(744, 672, 744, 720, 744, 720, 744, 744, 720, 744, 720, 720))
  cap.factor = merge(cap.factor, hours.per.month, by='Month')
  cap.factor[property == 'Installed Capacity', value := value * hours / 1000]
  cap.factor = dcast(cap.factor, Type~property, value.var='value', fun.aggregate=sum)
  cap.factor$Cap.Factor = cap.factor$Generation / cap.factor$`Installed Capacity` * 100
  cap.factor$Scenario = name
  cap.factor = dcast(cap.factor, Scenario~Type, value.var='Cap.Factor')
  
  return(cap.factor)
}


# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# TX Congestion ---------------------------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Query # of hours each line is congested, and sum that number for each time period
tx_congestion = function(database) { 
  
  # Scenario name
  scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  # Query and data manipulation  
  congestion = data.table(query_interval(database, 'Line', 'Hours Congested', time.range = query.time.range ))[, .(time, name, value)]
  congestion = congestion[, .(value = sum(value)), by=time]
  names(congestion) = c('time', scenario)
  
  return(congestion)
  
}

# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Hourly Reserve Shortages ---------------------------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Query total reserve shortage for all reserve objects at each time period. 
reserve_shortage = function(database) { 
  
  # Scenario name
  scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  # Query and data manipulation
  shortage = data.table(query_interval(database, 'Reserve', 'Shortage', time.range = query.time.range ))[, .(time, name, value)]
  shortage = shortage[, .(value = sum(value)), by=time]
  colnames(shortage) = c('time', scenario)
  
  return(shortage)
  
}

# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Hourly TX Flow violation ---------------------------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Query line violations at each interval, then violations of all lines for each time period.
tx_flow_violation = function(database) { 
  
  # Scenario name
  scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  # Query and data manipulation
  flow.violation = data.table(query_interval(database, 'Line', 'Violation', time.range = query.time.range ))[, .(time, name, value)]
  flow.violation[, value := abs(value)]
  flow.violation = flow.violation[, .(value = sum(value)), by=time]
  colnames(flow.violation) = c('time', scenario)
  
  return(flow.violation)
}
  
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Hourly Unserved Energy ---------------------------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Query unserved energy in each zone at each period, then sum all the zones for each time period.
unserved_energy = function(database) { 
  
  scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  # Query and data manipulation
  unserved.energy = data.table(query_interval(database, 'Zone', 'Unserved Energy', time.range = query.time.range ))[, .(time, name, value)]
  unserved.energy = unserved.energy[, .(value = sum(value)), by=time]
  colnames(unserved.energy) = c('time', scenario)
  
  return(unserved.energy)
}
  
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Hourly Hydro Energy Violation ---------------------------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Query hydro energy constraint violations and report the total violation at each time period.
hydro_violation = function(database) { 
  
  scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  # Query and data manipulation
  hydro.violation = data.table(query_interval(database, 'Constraint', 'Violation', time.range = query.time.range ))[, .(time, name, value)]
  hydro.violation = hydro.violation[grep('GenMaxEne_', hydro.violation$name), ]
  hydro.violation[value < 0.001, value := 0]
  hydro.violation = hydro.violation[, .(value = sum(value)), by=time]
  colnames(hydro.violation) = c('time', scenario)
  
  return(hydro.violation)
}


# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Electricity Prices ---------------------------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Query the average price for each time period, for the entire year, for each region
prices_annual_region_avg = function(database) {
  
  name = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  prices = data.table(query_interval(database, 'Region', 'Price', time.range = query.time.range ))[, .(name, time, value)]
  avg.price = prices[, .(Price = mean(value)), by=name]
  colnames(avg.price) = c('region', name)
  
  return(avg.price)
}

# Query the average price for each time period, doing a generation-weighted average, for each region.
prices_annual_weighted_region_avg = function(database) {
  
  name = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  prices = data.table(query_interval(database, 'Region', 'Price', time.range = query.time.range ))[, .(name, time, value)]
  setnames(prices, 'value', 'price')
  gen = data.table(query_interval(database, 'Region', 'Generation', time.range = query.time.range ))[, .(name, time, value)]
  setnames(gen, 'value', 'gen')
  
  prices = merge(prices, gen)
  prices[, price.gen := price*gen]
  prices = prices[, .(price.gen = sum(price.gen)), by=.(name, time)]
  
  gen = gen[, .(gen = sum(gen)), by=.(name, time)]
  
  prices = merge(prices, gen)
  prices[, weighted.price := price.gen / gen, by=.(name, time)]
  
  avg.price = prices[, .(price = mean(weighted.price)), by=name]
  avg.price[is.na(price), price := 0]
  
  colnames(avg.price) = c('region', name)
  
  return(avg.price)
  
}

# Query the average interval price for each interval (time period), across all regions.
prices_interval_avg = function(database) {
  
  name = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  prices = data.table(query_interval(database, 'Region', 'Price', time.range = query.time.range ))
  prices = prices[, .(name, time, value)]  
  avg.price = prices[, .(Price = mean(value)), by=time]
  colnames(avg.price) = c('time', name)
  
  return(avg.price)
  
}

# Query the average annual price across all intervals and regions
price_annual_avg_fromRegionPrice = function(database) {
  name = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  prices = data.table(query_interval(database, 'Region', 'Price', time.range = query.time.range ))[, .(name, time, value)] 
  prices = prices[, .(Price = mean(value)), by=time]
  avg.price = data.table(Scenario = name, `Price  ($/MWh)` = mean(prices$Price))
  return(avg.price)
}

# Query the avearage annual price across all intervals and zones
price_annual_avg_fromZonePrice = function(database) {
  scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  prices = data.table(query_interval(database, 'Zone', 'Price', time.range = query.time.range ))[, .(name, time, value)]
  prices = prices[, .(Price = mean(value)), by=time]
  avg.price = data.table(Scenario = scenario, `Price  ($/MWh)` = mean(prices$Price))
  return(avg.price)
}

# Query the total number of hours any zone price is above 100 $/MWh
hours_over_100 = function(database) {
  
  scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  prices = data.table(query_interval(database, 'Zone', 'Price', time.range = query.time.range ))[, .(name, time, value)]
  hours = length(unique(prices[value > 100, time]))
  hours = data.table(Scenario = scenario, `# Hours over 100 $/MWh` = hours)
  return(hours)
  
}

# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Percent of reserve by generation type ---------------------------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# query the percent of total reserve provisions coming from each generation type
annual_reserve_by_type = function(database) {
  
  # Scenario name
  scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  # Query and data manipulation
  reserve.type = data.table(sum_month(database, 'Reserve.Generators', 'Provision', time.range = query.time.range ))[, .(name, value)]
  reserve.type = merge(reserve.type, csvFileName, by='name')
  reserve.type = reserve.type[, .(value=sum(value)), by=Type]
  reserve.type[, value := value/sum(value)*100]
  reserve.type$Scenario = scenario
  reserve.type = dcast(reserve.type, Scenario~Type)
  
  return(reserve.type)
  
}


# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Reserve provision as percent of generator generation + reserves --------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Query the percent of total generation + reserves for each generation type, that is being held as reserve provision
generation_reserves = function(database) {
  
  # Scenario name
  scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  # Reserve provision data
  reserve.type = data.table(sum_month(database, 'Reserve.Generators', 'Provision', time.range = query.time.range ))[, .(name, value)]
  reserve.type = merge(reserve.type, csvFileName, by='name')
  reserve.type = reserve.type[, .(provision=sum(value)), by=Type]
  
  # Generation data
  generation = data.table(sum_month(database, 'Generator', 'Generation', time.range = query.time.range ))[, .(name, value)]
  generation = merge(generation, csvFileName, by='name')
  generation = generation[, .(generation = sum(value)), by=Type]
  
  # Calculating percent of generation and reserves that is reserves
  reserve.percent = merge(reserve.type, generation, by='Type', all.y=FALSE)
  reserve.percent$total = reserve.percent$provision+reserve.percent$generation
  reserve.percent = reserve.percent[, .(Type, percent = (provision / total * 100))]
  
  reserve.percent$Scenario = scenario
  reserve.percent = dcast(reserve.percent, Scenario~Type, value.var='percent')
  
  return(reserve.percent)
  
}

# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Hours coal not at max cap---------------------------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Query the number of hours that the coal fleet is not operating at max capacity (lower efficiency)
hours_not_max_all_coal = function(database) {
  
  # Scenario name
  scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  # Generation and max capacity queries, then data manipulation and rearranging
  gen.data = data.table(query_interval(database, 'Generator', c('Generation', 'Max Capacity'),  time.range = query.time.range, 
                                       filter = list(name = subset(csvFileName, Type == 'Coal')$name) ))[, .(time, name, value, property)]
  gen.data = data.table(dcast(gen.data, time+name~property, value.var='value'))
  gen.data[, GenDiff := `Max Capacity`*.9 - Generation]
  gen.data[GenDiff<0,GenDiff := 0]
  gen.data[, c('Generation', 'Max Capacity') := NULL]
  gen.data[GenDiff>0, GenDiff := 1]
  gen.data = gen.data[, .(GenDiff = sum(GenDiff)), by=time]
  colnames(gen.data) = c('time', scenario)
  
  return(gen.data)
  
}

# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Generation dispatch stack by region ---------------------------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Total generation in each region, plotted as dispatch stack showing individual generation types
gen_by_type_region = function(database) {
  
  # Scenario name
  scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  # Query and data manipulation
  gen = data.table(sum_month(database, 'Generator', 'Generation', time.range = query.time.range))[, .(name, value)]
  gen = merge(gen, csvFileName, by='name')
  gen = gen[, .(value = sum(value)), by=.(Type, TEPPC.Region)]
  setnames(gen, c('Type', 'TEPPC.Region', scenario))
  
  return(gen)
  
}

# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Reserve Provision by region ---------------------------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Query the regional reserve provision
reserve_provision_region = function(database) {
  
  # scenario name
  scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  # Query and data manipulation
  reserve.type = data.table(sum_month(database, 'Reserve.Generators', 'Provision', time.range = query.time.range ))[, .(name, value)]
  reserve.type = merge(reserve.type, csvFileName, by='name')
  reserve.type = reserve.type[, .(provision=sum(value)), by=.(Type, TEPPC.Region)]
  setnames(reserve.type, c('provision'), c(scenario))
  
  return(reserve.type)
  
}

# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Available Hydro Energy for plotting order ---------------------------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Queries the total available hydro energy, so that the order of available hydro in each year can be determined.
# * This isn't used anymore since PNNL specified which years are wettest, driest, etc. *
available_hydro = function(database) {
  
  # Scenario name
  scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  # Query and data manipulation
  hydro = data.table(sum_month(database, 'Generator', 'Available Energy'), time.range = query.time.range , filter = list(name = subset(csvFileName, Type == 'Hydro')$name))[, .(value)]
  hydro$scenario = scenario
  hydro = hydro[, .(value = sum(value)), by=scenario]

  return(hydro)
}


# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# TEPPC.Region Imports and Exports (GWh) ---------------------------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Query imports and exports between all regions
imports_exports = function(database) {
  
  # Scenario name
  scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  # Query and data manipulation
  imports.exports = data.table(sum_month(database, 'Region', c('Imports', 'Exports'), time.range = query.time.range ))[, .(name, property, value)]
  setnames(imports.exports, 'name', 'Region')
  imports.exports = join(imports.exports, csvFileName[,c('Region', 'TEPPC.Region')], by='Region', match='first')
  imports.exports = imports.exports[, .(value = sum(value)), by=.(property, TEPPC.Region)]
  setnames(imports.exports, 'value', scenario)
  
  return(imports.exports)
}

# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Region Load (GWh) ---------------------------------------------------------------
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Query load for all regions, which is used in regional dispatch plots
load = function(database) {
  
  # Scenario name
  scenario = unique(gsub(' Solution-rplexos.db', '', gsub('.*Model ', '', database$scenario)))
  
  # Query and data manipulation
  annual.load = data.table(sum_month(database, 'Region', 'Load', time.range = query.time.range ))[, .(name, property, value)]
  annual.load[, property := NULL]
  setnames(annual.load, 'value', scenario)
  
  return(annual.load)
}



# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Generate results -----
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
 
# Annual run violations - Unserved Energy, # Hours with Unserved Energy, Hydro Energy Violations, # Hours with Hydro Violation,  Line Flow Violation, # hours with line flow violation
violations = loop_row_join(runs, im3_run_violations)
print('Annual violations - Done')

# Annual results - cost, load, generation, curtailment, line congestion, reserves, fuel offtake
annual.gen.stats = loop_row_join(runs, im3_annual_results)
print('Annual results - Done')

# Generation by type
gen.type.percent = loop_row_join(runs, gen_by_type)
print('Gen by type - Done')

# Avg price for the entire year calculated from region prices ($/MWh)
avg.prices.regions = loop_row_join(runs, price_annual_avg_fromRegionPrice)
print('Avg Region Price - Done')

# Average price for the entire year calculated from zone prices ($/MWh)
avg.prices.zones = loop_row_join(runs, price_annual_avg_fromZonePrice)
print('Avg Zone Price - Done')

# Hours with zone price over 100 $/MWh
hours.high.price = loop_row_join(runs, hours_over_100)
print('High Price Hours - Done')

# Capacity factor for each generation type
cap.factor = loop_row_join(runs, cap_factor)
print('Capacity factor - Done')

# Reserve provision by generator type
reserve.type = loop_row_join(runs, annual_reserve_by_type)
print('Reserve provision by gen type - Done')

# Percent reserve provision of reserve + generation
reserve.gen.percent = loop_row_join(runs, generation_reserves)
print('Percent reserves - Done')

# Hydro available energy
hydro.available = loop_row_join(runs, available_hydro)
print('Hydro vailable energy - Done')

# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Below functions use loop_column_join function
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

# Number of lines congested at each hour
tx.congest = loop_column_join(runs, tx_congestion)
print('Congested lines - Done')

# Average region price for the entire year ($/MWh)
avg.region.price = loop_column_join(runs, prices_annual_region_avg)
print('Avg Region Price - Done')

# Average region price weighted by generation for the entire year ($/MWh)
avg.weighted.region.price = loop_column_join(runs, prices_annual_weighted_region_avg)
print('Avg weighted region price - Done')

# Average price for each hour for the entire year - calculated from region price
avg.interval.price = loop_column_join(runs, prices_interval_avg)
print('Avg interval price - Done')

# Reserve shortage at each hour of the year
reserve.shortages = loop_column_join(runs, reserve_shortage)
print('Hourly reserve shortage - Done')

# Hydro energy violation at each hour of the year
hydro.violation = loop_column_join(runs, hydro_violation)
print('Hourly hydro energy violations - done')

# Unserved energy at each hour of the year
unserved.energy = loop_column_join(runs, unserved_energy)
print('Hourly unserved energy - Done')

# TX Flow violation at each hour of the year
tx.violation = loop_column_join(runs, tx_flow_violation)
print('Hourly TX violations - Done')

# Number of coal gens not at 90% or higher capacity for each hour of the year
coal.not.max = loop_column_join(runs, hours_not_max_all_coal)
print('# Coal gens not at 90% or higher capacity - Done')

# Generation by type and region
gen.type.region = loop_column_join(runs, gen_by_type_region)
print('Gen by type and region - Done')

# Reserve provision by generation type and reserge group
reserve.provision.region = loop_column_join(runs, reserve_provision_region)
print('Reserve provision by type and region - done')

# TEPPC Region Imports and Exports
imports.exports = loop_column_join(runs, imports_exports)
print('Imports and Exports Done')

# Reigon load 
annual.load = loop_column_join(runs, load)
print('Annual Region Load Done')


# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
# Save all results
# % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

save(violations, annual.gen.stats, gen.type.percent, avg.prices.regions, avg.prices.zones, hours.high.price,
     cap.factor, reserve.type, reserve.gen.percent, hydro.available, tx.congest, avg.region.price, avg.weighted.region.price,
     avg.interval.price, reserve.shortages, hydro.violation, unserved.energy, tx.violation, coal.not.max, gen.type.region, 
     reserve.provision.region, imports.exports, annual.load, file=rData.filename)

