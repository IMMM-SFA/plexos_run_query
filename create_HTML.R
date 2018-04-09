
# Load required R libraries 
library(rmarkdown)
library(data.table)
library(RColorBrewer)

# Load saved RData file from the query_results.R file.
load('//nrelqnap02/plexos/projects/im3/Run Results/IM3_future_inm_rcp85.RData')

# Filename to save .HTML file.
output.filename = 'IM3_plots_future_inm_rcp85.html'

# Name of the scenario to produce results for. This should be the folder name, which inside has all the solution files in their own folders (if partitioned), or just inside if not partitioned.
future.scenario = 'inm_rcp85'

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Order to display results when creating plots (these will be across the x-axis)

# Read a file stating the order to plot each years' results. 
plotting.order = fread('//nrelqnap02/plexos/projects/im3/Future_years_dry_to_wet.csv')
plotting.order = rev(as.character(plotting.order[, get(future.scenario)]))
plotting.order = paste0(future.scenario, '_', plotting.order)

# # If using the available hydro data queried from the query_results.R file, use the line below. 
# # Plot according to available hydro generation:
# plotting.order = gsub('DA_IM3_year_', '', hydro.available$scenario[order(hydro.available$value, decreasing=TRUE)])
# plotting.order = gsub('DA_IM3_year_', '', gen.type.percent$Scenario[order(gen.type.percent[,.(Hydro)], decreasing=TRUE)])
  
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Shortening scenario names so they match the names in plotting order above and aren't unnecessarily long.
annual.gen.stats[, Scenario := gsub('.*year_', '', annual.gen.stats$Scenario)]
avg.prices[, Scenario := gsub('.*year_', '', avg.prices$Scenario)]
avg.zone.price[, Scenario := gsub('.*year_', '', avg.zone.price$Scenario)]
hours.high.price[, Scenario := gsub('.*year_', '', hours.high.price$Scenario)]
cap.factor[, Scenario := gsub('.*year_', '', cap.factor$Scenario)]
gen.type.percent[, Scenario := gsub('.*year_', '', gen.type.percent$Scenario)]
reserve.gen.percent[, Scenario := gsub('.*year_', '', reserve.gen.percent$Scenario)]
reserve.type[, Scenario := gsub('.*year_', '', reserve.type$Scenario)]
violations[, Scenario := gsub('.*year_', '', violations$Scenario)]

# Filtering out any scenarios which are not listed in the plotting.order object defined above. This can also be used to only show results for some of the runs.
annual.gen.stats = annual.gen.stats[Scenario %in% plotting.order, ]
avg.prices = avg.prices[Scenario %in% plotting.order, ]
avg.zone.price = avg.zone.price[Scenario %in% plotting.order, ]
hours.high.price = hours.high.price[Scenario %in% plotting.order, ]
cap.factor = cap.factor[Scenario %in% plotting.order, ]
gen.type.percent = gen.type.percent[Scenario %in% plotting.order, ]
reserve.gen.percent = reserve.gen.percent[Scenario %in% plotting.order, ]
reserve.type = reserve.type[Scenario %in% plotting.order, ]
violations = violations[Scenario %in% plotting.order, ]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Set order for generation types to appear in plots, and the color each generaiton type should be. 
gen.order = c('Nuclear', 'Coal', 'Hydro', 'Gas CC', 'Gas CT', 'Steam', 'CHP-QF', 'ICE Gas', 
              'Biomass', 'Geothermal', 'Other', 'Storage', 'CSP', 'PV', 'Wind')

gen.color = c('firebrick', 'gray20', 'lightblue', 'darkolivegreen4', 'lightpink', 'orchid4', 'gray20', 'gray60', 
              'mediumpurple2', 'khaki1', 'mediumpurple3', 'gray45', 'darkorange2', 'goldenrod1', 'steelblue3') 

gen.color = setNames( gen.color, gen.order )

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Set colors to plot all the scenarios as
scenarios = plotting.order
scenario.color = setNames(rainbow(length(scenarios)), scenarios)


# ##########################################################################
# Below is calculations and data manipulation to get the data in the correct format for plotting in the next file
# ##########################################################################

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculate total cost including violations and rank scenarios according to total cost
annual.gen.stats[, `Total Cost ($)` := (`Cost ($)` + violations[,`Unserved Energy Cost ($)`] + violations[,`Reserve Shortage Cost (M$)`]*1000 + 
                                          violations[,`Line Flow Violation Cost (M$)`]*1000000 + violations[, `Hydro violation cost (M$)`]*1000) ]
annual.gen.stats[, Rank := rank(annual.gen.stats$`Total Cost ($)`)]
annual.gen.stats$Scenario = factor(annual.gen.stats$Scenario, levels = plotting.order)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Make penalty data table for penalty cost bar stack
penalty = violations[, .(Scenario, Hydro = `Hydro violation cost (M$)`*1000, Line = `Line Flow Violation Cost (M$)`*1000000, 
                         Reserve = `Reserve Shortage Cost (M$)`*1000, Energy = `Unserved Energy Cost ($)`)]
penalty = melt(penalty, id.vars = c('Scenario'), variable.name='violation', value.name='cost')

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data for fuel offtake stack of coal and NG
fuel.offtake = annual.gen.stats[, .(Scenario, Coal = `Coal Offtake (GBtu)`, NG = `NG Offtake (GBtu)`)]
fuel.offtake = melt(fuel.offtake, id.vars='Scenario', variable.name = 'type')
fuel.offtake$type = factor(fuel.offtake$type, levels = c('NG', 'Coal'))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region Imports and Exports
imports.exports = melt(imports.exports, id.vars=c('property', 'TEPPC.Region'), variable.name='Scenario')
imports.exports = dcast(imports.exports, Scenario+TEPPC.Region~property)
imports.exports$Net.Exports = imports.exports$Exports - imports.exports$Imports
imports.exports[, Scenario := gsub('.*year_', '', imports.exports$Scenario)]
imports.exports = imports.exports[Scenario %in% plotting.order, ]
imports.exports$Scenario = factor(imports.exports$Scenario, levels=plotting.order)



# ##########################################################################
# ##########################################################################
# Duration curve plots

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Rank interval prices for duration curve
prices = melt(all.price, id.var='time', variable.name='Scenario', value.name='Cost')
prices[, interval := rank(-Cost,ties.method="random"), by=Scenario]
prices[, Scenario := gsub('.*year_', '', prices$Scenario)]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Rank coal gen not at max cap for duration curve
coal.gen = melt(coal.not.max, id.var='time', variable.name='Scenario')
coal.gen[, interval := rank(-value,ties.method="random"), by=Scenario]
coal.gen[, Scenario := gsub('.*year_', '', coal.gen$Scenario)]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Reserve shortage duration curve
reserve.shortage = melt(reserve.shortages, id.var='time', variable.name='Scenario')
reserve.shortage[, interval := rank(-value,ties.method="random"), by=Scenario]
reserve.shortage[, Scenario := gsub('.*year_', '', reserve.shortage$Scenario)]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# TX line congestion duration curve
tx.congestion = melt(tx.congest, id.var='time', variable.name='Scenario')
tx.congestion[, interval := rank(-value,ties.method="random"), by=Scenario]
tx.congestion[, Scenario := gsub('.*year_', '', tx.congestion$Scenario)]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# TX line violation duration curve
tx.violations = melt(tx.violation, id.var='time', variable.name='Scenario')
tx.violations[, interval := rank(-value,ties.method="random"), by=Scenario]
tx.violations[, Scenario := gsub('.*year_', '', tx.violations$Scenario)]



# ##########################################################################
# ##########################################################################
# Plots with all generation types

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generation by type stacks and order generation types according to specified order
gen.by.type = melt(gen.type.percent, id.vars='Scenario', variable.name='type')
gen.by.type$type = factor(gen.by.type$type, levels = rev(gen.order))
gen.by.type$Scenario = factor(gen.by.type$Scenario, levels = plotting.order)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Gen by type and region
gen.type.region = melt(gen.type.region, id.vars=c('Type', 'TEPPC.Region'), variable.name='Scenario')
gen.type.region$Type = factor(gen.type.region$Type, levels = rev(gen.order))
gen.type.region[, Scenario := gsub('.*year_', '', gen.type.region$Scenario)]
gen.type.region = gen.type.region[Scenario %in% plotting.order, ]
gen.type.region$Scenario = factor(gen.type.region$Scenario, levels = plotting.order)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Capacity Factor
capacity.factor = melt(cap.factor, id.vars='Scenario', variable.name='type')
capacity.factor$type = factor(capacity.factor$type, levels = rev(gen.order))
capacity.factor$Scenario = factor(capacity.factor$Scenario, levels = plotting.order)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Reserve Gen Percent
reserve.generation.percent = melt(reserve.gen.percent, id.vars='Scenario', variable.name='type')
reserve.generation.percent$type = factor(reserve.generation.percent$type, levels = rev(gen.order))
reserve.generation.percent$Scenario = factor(reserve.generation.percent$Scenario, levels = plotting.order)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Reserve Provision
reserve.provision = melt(reserve.type, id.vars='Scenario', variable.name='type')
reserve.provision$type = factor(reserve.provision$type, levels = rev(gen.order))
reserve.provision$Scenario = factor(reserve.provision$Scenario, levels = plotting.order)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Reserve provision by type and region
reserve.provision.region = melt(reserve.provision.region, id.vars=c('Type', 'TEPPC.Region'), variable.name='Scenario')
reserve.provision.region$Type = factor(reserve.provision.region$Type, levels = rev(gen.order))
reserve.provision.region[, Scenario := gsub('.*year_', '', reserve.provision.region$Scenario)]
reserve.provision.region = reserve.provision.region[Scenario %in% plotting.order, ]
reserve.provision.region$Scenario = factor(reserve.provision.region$Scenario, levels = plotting.order)



# ##########################################################################
# ##########################################################################
# Call the .Rmd file which creates the resulting HTML report file. 
render(input='//nrelqnap02/plexos/projects/im3/Run Results/plexos_run_query/plot_creator.Rmd', c("html_document"), 
       output_file=output.filename, output_dir = '//nrelqnap02/plexos/projects/im3/Run Results')
