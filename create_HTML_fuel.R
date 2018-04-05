
library(rmarkdown)
library(data.table)
library(RColorBrewer)

load('//nrelqnap02/plexos/projects/im3/Run Results/fuel_price_data.RData')

output.filename = 'IM3_plots_fuel_price_report_neworder.html'

ba.mapping = fread('//nrelqnap02/plexos/projects/im3/Run Results/ba_to_teppc_region.csv')
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Year plotting order

# Plotting order for fuel price runs
plotting.order = c('1993_fuel_2_5', '2010_fuel_2_5', '1980_fuel_2_5', '1985_fuel_2_5', '1967_fuel_2_5', '1993_fuel_5', '2010_fuel_5', '1980_fuel_5', '1985_fuel_5', '1967_fuel_5',
                   '1993_fuel_7_5', '2010_fuel_7_5', '1980_fuel_7_5', '1985_fuel_7_5', '1967_fuel_7_5', '1993_fuel_10', '2010_fuel_10', '1980_fuel_10', '1985_fuel_10', '1967_fuel_10' )
# plotting.order = c('1993_fuel_2_5', '1993_fuel_5', '1993_fuel_7_5', '1993_fuel_10', '2010_fuel_2_5', '2010_fuel_5', '2010_fuel_10', '2010_fuel_7_5', 
#                    '1980_fuel_2_5', '1980_fuel_5', '1980_fuel_7_5', '1980_fuel_10', '1985_fuel_2_5', '1985_fuel_5', '1985_fuel_7_5', '1985_fuel_10', 
#                    '1967_fuel_2_5', '1967_fuel_5','1967_fuel_7_5', '1967_fuel_10' )
# # plotting.order = c('2010_fuel_2_5', '2010_fuel_5', '2010_fuel_7_5', '2010_fuel_10' )

# # Plot according to water availability
# plotting.order = rev(c('1967', '1989', '1985', '1990', '1991', '1980', '1982', '1996', '1966', '1956', '1981', '1959', '1958', '1988', '1995', '1999',
#                    '1973', '1974', '2010', '1984', '1979', '1997', '1957', '2006', '2007', '2009', '1983', '1992', '1965', '1964', '2000', '2004',
#                    '2005', '1972', '1998', '1987', '2008', '2003', '1963', '1962', '1969', '1977', '1960', '1978', '1976', '1975', '1961', '1968',
#                    '1994', '1971', '2001', '1986', '1970', '1993', '2002'))

# Plot according to available hydro generation
# plotting.order = gsub('DA_IM3_year_', '', hydro.available$scenario[order(hydro.available$value, decreasing=TRUE)])

annual.gen.stats[, Scenario := gsub('.*year_', '', annual.gen.stats$Scenario)]
avg.prices.regions[, Scenario := gsub('.*year_', '', avg.prices.regions$Scenario)]
avg.prices.zones[, Scenario := gsub('.*year_', '', avg.prices.zones$Scenario)]
hours.high.price[, Scenario := gsub('.*year_', '', hours.high.price$Scenario)]
cap.factor[, Scenario := gsub('.*year_', '', cap.factor$Scenario)]
gen.type.percent[, Scenario := gsub('.*year_', '', gen.type.percent$Scenario)]
reserve.gen.percent[, Scenario := gsub('.*year_', '', reserve.gen.percent$Scenario)]
reserve.type[, Scenario := gsub('.*year_', '', reserve.type$Scenario)]
violations[, Scenario := gsub('.*year_', '', violations$Scenario)]

annual.gen.stats = annual.gen.stats[Scenario %in% plotting.order, ]
avg.prices.regions = avg.prices.regions[Scenario %in% plotting.order, ]
avg.prices.zones = avg.prices.zones[Scenario %in% plotting.order, ]
hours.high.price = hours.high.price[Scenario %in% plotting.order, ]
cap.factor = cap.factor[Scenario %in% plotting.order, ]
gen.type.percent = gen.type.percent[Scenario %in% plotting.order, ]
reserve.gen.percent = reserve.gen.percent[Scenario %in% plotting.order, ]
reserve.type = reserve.type[Scenario %in% plotting.order, ]
violations = violations[Scenario %in% plotting.order, ]

# violations$`Line Flow Violation Cost (MM$)` = violations$`Line Flow Violation Cost (MM$)`*1000
# setnames(violations, 'Line Flow Violation Cost (MM$)', 'Line Flow Violation Cost (M$)')

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Set gen order and gen colors
gen.order = c('Nuclear', 'Coal', 'Hydro', 'Gas CC', 'Gas CT', 'Steam', 'CHP-QF', 'ICE Gas', 
              'Biomass', 'Geothermal', 'Other', 'Storage', 'CSP', 'PV', 'Wind')

gen.color = c('firebrick', 'gray20', 'lightblue', 'darkolivegreen4', 'lightpink', 'orchid4', 'gray20', 'gray60', 
              'mediumpurple2', 'khaki1', 'mediumpurple3', 'gray45', 'darkorange2', 'goldenrod1', 'steelblue3') 

gen.color = setNames( gen.color, gen.order )

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Set fuel price year colors
color.1993 = 'gray0'
color.2010 = 'gray25'
color.1980 = 'gray65'
color.1985 = 'gray82'
color.1967 = 'gray95'

fuel.color = setNames(rep(c(color.1993, color.2010, color.1980, color.1985, color.1967), times=4), plotting.order)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Set scenario colors
# scenarios = annual.gen.stats$Scenario
scenarios = plotting.order
# scenario.color = setNames(brewer.pal(length(scenarios), "Dark2"), scenarios)
scenario.color = setNames(rainbow(length(scenarios)), scenarios)


# ##########################################################################
# ##########################################################################

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculate total cost including violations and rank scenarios according to total cost
annual.gen.stats[, `Total Cost ($)` := (`Cost ($)` + violations[,`Unserved Energy Cost ($)`] + violations[,`Reserve Shortage Cost (M$)`]*1000 + 
                                          violations[,`Line Flow Violation Cost (M$)`]*1000000 + violations[, `Hydro violation cost (M$)`]*1000) ]
annual.gen.stats[, Rank := rank(annual.gen.stats$`Total Cost ($)`)]
# annual.gen.stats$Scenario = factor(annual.gen.stats$Scenario, levels = annual.gen.stats$Scenario[order(annual.gen.stats$Rank)])
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

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Region Prices and Weighted Region Prices
region.price = melt(avg.region.price, id.vars='region', variable.name='Scenario')
region.price[, Scenario := gsub('.*year_', '', region.price$Scenario)]
region.price = region.price[Scenario %in% plotting.order, ]
region.price$Scenario = factor(region.price$Scenario, levels = plotting.order)

region.price.teppc = merge(region.price, ba.mapping, by.x='region', by.y='name')
region.price.teppc[, Scenario := gsub('.*year_', '', region.price.teppc$Scenario)]
region.price.teppc = region.price.teppc[, .(value = mean(value)), by=.(Scenario, TEPPC.Region)]
region.price.teppc = region.price.teppc[Scenario %in% plotting.order, ]
region.price.teppc$Scenario = factor(region.price.teppc$Scenario, levels = plotting.order)

region.price.w = melt(avg.weighted.region.price, id.vars='region', variable.name='Scenario')
region.price.w[, Scenario := gsub('.*year_', '', region.price.w$Scenario)]
region.price.w = region.price.w[Scenario %in% plotting.order, ]
region.price.w$Scenario = factor(region.price.w$Scenario, levels = plotting.order)

region.price.w.teppc = merge(region.price.w, ba.mapping, by.x='region', by.y='name')
region.price.w.teppc[, Scenario := gsub('.*year_', '', region.price.w.teppc$Scenario)]
region.price.w.teppc = region.price.w.teppc[, .(value = mean(value)), by=.(Scenario, TEPPC.Region)]
region.price.w.teppc = region.price.w.teppc[Scenario %in% plotting.order, ]
region.price.w.teppc$Scenario = factor(region.price.w.teppc$Scenario, levels = plotting.order)



# ##########################################################################
# ##########################################################################
# Duration curve plots

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Rank interval prices for duration curve
prices = melt(avg.interval.price, id.var='time', variable.name='Scenario', value.name='Cost')
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
# gen.by.type$Scenario = factor(gen.by.type$Scenario, levels = annual.gen.stats$Scenario[order(annual.gen.stats$Rank)])
gen.by.type$Scenario = factor(gen.by.type$Scenario, levels = plotting.order)

# Gen by type and region
gen.type.region = melt(gen.type.region, id.vars=c('Type', 'TEPPC.Region'), variable.name='Scenario')
gen.type.region$Type = factor(gen.type.region$Type, levels = rev(gen.order))
gen.type.region[, Scenario := gsub('.*year_', '', gen.type.region$Scenario)]
gen.type.region = gen.type.region[Scenario %in% plotting.order, ]
gen.type.region$Scenario = factor(gen.type.region$Scenario, levels = plotting.order)

# Annual region load
annual.load = merge(annual.load, ba.mapping)
annual.load = melt(annual.load, id.vars=c('name', 'TEPPC.Region'), variable.name='Scenario')
annual.load$Type = 'Load'
annual.load = annual.load[, .(value = sum(value)), by=.(TEPPC.Region, Scenario, Type)]
annual.load[, Scenario := gsub('.*year_', '', annual.load$Scenario)]
annual.load = annual.load[Scenario %in% plotting.order, ]

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Capacity Factor
capacity.factor = melt(cap.factor, id.vars='Scenario', variable.name='type')
capacity.factor$type = factor(capacity.factor$type, levels = rev(gen.order))
# capacity.factor$Scenario = factor(capacity.factor$Scenario, levels = annual.gen.stats$Scenario[order(annual.gen.stats$Rank)])
capacity.factor$Scenario = factor(capacity.factor$Scenario, levels = plotting.order)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Reserve Gen Percent
reserve.generation.percent = melt(reserve.gen.percent, id.vars='Scenario', variable.name='type')
reserve.generation.percent$type = factor(reserve.generation.percent$type, levels = rev(gen.order))
# reserve.generation.percent$Scenario = factor(reserve.generation.percent$Scenario, levels = annual.gen.stats$Scenario[order(annual.gen.stats$Rank)])
reserve.generation.percent$Scenario = factor(reserve.generation.percent$Scenario, levels = plotting.order)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Reserve Provision
reserve.provision = melt(reserve.type, id.vars='Scenario', variable.name='type')
reserve.provision$type = factor(reserve.provision$type, levels = rev(gen.order))
# reserve.provision$Scenario = factor(reserve.provision$Scenario, levels = annual.gen.stats$Scenario[order(annual.gen.stats$Rank)])
reserve.provision$Scenario = factor(reserve.provision$Scenario, levels = plotting.order)

# Reserve provision by type and region
reserve.provision.region = melt(reserve.provision.region, id.vars=c('Type', 'TEPPC.Region'), variable.name='Scenario')
reserve.provision.region$Type = factor(reserve.provision.region$Type, levels = rev(gen.order))
reserve.provision.region[, Scenario := gsub('.*year_', '', reserve.provision.region$Scenario)]
reserve.provision.region = reserve.provision.region[Scenario %in% plotting.order, ]
reserve.provision.region$Scenario = factor(reserve.provision.region$Scenario, levels = plotting.order)



# ##########################################################################
# ##########################################################################
render(input='//nrelqnap02/plexos/projects/im3/Run Results/plot_creator_fuel.Rmd', c("html_document"), 
       output_file=output.filename, output_dir = '//nrelqnap02/plexos/projects/im3/Run Results')
