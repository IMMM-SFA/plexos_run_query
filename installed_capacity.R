
library(rplexos)
library(data.table)

runs = plexos_open('//nrelqnap02/plexos/projects/im3/PLEXOS_database/Solutions/Fuel Price Solutions/Model DA_IM3_year_2010_fuel_5')

installed.capacity = data.table(query_year(runs, 'Generator', 'Installed Capacity'))

gen.name.mapping = fread('//nrelqnap02/plexos/projects/im3/Run Results/gen_name_mapping_aggregated_gas.csv')


capacity = merge(installed.capacity, gen.name.mapping, by='name')

capacity = capacity[, .(value = mean(value)), by=.(Type, TEPPC.Region, name)]
capacity = capacity[, .(value = sum(value)), by=.(Type, TEPPC.Region)]
capacity = capacity[!TEPPC.Region %in% c('Canada', 'Mexico'), ]
capacity[TEPPC.Region == 'Desert SouthWest', TEPPC.Region := 'Desert Southwest']
capacity[, Region.Sum := sum(value), by=.(TEPPC.Region)]
capacity = capacity[, .(Percent = value/Region.Sum*100), by=.(Type, TEPPC.Region)]

capacity = dcast(capacity, Type~TEPPC.Region, value.var='Percent')
capacity[is.na(capacity)] = 0

