
library(data.table)
library(ggplot2)

water = data.table( NW = c(-18.9, -16.3, -14.9, -14.9), `N-CA` = c(-12.1, -26.6, -27.2, -29.2),
                        `S-CA` = c(20.3, 14.2, 14.6, 13.6), `D-SW` = c(17.6, 33.3, 30.5, 31.7),
                        Basin = c(-5, -7.1, -7.1, -6.3), Rockies = c(17.7, -4.3, -3.9, -3.8) )
# `Fuel.Price` = c('Low', 'Low.Medium', 'High.Medium', 'High')

water$type = 'Decreasing Water Availability'
water = melt(water, id.vars=c('type'), variable.name = 'TEPPC.Region')


# `Water.Year` = c(1993, 2010, 1980, 1985, 1967), 
fuel.price = data.table( NW = c(-4.8, -1.2, 0.7, -0.5, -0.5),
                         `N-CA` = c(-20.5, -28.2, -35.9, -33.9, -34.5), `S-CA` = c(-27.9, -32.1, -35.7, -33.8, -34.7),
                         `D-SW` = c(8.8, 13.4, 20.5, 21.9, 20.2), Basin = c(53.6, 52, 49.1, 49.4, 50.9), Rockies = c(46.9, 36.5, 30.3, 29.3, 30.1) )
fuel.price$type = 'Increasing Natural Gas Price'
fuel.price = melt(fuel.price, id.vars = c('type'), variable.name='TEPPC.Region')

water$TEPPC.Region = factor(water$TEPPC.Region, levels = c('N-CA', 'NW', 'Basin', 'Rockies', 'S-CA', 'D-SW'))
fuel.price$TEPPC.Region = factor(fuel.price$TEPPC.Region, levels = c('S-CA', 'N-CA', 'NW', 'D-SW', 'Rockies', 'Basin'))

plot.data = rbind(water, fuel.price)


ggplot(data = plot.data[type=='Increasing Natural Gas Price'], aes(TEPPC.Region, value)) +
  geom_point() +
  ylab('Max % Change in Generation') +
  xlab(NULL) + ylim(c(-55, 55)) +
  facet_grid(.~type) + 
  theme_bw() + 
    theme(  axis.title       = element_text(size=12, face='bold'),
            axis.text.y      = element_text(size=12, face='bold'),
            axis.text.x      = element_text(size=12, face='bold'),
            # axis.text.x    = element_text(face='bold'),
            strip.text       = element_text(size = 12, face = 'bold'),
            panel.spacing    = unit(1.5, "lines"),
            panel.grid.major = element_line(color= 'gray50'))

ggplot(data = plot.data[type=='Decreasing Water Availability'], aes(TEPPC.Region, value)) +
  geom_point() +
  ylab('Max % Change in Generation') +
  xlab(NULL) + ylim(c(-55, 55)) +
  facet_grid(.~type) + 
  theme_bw() + 
  theme(  axis.text        = element_text(size=12, face='bold'),
          axis.text.x      = element_text(size=12, face='bold'),
          axis.title       = element_text(size=12, face='bold'),
          strip.text       = element_text(size = 12, face = 'bold'),
          panel.spacing    = unit(1.5, "lines"),
          panel.grid.major = element_line(color='gray50'))

# ggplot(data = plot.data, aes(TEPPC.Region, value)) +
#   geom_boxplot() +
#   ylab('Max % Change in Generation') +
#   xlab(NULL) +
#   ylim(c(-55, 55)) +
#   facet_grid(.~type) + 
#   theme(  axis.text        = element_text(size=8, face='bold'),
#           # axis.text.x    = element_text(face='bold'),
#           axis.title       = element_text(size=10, face='bold'),
#           strip.text       = element_text(size = 10, face = 'bold'),
#           panel.spacing    = unit(1.5, "lines"),
#           panel.grid.major = element_line(color='gray50'))
