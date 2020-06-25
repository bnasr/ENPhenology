library(data.table)

response <- fread('data/stardot_5MP_quantum_efficiency.csv')

png('figure/response.png', width = 6, height = 2, units = 'in', res = 600)
par(mar = c(4,4,0.5,0), bty = 'n', font= 2)
response[, plot(wavelength, red*100, col = '#ca0020', type = 'l', lwd =2,  
                xlim = c(400, 900), ylim = c(c(0,50)), font.axis = 2, font.lab = 2,
                ylab = 'Quantum Efficiency (%)', xlab = c('Wavelength (nm)'),
                )]
response[, lines(wavelength, green*100,  col = '#05b071', type = 'l', xlim = c(400, 900), lwd =2)]
response[, lines(wavelength, blue*100, col = '#0571b0', type = 'l', xlim = c(400, 900), lwd =2)]
dev.off()
