library(xpose4)

pdf(file=pdf.filename, width=10, height=7, title=pdf.title)

if (!exists('mix')) {     # A mixture model is a special case
    if (is.tte) {
        #data is in the model directory, go there to read input
        setwd(model.directory)
        xpdb <- xpose.data(xpose.runno)
        plots <- kaplan.plot(object=xpdb, VPC=T)
        #go back to vpc directory 
        setwd(working.directory)
    } else if (is.categorical) { 
        plots <- xpose.VPC.categorical(vpc.info=tool.results.file, vpctab=vpctab)
    } else if (have.loq.data | have.censored) {
        plots <- xpose.VPC.both(vpc.info=tool.results.file, vpctab=vpctab)
    } else {
        plots <- xpose.VPC(vpc.info=tool.results.file, vpctab=vpctab)
    }
    print(plots) 
} else {
    if (require("vpc")) {
        source(paste0(rscripts.directory, "/vpc/vpc_mixtures.R"))
        observations_tablefile <- paste0(working.directory, '/m1/vpc_original.npctab.dta')
        simulations_tablefile <- paste0(working.directory, '/m1/vpc_simulation.1.npctab.dta')

        obs <- read_table_nm(observations_tablefile)
        sim <- read_table_nm(simulations_tablefile)
        if (!mix_random) {
            plots <- vpc_mixtures(obs=obs, sim=sim, numsims=samples, mixcol=mix, dv=dv)
        } else {
            plots <- vpc_mixtures(obs=obs, sim=sim, numsims=samples, mixcol=mix, dv=dv, phm=phm_file)
        }

        for (p in plots) {
            print(p)
        }
    }
}

dev.off()
