# Library for Brazil Dolphins

# Prints only the reference spectra

# # Code to generate MSP text file:
# 
# x <- MetaDataDolphin[, c("Compound", "SpectrumFilename")]
# names(x) <- c("compound", "filename")
# WriteMspFile(SpecDataDolphin, x, filename = "SpecLibDolphin2016.txt",
#              comment = "Contaminants in bottlenose dolphins (Tursiops truncatus) collected between 2004 to 2011 from Rio de Janeiro, Brazil, Alonso et al.")

LibraryReport <- function(spectra = SpecDataDolphin,
                          metadata = MetaDataDolphin,
                          structures = paste(system.file(package = "SpecLibDolphin2016"), "/extdata/struct/", sep = ""),
                          pdfFile = "SpecLibDolphin2016.pdf",
                          pdfTitle = "SpecLibDolphin2016 Library",
                          xMin = 40) {
  
  structureList <- unlist(lapply(strsplit(dir(structures), split = "\\."), function(x) x[1]))
  
  pdf(file = pdfFile, width = 10.5, height = 8, title = pdfTitle, paper = "usr")
  
  # Title Page
  
  grid.newpage()
  
  pushViewport(viewport(layout = grid.layout(nrow = 4, ncol = 1, heights = unit(c(0.1, 0.2, 0.5, 0.2), "npc"))))
  
  pushViewport(viewport(layout.pos.row = 1))
  
  grid.text("SpecLibDolphin2016 Mass Spectral Library", y = 0.5, gp = gpar(cex = 1.25))
  grid.lines(x = unit(c(0,1), "npc"), y = unit(c(0,0), "npc"))
  
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 2))
  
  grid.text("Brazilian Bottlenose Dolphin Blubber", y = 0.5, gp = gpar(cex = 2))
  
  popViewport()
  
  pushViewport(viewport(layout.pos.row = 3))
  
  grid.text("Non-targeted screening of halogenated organic compounds in\nbottlenose dolphins (Tursiops truncatus) from Rio de Janeiro, Brazil.", y = 0.9, gp = gpar(cex = 1.25))
  grid.text("Authors: Mariana B. Alonso, Keith A. Maruya, Nathan G. Dodder, Jose Lailson Brito Jr.,\nAlexandre Azevedo, Elitieri Santos-Neto, Joao Paulo M. Torres, Olaf Malm, Eunha Hoh", y = 0.6, gp = gpar(cex = 1.25))
  grid.text("Web Reference: http://OrgMassSpec.github.io", y = 0.3, gp = gpar(cex = 1.25))

  popViewport()
  
  pushViewport(viewport(layout.pos.row = 4))
  
  session.info <- sessionInfo()
  
  grid.text(paste("Prepared:", Sys.time()), y = 0.8)
  grid.text(paste("SpecLibDolphin2016 version", session.info$otherPkgs$SpecLibDolphin2016$Version), y = 0.65)
  grid.text(paste("OrgMassSpecR version", session.info$otherPkgs$OrgMassSpecR$Version), y = 0.5)
  grid.text(paste("png version", session.info$otherPkgs$png$Version), y = 0.35)
  grid.text(session.info$R.version$version.string, y = 0.2)
  
  popViewport()
  
  # Select unique compounds used to make identifications (reference spectra) and
  # subset the metadata to include only these
  
  uniqueCompounds <- unique(metadata$ReferenceSpectrum)
  
  filteredMetadata <- metadata[metadata$SpectrumFilename %in% uniqueCompounds, ]
  
  filteredMetadata$page <- 1:nrow(filteredMetadata)
  
  # Function for spectum pages
  
  DrawSpectrum <- function(currentFilename) {
    
    currentSpectrum <- spectra[spectra$filename == as.character(currentFilename), ]
    
    currentMetadata <- metadata[metadata$SpectrumFilename == currentFilename, ]
    
    message("Making spectrum for ", currentMetadata$Compound)
    
    grid.newpage()
    
    spec.layout <- grid.layout(nrow = 5, ncol = 1, heights = unit(c(0.05, 0.1, 0.55, 0.25, 0.05), "npc"))
    
    pushViewport(viewport(layout = spec.layout))
    
    
    #---------- Compound Name and Class ----------
    
    pushViewport(viewport(layout.pos.row = 1))
    
    grid.text(paste("Name:", currentMetadata$Compound), 
              x = 0, y = 0.5, just = c("left", "center"), gp = gpar(cex = 1.25))
    
    grid.text(paste("Class:", currentMetadata$Class),
              x = 0.67, y = 0.5, just = c("left", "center"), gp = gpar(cex = 1.25))
    
    grid.lines(x = unit(c(0,1), "npc"), y = unit(c(0,0), "npc"))
    
    popViewport()  
    
    
    #---------- Compound Info ----------
    
    pushViewport(viewport(layout.pos.row = 2))
    
    grid.text(paste("Matrix:", currentMetadata$Matrix), x = 0, y = 0.75, hjust = 0)
    grid.text(paste("In N. Atlantic: ", currentMetadata$InAtlantic, ", In N. Pacific: ", currentMetadata$InPacific, sep = ""), x = 0, y = 0.5, hjust = 0)             
    grid.text(paste("Typically Monitored:", currentMetadata$TypicallyMonitored), x = 0, y = 0.25, hjust = 0)                
    grid.text(paste("Instrument:", currentMetadata$Instrument), x = 0.33, y = 0.75, hjust = 0)
    grid.text(paste("1D RT, 2D RT (s): ", currentMetadata$RetentionTime1D, ", ", currentMetadata$RetentionTime2D, sep = ""), x = 0.33, y = 0.5, hjust = 0)   
    grid.text(paste("Quantitative Ion m/z:", currentMetadata$QuantIon), x = 0.33, y = 0.25, hjust = 0)
    
    if(!is.na(currentMetadata$Formula)) {
      grid.text(paste("Elemental Formula:", currentMetadata$Formula), x = 0.67, y = 0.75, hjust = 0)
    } else {
      grid.text(paste("Elemental Formula:"), x = 0.67, y = 0.75, hjust = 0)
    }
      
    grid.text(paste("Source:", currentMetadata$Source), x = 0.67, y = 0.5, hjust = 0)    
    grid.text(paste("Identification:", currentMetadata$Identification),x = 0.67, y = 0.25, hjust = 0) 
    
    if(!is.na(currentMetadata$Comment)) {
    grid.text(paste("Comment:", currentMetadata$Comment), x = 0, y = 0, hjust = 0)
    } else {
      grid.text(paste("Comment:"), x = 0, y = 0, hjust = 0)
    }
    
    popViewport()
    
    
    #---------- Draw spectrum ----------
    
    pushViewport(viewport(layout.pos.row = 3))
    
    if(nrow(currentSpectrum) > 0) {
      
      currentSpectrum$percent.intensity <- with(currentSpectrum, intensity / max(intensity) * 100)
      
      # Calculate molecular weight to set x-axis upper limit
      
      if(!is.na(currentMetadata$Formula)) {
        
        mw <- MolecularWeight(formula = ListFormula(currentMetadata$Formula))
        xMax <- mw + (mw * 0.03)
        
      } else {
        
        m <- max(currentSpectrum$mz) 
        xMax <- m + (m * 0.03)
        
      }
      
      plot.data <-currentSpectrum[currentSpectrum$mz >= xMin & currentSpectrum$mz <= xMax, ]
      
      pushViewport(plotViewport(c(3.75, 3.5, 1.5, 1)))
      pushViewport(dataViewport(xscale = c(xMin, xMax),
        yscale = c(0, 110)))
      
      grid.rect()
      p.ticks <- pretty(plot.data$mz, n = 10)
      x.ticks <- p.ticks[p.ticks >= xMin & p.ticks <= xMax]
      grid.xaxis(at = x.ticks)
      grid.yaxis(at = c(0, 25, 50, 75, 100))
      
      grid.segments(plot.data$mz,
        plot.data$percent.intensity,
        plot.data$mz,
        rep(0, length(plot.data$intensity)),
        default.units = "native",
        gp = gpar(lwd = 0.75))
      
      ## print m/z values in plot
      
      display.values <- plot.data$mz[plot.data$display == TRUE]
      if (length(display.values) > 0) {
        grid.text(display.values,
          x = display.values,
          y = plot.data$percent.intensity[plot.data$display == TRUE] + 5,
          default.units = "native",
          gp = gpar(col = "blue"))
      }
      
      grid.text("intensity (%)", x = unit(-3.2, "lines"), rot = 90)
      grid.text("m/z", y = unit(-2.5, "lines"))
      
      # Automatic display of values every 25 units
      
      plotSegments <- cut(plot.data$mz, breaks = xMax/25)
      
      plot.data <- cbind(plot.data, plotSegments)
      
      for(i in 1:length(unique(plotSegments))) {
        
        segmentTmp <- plot.data[plot.data$plotSegments == unique(plotSegments)[i], ]
        
        maxPeak <- segmentTmp[segmentTmp$percent.intensity == max(segmentTmp$percent.intensity, na.rm = TRUE), ]
        
        if(maxPeak$percent.intensity[1] >= 5) {
          
          grid.text(maxPeak$mz,
            x = maxPeak$mz,
            y = maxPeak$percent.intensity + 5,
            default.units = "native",
            gp = gpar(col = "blue"))
        }
        
      }
      
      popViewport(3)
      
    } 
       
    else {
      
      grid.text("Missing Spectrum")
      popViewport(1)
      
    }
    
    # Define area below spectrum
    
    pushViewport(viewport(layout.pos.row = 4))
    
    pushViewport(viewport(layout = grid.layout(nrow = 1, ncol = 5, 
                                               widths = unit(c(0.1, 0.3, 0.15, 0.4, 0.05), "npc"))))
    
    
    # ---------- Display raster image ----------
    
    pushViewport(viewport(layout.pos.col = 2))
    
    if(as.character(currentFilename) %in% structureList) {

      img <- readPNG(paste(structures, currentFilename, ".png", sep = ""))

      grid.raster(img)

    }

    grid.rect(gp = gpar(col = "black"))
    
    popViewport()
    
    
    #---------- Write fragment ion identifications ----------
    
    pushViewport(viewport(layout.pos.col = 4))
    
    grid.rect(x = unit(0, "npc"), height = unit(1, "npc"), width = unit(1, "npc"), gp = gpar(col = "black"), just = "left")
    
    grid.text("m/z [Fragment]", x = unit(0.075, "npc"), y = unit(0.9, "npc"), gp = gpar(col="blue"), just = "left")
    
    grid.lines(x = unit(c(0, 1), "npc"), y = unit(0.8, "npc"), gp = gpar(col = "black"))
    
    if(!is.na(currentMetadata$FragmentIdentification)) {
      
      fragmentText <- gsub(pattern = "; ", replacement = "\n", 
                           x = currentMetadata$FragmentIdentification, 
                           fixed = TRUE)
      
      grid.text(fragmentText, x = unit(0.075, "npc"), y = unit(0.7, "npc"), gp = gpar(col = "black"), just = c("left", "top"))
    
    }
    
    popViewport(3)
    
    
    #---------- Write filename and page number ----------
    
    pushViewport(viewport(layout.pos.row = 5))
    
    page <- filteredMetadata$page[filteredMetadata$SpectrumFilename == currentMetadata$SpectrumFilename] + 1
    
    grid.text(paste("Filename: ", currentMetadata$SpectrumFilename, ", Page: ", page, sep = ""),
              x = 1,
              just = c("right", "top"),
              gp = gpar(col = "dark grey"))
  
  }
  
  sapply(filteredMetadata$SpectrumFilename, 
       DrawSpectrum)
  
  graphics.off()
  
}

