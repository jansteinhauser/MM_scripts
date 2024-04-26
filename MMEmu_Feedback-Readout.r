library(magclass)
library(magpie4)
library(madrat)
library(stringr)
library(magpiesets)
library(gdx)
library(piamInterfaces)
library(iamc)

getReportMESSAGE <- function(
  gdx, file = NULL, detail = TRUE, baseyear = "y2005", bii_path = ".", ...) {

  tryReport <- function(report, width, gdx) {
    regs  <- c(readGDX(gdx, "i"), "GLO")
    years <- readGDX(gdx, "t")
    message("   ", format(report, width = width), appendLF = FALSE)
    x <- try(eval(parse(text=paste0("suppressMessages(", report, ")"))), silent = TRUE)
    if(is(x, "try-error")) {
      message("ERROR")
      x <- NULL
    } else if(is.null(x)) {
      message("no return value")
      x <- NULL
    } else if(!is.magpie(x)) {
      message("ERROR - no magpie object")
      x <- NULL
    } else if(!setequal(getYears(x),years)) {
      message("ERROR - wrong years")
      x <- NULL
    } else if(!setequal(getRegions(x),regs)) {
      message("ERROR - wrong regions")
      x <- NULL
    } else if(any(grepl(".",getNames(x),fixed=TRUE))){
      message("ERROR - data names contain dots (.)")
      x <- NULL
    } else {
      message("success")
    }
    return(x)
  }

  tryList <- function(..., gdx) {
    width <- max(nchar(c(...))) + 1
    return(lapply(unique(list(...)),tryReport, width, gdx))
  }

  message("Start getReportMESSAGE(gdx)...")

  output <- tryList(
                    "reportPopulation(gdx)",
#                    "reportIncome(gdx)",
#                    "reportProducerPriceIndex(gdx)",
                    "reportPriceGHG(gdx)",
#                    "reportFoodExpenditure(gdx)",
                    "reportKcal(gdx,detail=detail)",
#                    "reportIntakeDetailed(gdx,detail=detail)",
#                    "reportLivestockShare(gdx)",
#                    "reportLivestockDemStructure(gdx)",
#                    "reportVegfruitShare(gdx)",
#                    "reportHunger(gdx)",
#                    "reportPriceShock(gdx)",
#                    "reportPriceElasticities(gdx)",
                    "reportBII(gdx, dir=bii_path)",
                    "reportProduction(gdx,detail=detail,agmip=TRUE)",
                    "reportDemand(gdx,detail=detail,agmip=TRUE)",
                    "reportDemandBioenergy(gdx,detail=detail)",
#                    "reportFeed(gdx,detail=detail)",
#                    "reportTrade(gdx,detail=detail)",
                    "reportLandUse(gdx)",
#                    "reportLandUseChange(gdx)",
                    "reportProtectedArea(gdx)",
                    "reportCroparea(gdx,detail=detail)",
                    "reportNitrogenBudgetCropland(gdx)",
#                    "reportNitrogenBudgetPasture(gdx)",
#                    "reportManure(gdx)",
                    "reportYields(gdx,detail=detail)",
                    "reportTau(gdx)",
#                    "reportTc(gdx)",
                     "reportCostTC(gdx)",
#                    "reportYieldShifter(gdx)",
                    "reportEmissions(gdx)",
#                    "reportEmisAerosols(gdx)",
#                    "reportEmissionsBeforeTechnicalMitigation(gdx)",
#                    "reportEmisPhosphorus(gdx)",
#                    "reportCosts(gdx)",
#                    "reportCostsPresolve(gdx)",
                    "reportPriceFoodIndex(gdx, baseyear = baseyear)",
#                    "reportPriceAgriculture(gdx)",
#                    "reportPriceBioenergy(gdx)",
#                    "reportPriceLand(gdx)",
                    "reportPriceWater(gdx)",
#                    "reportValueTrade(gdx)",
#                    "reportValueConsumption(gdx)",
#                    "reportProcessing(gdx, indicator='primary_to_process')",
#                    "reportProcessing(gdx, indicator='secondary_from_primary')",
#                    "reportAEI(gdx)",
                    "reportWaterUsage(gdx)",
#                    "reportAAI(gdx)",
#                    "reportSOM(gdx)",
#                    "reportGrowingStock(gdx)",
#                    "reportSDG1(gdx)",
                    "reportSDG2(gdx)",
#                    "reportSDG3(gdx)",
#                    "reportSDG6(gdx)",
#                    "reportSDG12(gdx)",
#                    "reportSDG15(gdx)",
#                    "reportForestYield(gdx)",
                    "reportharvested_area_timber(gdx)",
#                    "reportPlantationEstablishment(gdx)",
#                    "reportRotationLength(gdx)",
                    "reportTimber(gdx)",
                    gdx=gdx)


  if (!is.null(file)) write.report2(output, file = file, ...)
  else return(output)
}




#################################################################
magpie_folder <- "2311-magpie"
feedback_folder <- "SCP_23-11-27"
feedback_runs <- c("MP00BI00/Feedback_MP00BI00_600f")
#################################################################

magpie_folder <- paste(
    "/p/projects/magpie/users/janstein",
    magpie_folder, sep = "/")

feedback_folder <- paste(magpie_folder, "output", feedback_folder, sep = "/")

map_file <- "map_magpie_message.csv"
map_file <- paste0(
  "/p/projects/magpie/users/janstein/utilities/", map_file
  )


setwd(feedback_folder)

for (r in feedback_runs) {

  time <- format(Sys.time(), "%y%m%d-%H%M%S")

  gdx_path <- r
  gdx <- paste(gdx_path, "fulldata.gdx", sep = "/")


  a <- mbind(getReportMESSAGE(gdx, bii_path = gdx_path))

  ### GHG Prices
  g <- a[, , "Prices|GHG Emission|CO2|Peatland (US$2005/tCO2)"]
  g <- setNames(g, "Prices|GHG Emission|CO2 (US$2005/tCO2)")
  a <- mbind(a, g)

  ### Add Filler Zero object for mapping
  z <- new.magpie(getRegions(a), getYears(a), names = "ZERO", fill = 0)
  a <- mbind(a, z)

  ### Write output file

  model <- paste("MAgPIE", r, sep = "_")
  ssp <- "SSP2"
  scen <- r
  of_raw <- paste(r, "raw.csv", sep = "_")
  of_map <- paste(r, "map.csv", sep = "_")
  tmp <- "tmp.csv"
  ofile <- paste0(r, "-", time, ".csv")

  # Apply mapping

  write.report(
      a,
      file = of_raw,
      model = model,
      scenario = scen,
      ndigit = 10,
      append = FALSE
  )
  a <- read.report(file = of_raw)
  a <- write.reportProject(a, mapping = map_file, file = of_map)
  a <- read.report(file = of_map, as.list = FALSE)

  # Add additional scenario info
  a <- add_dimension(a, dim = 3.1, add = "SSPscen", nm = ssp)
  a <- add_dimension(a, dim = 3.1, add = "SDGscen", nm = "noSDG_rcpref")

  write.report(
      a,
      file = ofile,
      model = model,
      scenario = scen,
      ndigit = 10,
      append = FALSE,
      extracols = c("SSPscen", "SDGscen")
  )

} # end runs
