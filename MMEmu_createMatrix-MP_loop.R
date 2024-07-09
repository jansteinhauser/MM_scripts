library(magclass)
library(magpie4)
library(madrat)
library(stringr)
library(magpiesets)
library(gdx)
library(piamInterfaces)
library(iamc)
library(hash)


flatten_the_curve <- function(b, c) {
    for (i in seq_along(b)) {
        if (!is.na(b[i]) && b[i] > c[i]) {
            b[i] <- c[i]
        }
    }
    return(b)
}

sum_glo <- function(b) {
    b <- add_columns(b, addnm = "GLO", dim = 1, fill = 0)
    b["GLO", , ] <- toolAggregate(b, to = "global")
    return(b)
}

getReportMESSAGE <- function(
  gdx, file = NULL, detail = TRUE, baseyear = "y2005", bii_path = ".", food_only = FALSE, ...) {

  tryReport <- function(report, width, gdx) {
    regs  <- c(readGDX(gdx, "i"), "GLO")
    years <- readGDX(gdx, "t")
    message("   ", format(report, width = width), appendLF = FALSE)
    x <- try(
      eval(parse(text = paste0("suppressMessages(", report, ")"))
      ), silent = TRUE)
    if (is(x, "try-error")) {
      message("ERROR")
      x <- NULL
    } else if (is.null(x)) {
      message("no return value")
      x <- NULL
    } else if (!is.magpie(x)) {
      message("ERROR - no magpie object")
      x <- NULL
    } else if (!setequal(getYears(x), years)) {
      message("ERROR - wrong years")
      x <- NULL
    } else if (!setequal(getRegions(x), regs)) {
      message("ERROR - wrong regions")
      x <- NULL
    } else if (any(grepl(".", getNames(x), fixed = TRUE))) {
      message("ERROR - data names contain dots (.)")
      x <- NULL
    } else {
      message("success")
    }
    return(x)
  }

  tryList <- function(..., gdx) {
    width <- max(nchar(c(...))) + 1
    return(lapply(unique(list(...)), tryReport, width, gdx))
  }

  message("Start getReportMESSAGE(gdx)...")

  if (!food_only) {

    output <- tryList(
                      "reportPopulation(gdx)",
  #                    "reportIncome(gdx)",
  #                    "reportProducerPriceIndex(gdx)",
  #                    "reportPriceGHG(gdx)",
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
                      "reportPBbiosphere(gdx, dir=bii_path)",
                      gdx = gdx)
  }  else {
    output <- tryList(
                      "reportDemand(gdx,detail=detail,agmip=TRUE)",
                      gdx = gdx)
  }


  if (!is.null(file)) write.report2(output, file = file, ...)
  else return(output)
}

phases <- c("raw")
# valid options: "raw" (most time consumed here), "map", "matrix"

reduced_messages <- TRUE
 
# loop over raw and map process until no new raw files have
# been generated in a while. Blocks matrix generation
loop <- FALSE
#wait time before restarting the loop
# increased with every loop, reset to 0 if raw file is written
loop_wait <- 0

check_file <-  "cell.bii_0.5.nc"

magpie_folder_v <- c("2311-magpie")
matrix_folder_v <- c("SCP_23-11-27")
price_folder_v <- matrix_folder_v

e_vec <- c(0, 5, 7, 10, 15, 25, 45) # 0, 5, 7, 10, 15, 25, 45
g_vec <- c(0, 10, 20, 50, 100, 200, 400, 600, 1000, 2000, 3000, 4000)
# 0, 10, 20, 50, 100, 200, 400, 600, 990, 2000, 3000, 4000

mp_vec <- c(0, 25, 50, 75) # 0, 25, 50, 75
bl_vec <- c(0, 70, 74, 78) # 0, 70, 74, 78

input_postflag <- "demand"
price_postflag <- "price"
output_postflag <- "nov23"
correct_emissions <- FALSE

map_file <- "map_magpie_message.csv"
map_file <- paste0(
  "/p/projects/magpie/users/janstein/MM_scripts/", map_file
  )
# generate with:
# generateMappingfile("MESSAGE_MAgPIE", fileName = map, output = ".") #nolint

while (loop_wait <= 30) {
  for (f in seq_along(magpie_folder_v)){

  magpie_folder <- paste(
    "/p/projects/magpie/users/janstein",
    magpie_folder_v[f], "output", sep = "/")
  matrix_folder <- matrix_folder_v[f]
  price_folder <- price_folder_v[f]

#  if (!file.exists(map_file)){
#    file.copy(map_file, paste(magpie_folder, map_file, sep = "/"))
#  }

  setwd(magpie_folder)
  message("SWITCHING FOLDER: ", magpie_folder)

  time <- format(Sys.time(), "%y%m%d-%H%M%S")

    for (mp in mp_vec) { #nolint
      for (bl in bl_vec) {
        bd <- 0
        if (bl == 0) {
          bd <- 1
        }

        input_preflag <- paste0(
          "MP", str_pad(mp, 2, pad = "0"),
          "BI", str_pad(bl, 2, pad = "0")
          )
        price_preflag <- input_preflag
        food_preflag <- paste0(
          "MP00",
          "BI", str_pad(bl, 2, pad = "0")
          )
        output_flag <- paste0(
          "MP", str_pad(mp, 2, pad = "0"),
          "BI", str_pad(bl, 2, pad = "0"),
	 output_postflag
          )

        output_folder <- paste(matrix_folder, input_preflag, sep = "/")
        if (!dir.exists(output_folder)) {
          dir.create(output_folder)
        }


        for (e in e_vec) {
        e_f <- paste0(
          "E", str_pad(e, 2, pad = "0")
          ) # Input naming convention
        e_o <- paste0(
          "BIO", str_pad(e, 2, pad = "0")
          ) # Expected naming convention by MESSAGE

          for (l in seq_along(g_vec)){
            g <- g_vec[l]
            g_f <- paste0(
            "G", str_pad(g, 4, pad = "0")
            ) # Input naming convention
            g_o <- paste0(
            "GHG", str_pad(g, 3, pad = "0")
            ) # Expected naming convention by MESSAGE
            gdx_path <- paste0(input_preflag, e_f, g_f, input_postflag)
            gdx <- paste(
            matrix_folder, input_preflag, gdx_path, "fulldata.gdx", sep = "/"
            )
            bii_path <- paste(matrix_folder, input_preflag, gdx_path, sep = "/")

            gdx_check <- paste(
              matrix_folder, input_preflag, gdx_path,
              check_file, sep = "/"
              )


            # Output file settings
            model <- paste0("MAgPIE", input_preflag)
            ssp <- "SSP2"
            scen <- paste0(ssp, e_o, g_o)
            of_raw <- paste0(
              output_folder, "/", input_preflag, e_o, g_o, "raw.csv"
            )
            of_map <- paste0(
              output_folder, "/", input_preflag, e_o, g_o, "map.csv"
            )
            ofile <- paste0(
              output_folder, "/", "magpie_input-premap_",
              output_flag, "_", time, ".csv"
            )
            matrix_file <- paste0(
            matrix_folder, "/", "magpie_input_", output_flag, ".csv"
            )

            lock_file <- paste(
              matrix_folder, input_preflag, gdx_path, ".lock", sep = "/"
            )
            ### read run data
            if ("raw" %in% phases) {
              if (
                !file.exists(of_raw) && file.exists(gdx_check) &&
                file.exists(gdx) && file.size(gdx_check) > 3000000 &&
                !file.exists(lock_file)) {

                file.create(lock_file)
                loop_wait <- 0

                message("Start report gdx= ", gdx_path, "...")
                a <- mbind(getReportMESSAGE(gdx, bii_path = bii_path))

                ### get emissions pre correction
                lu_ch4 <- a[, , "Emissions|CH4|Land (Mt CH4/yr)"]
                lu_co2 <- a[, , 
                "Emissions|CO2|Land|+|Land-use Change (Mt CO2/yr)"
                ]
                lu_n2o <- a[, , "Emissions|N2O|Land (Mt N2O/yr)"]

                if (correct_emissions) {

                  ### correct emissions trajectories on regional level

                  lu_ch4_cor <- lu_ch4["GLO", , , invert = TRUE]
                  lu_co2_cor <- lu_co2["GLO", , , invert = TRUE]
                  lu_n2o_cor <- lu_n2o["GLO", , , invert = TRUE]

                  if (l > 1) {
                      lu_ch4_cor <- flatten_the_curve(lu_ch4_cor, lu_ch4_up)
                      lu_co2_cor <- flatten_the_curve(lu_co2_cor, lu_co2_up)
                      lu_n2o_cor <- flatten_the_curve(lu_n2o_cor, lu_n2o_up)
                  }
                  # upper bounds for next GHG price category
                  lu_ch4_up <- lu_ch4_cor
                  lu_co2_up <- lu_co2_cor
                  lu_n2o_up <- lu_n2o_cor

                  ### re-calculate global level
                  lu_ch4_cor <- sum_glo(lu_ch4_cor)
                  lu_co2_cor <- sum_glo(lu_co2_cor)
                  lu_n2o_cor <- sum_glo(lu_n2o_cor)

                  ### adjust subcategories
                  n <- getNames(a[, , "Emissions|CH4|Land|", pmatch = TRUE])
                  lu_ch4_sub <- a[, , n] * lu_ch4_cor / lu_ch4
                  lu_ch4_sub <- setNames(collapseDim(lu_ch4_sub), n)

                  n <- getNames(
                      a[, , "Emissions|CO2|Land|Land-use Change|",
                      pmatch = TRUE
                      ]
                  )
                  lu_co2_sub <- a[, , n] * lu_co2_cor / lu_co2
                  lu_co2_sub <- setNames(collapseDim(lu_co2_sub), n)

                  n <- getNames(a[, , "Emissions|N2O|Land|", pmatch = TRUE])
                  lu_n2o_sub <- a[, , n] * lu_n2o_cor / lu_n2o
                  lu_n2o_sub <- setNames(collapseDim(lu_n2o_sub), n)

                  lu_emis_cor <- mbind(
                      lu_ch4_cor,
                      lu_co2_cor,
                      lu_n2o_cor,
                      lu_ch4_sub,
                      lu_co2_sub,
                      lu_n2o_sub
                  )

                  a <- a[, , getNames(lu_emis_cor), invert = TRUE]
                  a <- mbind(a, lu_emis_cor)
                } else {
                  if (!reduced_messages) {print("Emissions not corrected")}

                  lu_ch4_sub <- a[, , "Emissions|CH4|Land|", pmatch = TRUE]
                  lu_co2_sub <-
                    a[, , "Emissions|CO2|Land|Land-use Change|", pmatch = TRUE]
                  lu_n2o_sub <- a[, , "Emissions|N2O|Land|", pmatch = TRUE]

                  lu_emis <- mbind(
                    lu_ch4,
                    lu_co2,
                    lu_n2o,
                    lu_ch4_sub,
                    lu_co2_sub,
                    lu_n2o_sub
                  )
                }

                ### Price
                regions <- getRegions(a["GLO", , , invert = TRUE])
                years <- getYears(a)

                # bioenergy
                price_gdx <- paste0(price_preflag, e_f, "G0000", price_postflag)
                price_gdx <- paste(
                  price_folder, price_preflag,
                  price_gdx, "fulldata.gdx", sep = "/"
                  )

                if (e != 0) {
                    price_bio <- readGDX(
                        price_gdx,
                        "i60_2ndgen_bioenergy_subsidy", react = "silent"
                    )[, years, ]
                    price_bio <- add_columns(
                        price_bio,
                        addnm = regions,
                        dim = 1,
                        fill = NA
                    )
                    price_bio[, , ] <- price_bio["GLO", , ]
                    getNames(price_bio) <- "Prices|Bioenergy (US$05/GJ)"
                }

                # emissions

                price_emis_co2 <- readGDX(
                  gdx, "p56_pollutant_prices_input",
                  react = "silent")[, years, "co2_c.peatland"] / 44 * 12
                price_emis_co2 <- add_columns(
                  price_emis_co2, addnm = "GLO", dim = 1, fill = NA
                  )
                price_emis_co2[, , ] <- price_emis_co2["LAM", , ]
                getNames(price_emis_co2) <- "Prices|GHG Emission|CO2 (US$2005/tCO2)"

                if (e != 0) prices <- mbind(price_bio, price_emis_co2)
                else prices <- price_emis_co2

                a <- a[, , getNames(prices), invert = TRUE]
                a <- mbind(a, prices)


                ### MP split
                # Load a reduced reporting (food demand only) from a baseline run (MP00BIXXEYYGZZZZ)               
                food_gdx_path <- paste0(food_preflag, e_f, g_f, input_postflag)
                food_gdx <- paste(
                  matrix_folder, food_preflag, food_gdx_path, "fulldata.gdx", sep = "/"
                  )                
                f <- mbind(getReportMESSAGE(food_gdx, food_only = TRUE))

                # Get Delta between baseline and MP for beef and dairy
                d <- "Demand|Food|Livestock products|+|Ruminant meat (Mt DM/yr)"
                beef_base <- f[,,d]
                beef <- a[,,d]
                beef_delta <- beef_base - beef
                getNames(beef_base) <- "Demand|Food|Livestock products|Ruminant meat|Baseline (Mt DM/yr)"

                d <- "Demand|Food|Livestock products|+|Dairy (Mt DM/yr)"
                dairy_base <- f[,,d]
                dairy <- a[,,d]
                dairy_delta <- dairy_base - dairy
                getNames(dairy_base) <- "Demand|Food|Livestock products|Dairy|Baseline (Mt DM/yr)"

                # Get protein content of beef, dairy, MP
                prot_beef <- readGDX(gdx, "f15_nutrition_attributes",
                  react = "silent")[, years, "livst_rum.protein"]
                prot_dairy <- readGDX(gdx, "f15_nutrition_attributes",
                  react = "silent")[, years, "livst_dairy.protein"]
                prot_MP <- readGDX(gdx, "f15_nutrition_attributes",
                  react = "silent")[, years, "scp.protein"]

                # Multiply by delta and MP tonnage respectively
                # Get share of delta protein of total MP Protein
                # Share (beef, dairy protein) * MP tonnage = tonnage MP (beef, dairy)
                beef_mp  <- beef_delta  * prot_beef  / prot_MP 
                dairy_mp <- dairy_delta * prot_dairy / prot_MP

                # Rename magpie objects:
                ## Demand|Food|Secondary products|Microbial protein|+|Ruminant meat (Mt DM/yr)
                ## Demand|Food|Secondary products|Microbial protein|+|Dairy (Mt DM/yr)
                getNames(beef_mp) <- "Demand|Food|Secondary products|Microbial protein|+|Ruminant meat (Mt DM/yr)"
                getNames(dairy_mp) <- "Demand|Food|Secondary products|Microbial protein|+|Dairy (Mt DM/yr)"

                a <- mbind(a, beef_base, dairy_base, beef_mp, dairy_mp)

                ### Add Filler Zero object for mapping
                z <- new.magpie(
                  getRegions(a), getYears(a), names = "ZERO", fill = 0
                  )
                a <- mbind(a, z)

                # Write raw files for mapping
                print(paste0("writing to ", of_raw))
                write.report(
                    a,
                    file = of_raw,
                    model = model,
                    scenario = scen,
                    ndigit = 10,
                    append = FALSE,
                    skipempty = FALSE
                )
                print("raw written")
                file.remove(lock_file)
                } else if (file.exists(of_raw)) {
                  if (!reduced_messages) {
                    message(gdx_path, ": raw file already exists. Skipped")
                  }
                } else if (file.exists(lock_file)) {
                    message(gdx_path, ": folder locked by other script. Skipped")
                } else {
                  message(
                    gdx_path, ": run not started, reporting not finished yet, or infeasible, check log. Skipped." #nolint
                    )
                  message("Fulldata exists: ", file.exists(gdx),
                  "; reporting finished: ", file.exists(gdx_check))
                  if (file.exists(gdx_check)) {
                    message(gdx_check, " size: ", file.size(gdx_check) / 1024, " kb") #nolint
                  }

                }
            } else { # end raw phase
              message("raw phase skipped")
            }

            ### Read raw files and remap them
            if ("matrix" %in% phases && !loop) {
              if (file.exists(of_raw)) {
                message(gdx_path, ": remap and merge into ", ofile)

                a <- read.report(file = of_raw)
                a <- write.reportProject(a, mapping = map_file, file = of_map)
                a <- read.report(file = of_map, as.list = FALSE)

                # Add additional scenario info
                a <- add_dimension(a, dim = 3.1, add = "SSPscen", nm = ssp)
                a <- add_dimension(a, dim = 3.1, add = "GHGscen", nm = g_o)
                a <- add_dimension(a, dim = 3.1, add = "BIOscen", nm = e_o)
                a <- add_dimension(
                  a,
                  dim = 3.1,
                  add = "SDGscen",
                  nm = "noSDG_rcpref"
                  )

                write.report(
                    a,
                    file = ofile,
                    model = model,
                    scenario = scen,
                    ndigit = 10,
                    append = TRUE,
                    skipempty = FALSE,
                    extracols = c("SSPscen", "GHGscen", "BIOscen", "SDGscen")
                )

                #file.remove(of_raw)
                #file.remove(of_map)
              } else {
                message(of_raw, " does not exist. Skipped")
              }
            } #end map phase

          } # close GHG
        } # close BE

        if ("matrix" %in% phases && !loop) {
          ### Edit final input file
          a <- read.csv(file = ofile, sep = ";")

          #Rename regions
          rename <- hash()
          i <- c(
          "AFR", "CHA", "CPA", "EEU", "FSU", "LAM", "MEA",
          "NAM", "PAO", "PAS", "SAS", "WEU", "GLO"
          )
          j <- c(
          "SubSaharanAfrica", "ChinaReg", "PlannedAsiaChina", "CentralEastEurope",
          "FormerSovietUnion", "LatinAmericaCarib", "MidEastNorthAfrica",
          "NorthAmerica", "PacificOECD", "OtherPacificAsia",
          "SouthAsia", "WesternEurope", "World"
          )

          for (k in seq_along(i)) {
          rename[[i[k]]] <- j[k]
          }

          for (i in keys(rename)){
          a[a == i] <- rename[[i]]
          }

          # Remove unnecessary columns (Model, Scenario, empty last column)
          a <- a[, 3:(length(a) - 1)]

          # Fix read-in issue with numeric columns
          names(a) <- gsub("X", "", names(a))

          ### Re-order named Columns
          c <- c(
          "Region", "Variable", "Unit",
          "SSPscen", "GHGscen", "BIOscen", "SDGscen"
          )
          # Get annual columns
          n <- names(a)
          n <- n[!(n %in% c)]
          c <- append(c, n)

          # Apply new order
          a <- a[, c]
          #head(a)

          # And write!
          message(ofile, ": written into Matrix file ", matrix_file)

          write.csv(
              a,
              file = matrix_file,
              row.names = FALSE,
              quote = c(1, 2, 3, 4, 5, 6, 7)
          )
        } # end matrix phase
      } # close BII LO
    } # close mp
  } # close folder settings

  if (loop && (loop_wait < 30)) {
    message("Loop ended without new raw file. Waiting ", loop_wait, " minutes")
    Sys.sleep(loop_wait * 60)
    loop_wait <- loop_wait + 3 
  } else if (loop) {
    message("Loop ended without new raw file and maximum wait time reached!")
    loop_wait <- loop_wait + 5
  } else {
    message("Nothing to loop here. All done for now")
    break
  }
} # close check and write loop
