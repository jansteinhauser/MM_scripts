library(magpie4)
library(magpiesets)
library(stringr)



magpie_folder <- "2311-magpie"
magpie_folder <- paste("..", magpie_folder, "output", sep = "/")
setwd(magpie_folder)
print(getwd())
run_folder <- "SCP_23-11-27"
setwd(run_folder)
print(getwd())
subfolders <- TRUE

e_v <- c(0, 5, 7, 10, 15, 25, 45)

#bd_v <- c(1)
bi_v <- c(0, 70, 74, 78) # 0, 70, 74, 78
#mp_v <- c(0, 30, 50, 76)
mp_v <- c(49)

flag <- "price"
output_flag <- ""

ofile <- paste(output_flag, "BE-Crop-Potential.cs3", sep = "_")

if (file.exists(ofile)) {
  ofile_old <- paste0("old_", ofile)
  if (file.exists(ofile_old)) {
    file.remove(ofile_old)
  }
  file.rename(ofile, ofile_old)
}

for (mp in mp_v) { #nolint
  mp <- paste0("MP", str_pad(mp, 2, pad = "0"))
  #for (bd in bd_v) {
    for (bi in bi_v) {

      if (bi == 0) {
        bd <- 1
      } else {
        bd <- 0
      }
      bd <- paste0("BD", bd)
      bi <- paste0("BI", str_pad(bi, 2, pad = "0"))
      preflag <- paste0(mp,  bi)
      print(preflag)
      for (e in e_v) {
        e <- paste0("E", str_pad(e, 2, pad = "0"))
        gdx_folder <- paste0(preflag, e, "G0000", flag)
        
	if (subfolders) {
		gdx <- paste(preflag, gdx_folder, "fulldata.gdx", sep = "/")
	} else {
		gdx <- paste(gdx_folder, "fulldata.gdx", sep = "/")
	}

        a <- reportProductionBioenergy(gdx, detail = FALSE)

        a <- a[, , "2nd generation|++", pmatch = TRUE]

        o <- setNames(a * 1000, paste0(preflag, e))
#	o <- setNames(a * 1000, paste0(bi, e))


        # fill data gaps
        n <- getYears(o)
        y <- 1995
        while (y <= 2150) {
          yy <- paste0("y", y)
          if (!(yy %in% n)) {
            o <- add_columns(o, addnm = yy, dim = 2, fill = NA)
            o[, yy, ] <- if (y < 2100) (
              (o[, paste0("y", y - 5), ] + o[, paste0("y", y + 5), ]) / 2 )
              else o[, "y2100", ]
          }
          y <- y + 5
        }

        # sort filled data
        o_sorted <- o[, "y1995", ]
        y <- 2000
        while (y <= 2150) {
          yy <- paste0("y", y)
          o_sorted <- add_columns(o_sorted, addnm = yy, dim = 2, fill = NA)
          o_sorted[, yy, ] <- o[, yy, ]
          y <- y + 5
        }

        # write to file for checking / copy-pasting into f60_bioenergy_dem.cs3
        write.magpie(
          o_sorted,
          file_name = ofile,
          file_type = "cs3",
          append = TRUE
          )

        if (file.exists("f60_bioenergy_dem.cs3")) {
          write.magpie(
            o_sorted["GLO", , , invert = TRUE],
            file_name = "f60_bioenergy_dem.cs3",
            file_type = "cs3",
            append = TRUE
          )
        }

      } # close BE
    } # close bi
  #} # close bd
} # close mp
