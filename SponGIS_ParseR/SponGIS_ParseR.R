# SponGIS Excel Spreadsheet to Darwin Core Archive Parser
# Andy Davies Dec 2018

# Load Sheets from your spreadsheet file
# Set filename of the template that you want to read, here or on the Rmd file.
#filename <- "/Users/andrewdavies_mbp/Andy's Documents/Websites and comittees/SponGIS/Github/SponGIS/SponGIS_ParseR/SponGIS_boreo_arctic_Geodia_mkII.xlsx"
filename <- "/Users/andrewdavies_mbp/Andy's Documents/Websites and comittees/SponGIS/Github/SponGIS/SponGIS_ParseR/Uploads/Pending/Jon/Xavier_Aphro/X_Aphro_SponGIS_Template.xlsx"
filename_globenv <- "/Users/andrewdavies_mbp/Andy's Documents/Websites and comittees/SponGIS/Github/SponGIS/SponGIS_ParseR/GlobENV/brick.tif"

# If you already have a meta id for a dataset enter it here, otherwise leave blank
meta_uuid = "90786a17-6ee4-4d00-b27a-b73ddaae08b4"

#####################################################
# Load required packages, you may need to install them using: install("gdata"), and so on.
#####################################################
packages <- c("gdata", "xts", "parsedate", "rworldmap", "knitr", "uuid", "XML", "taxize", "jsonlite", "worms", "obistools", "stringr", "dplyr", "raster", "sp")
sapply(packages, require, character.only = TRUE); rm(packages)
setwd(dirname(parent.frame(2)$filename))
#####################################################



#####################################################
# Step 1 - Reading in files and basic manipulation
#####################################################

# load in data into dataframes
metadata <- read.xls(filename, sheet = 2, header = FALSE, stringsAsFactors = FALSE, fileEncoding="latin1") 
taxon <- read.xls(filename, skip= 3, sheet = 3, header = FALSE, stringsAsFactors = FALSE, fileEncoding="latin1") 
events <- read.xls(filename, sheet = 4, skip= 2, header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
occurrences <- read.xls(filename, skip= 1, sheet = 5, header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")
measurements <- read.xls(filename, skip= 1, sheet = 6, header = TRUE, stringsAsFactors = FALSE, fileEncoding="latin1")


# Generate UUID for dataset - save it to avoid regenerating.
if(!nchar(meta_uuid) > 1) {
meta_uuid <- UUIDgenerate()
meta_uuid
}

# Step 1 - Pull metadata from metadata sheet and XML file
meta_formatted <- data.frame(Field.Name=character(), Value=character(), stringsAsFactors = FALSE)
meta_formatted[1,] <- c("UniqueDatasetID", meta_uuid)
meta_formatted[2,] <- c("ShortTitle", metadata[3,2])
meta_formatted[3,] <- c("FullTitle", metadata[4,2])
meta_formatted[4,] <- c("DateCreated", metadata[5,2])
meta_formatted[5,] <- c("Citation", metadata[6,2])
meta_formatted[6,] <- c("Abstract", metadata[7,2])
meta_formatted[7,] <- c("GeographicCoverage", metadata[8,2])
meta_formatted[8,] <- c("AccessRights", metadata[9,2])

meta_authors <- metadata[c(12:nrow(metadata)),c(1:13)]

colnames(meta_authors) <- metadata[11,c(1:13)]
meta_authors <- meta_authors[!apply(meta_authors[,c(2:13)] == "", 1, all),]
#####################################################


#####################################################
# Step 2 - Build Taxon Table for SponGIS
#####################################################
# Check for taxon table rows, if not, build taxon table from occurrences
if(!exists("taxon")) {
  print("Taxon sheet is not completed, checking records from SponGIS to see if updates are needed..")
  #If Taxon sheet not filled in, do an update check on species in listed in extendedoccurrences and SponGIS
  scientificNameID <- unique(occurrences$scientificNameID)
  f <- function(x) {
    url1 = paste("https://rest.spongis.org/api//taxon?scientificNameID=eq.", x, sep = "")
    print(url1)
    return(fromJSON(url1))
  }
  taxon <- lapply(scientificNameID, f) %>% bind_rows()
  
  if(nrow(taxon) != length(scientificNameID) && nrow(taxon) != 0) {
    # If any species are in the dataset but missing from SponGIS - We need to work around this
    taxon_missing <- taxon[0, ]
    scientificNameID_missing <- unique(taxon$scientificNameID)
    scientificNameID_missing <- setdiff(scientificNameID,scientificNameID_missing)
    for (i in 1:nchar(scientificNameID_missing)) {
      taxon_missing[i, 1] <- scientificNameID_missing[i]
    }
    taxon <- rbind(taxon, taxon_missing)
  } else if(nrow(taxon) == 0) {
    taxon <- data.frame(matrix(ncol = 11, nrow = 0))
    colnames(taxon) <- c("scientificNameID", "acceptedNameUsageID", "scientificName",	"scientificNameAuthorship", "taxonRemarks",	"taxonDescription",	"taxonImage", "higherClassification", "vernacularName", "taxonRank", "taxonomicStatus")
    scientificNameID_df = as.data.frame(unique(scientificNameID))
    colnames(scientificNameID_df) <- "scientificNameID"
    taxon <- rbind.fill(scientificNameID_df, taxon)
    
    f <- function(x) {
      url1 = paste("https://rest.spongis.org/api//taxon?scientificNameID=eq.", x, sep = "")
      print(url1)
      return(fromJSON(url1))
    }
    taxon_pre_spongis <- lapply(scientificNameID, f) %>% bind_rows()
  }
} else {
  print("Taxon sheet is completed, building taxon table and checking to see if updates are needed..")
  col_list_taxon <- c("scientificNameID",	"acceptedNameUsageID", "scientificName", "scientificNameAuthorship", "taxonRemarks",	"taxonDescription",	"taxonImage")
  colnames(taxon) <- col_list_taxon
  taxon <- taxon[, col_list_taxon]
  taxon$taxonRank <- NA 
  taxon$higherClassification <- NA 
  taxon$vernacularName <- NA
  taxon$taxonomicStatus <- NA
  
  scientificNameID <- unique(taxon$scientificNameID)
  
  f <- function(x) {
    url1 = paste("https://rest.spongis.org/api//taxon?scientificNameID=eq.", x, sep = "")
    print(url1)
    return(fromJSON(url1))
  }
  taxon_pre_spongis <- lapply(scientificNameID, f) %>% bind_rows()
}

# Add Taxonomy from WoRMs
f <- function(x) {
  wormid <- as.wormsid(x[1])
  wormid_taxon <- classification(wormid)
  wormid_taxon_df <- do.call(rbind, wormid_taxon)
  toJSON(wormid_taxon_df)
}
taxon$higherClassification <- apply(taxon, 1, f)

#  Use higherClassification to populate taxonomy
taxon_tree_list = list()
for(i in 1:nrow(taxon)){
  taxon_tree <- fromJSON(taxon$higherClassification[i])
  taxon_tree<-taxon_tree[!(taxon_tree$id==taxon$scientificNameID[i]),]
  taxon$scientificNameID[i]
  taxon_tree$i <- i  # maybe you want to keep track of which iteration produced it?
  taxon_tree_list[[i]] <- taxon_tree # add it to your list
}
taxon_tree_ = do.call(rbind, taxon_tree_list)
taxon_tree_ = unique(taxon_tree_)
taxon_tree_list = as.data.frame(unique(taxon_tree_$id))
colnames(taxon_tree_list) <- "scientificNameID"
taxon <- rbind.fill(taxon_tree_list, taxon)
taxon = taxon[!duplicated(taxon$scientificNameID),]


# Add scientificName from WoRMs if not present
f <- function(x) {
  wormid <- wormsbyid(as.numeric(x[[1]]), verbose=FALSE)
  wormid$scientificname
}
taxon$scientificName <- apply(taxon, 1, f)

# Add Taxonomy from WoRMs
for(i in taxon$scientificNameID){
  wormid <- as.wormsid(i)
  wormid_taxon <- classification(wormid)
  wormid_taxon_df <- rbind(wormid_taxon)
  x <- toJSON(wormid_taxon_df)
  taxon$higherClassification[taxon$scientificNameID==i] <- x
}

# Add highest rank from WoRMs
f <- function(x) {
  wormid <- wormsbyid(as.numeric(x[[1]]), verbose=FALSE)
  wormid$rank
}
taxon$taxonRank <- apply(taxon, 1, f)
# Add Common from WoRMs
f <- function(x) {
  tryCatch({
  common <- do.call(rbind, sci2comm(as.wormsid(x[1]), db = 'worms', simplify = FALSE))
  toJSON(common)
  },
  error=function(e) NA)
}
taxon$vernacularName <- apply(taxon, 1, f)
# Add acceptedNameID
f <- function(x) {
  wormid <- wormsbyid(as.numeric(x[[1]]), verbose=FALSE)
  if (as.numeric(wormid$valid_AphiaID) == as.numeric(x[[1]])) {
    wormid$valid_AphiaID <- NA
  } else {
    wormid$valid_AphiaID
  }
}
taxon$acceptedNameUsageID <- apply(taxon, 1, f)

# Add taxonomicStatus and reason
f <- function(x) {
  wormid <- wormsbyid(as.numeric(x[[1]]), verbose=FALSE)
  if (is.na(wormid$unacceptreason)) {
    wormid$unacceptreason <- ""
  }
  trimws(paste(wormid$status, wormid$unacceptreason))
}
taxon$taxonomicStatus <- apply(taxon, 1, f)
# Add scientificNameAuthorship (gsub to remove () if they are present)
f <- function(x) {
  wormid <- wormsbyid(as.numeric(x[[1]]), verbose=FALSE)
  gsub("[()]", "", wormid$authority)
}
taxon$scientificNameAuthorship <- apply(taxon, 1, f)

taxon[taxon==""]<-NA
taxon[taxon=="[[null]]"]<-NA


# Check SponGIS for scientificNameID, and update if required
scientificNameID <-  as.vector(taxon['scientificNameID'])
f <- function(x) {
  url1 = paste("https://rest.spongis.org/api//taxon?scientificNameID=eq.", x, sep = "")
  return(fromJSON(url1))
}
taxon_spongis <- lapply(scientificNameID[["scientificNameID"]], f) %>% bind_rows()
if(nrow(taxon_spongis) > 0) {
  taxon_spongis <- taxon_spongis[c("scientificNameID",	"acceptedNameUsageID", "scientificName",	"scientificNameAuthorship", "taxonRemarks",	"taxonDescription",	"taxonImage", "taxonRank", "higherClassification", "vernacularName", "taxonomicStatus")]
}
taxon_spongis[] <- lapply(taxon_spongis, as.character)
taxon[] <- lapply(taxon, as.character)
taxon_full_list <- taxon



if(nrow(taxon_spongis) != nrow(taxon) && nrow(taxon_pre_spongis) > 1) {
  # If any species are in the dataset but missing from SponGIS - We need to work around this
  taxon_missing <- taxon_pre_spongis[0, ]
  scientificNameID <- unique(taxon$scientificNameID)
  scientificNameID_missing <- unique(taxon_pre_spongis$scientificNameID)
  scientificNameID_missing <- setdiff(scientificNameID,scientificNameID_missing)
  for (i in 1:length(scientificNameID_missing)) {
    taxon_missing[i, 1] <- scientificNameID_missing[i]
  }
} else if(nrow(taxon_spongis) != nrow(taxon)) {
  taxon_missing <- taxon[0, ]
  scientificNameID <- unique(taxon$scientificNameID)
  scientificNameID_missing <- unique(taxon_spongis$scientificNameID)
  scientificNameID_missing <- setdiff(scientificNameID,scientificNameID_missing)
  for (i in 1:length(scientificNameID_missing)) {
    taxon_missing[i, 1] <- scientificNameID_missing[i]
  }
}
  
  
if(!identical(taxon, taxon_spongis) && nrow(taxon)==nrow(taxon_spongis)) {
  # Attept autofix
  taxon_pre_autofix <- taxon
  #taxon_pre_autofix -> taxon
  taxon_f <- anti_join(taxon, taxon_spongis)
  taxon_b <- anti_join(taxon_spongis, taxon)
  
  idx <- is.na(taxon_f)
  taxon_f[idx] = taxon_b[idx]
  
  taxon <- taxon_f
  
  if(!identical(taxon, taxon_b)) {
      print("Differences between SponGIS taxon records and your data have been detected. We will attempt to fix this and generate a taxon upload table, you will want to compare taxon, taxon_pre_autofix and taxon_spongis prior to committing any updates.")
      print(paste("There are:", nrow(taxon), "that need updating.. But check!"))
  }
  
} else if(!identical(taxon, taxon_spongis) && nrow(taxon) > nrow(taxon_spongis)) {
  print("You have new species that need to be uploaded onto SponGIS")
  if(exists("taxon_missing")) {
    taxon_pre_autofix <- taxon
    taxon_missing <- taxon[ match(scientificNameID_missing,taxon$scientificNameID), ]
    taxon <- taxon[ !(taxon$scientificNameID %in% scientificNameID_missing), ]
    taxon_pre_autofix <- taxon
    taxon_f <- anti_join(taxon, taxon_spongis)
    taxon_b <- anti_join(taxon_spongis, taxon)
    
    idx <- is.na(taxon_f)
    taxon_f[idx] = taxon_b[idx]
    
    taxon <- taxon_f
    taxon <- rbind(taxon, taxon_missing)
    if(!identical(taxon, taxon_b)) {
      print("Differences between SponGIS taxon records and your data have been detected. We will attempt to fix this and generate a taxon upload table, you will want to compare taxon, taxon_pre_autofix and taxon_spongis prior to committing any updates.")
      print(paste("There are:", nrow(taxon), "that need updating.. But check!"))
    }
    rm(taxon_b, taxon_f, taxon_missing)
  } 
  
} else {
  print("Species listed on this taxon sheet are the same as what is currently on SponGIS.. No updates needed, the taxon sheet does not need uploading to SponGIS")
}

# Check and update occurrence sheet with taxon information from worms
for(i in taxon$scientificNameID){
  occurrences$scientificNameAuthorship[occurrences$scientificNameID == i] <- taxon_full_list$scientificNameAuthorship[taxon_full_list$scientificNameID == i]
  occurrences$scientificName[occurrences$scientificNameID == i] <- taxon_full_list$scientificName[taxon_full_list$scientificNameID == i]
  occurrences$acceptedNameUsageID[occurrences$scientificNameID == i] <- taxon_full_list$acceptedNameUsageID[taxon_full_list$scientificNameID == i]
  occurrences$taxonRank[occurrences$scientificNameID == i] <- taxon_full_list$taxonRank[taxon_full_list$scientificNameID == i]  
}
#####################################################


#####################################################
# Step 3 - Clean columns to remove extraneous columns from Excel import and 
# check that minimum required columns are present for SponGIS
#####################################################
col_list_events <- c("eventID","parentEventID","eventDate","eventRemarks","habitat","locationID","continent","waterBody","country","locality","minimumDepthInMeters","maximumDepthInMeters","decimalLatitude","decimalLongitude","coordinateUncertaintyInMeters","geodeticDatum","locationAccordingTo","locationRemarks","footprintWKT")

if(exists("occurrences")) {
col_list_occurrences <- c("eventID","scientificNameID","scientificName","scientificNameAuthorship","taxonRank","acceptedNameUsageID","identifiedBy","dateIdentified","identificationReferences","identificationRemarks","identificationQualifier","typeStatus","catalogNumber","occurrenceRemarks","recordedBy","occurrenceStatus","preparations","associatedMedia","associatedReferences","associatedSequences","modified","collectionCode","basisOfRecord","dataGeneralisations","dynamicProperties")
}

if(exists("measurements")) {
  col_list_measurements <- c("eventID","measurementID","measurementType","measurementTypeID","measurementValue","measurementValueID","measurementUnit","measurementUnitID","measurementAccuracy","measurementDate","measurementDeterminedBy","measurementRemarks")
}

### Main Data Checks
# Generate a data-frame of import statistics
status <- data.frame(Input.Sheet=character(), Columns.Imported=character(), Columns.Dropped=character(),Rows.Imported=character(),Import.Status=character(), stringsAsFactors = FALSE) 

# Events
if(exists("events")){
events_ncols_imported <- ncol(events)
events <- events[, col_list_events]
events_ncols_dropped <- events_ncols_imported - ncol(events)
events_nrows <- nrow(events)
event_status <- if(max(match(col_list_events, colnames(events))) == length(col_list_events)) { "Successful";} else {"WARNING: Missing columns";}

status[1,] <- c("Events", events_ncols_imported, events_ncols_dropped, events_nrows, event_status)
events$dataID <- meta_uuid
events <-events[, c(20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
}

# Occurrences
if(exists("occurrences")) {
occurrences_ncols_imported <- ncol(occurrences)
occurrences <- occurrences[, col_list_occurrences]
occurrences_ncols_dropped <- occurrences_ncols_imported - ncol(occurrences)
occurrences_nrows <- nrow(occurrences)
occurrences_status <- if(max(match(col_list_occurrences, colnames(occurrences))) == length(col_list_occurrences)) { "Successful";} else {"WARNING: Missing columns";}

status[2,] <- c("Occurrences", occurrences_ncols_imported, occurrences_ncols_dropped, occurrences_nrows, occurrences_status)
}

# Measurements
if(exists("measurements")){
measurements_ncols_imported <- ncol(measurements)
measurements <- measurements[, col_list_measurements]
measurements_ncols_dropped <- measurements_ncols_imported - ncol(measurements)
measurements_nrows <- nrow(measurements)
measurements_status <- if(max(match(col_list_measurements, colnames(measurements))) == length(col_list_measurements)) { "Successful";} else {"WARNING: Missing columns";}

status[3,] <- c("Measurements", measurements_ncols_imported, measurements_ncols_dropped, measurements_nrows, measurements_status)
}

# Step 2 - Checks 

# Check dates for compliance to ISO8601
na_date_rows <- sum(is.na(events$eventDate))
invalid_date_rows_2 <- check_eventdate(events)
invalid_date_rows_3 <- paste("There are: ", nrow(invalid_date_rows_2), " potentially invalid dates to check, and you are missing: ", na_date_rows, "dates.")

# 3 Check that all spatial values are in Decimal Degrees, and have corresponding coordinateUncertainty and geodeticDatum values
missing_spatial_rows <- which(is.na(events$decimalLatitude) & is.na(events$decimalLongitude), arr.ind=TRUE) + 3
missing_spatial_rows_2 <- nchar(missing_spatial_rows)

# Build Footprint of Dataset
max_lon <- max(events$decimalLongitude, na.rm=TRUE)
max_lat <- max(events$decimalLatitude, na.rm=TRUE)
min_lon <- min(events$decimalLongitude, na.rm=TRUE)
min_lat <- min(events$decimalLatitude, na.rm=TRUE)

## If single point, extraploate to 0.25 degree to provide extra 
if(max_lon == min_lon){
max_lon = max_lon + 0.125
}
if(max_lat == min_lat){
  max_lat = max_lat + 0.125
}

footprintwkt <- paste("POLYGON((", 
                      min_lon, min_lat, ",",
                      max_lon, min_lat, ",", 
                      max_lon, max_lat, ",",
                      min_lon, max_lat, "))")

meta_formatted[9,] <- c("FootprintWKT", footprintwkt)
#####################################################

#####################################################
# Step 4 - Get environmental data from GlobENV and add records to Measurement Table
#####################################################
if(exists("occurrences") && !exists("measurements")) {
measurements <- data.frame("eventID"=character(),"measurementID"=character(),"measurementType"=character(),"measurementTypeID"=character(),"measurementValue"=character(),"measurementValueID"=character(),"measurementUnit"=character(),"measurementUnitID"=character(),"measurementAccuracy"=character(),"measurementDate"=character(),"measurementDeterminedBy"=character(),"measurementRemarks"=character())
}

# raster_brick <- brick(raster("GlobENV/sal_.tif"), raster("GlobENV/diso2_.tif"), raster("GlobENV/temp_.tif"))
# outfile <- writeRaster(raster_brick, filename='sal.tif', format="GTiff", overwrite=TRUE,options=c("INTERLEAVE=BAND","COMPRESS=LZW"))

if(exists("occurrences")) {
raster_brick <- brick(filename_globenv)
names(raster_brick) <- c("sal","diso2","temp")
spdf <- SpatialPointsDataFrame(events[,c(15:14)], proj4string=CRS("+proj=longlat +datum=WGS84"), events)
spdf <- suppressWarnings(extract(raster_brick, spdf, sp = T))
spdf <- as.data.frame(spdf)

glob_sal_events <- na.omit(spdf[,c(2,21)])
glob_diso2_events <- na.omit(spdf[,c(2,22)])
glob_temp_events <- na.omit(spdf[,c(2,23)])

glob_sal_events$measurementID <- paste(glob_sal_events$eventID, "globenv-sal", sep="-")
glob_sal_events$measurementType <- "Extrapolated value of Salinity at the seafloor from GlobENV model derived from WOA13 and GEBCO14 data"
glob_sal_events$measurementTypeID <- "http://vocab.nerc.ac.uk/collection/C00/current/GEBCO1401/"
glob_sal_events$measurementValue <- glob_sal_events$sal
glob_sal_events$measurementValueID <- "https://www.nodc.noaa.gov/OC5/woa13/"
glob_sal_events$measurementUnit <- "unitless"
glob_sal_events$measurementDeterminedBy <- "Davies AJ, URI"
glob_sal_events$measurementUnitID <- NA
glob_sal_events$measurementAccuracy <- NA
glob_sal_events$measurementDate <- NA
glob_sal_events$measurementRemarks <- NA
glob_sal_events <- glob_sal_events[,-2]

glob_diso2_events$measurementID <- paste(glob_diso2_events$eventID, "globenv-diso2", sep="-")
glob_diso2_events$measurementType <- "Extrapolated value of Dissolved Oxygen at the seafloor from GlobENV model derived from WOA13 and GEBCO14 data"
glob_diso2_events$measurementTypeID <- "http://vocab.nerc.ac.uk/collection/C00/current/GEBCO1401/"
glob_diso2_events$measurementValue <- glob_diso2_events$diso2
glob_diso2_events$measurementValueID <- "https://www.nodc.noaa.gov/OC5/woa13/"
glob_diso2_events$measurementUnit <- "ml/l"
glob_diso2_events$measurementDeterminedBy <- "Davies AJ, URI"
glob_diso2_events$measurementUnitID <- NA
glob_diso2_events$measurementAccuracy <- NA
glob_diso2_events$measurementDate <- NA
glob_diso2_events$measurementRemarks <- NA
glob_diso2_events <- glob_diso2_events[,-2]

glob_temp_events$measurementID <- paste(glob_temp_events$eventID, "globenv-temp", sep="-")
glob_temp_events$measurementType <- "Extrapolated value of Temperature at the seafloor from GlobENV model derived from WOA13 and GEBCO14 data"
glob_temp_events$measurementTypeID <- "http://vocab.nerc.ac.uk/collection/C00/current/GEBCO1401/"
glob_temp_events$measurementValue <- glob_temp_events$temp
glob_temp_events$measurementValueID <- "https://www.nodc.noaa.gov/OC5/woa13/"
glob_temp_events$measurementUnit <- "degrees centigrade"
glob_temp_events$measurementDeterminedBy <- "Davies AJ, URI"
glob_temp_events$measurementUnitID <- NA
glob_temp_events$measurementAccuracy <- NA
glob_temp_events$measurementDate <- NA
glob_temp_events$measurementRemarks <- NA
glob_temp_events <- glob_temp_events[,-2]

measurements <- rbind(measurements, glob_diso2_events, glob_sal_events, glob_temp_events)
rm(glob_diso2_events, glob_sal_events, glob_temp_events)
}
#####################################################

#####################################################
# Step 5 - Interactive checks on the data, you may need to go back to source and fix
#####################################################
status
plot_map_leaflet(events)
check_extension_eventids(events, occurrences)
check_extension_eventids(occurrences, events)
check_extension_eventids(events, measurements)
check_eventdate(events)
which(duplicated(events$eventID))+3
dupe_test <- paste(occurrences$eventID,occurrences$scientificNameID,sep="-")
which(duplicated(dupe_test))+3
dupe_test <- paste(measurements$eventID,measurements$measurementID,sep="-")
which(duplicated(dupe_test))+3
check_fields(occurrences)

#####################################################


#####################################################
# Step 6 - Build outputs if everything has gone OK
#####################################################
## Trim white space, leading and trailing
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## Meta Table
spongis_meta <- as.data.frame(t(meta_formatted))
colnames(spongis_meta) <- c("dataID", "title_short", "title", "created", "bibliographicCitation", "description","geographicoverage","accessRights", "footprintWKT")
spongis_meta <- spongis_meta[-1, ]
spongis_meta$institutionCode <- NA

spongis_meta_creator <- meta_authors[which(meta_authors$`Author Type` == "Rights Holder"), ]
spongis_meta_creator <- spongis_meta_creator[, -1]
spongis_meta$rightsHolder <- paste(spongis_meta_creator$`First Name`, " ", spongis_meta_creator$`Last Name`)
spongis_meta_creator[spongis_meta_creator == ""] <- NA
spongis_meta_creator <- spongis_meta_creator[,colSums(is.na(spongis_meta_creator))<nrow(spongis_meta_creator)]
spongis_meta_creator <- spongis_meta_creator[, c(-1,-2)]
if(ncol(as.data.frame(spongis_meta_creator)) > 1) {
  data <- apply(spongis_meta_creator, 1, paste, collapse = ", " )
} else {
  data <- spongis_meta_creator
}
spongis_meta$rightsHolder <- paste(spongis_meta$rightsHolder, data, sep=", ")
rm(spongis_meta_creator)
                                   
spongis_meta_creator <- meta_authors[which(meta_authors$`Author Type` == "Resource Creator"), ]
spongis_meta_creator <- spongis_meta_creator[, -1]
spongis_meta$creator <- paste(spongis_meta_creator$`First Name`, " ", spongis_meta_creator$`Last Name`)
spongis_meta_creator[spongis_meta_creator == ""] <- NA
spongis_meta_creator <- spongis_meta_creator[,colSums(is.na(spongis_meta_creator))<nrow(spongis_meta_creator)]
spongis_meta_creator <- spongis_meta_creator[, c(-1,-2)]
if(ncol(as.data.frame(spongis_meta_creator)) > 1) {
  data <- apply(spongis_meta_creator, 1, paste, collapse = ", " )
} else {
  data <- spongis_meta_creator
}
spongis_meta$creator <- paste(spongis_meta$creator, data, sep=", ")

spongis_meta2 <-spongis_meta[, c(1,3,4,5,6,12,9,11,8)]
spongis_meta2$creator <- gsub("\\s+", " ", spongis_meta2$creator)
spongis_meta2$rightsHolder <- gsub("\\s+", " ", spongis_meta2$rightsHolder)

# Create XML for DWC-A - eml.xml - the dataset xml
eml = newXMLDoc()
eml_ = newXMLNode("eml", parent=eml)
dataset = newXMLNode("dataset", parent=eml_)
alternateIdentifier = newXMLNode("alternateIdentifier", spongis_meta2$dataID, parent=dataset)
title = newXMLNode("title", spongis_meta2$title, parent=dataset,  attrs = c("xml:lang" = "eng"),  suppressNamespaceWarning = getOption("suppressXMLNamespaceWarning", TRUE))
creator = newXMLNode("creator", parent=dataset)
individualName = newXMLNode("individualName", parent=creator)
givenName = newXMLNode("givenName", trim(meta_authors$`First Name`[meta_authors$`Author Type` == "Rights Holder"]), parent=individualName)
surName = newXMLNode("surName",  trim(meta_authors$`Last Name`[meta_authors$`Author Type` == "Rights Holder"]), parent=individualName)
organization = newXMLNode("organizationName", trim(meta_authors$Organization[meta_authors$`Author Type` == "Rights Holder"]), parent=creator)
positionName = newXMLNode("positionName", trim(meta_authors$Position[meta_authors$`Author Type` == "Rights Holder"]), parent=creator)
if(nchar(trim(meta_authors$Email[meta_authors$`Author Type` == "Rights Holder"]))>1){ email = newXMLNode("electronicMailAddress", trim(meta_authors$Email[meta_authors$`Author Type` == "Rights Holder"]), parent=creator) }
if(nchar(trim(meta_authors$Address[meta_authors$`Author Type` == "Rights Holder"]))>1 ||
   nchar(trim(meta_authors$City[meta_authors$`Author Type` == "Rights Holder"]))>1 ||
   nchar(trim(meta_authors$`State/Province`[meta_authors$`Author Type` == "Rights Holder"]))>1 ||
   nchar(trim(meta_authors$Country[meta_authors$`Author Type` == "Rights Holder"]))>1 ||
   nchar(trim(meta_authors$`Zip/Postal Code`[meta_authors$`Author Type` == "Rights Holder"]))>1) {
  address = newXMLNode("address", parent=creator)
}
if(nchar(trim(meta_authors$Address[meta_authors$`Author Type` == "Rights Holder"]))>1){ address1 = newXMLNode("address", trim(meta_authors$Address[meta_authors$`Author Type` == "Rights Holder"]), parent=address) }
if(nchar(trim(meta_authors$City[meta_authors$`Author Type` == "Rights Holder"]))>1){ city = newXMLNode("city", trim(meta_authors$City[meta_authors$`Author Type` == "Rights Holder"]), parent=address) }
if(nchar(trim(meta_authors$`State/Province`[meta_authors$`Author Type` == "Rights Holder"]))>1){ state = newXMLNode("state", trim(meta_authors$`State/Province`[meta_authors$`Author Type` == "Rights Holder"]), parent=address) }
if(nchar(trim(meta_authors$Country[meta_authors$`Author Type` == "Rights Holder"]))>1){ country = newXMLNode("country", trim(meta_authors$Country[meta_authors$`Author Type` == "Rights Holder"]), parent=address) }
if(nchar(trim(meta_authors$`Zip/Postal Code`[meta_authors$`Author Type` == "Rights Holder"]))>1){ zippostal = newXMLNode("zippostal", trim(meta_authors$`Zip/Postal Code`[meta_authors$`Author Type` == "Rights Holder"]), parent=address) }
md_metadataProvider = newXMLNode("metadataProvider", parent=dataset)
md_individualName = newXMLNode("individualName", parent=md_metadataProvider)
md_givenName = newXMLNode("givenName", trim(meta_authors$`First Name`[meta_authors$`Author Type` == "Resource Creator"]), parent=md_individualName)
md_surName = newXMLNode("surName",  trim(meta_authors$`Last Name`[meta_authors$`Author Type` == "Resource Creator"]), parent=md_individualName)
md_organization = newXMLNode("organizationName", trim(meta_authors$Organization[meta_authors$`Author Type` == "Resource Creator"]), parent=md_metadataProvider)
md_positionName = newXMLNode("positionName", trim(meta_authors$Position[meta_authors$`Author Type` == "Resource Creator"]), parent=md_metadataProvider)
if(nchar(trim(meta_authors$Email[meta_authors$`Author Type` == "Resource Creator"]))>1){ md_email = newXMLNode("electronicMailAddress", trim(meta_authors$Email[meta_authors$`Author Type` == "Resource Creator"]), parent=md_metadataProvider) }
if(nchar(trim(meta_authors$Address[meta_authors$`Author Type` == "Resource Creator"]))>1 ||
   nchar(trim(meta_authors$City[meta_authors$`Author Type` == "Resource Creator"]))>1 ||
   nchar(trim(meta_authors$`State/Province`[meta_authors$`Author Type` == "Resource Creator"]))>1 ||
   nchar(trim(meta_authors$Country[meta_authors$`Author Type` == "Resource Creator"]))>1 ||
   nchar(trim(meta_authors$`Zip/Postal Code`[meta_authors$`Author Type` == "Resource Creator"]))>1) {
  md_address = newXMLNode("address", parent=md_metadataProvider)
}
if(nchar(trim(meta_authors$Address[meta_authors$`Author Type` == "Resource Creator"]))>1){ md_address1 = newXMLNode("address", trim(meta_authors$Address[meta_authors$`Author Type` == "Resource Creator"]), parent=md_address) }
if(nchar(trim(meta_authors$City[meta_authors$`Author Type` == "Resource Creator"]))>1){ md_city = newXMLNode("city", trim(meta_authors$City[meta_authors$`Author Type` == "Resource Creator"]), parent=md_address) }
if(nchar(trim(meta_authors$`State/Province`[meta_authors$`Author Type` == "Resource Creator"]))>1){ md_state = newXMLNode("state", trim(meta_authors$`State/Province`[meta_authors$`Author Type` == "Resource Creator"]), parent=md_address) }
if(nchar(trim(meta_authors$Country[meta_authors$`Author Type` == "Resource Creator"]))>1){ md_country = newXMLNode("country", trim(meta_authors$Country[meta_authors$`Author Type` == "Resource Creator"]), parent=md_address) }
if(nchar(trim(meta_authors$`Zip/Postal Code`[meta_authors$`Author Type` == "Resource Creator"]))>1){ md_zippostal = newXMLNode("zippostal", trim(meta_authors$`Zip/Postal Code`[meta_authors$`Author Type` == "Resource Creator"]), parent=md_address) }
pubDate = newXMLNode("pubDate", spongis_meta2$created, parent=dataset)
language = newXMLNode("language", "eng", parent=dataset)
abstract = newXMLNode("abstract", parent=dataset)
abstract_para = newXMLNode("para", spongis_meta2$description, parent=abstract)

if(spongis_meta2$accessRights == "CC0 1.0"){
  intellectualRights = newXMLNode("intellectualRights", '<para>To the extent possible under law, the publisher has waived all rights to these data and has dedicated them to the <ulink url="http://creativecommons.org/publicdomain/zero/1.0/legalcode"><citetitle>Public Domain (CC0 1.0)</citetitle></ulink>. Users may copy, modify, distribute and use the work, including for commercial purposes, without restriction.</para>', parent=dataset) }

coverage = newXMLNode("coverage", parent=dataset)
geographicCoverage = newXMLNode("geographicCoverage", parent=coverage)
geographicDescription = newXMLNode("geographicDescription", spongis_meta$geographicoverage, parent=geographicCoverage)
boundingCoordinates = newXMLNode("boundingCoordinates", parent=geographicCoverage)
westBoundingCoordinate = newXMLNode("westBoundingCoordinate", min_lon, parent=boundingCoordinates)
eastBoundingCoordinate = newXMLNode("eastBoundingCoordinate", max_lon, parent=boundingCoordinates)
northBoundingCoordinate = newXMLNode("northBoundingCoordinate", max_lat, parent=boundingCoordinates)
southBoundingCoordinate = newXMLNode("southBoundingCoordinate", min_lat, parent=boundingCoordinates)
taxonomicCoverage = newXMLNode("taxonomicCoverage", parent=coverage)
generalTaxonomicCoverage = newXMLNode("generalTaxonomicCoverage", "Deep-sea Sponges", parent=taxonomicCoverage)
taxonomicClassification = newXMLNode("taxonomicClassification", parent=taxonomicCoverage)
taxonRankName = newXMLNode("taxonRankName", "phylum", parent=taxonomicClassification)
taxonRankValue = newXMLNode("taxonRankValue", "Porifera", parent=taxonomicClassification)
maintenance = newXMLNode("maintenance", parent=dataset)
main_description = newXMLNode("description", parent=maintenance)
main_para = newXMLNode("para", "", parent=main_description)
main_update = newXMLNode("maintenanceUpdateFrequency", "unknown", parent=main_description)
co_contact = newXMLNode("contact", parent=dataset)
co_individualName = newXMLNode("individualName", parent=co_contact)
co_givenName = newXMLNode("givenName", trim(meta_authors$`First Name`[meta_authors$`Author Type` == "Resource Creator"]), parent=co_individualName)
co_surName = newXMLNode("surName",  trim(meta_authors$`Last Name`[meta_authors$`Author Type` == "Resource Creator"]), parent=co_individualName)
co_organization = newXMLNode("organizationName", trim(meta_authors$Organization[meta_authors$`Author Type` == "Resource Creator"]), parent=co_contact)
co_positionName = newXMLNode("positionName", trim(meta_authors$Position[meta_authors$`Author Type` == "Resource Creator"]), parent=co_contact)
if(nchar(trim(meta_authors$Email[meta_authors$`Author Type` == "Resource Creator"]))>1){ co_email = newXMLNode("electronicMailAddress", trim(meta_authors$Email[meta_authors$`Author Type` == "Resource Creator"]), parent=co_contact) }
if(nchar(trim(meta_authors$Address[meta_authors$`Author Type` == "Resource Creator"]))>1 ||
   nchar(trim(meta_authors$City[meta_authors$`Author Type` == "Resource Creator"]))>1 ||
   nchar(trim(meta_authors$`State/Province`[meta_authors$`Author Type` == "Resource Creator"]))>1 ||
   nchar(trim(meta_authors$Country[meta_authors$`Author Type` == "Resource Creator"]))>1 ||
   nchar(trim(meta_authors$`Zip/Postal Code`[meta_authors$`Author Type` == "Resource Creator"]))>1) {
  co_address = newXMLNode("address", parent=co_contact)
}
if(nchar(trim(meta_authors$Address[meta_authors$`Author Type` == "Resource Creator"]))>1){ co_address1 = newXMLNode("address", trim(meta_authors$Address[meta_authors$`Author Type` == "Resource Creator"]), parent=co_address) }
if(nchar(trim(meta_authors$City[meta_authors$`Author Type` == "Resource Creator"]))>1){ co_city = newXMLNode("city", trim(meta_authors$City[meta_authors$`Author Type` == "Resource Creator"]), parent=co_address) }
if(nchar(trim(meta_authors$`State/Province`[meta_authors$`Author Type` == "Resource Creator"]))>1){ co_state = newXMLNode("state", trim(meta_authors$`State/Province`[meta_authors$`Author Type` == "Resource Creator"]), parent=co_address) }
if(nchar(trim(meta_authors$Country[meta_authors$`Author Type` == "Resource Creator"]))>1){ co_country = newXMLNode("country", trim(meta_authors$Country[meta_authors$`Author Type` == "Resource Creator"]), parent=co_address) }
if(nchar(trim(meta_authors$`Zip/Postal Code`[meta_authors$`Author Type` == "Resource Creator"]))>1){ co_zippostal = newXMLNode("zippostal", trim(meta_authors$`Zip/Postal Code`[meta_authors$`Author Type` == "Resource Creator"]), parent=co_address) }

cat(saveXML(eml, indent = TRUE, prefix = ""), file="eml.xml")

# meta.xml
meta = newXMLDoc()
archive = newXMLNode("archive", parent=meta,  namespaceDefinitions = c("http://rs.tdwg.org/dwc/text/"), attrs = c("metadata" = "eml.xml"),  suppressNamespaceWarning = getOption("suppressXMLNamespaceWarning", TRUE))
core = newXMLNode("core", parent=archive, attrs = c("encoding" = "UTF-8", "fieldsTerminatedBy" = "\\t", "linesTerminatedBy" = "\\n", "fieldsEnclosedBy" = "", "ignoreHeaderLines" = "1", "rowType" = "http://rs.tdwg.org/dwc/terms/Event"),  suppressNamespaceWarning = getOption("suppressXMLNamespaceWarning", TRUE))
e_files = newXMLNode("files", parent=core)
e_files_location = newXMLNode("location", "event.txt", parent=e_files)
e_id = newXMLNode("id", attrs = c("index" = "0"), parent=core)
e1 = newXMLNode("field", attrs = c("index" = "1", "term" = "http://purl.org/dc/terms/modified"), parent=core)
e2 = newXMLNode("field", attrs = c("index" = "2", "term" = "http://rs.tdwg.org/dwc/terms/eventID"), parent=core)
e3 = newXMLNode("field", attrs = c("index" = "3", "term" = "http://rs.tdwg.org/dwc/terms/parentEventID"), parent=core)
e4 = newXMLNode("field", attrs = c("index" = "4", "term" = "http://rs.tdwg.org/dwc/terms/eventDate"), parent=core)
e5 = newXMLNode("field", attrs = c("index" = "5", "term" = "http://rs.tdwg.org/dwc/terms/eventRemarks"), parent=core)
e6 = newXMLNode("field", attrs = c("index" = "6", "term" = "http://rs.tdwg.org/dwc/terms/habitat"), parent=core)
e7 = newXMLNode("field", attrs = c("index" = "7", "term" = "http://rs.tdwg.org/dwc/terms/locationID"), parent=core)
e8 = newXMLNode("field", attrs = c("index" = "8", "term" = "http://rs.tdwg.org/dwc/terms/continent"), parent=core)
e9 = newXMLNode("field", attrs = c("index" = "9", "term" = "http://rs.tdwg.org/dwc/terms/waterBody"), parent=core)
e10 = newXMLNode("field", attrs = c("index" = "10", "term" = "http://rs.tdwg.org/dwc/terms/country"), parent=core)
e11 = newXMLNode("field", attrs = c("index" = "11", "term" = "http://rs.tdwg.org/dwc/terms/locality"), parent=core)
e12 = newXMLNode("field", attrs = c("index" = "12", "term" = "http://rs.tdwg.org/dwc/terms/minimumDepthInMeters"), parent=core)
e13 = newXMLNode("field", attrs = c("index" = "13", "term" = "http://rs.tdwg.org/dwc/terms/maximumDepthInMeters"), parent=core)
e14 = newXMLNode("field", attrs = c("index" = "14", "term" = "http://rs.tdwg.org/dwc/terms/decimalLatitude"), parent=core)
e15 = newXMLNode("field", attrs = c("index" = "15", "term" = "http://rs.tdwg.org/dwc/terms/decimalLongitude"), parent=core)
e16 = newXMLNode("field", attrs = c("index" = "14", "term" = "http://rs.tdwg.org/dwc/terms/coordinateUncertaintyInMeters"), parent=core)
e17 = newXMLNode("field", attrs = c("index" = "15", "term" = "http://rs.tdwg.org/dwc/terms/geodeticDatum"), parent=core)
e18 = newXMLNode("field", attrs = c("index" = "16", "term" = "http://rs.tdwg.org/dwc/terms/locationAccordingTo"), parent=core)
e19 = newXMLNode("field", attrs = c("index" = "17", "term" = "http://rs.tdwg.org/dwc/terms/locationRemarks"), parent=core)
e20 = newXMLNode("field", attrs = c("index" = "18", "term" = "http://rs.tdwg.org/dwc/terms/footprintWKT"), parent=core)

if(exists("measurements")){
  extension = newXMLNode("extension", parent=archive, attrs = c("encoding" = "UTF-8", "fieldsTerminatedBy" = "\\t", "linesTerminatedBy" = "\\n", "fieldsEnclosedBy" = "", "ignoreHeaderLines" = "1", "rowType" = "http://rs.iobis.org/obis/terms/ExtendedMeasurementOrFact"),  suppressNamespaceWarning = getOption("suppressXMLNamespaceWarning", TRUE))
  exm_files = newXMLNode("files", parent=extension)
  exm_files_location = newXMLNode("location", "extendedmeasurementorfact.txt", parent=exm_files)
  exm_id = newXMLNode("coreid", attrs = c("index" = "0"), parent=extension)
  exm1 = newXMLNode("field", attrs = c("index" = "1", "term" = "http://rs.tdwg.org/dwc/terms/eventID"), parent=extension)
  exm2 = newXMLNode("field", attrs = c("index" = "2", "term" = "http://rs.tdwg.org/dwc/terms/measurementID"), parent=extension)
  exm3 = newXMLNode("field", attrs = c("index" = "3", "term" = "http://rs.tdwg.org/dwc/terms/measurementType"), parent=extension)
  exm4 = newXMLNode("field", attrs = c("index" = "4", "term" = "http://rs.gbif.org/extension/obis/extended_measurement_or_fact.xml"), parent=extension)
  exm5 = newXMLNode("field", attrs = c("index" = "5", "term" = "http://rs.tdwg.org/dwc/terms/measurementValue"), parent=extension)
  exm6 = newXMLNode("field", attrs = c("index" = "6", "term" = "http://rs.gbif.org/extension/obis/extended_measurement_or_fact.xml"), parent=extension)
  exm7 = newXMLNode("field", attrs = c("index" = "7", "term" = "http://rs.tdwg.org/dwc/terms/measurementUnit"), parent=extension)
  exm8 = newXMLNode("field", attrs = c("index" = "8", "term" = "http://rs.gbif.org/extension/obis/extended_measurement_or_fact.xml"), parent=extension)
  exm9 = newXMLNode("field", attrs = c("index" = "9", "term" = "http://rs.tdwg.org/dwc/terms/measurementAccuracy"), parent=extension)
  exm10 = newXMLNode("field", attrs = c("index" = "10", "term" = "http://rs.tdwg.org/dwc/terms/measurementDeterminedDate"), parent=extension)
  exm11 = newXMLNode("field", attrs = c("index" = "11", "term" = "http://rs.tdwg.org/dwc/terms/measurementDeterminedBy"), parent=extension)
  exm12 = newXMLNode("field", attrs = c("index" = "12", "term" = "http://rs.tdwg.org/dwc/terms/measurementRemarks"), parent=extension)
}

if(exists("occurrences")){
  extensiono = newXMLNode("extension", parent=archive, attrs = c("encoding" = "UTF-8", "fieldsTerminatedBy" = "\\t", "linesTerminatedBy" = "\\n", "fieldsEnclosedBy" = "", "ignoreHeaderLines" = "1", "rowType" = "http://rs.tdwg.org/dwc/terms/Occurrence"),  suppressNamespaceWarning = getOption("suppressXMLNamespaceWarning", TRUE))
  exo_files = newXMLNode("files", parent=extensiono)
  exo_files_location = newXMLNode("location", "occurrence.txt", parent=exo_files)
  exo_id = newXMLNode("coreid", attrs = c("index" = "0"), parent=extensiono)
  exo1 = newXMLNode("field", attrs = c("index" = "1", "term" = "http://rs.tdwg.org/dwc/terms/eventID"), parent=extensiono)
  exo2 = newXMLNode("field", attrs = c("index" = "2", "term" = "http://rs.tdwg.org/dwc/terms/scientificNameID"), parent=extensiono)
  exo3 = newXMLNode("field", attrs = c("index" = "3", "term" = "http://rs.tdwg.org/dwc/terms/scientificName"), parent=extensiono)
  exo4 = newXMLNode("field", attrs = c("index" = "4", "term" = "http://rs.tdwg.org/dwc/terms/scientificNameAuthorship"), parent=extensiono)
  exo5 = newXMLNode("field", attrs = c("index" = "5", "term" = "http://rs.tdwg.org/dwc/terms/taxonRank"), parent=extensiono)
  exo6 = newXMLNode("field", attrs = c("index" = "6", "term" = "http://rs.tdwg.org/dwc/terms/acceptedNameUsageID"), parent=extensiono)
  exo7 = newXMLNode("field", attrs = c("index" = "7", "term" = "http://rs.tdwg.org/dwc/terms/identifiedBy"), parent=extensiono)
  exo8 = newXMLNode("field", attrs = c("index" = "8", "term" = "http://rs.tdwg.org/dwc/terms/dateIdentified"), parent=extensiono)
  exo9 = newXMLNode("field", attrs = c("index" = "9", "term" = "http://rs.tdwg.org/dwc/terms/identificationReferences"), parent=extensiono)
  exo10 = newXMLNode("field", attrs = c("index" = "10", "term" = "http://rs.tdwg.org/dwc/terms/identificationRemarks"), parent=extensiono)
  exo11 = newXMLNode("field", attrs = c("index" = "11", "term" = "http://rs.tdwg.org/dwc/terms/identificationQualifier"), parent=extensiono)
  exo12 = newXMLNode("field", attrs = c("index" = "12", "term" = "http://rs.tdwg.org/dwc/terms/typeStatus"), parent=extensiono)
  exo13 = newXMLNode("field", attrs = c("index" = "13", "term" = "http://rs.tdwg.org/dwc/terms/catalogNumber"), parent=extensiono)
  exo14 = newXMLNode("field", attrs = c("index" = "14", "term" = "http://rs.tdwg.org/dwc/terms/occurrenceRemarks"), parent=extensiono)
  exo15 = newXMLNode("field", attrs = c("index" = "15", "term" = "http://rs.tdwg.org/dwc/terms/recordedBy"), parent=extensiono)
  exo16 = newXMLNode("field", attrs = c("index" = "16", "term" = "http://rs.tdwg.org/dwc/terms/occurrenceStatus"), parent=extensiono)
  exo17 = newXMLNode("field", attrs = c("index" = "17", "term" = "http://rs.tdwg.org/dwc/terms/preparations"), parent=extensiono)
  exo18 = newXMLNode("field", attrs = c("index" = "18", "term" = "http://rs.tdwg.org/dwc/terms/associatedMedia"), parent=extensiono)
  exo19 = newXMLNode("field", attrs = c("index" = "19", "term" = "http://rs.tdwg.org/dwc/terms/associatedReferences"), parent=extensiono)
  exo20 = newXMLNode("field", attrs = c("index" = "20", "term" = "http://rs.tdwg.org/dwc/terms/associatedSequences"), parent=extensiono)
  exo21 = newXMLNode("field", attrs = c("index" = "21", "term" = "http://rs.tdwg.org/dwc/terms/dcterms:modified"), parent=extensiono)
  exo22 = newXMLNode("field", attrs = c("index" = "22", "term" = "http://rs.tdwg.org/dwc/terms/collectionCode"), parent=extensiono)
  exo23 = newXMLNode("field", attrs = c("index" = "23", "term" = "http://rs.tdwg.org/dwc/terms/basisOfRecord"), parent=extensiono)
  exo24 = newXMLNode("field", attrs = c("index" = "24", "term" = "http://rs.tdwg.org/dwc/terms/dataGeneralizations"), parent=extensiono)
  exo25 = newXMLNode("field", attrs = c("index" = "25", "term" = "http://rs.tdwg.org/dwc/terms/dynamicProperties"), parent=extensiono)
  exo26 = newXMLNode("field", attrs = c("index" = "26", "term" = "http://rs.tdwg.org/dwc/terms/occurrenceID"), parent=extensiono)
}

cat(saveXML(meta, indent = TRUE, prefix = ""), file="meta.xml")
files <- "eml.xml"
files <- c(files, "meta.xml")

# DWC-A file outputs

# event.txt
write.table(x = events, file = "UPLOAD-3_spongis_table-events.tab", row.names=FALSE, col.names = FALSE, fileEncoding="UTF-8", na = "", sep="\t", eol="\n", quote = FALSE)
events <- events[,-1]
events$id = events$eventID
events$modified = NA
events <- events[, c(20,21,1:19)]
write.table(x = events, sep="\t", eol="\n", file = "event.txt", row.names=FALSE, fileEncoding="UTF-8", na = "", quote = FALSE)
files <- c(files, "event.txt")

#extendedmeasurementorfact.txt
if(exists("measurements")){
measurement2 <- measurements
measurements$id = paste(measurements$eventID)
measurements3 <- measurements[,c(1,13,2:12)]
write.table(x = measurements3, file = "UPLOAD-5_spongis_table-measurements.tab", row.names=FALSE, col.names = FALSE, fileEncoding="UTF-8", na = "", sep="\t", eol="\n", quote = FALSE)
measurements$id = measurements$eventID
measurements <- measurements[,c(13,1:12)]
write.table(x = measurements, sep="\t", eol="\n", file = "extendedmeasurementorfact.txt", row.names=FALSE, fileEncoding="UTF-8", na = "", quote = FALSE)
files <- c(files, "extendedmeasurementorfact.txt")
}

#occurrence.txt
if(exists("occurrences")){
occurrences$id = paste(occurrences$eventID, occurrences$scientificNameID, sep="-")
occurrences2 = occurrences[c(1,2,26,3:5,7:25,6)]
write.table(x = occurrences2, file = "UPLOAD-4_spongis_table-occurrences.tab", row.names=FALSE, col.names = FALSE, fileEncoding="UTF-8", na = "", sep="\t", eol="\n", quote = FALSE)
occurrences$id = paste(occurrences$eventID, occurrences$scientificNameID, sep="-")
occurrences <- occurrences[,c(26,1:25)]
occurrences$occurrenceID <- occurrences$id
write.table(x = occurrences, sep="\t", eol="\n", file = "occurrence.txt", row.names=FALSE, fileEncoding="UTF-8", na = "", quote = FALSE)
files <- c(files, "occurrence.txt")
}

# Create DWC-A
zip(zipfile=paste0(spongis_meta$title_short, ".zip"), files=files)
file.remove(files)

## Taxon Table
if(exists("taxon")) {
taxon2 <- taxon[, c("scientificNameID",	"acceptedNameUsageID", "scientificName","scientificNameAuthorship", "taxonRank","taxonRemarks",	"taxonDescription",	"taxonImage", "higherClassification", "taxonomicStatus", "vernacularName")]
taxon2 <- data.frame(lapply(taxon2, trimws), stringsAsFactors = FALSE)
write.table(x = taxon2, file = "UPLOAD-2_spongis_table-taxon.tab", row.names=FALSE, col.names = FALSE, fileEncoding="UTF-8", na = "", sep="\t", eol="\n", quote = FALSE)
}

## Meta Table
if(exists("spongis_meta2")) {
  write.table(x = spongis_meta2, file = "UPLOAD-1_spongis_table-meta.tab", row.names=FALSE, col.names = FALSE, fileEncoding="UTF-8", na = "", sep="\t", eol="\n", quote = FALSE)
}

#####################################################


