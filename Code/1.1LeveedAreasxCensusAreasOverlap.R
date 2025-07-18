library(sf)
library(data.table)

# Read in cleaned leveed area polygons
leveed_areas <- st_read("Data/Cleaned/LeveeArea_2023_spatial.geojson")

#______________________________________________________________________________
#For NFIP data, find overlap between leveed areas and block groups
#Use 2010 block groups, as it forms the basis of the NFIP data

# Download 2010 CA block group geometries
bgs2010 <- st_read("Data/Raw/Census/CA_blockgroups_2010.gpkg") 

# Check intersection
bgs2010$intersects_leveedarea <- as.integer(lengths(st_intersects(bgs2010, leveed_areas)) > 0)

# Output table
out <- data.table(GEOID10 = bgs2010$GEOID10, intersects_leveedarea = bgs2010$intersects_leveedarea)
fwrite(out, "Data/Cleaned/bgs_leveedarea_overlap.csv")

#______________________________________________________________________________
# Repeat for census tracts

#Get 2021 Census tracts, to be linked with 2021 Census data
tracts2021 <- st_read("Data/Raw/Census/CA_tracts_2021.gpkg") %>% 
    st_transform(st_crs(leveed_areas))
tracts2021$intersects_leveedarea <- as.integer(lengths(st_intersects(tracts2021, leveed_areas)) > 0)
tracts_out <- data.table(GEOID = tracts2021$GEOID, intersects_leveedarea = tracts2021$intersects_leveedarea)
fwrite(tracts_out, "Data/Cleaned/tracts_leveedarea_overlap.csv")

