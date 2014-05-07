#############################
# @final-project
# @course: AM 207
# @author: Anita Mehrotra
#############################

# load necessary packages
library("maptools")
library("spatial")
library("RColorBrewer")
library("geoR")
library("classInt")
library("ggplot2")
library("grDevices")

#setRepositories(ind=1:2) # installing with MacOS
#install.packages("rgdal")
library("rgdal")

# set working directory
wd = "/Users/anita/Documents/Spring 2014/Final Projects/Stats 225/"
setwd(wd)

## STEP 1: LOAD DATA & TRANSFORM

# pull in shapefile data of USA counties
shape = readShapePoly(paste(wd, "/data/gz_2010_us_050_00_500k/gz_2010_us_050_00_500k.shp", sep=""))

# remove Virgin Islands
counties_removeVI = c("St. Croix", "St. John", "St. Thomas")

# remove Hawaii
counties_removeHI = c("Hawaii", "Honolulu", "Kalawao", "Kauai", "Maui")

# remove Alaska
counties_removeAK = c("Aleutians East", "Aleutians West", "Anchorage", "Bethel", "Bristol Bay",
                      "Denali", "Dillingham", "Fairbanks North Star", "Haines", "Juneau",
                      "Kenai Peninsula", "Ketchikan Gateway", "Kodiak Island", "Matanuska-Susitna",
                      "Nome", "North Slope", "Northwest Arctic", "Lake and Peninsula",
                      "Haines", "Sitka", "Hoonah-Angoon", "Skagway", "Southeast Fairbanks", 
                      "Valdez-Cordova", "Wade Hampton", "Wrangell", "Petersburg", "Yakutat", 
                      "Yukon-Koyukuk", "Prince of Wales-Hyder")

# remove Puerto Rico
counties_removePR = c("Adjuntas", "Aguada", "Aguadilla", "Aguas Buenas", "Aibonito", "A\xf1asco",
                      "Arecibo", "Arroyo", "Barceloneta", "Barranquitas", "Bayam\xf3n", "Cabo Rojo",
                      "Caguas", "Camuy", "Can\xf3vanas", "Carolina", "Cata\xf1o", "Cayey", "Ceiba",
                      "Ciales", "Cidra", "Coamo", "Comer\xedo", "Corozal", "Culebra", "Dorado", "Fajardo",
                      "Florida", "Gu\xe1nica", "Guayama", "Guayanilla", "Guaynabo", "Gurabo", "Hatillo",
                      "Hormigueros", "Humacao", "Isabela", "Jayuya", "Juana D\xedaz", "Juncos", "Lajas",
                      "Lares", "Las Mar\xedas", "Las Piedras", "Lo\xedza", "Luquillo", "Manat\xed", "Maricao",
                      "Maunabo", "Mayag\xfcez", "Moca", "Morovis", "Naguabo", "Naranjito", "Orocovis",
                      "Patillas", "Pe\xf1uelas", "Ponce", "Quebradillas", "Rinc\xf3n", "R\xedo Grande", "Sabana Grande",
                      "Salinas", "San Germ\xe1n", "San Juan", "San Lorenzo", "San Sebasti\xe1n", "Santa Isabel",
                      "Toa Alta", "Toa Baja", "Trujillo Alto", "Utuado", "Vega Alta", "Vega Baja", "Vieques",
                      "Villalba", "Yabucoa", "Yauco")

counties_remove = c( counties_removeVI, counties_removeHI, counties_removeAK)
ind_to_remove = match(counties_remove, shape$NAME)

# handle Puerto Rico separately 
lenPR = length(counties_removePR)
PR_ind_to_remove = match(counties_removePR, shape$NAME)
PR = append(sort(PR_ind_to_remove)[2:lenPR], 3156)

all_ind_to_remove = c(ind_to_remove, PR)
shape_data = shape[-na.omit(all_ind_to_remove),]

proj4string(shape_data) = CRS("+proj=longlat")
proj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
USA_proj = spTransform(shape_data, CRS(proj))

num_color = 9
plot_colors = rev(brewer.pal(num_color,"PuRd"))

income = read.csv("/Users/anita/Documents/Spring 2014/Final Projects/AM207/am207-final-project/data/income.csv")
parent_income = income$Mean.Parent.Income
child_income = income$Mean.Child.Income

class_parent = classIntervals(parent_income, num_color, style="quantile")
colcode_parent = findColours(class_parent, plot_colors)

png( "parent_income.png", width = 5000, height = 4500 )
plot(USA_proj, col=colcode_parent, border="grey", lwd=0.75)
legend(-75, 35, legend=names(attr(colcode_parent, "table")), 
       fill=attr(colcode_parent, "palette"), cex=5, bty="n")
dev.off()

class_child = classIntervals(child_income, num_color, style="quantile")
colcode_child = findColours(class_child, plot_colors)

png( "child_income.png", width = 5000, height = 4500 )
plot(USA_proj, col=colcode_child, border="grey", lwd=0.75)
legend(-75, 35, legend=names(attr(colcode_child, "table")), 
       fill=attr(colcode_child, "palette"), cex=5, bty="n")
dev.off()