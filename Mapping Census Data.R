###
### Mapping Census Data in R
###

####Libraries

require(acs) #Package for pulling ACS Data
#http://dusp.mit.edu/sites/dusp.mit.edu/files/attachments/publications/working_with_acs_R_v_2.0.pdf

require(leaflet) #Map Rendering
require(tigris) #download and subset shapefile form Census
require(dplyr)  #Data manipulation
require(stringr) #string manipulation



### Here we use the "acs" package to pull data from the Census API.
### This package makes it easier to pull data and understand the variables

#Census API Key, I included mine so you can try it out
#but you can get one of your own at their website

api.key<-"b98ceba64357dca514cb348213844e8719f901e6"

#Register Your API key with the package
api.key.install(key=api.key)


#First What Geographies do we want?
#Lets pull a few counties of interest from VA
geo.lookup(state="VA",county=c(13,59,510,600))

#Create Geography Object you Want to Pull
my.geo<-geo.make(state="VA",county=c(13,59,510,600),tract="*",check=T)

#We know what geographies we want, but now what data do we want?
#This took some drilling down to find
acs.lookup(endyear=2014, table.number = "B19113", keyword = "Median family income")

#Pull Data
income_data<-acs.fetch(geography = my.geo,endyear = 2014,table.number="B19113")

#Create an object that we will later merge to our spatial data
#NOTE: we need to pad the county FIPS code with 0's so it has width = 3
income_df<- data.frame( GEOID = paste0(income_data@geography$state,
                                       str_pad(income_data@geography$county,
                                               width=3,
                                               side="left",
                                               pad="0"),
                                       income_data@geography$tract
                                       ),
                        median_income = as.numeric(income_data@estimate),
                        row.names=NULL)


############# Getting Shapefile and Merging Data
#We use tigris package to pull and subset 
#a shape file from Census, you can always do
#all this on your own with only a little more work
#this is just the easiest way


#download and create shapefile for Arlington, Fairfax and Alexandria
nova.shape<-tracts(state="VA",county=c("013","059","510","600"))

#quick plot to see what it looks like
plot(nova.shape)

#create a temporary copy so that we can add data to it 
#without overwriting the original
nova.shape2<-nova.shape

##Merge out income data onto the spatial dataset
nova.shape2@data<-nova.shape@data %>% left_join(income_df,by="GEOID") %>% filter(median_income>=0)



#### Create Leaflet Map


### Create Color Pallete for Choropleth
pal<-colorNumeric(palette = "Purples", 
                   domain = NULL)

#Create "on-click" popup information for out map
# it simply will display the tract number and the
#median income
tract_popup <- paste0("<strong>Tract: </strong>", 
                      nova.shape2@data$TRACTCE,
                      "<br><strong>",
                      "Value", 
                      ": </strong>",
                      paste("$",format(nova.shape2@data$median_income, big.mark=",",
                                       trim=TRUE),sep=""))

#finally create map; view and zoom were set by trial and error
leaflet() %>% setView(lng=-77.2876, 
                         lat=38.8041, 
                         zoom = 10) %>% 
              #Add Map Tiles (the background map)
              addProviderTiles("CartoDB.Positron") %>% 
              #Add our Census Tracts and color them
              #using our palette
              #Also inclue pop-ups we created above
              addPolygons(data = nova.shape2,weight=1,color="#000000",
                             fillColor=~pal((nova.shape2@data$median_income)),
                             fillOpacity = .6,
                             popup=tract_popup) %>%
               #finally include 
              addLegend("bottomleft",
                          pal=pal,
                          values =nova.shape2@data$median_income,
                          title="Median Family Income",labFormat=labelFormat(prefix = "$"))



  


