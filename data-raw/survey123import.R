############################
# Initial setup steps that #  
# apply to everything else #  
# in this process          #   
############################

# load libraries

library(tidyverse)
library(httr)
library(jsonlite)
library(reticulate)
library(readxl)
library(sf)
library(stringdist)

# this is the workflow to us python code 
# to import data from the survey123 cloud storage

use_python("C:/Program Files/ArcGIS/Pro/bin/Python/envs/arcgispro-py3/python.exe", required = TRUE)

#import the arcgis Python module

gis <- import("arcgis.gis")

# import the paandas python module

pandas <- import("pandas")

# setup the connection to Arc PRO; not sure if this will
# translate to IDFG setup; previously, I use this as
# the validation/credentials layer - if the user is signed
# into the organizations ArcPRO, then this part of the
# code finds that and it allows access to that users
# AGOL content as well

gis_conn <- gis$GIS("pro")

# helper functions in Python that will
# be used to port data from Sruvey123 into
# R

py_run_string("
import os

def safe_extract_attributes(table):
    features = table.query(where='1=1', out_fields='*', return_geometry=False, as_df=False)
    cleaned = []
    for f in features.features:
        attr = {}
        for k, v in f.attributes.items():
            # Convert large ints (like epoch ms) to strings
            if isinstance(v, int) and abs(v) > 2**31 - 1:
                attr[k] = str(v)
            else:
                attr[k] = v
        cleaned.append(attr)
    return cleaned

def list_attachments_for_table(table):
    # Use service's objectIdField to be safe across services
    oid_field = getattr(table.properties, 'objectIdField', 'OBJECTID')
    features = table.query(where='1=1', out_fields=oid_field + ',*', return_geometry=False, as_df=False)
    out = []

    for f in features.features:
        attrs = f.attributes
        oid = (attrs.get(oid_field)
               or attrs.get('OBJECTID')
               or attrs.get('ObjectID')
               or attrs.get('objectid'))
        if oid is None:
            continue

        # AttachmentManager method name
        atts = table.attachments.get_list(oid=oid)

        if atts:
            for att in atts:
                out.append({
                    oid_field: oid,
                    'attachment_id': att.get('id'),
                    'attachment_name': att.get('name'),
                    'content_type': att.get('contentType'),
                    'size': att.get('size'),
                    'globalId': att.get('globalId'),
                    'keywords': att.get('keywords')
                })

    return out

def download_attachments_for_table(table, save_dir):
    os.makedirs(save_dir, exist_ok=True)
    oid_field = getattr(table.properties, 'objectIdField', 'OBJECTID')
    features = table.query(where='1=1', out_fields=oid_field + ',*', return_geometry=False, as_df=False)
    out = []

    for f in features.features:
        attrs = f.attributes
        oid = (attrs.get(oid_field)
               or attrs.get('OBJECTID')
               or attrs.get('ObjectID')
               or attrs.get('objectid'))
        if oid is None:
            continue

        atts = table.attachments.get_list(oid=oid)

        if atts:
            for att in atts:
                att_id = att.get('id')
                att_name = att.get('name')

                downloaded = table.attachments.download(
                    oid=oid,
                    attachment_id=att_id,
                    save_path=save_dir
                )

                # download() may return a path or list of paths
                if isinstance(downloaded, list):
                    local_path = downloaded[0] if len(downloaded) > 0 else None
                else:
                    local_path = downloaded

                out.append({
                    oid_field: oid,
                    'attachment_id': att_id,
                    'attachment_name': att_name,
                    'content_type': att.get('contentType'),
                    'size': att.get('size'),
                    'globalId': att.get('globalId'),
                    'keywords': att.get('keywords'),
                    'local_path': local_path
                })

    return out
")

# read in spatial reference layers that get used
# to join in additional spatial data based on
# the project waypoint

idaho_huc8.sf <- read_rds("data-raw/huc8")

idaho_counties.sf <- read_rds("data-raw/idaho_counties") %>% 
  st_transform(crs=st_crs(idaho_huc8.sf))

idaho_regions.sf <- st_read("data-raw/idfg_regions.gpkg") %>% 
  st_transform(crs=st_crs(idaho_huc8.sf))

idaho_fmp.sf <- read_rds("data-raw/fmp_drainages.rds")%>% 
  st_transform(crs=st_crs(idaho_huc8.sf))

idfg_streams.sf <- st_read("data-raw/Hydrography_Public_3451083698128016512.geojson") %>% 
  st_transform(crs=st_crs(idaho_huc8.sf)) %>% 
  st_zm() %>% 
  st_join(idaho_huc8.sf)

# helper function to match stream name to actual flowlines

match_project_stream <- function(project_row, flowlines) {
  
  huc8_val <- project_row$huc8[[1]]
  stream_name_val <- project_row$stream_name[[1]]
  project_id_val <- project_row$globalid[[1]]
  
  cand <- flowlines |>
    filter(huc8 == huc8_val)
  
  if (nrow(cand) == 0) {
    return(tibble(
      project_id = project_id_val,
      huc8 = huc8_val,
      entered_stream = project_row$stream_name[[1]],
      match_status = "no_flowlines_in_huc8",
      NAME = NA_character_,
      LLID = NA_character_,
      match_rank = NA_integer_
    ))
  }
  
  exact <- cand |>
    filter(NAME == stream_name_val)
  
  if (nrow(exact) == 1) {
    return(exact |>
             st_drop_geometry() |>
             transmute(
               project_id = project_id_val,
               huc8 = huc8_val,
               entered_stream = project_row$stream_name[[1]],
               match_status = "exact",
               NAME,
               LLID,
               match_rank = 1
             ))
  }
  
  if (nrow(exact) > 1) {
    return(exact |>
             st_drop_geometry() |>
             transmute(
               project_id = project_id_val,
               huc8 = huc8_val,
               entered_stream = project_row$stream_name[[1]],
               match_status = "multiple_exact",
               NAME,
               LLID,
               match_rank = row_number()
             ))
  }
  
  fuzzy <- cand |>
    st_drop_geometry() |>
    mutate(
      string_dist = stringdist::stringdist(NAME, stream_name_val, method = "jw")
    ) |>
    arrange(string_dist) |>
    slice_head(n = 3) |>
    transmute(
      project_id = project_id_val,
      huc8 = huc8_val,
      entered_stream = project_row$stream_name[[1]],
      match_status = "fuzzy_candidate",
      NAME,
      LLID,
      string_dist,
      match_rank = row_number()
    )
  
  if (nrow(fuzzy) == 0) {
    return(tibble(
      project_id = project_id_val,
      huc8 = huc8_val,
      entered_stream = project_row$stream_name[[1]],
      match_status = "no_match",
      NAME = NA_character_,
      LLID = NA_character_,
      match_rank = NA_integer_
    ))
  }
  
  fuzzy
}


#################################################
# Read in data from Habitat Project Entry Survey#
#################################################

# specify the url extension where the data collected
# from Survey123 are stored; this can be found by going
# to AGOL, Content, then clicking the Feature Layer (or
# Table Layer if no Feature Layer) for the survey you're
# looking to connect to the data. in the URL within th
# web browser it will end with "id=XXXXX", and this id
# will be the alphanumeric string after the equals sign

item <- gis_conn$content$get("bc087841863f4f298c4aa649c277ba8d")

# identify which tables exist within that data URL; this will follow
# the order they show up when inspecting on the Content page; if there is 
# a feature layer for the main table then it will be under
# item$features[[1]], however this particular survey
# doesn't have a feature layer so everything is in item$tables[[]]

# identify the main projects table

table1 <- item$tables[[1]]


# Existing table extraction; mostly formatting
# steps to be able to handle in R 

table1.attrs_clean <- py$safe_extract_attributes(table1)

table1.df_py <- pandas$DataFrame(table1.attrs_clean)

table1.df <- py_to_r(table1.df_py)

# next step is to join in some spatial data

projects.spatialjoin1 <- table1.df %>% 
  st_as_sf(
    coords=c("primary_point_long",
             "primary_point_lat"),
    crs=st_crs(idaho_huc8.sf),
    remove=F)%>% 
  st_join(idaho_huc8.sf) %>% 
  st_join(idaho_counties.sf) %>% 
  st_join(idaho_regions.sf) %>% 
  st_join(idaho_fmp.sf) %>% 
  mutate(project_startdate=as_datetime(as.numeric(project_startdate)/1000,tz="UTC"),
         work_window_start=as_datetime(as.numeric(work_window_start)/1000,tz="UTC"),
         work_window_end=as_datetime(as.numeric(work_window_end)/1000,tz="UTC"),
         guidance_docs=str_replace_all(guidance_docs,"_"," "),
         stream_name=trimws(stream_name)) %>% 
  mutate(across(
    .cols=c(guidance_docs,partner_agency,project_category,
            idfg_staff,primary_species,secondary_species,life_stage,
            habitat_types,public_benefits,monitoring_methods),
    .fns= ~ str_replace_all(.x, "_", " ")
  )) %>% 
  select(project_name,globalid=globalid.x,idfg_trackingnumber,
         managing_org,partner_agency,guidance_docs,
         project_startdate,project_category,idfg_staff,
         land_ownership,work_window_start,work_window_end,
         project_description,primary_species,secondary_species,
         life_stage,habitat_types,stream_name,latitude=primary_point_lat,
         longitude=primary_point_long,monitoring_methods,public_benefits,
         public_visibility,public_suitability,internal_notes,huc8,
         huc6,idfg_region=region_name,fmp_drainage,county=NAME)

# run the function to join in flowlines base on 
# stream name and coordinates

stream_join <- projects.spatialjoin1 %>% 
  split(seq_len(nrow(.))) %>% 
  map_dfr(match_project_stream,flowlines=idfg_streams.sf) %>% 
  mutate(string_dist=coalesce(string_dist, 0)) %>% 
  group_by(project_id) %>% 
  slice(which.min(string_dist))

# now just join the actual llid in to the broader table
# also bring in the current actual contact for each
# given position so staff give a name and not just
# the position

staff_current <- read_excel("data-raw/position_list_current.xlsx") 

projects.df <- projects.spatialjoin1 %>% 
  left_join(stream_join,by=c("globalid"="project_id","huc8")) %>% 
  left_join(staff_current,by=c("idfg_staff"="old_survey")) %>% 
  select(-c(idfg_staff,position)) %>% 
  rename(idfg_staff=current) %>% 
  mutate(managing_org=str_replace_all(managing_org,"_"," "))

# now pull in the photo metadata and download the photos

photorepeat.table <- item$tables[[2]]

photorepeat.attrs_clean <- py$safe_extract_attributes(photorepeat.table)

photorepeat.df_py <- pandas$DataFrame(photorepeat.attrs_clean)

photorepeat.df <- py_to_r(photorepeat.df_py)

# list attachments on the photo repeat table

photorepeat.attachments <- py$list_attachments_for_table(photorepeat.table)

photorepeat.attachments.df <- py_to_r(pandas$DataFrame(photorepeat.attachments))

# download photo attachments

# set directory where downloads will go 

photo_dir <- "www/project_photos"

downloaded.photos <- py$download_attachments_for_table(photorepeat.table, photo_dir)

downloaded.photos.df <- py_to_r(pandas$DataFrame(downloaded.photos))

# make the photo reference table

photo_reference.table <- photorepeat.df %>% 
  left_join(photorepeat.attachments.df,by="objectid") %>% 
  select(project_globalid=parentglobalid,
         photo_file=attachment_name,
         caption=photo_caption,
         credit=credit) %>% 
  group_by(project_globalid) %>% 
  mutate(photo_order=row_number()) %>% 
  left_join(projects.df,by=c("project_globalid"="globalid")) %>% 
  ungroup() %>% 
  select(project_name,photo_order,photo_file,
         caption,credit)

# need dummy project with isolate project type
# to be able to get individual selections
# within the EB platform; this is only needed
# if trying to use in EB, so first export the
# existing version for use in Shiny app



dummy_projects.sf <- tibble(project_category=c("Beaver Dam Analogues","Channel Restoration",
                                               "Erosion Control and Sediment Management",
                                               "Fish-Friendly Water Intake Screens",
                                               "Floodplain Reconnection",
                                               "Flow and Irrigation Management",
                                               "Riparian Vegetation Restoration",
                                               "Side Channel Reconnection",
                                               "Streambank Stabilization",
                                               "Water Temperature Control",
                                               "Watershed Assessment",
                                               "Wetland Restoration")) %>% 
  mutate(pt_type="dummy",
         latitude=46.64244,
         longitude=-116.10712,
         project_status="Complete") %>% 
  st_as_sf(coords=c("longitude","latitude"),
           crs=st_crs(projects.df),
           remove=F)

projects.df <- projects.df %>%
  mutate(pt_type="real",
         project_status="In Progress") %>% 
  bind_rows(dummy_projects.sf) 


# for now exporting here, but will later
# want the export that funnels into AGOL
# to include stuff from the updates also

st_write(projects.df,
         "agol_layers/projects2.gpkg",
         delete_layer = T) 


####################################################
# Read in data from Habitat Project Updates Survey #
####################################################

# identify URL id for this survey

item2 <- gis_conn$content$get("88ea613777b1449bb7b8948ba9c72404")

# main info from this one is stored as a feature layer

main_layer <- item2$layers[[1]]

main_features <- main_layer$query(where = "1=1", out_fields = "*", return_geometry = FALSE, as_df = FALSE)

# Call the Python function to get the table
# into R initially

main.attrs_clean <- py$safe_extract_attributes(main_layer)

# Convert to pandas DataFrame

main.df_py <- pandas$DataFrame(main.attrs_clean)

# Convert to R

main.df <- py_to_r(main.df_py) %>% 
  mutate(creation_datetime=as_datetime(as.numeric(CreationDate)/1000,tz="UTC"),
         project_name=str_replace_all(project_id,"_"," ")) %>% 
  group_by(project_id,reporting_year) %>% 
  slice(which.max(creation_datetime))

# now bring in the repeat tables

# funding updates

funding <- item2$tables[[1]]

# Existing table extraction

funding.attrs_clean <- py$safe_extract_attributes(funding)

funding.df_py <- pandas$DataFrame(funding.attrs_clean)

funding.df <- py_to_r(funding.df_py) %>% 
  mutate(funding_date=as_datetime(as.numeric(funding_date)/1000,tz="UTC"),
         action_type="funding") %>% 
  inner_join(main.df,by=c("parentglobalid"="globalid")) %>% 
  select(project_id,reporting_year,action_type,funding_source,funding_details,funding_date,
         funding_amount)  


# planning updates

planning <- item2$tables[[2]]

# Existing table extraction

planning.attrs_clean <- py$safe_extract_attributes(planning)

planning.df_py <- pandas$DataFrame(planning.attrs_clean)

planning.df <- py_to_r(planning.df_py) %>% 
  mutate(planning_date=as_datetime(as.numeric(planning_action_date)/1000,tz="UTC"),
         action_type="planning") %>% 
  inner_join(main.df,by=c("parentglobalid"="globalid")) %>% 
  select(project_id,reporting_year,action_type,planning_action=planning_action_annual,
         planning_date)  

# requirements updates

requirements <- item2$tables[[3]]

# Existing table extraction

requirements.attrs_clean <- py$safe_extract_attributes(requirements)

requirements.df_py <- pandas$DataFrame(requirements.attrs_clean)

requirements.df <- py_to_r(requirements.df_py) %>% 
  mutate(completion_date=as_datetime(as.numeric(requirement_application_date)/1000,tz="UTC"),
         action_type="requirements") %>% 
  inner_join(main.df,by=c("parentglobalid"="globalid")) %>% 
  select(project_id,reporting_year,action_type,
         requirement_action=requirement_action_annual,
         requirement_details,design_phase,
         requirement_status=requirement_status_annual,completion_date)  

# implementations updates

implementations <- item2$tables[[4]]

# Existing table extraction

implementations.attrs_clean <- py$safe_extract_attributes(implementations)

implementations.df_py <- pandas$DataFrame(implementations.attrs_clean)

implementations.df <- py_to_r(implementations.df_py) %>% 
  mutate(implementation_date=as_datetime(as.numeric(implementation_action_date)/1000,tz="UTC"),
         action_type="implementation") %>% 
  inner_join(main.df,by=c("parentglobalid"="globalid")) %>% 
  select(project_id,reporting_year,action_type,
         implementation_date,
         implementation_lat=implementation_action_lat,
         implementation_long=implementation_action_long,
         barrier_action_type:enhancement_structure_count)

# delays updates

delays <- item2$tables[[5]]

# Existing table extraction

delays.attrs_clean <- py$safe_extract_attributes(delays)

delays.df_py <- pandas$DataFrame(delays.attrs_clean)

delays.df <- py_to_r(delays.df_py) %>% 
  mutate(delay_date=as_datetime(as.numeric(delay_date)/1000,tz="UTC"),
         action_type="delay") %>% 
  inner_join(main.df,by=c("parentglobalid"="globalid")) %>% 
  select(project_id,reporting_year,action_type,
         delay_date,delay_status,delay_details)

# bring in additional photos that were added in
# annual updates

update_photorepeat.table <- item2$tables[[6]]


update_photorepeat.attrs_clean <- py$safe_extract_attributes(update_photorepeat.table)

update_photorepeat.df_py <- pandas$DataFrame(update_photorepeat.attrs_clean)

update_photorepeat.df <- py_to_r(update_photorepeat.df_py)

# list attachments on the updated photo repeat table

update_photorepeat.attachments <- py$list_attachments_for_table(update_photorepeat.table)

update_photorepeat.attachments.df <- py_to_r(pandas$DataFrame(update_photorepeat.attachments))

# NEW: download attachments from update form photos; will go in 
# the same project directory here


update_downloaded.photos <- py$download_attachments_for_table(update_photorepeat.table, photo_dir)

update_downloaded.photos.df <- py_to_r(pandas$DataFrame(downloaded.photos))

# make the updated photo reference table

update_photo_reference.table <- update_photorepeat.df %>% 
  left_join(update_photorepeat.attachments.df,by="objectid") %>% 
  select(project_globalid=parentglobalid,
         photo_file=attachment_name,
         caption=photo_caption,
         credit=credit) %>% 
  group_by(project_globalid) %>% 
  mutate(photo_order=row_number()) %>% 
  inner_join(main.df,by=c("project_globalid"="globalid")) %>% 
  ungroup() %>% 
  select(project_name,photo_order,photo_file,
         caption,credit)

################################################################
# at this point all the information is pulled in, just need to #
# format/arrange things in a way that feeds into the dashboard #
################################################################

# where did bio response come in on those others?

# use values in  the project updates to 
# determine status, add in some other relevant stuff

# which fields to put into mega wide format?
# award_amount (sum all for a project)

project_funding_summary <- funding.df %>% 
  mutate(funding_source=str_replace_all(funding_source,"_"," ")) %>% 
  group_by(project_id) %>% 
  summarize(award_amount=sum(funding_amount,na.rm=T),
            funding_source=paste(unique(funding_source),
                                 collapse="; "),
            .groups="drop") %>% 
  mutate(project_name=str_replace_all(project_id,"_"," ")) %>% 
  select(project_name,award_amount,funding_source)

# relevant metrics

project_metrics.summary <- implementations.df %>% 
  group_by(project_id) %>%
  summarize(barriers_removed=sum(!barrier_action_type==""),
            stream_miles_reconnected=sum(barrier_removal_miles,na.rm=T),
            stream_miles_restored=sum(stream_restoration_miles,na.rm=T),
            floodplain_reconnect=sum(floodplain_reconnect_acres,na.rm=T),
            enhancement_structures=sum(enhancement_structure_count,na.rm=T),
            riparian_area=sum(riparian_affected_metric,na.rm=T),
            streambank_linearfeet=sum(streambank_affected_metric,na.rm=T),
            flow_restored_miles=sum(flowmanagement_metric,na.rm=T),
            wetland_acres=sum(wetland_affected_metric,na.rm=T),
            .groups="drop") %>% 
  mutate(project_name=str_replace_all(project_id,"_"," ")) %>% 
  select(-project_id)



# status? Will need to update
# the form and join in the main table here
# to query whether implementation is 
# complete and/or whether project has been closed out

project_status.df <- funding.df %>% 
  select(project_id,action_type,action_date=funding_date) %>% 
  bind_rows(planning.df %>% select(project_id,action_type,action_date=planning_date)) %>% 
  bind_rows(requirements.df %>% select(project_id,action_type,action_date=completion_date)) %>% 
  bind_rows(implementations.df %>% select(project_id,action_type,action_date=implementation_date)) %>% 
  bind_rows(delays.df %>% select(project_id,action_type,action_date=delay_date)) %>% 
  group_by(project_id) %>% 
  slice(which.max(action_date)) %>% 
  mutate(project_state=case_when(
    action_type  %in% c("planning","requirements","funding") ~ "Pre-Implementation",
    action_type %in% c("implementation","post_monitoring") ~ "Implementation Complete",
    action_type %in% c("delay") ~ "Delayed"
  )) %>% 
  ungroup() %>% 
  mutate(project_name=str_replace_all(project_id,"_"," ")) %>% 
  select(project_name,project_state)

### for now just joining in status here
# to go into EB, later will use the final cut

projects.df <- projects.df %>% 
  left_join(project_status.df,by="project_name") %>% 
  mutate(project_state=coalesce(project_state,"Pre-Implementation"))

# bind photo reference tables together; will need to 
# think about how unique ids feed into shiny queries;
# may be best to just use the assigned globalid from
# survey123

photos.bind <- photo_reference.table %>% 
  bind_rows(update_photo_reference.table) 

# get necessary columns to funnel project-level
# summaries into dashboard

projects_shiny.df <- projects.df %>% 
  select(globalid,project_name,idfg_trackingnumber,
         managing_org,project_startdate,project_description,
         idfg_staff,latitude,,longitude,stream_name, LLID, idfg_region,
         fmp_drainage, huc6, huc8, county, primary_species,secondary_species,
         life_stage, habitat_type=habitat_types,land_ownership,
         geometry,partner_agency,guidance_docs, project_category
  ) %>% 
  left_join(project_status.df,by="project_name") %>% 
  mutate(project_state=if_else(is.na(project_state),"Pre-Implementation",
                               project_state)) %>% 
  left_join(project_funding_summary,by="project_name") %>% 
  left_join(project_metrics.summary,by="project_name") %>% 
  filter(!is.na(globalid))

# export

saveRDS(projects_shiny.df,"shiny_pieces/project_table")

saveRDS(photos.bind,"shiny_pieces/photo_table")
