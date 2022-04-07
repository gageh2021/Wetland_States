# Wetland_States Variable Name Descriptions

### Acronyms:
>RdNBR: Relative difference Normalized Burn Ratio
>NDWI: Normalized difference Water Index (Using Green and NIR bands).
>NDMI: Normalized difference Moisture Index (Using NIR and SWIR bands).

## Full_RS_data.csv ##
Full_RS_data: Contains digitization information and summary statistics of remote sensing indices for each individual wetland in the study area.
### Variables:
>OID: Object ID of wetland boundary polygon (0-113)  
>State: Wetland state classification (A-E)  
>Area_m: Wetland area in square meters  
>Area_km: Wetland area in square kilometers  
>Area_Proportion: Proportion of total digitized area occupied by wetland  
>Pixel_count: Number of Sentinel-2A pixels inside the wetland  
>RdNBR_mean: Mean RdNBR in wetland (Sentinel-2A imagery)  
>RdNBR_SD: Standard deviation of RdNBR in wetland (Sentinel-2A imagery)  
>Mean_NDWI_Pre: Pre-fire mean NDWI in wetland (PlanetScope imagery)  
>SD_NDWI_Pre: Pre-fire standard deviation in NDWI in wetland (PlanetScope imagery)  
>Mean_NDWI_Post: Post-fire mean NDWI in wetland (PlanetScope imagery)  
>SD_NDWI_Post: Post-fire mean NDWI in wetland (PlanetScope imagery)  
>dNDVI_mean: Mean dNDVI between pre- and post-fire imagery (Sentinel-2A imagery)  
>dNVDI_SD: Standard deviation of dNDVI between pre- and post-fire imagery (Sentinel-2A imagery)  
>NDMI_mean:  Mean NDMI in wetland (Sentinel-2A imagery)  
>NDMI_SD: Standard deviation of NDMI in wetland (Sentinel-2A imagery)  
>Beaver_Dam: Presence (Y) or absence (N) of a beaver dam in the wetland  
>Veg_Area_m: Area that is not open water in square meters, based on an unsupervised two-class classification of 2016 COOP imagery  
>Water_Area_m: Area that is open water in square meters, based on an unsupervised two-class classification of 2016 COOP imagery  
>Water_Proportion: Proportion of wetland area occupied by open water, based on an unsupervised two-class classification of 2016 COOP imagery  
>Wetland_Severity: Qualitative description of wetland fire severity as high (H), moderate (M), low (L), or unchanged (U) based on RdNBR thresholds from Miller and Thode, 2007    

## Proportion_data.csv ##
Proportion_data: Contains summary statistics for wetland states.   
### Variables:
>State: Wetland state classification (A-E)   
>Number: Total count of wetlands for a particular state  
>Relative_Proportion: Relative frequency of wetland state  
>Total_Area_km: Total area occupied by wetland state in square kilometers  
>Area_Proportion_km: Proportion of total digitized area occupied by wetland state  
>U_Proportion: Proportion of wetlands of a given state that had a mean RdNBR classification of "unchanged", based on RdNBR thresholds from Miller and Thode, 2007  
>L_Proportion: Proportion of wetlands of a given state that had a mean RdNBR classification of "low", based on RdNBR thresholds from Miller and Thode, 2007  
>M_Proportion: Proportion of wetlands of a given state that had a mean RdNBR classification of "moderate", based on RdNBR thresholds from Miller and Thode, 2007  
>H_Proportion: Proportion of wetlands of a given state that had a mean RdNBR classification of "high", based on RdNBR thresholds from Miller and Thode, 2007  
>Dam_Proportion: Proportion of wetlands of a given state associated with a beaver dam  
>No_Dam_Proportion: Proportion of wetlands of a given state not associated with a beaver dam  

## Margin_Middle_RdNBR.csv ##
Margin_Middle_RdNBR: Contains data from each cell in the Sentinel-2A raster that occurred within a digitized wetland.  
### Variables:
>pointid: Point ID number.  
>RdNBR: RdNBR value for the point (Sentinel-2A imagery)  
>Distance_m: Distance from wetland perimeter  
>Near_OID: Object ID of the wetland which the point was within  
>State: Successional state of wetland  
>Area_km: Wetland area in square kilometers  
>RdNBR_mean: Mean RdNBR of wetland (Sentinel-2A imagery)  
>RdNBR_SD: Standard deviation of RdNBR in wetland (Sentinel-2A imagery)  
>Mean_NDWI_Pre: Pre-fire mean NDWI in wetland (PlanetScope imagery)  
>SD_NDWI_Pre: Pre-fire standard deviation in NDWI in wetland (PlanetScope imagery)  
>Mean_NDWI_Post: Post-fire mean NDWI in wetland (PlanetScope imagery)  
>SD_NDWI_Post: Post-fire mean NDWI in wetland (PlanetScope imagery)  
>Pixel_Count: Number of Sentinel-2A pixels in the wetland  
>Beaver_Dam: Presence (Y) or absence (N) of a beaver dam in the wetland  
>Wetland_Severity: Qualitative description of pixel fire severity as high (H), moderate (M), low (L), or unchanged (U) based on RdNBR thresholds from Miller and Thode, 2007  
>Wetland_Severity: Qualitative description of wetland fire severity as high (H), moderate (M), low (L), or unchanged (U) based on RdNBR thresholds from Miller and Thode, 2007  

## Correlation_Plot_Data.csv ##
Correlation_Plot_Data: Data found in Full_RS_data, organized to be used in a correlation matrix. All variable names are the same as those in Full_RS_data.  
