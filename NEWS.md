# Version dev

## General
* Add citation information (#4)
* Add tooltips (#17)
* Use combined `lithology_raw_data`, `lithology_description_code`, `lithology_material_code`, 
  `lithology_colour_code`, `lithology_hardness_code`, and `lithology_observation` (#6)
* Add drawdown() to create Excel worksheets for Pumping wells

## Working with elevation data
* Add option to merge Lidar and TRIM elevation data (#3)
* Add option to use custom DEM file (#20)
* Add option to export cropped DEM file (#24)

## Data Quality
* Update flags - Categorize, rename and add new 
  (`flag_int_overlap`, `flag_int_gap`, `flag_int_note`, `flag_int_bottom`, `flag_yield_zero`, etc.) (#9, #10, #11, #12, #14, #16, #23)
* Add detections for duplicate lithologies/wells (#15)
* Fix yield values where no 0 before the decimal and units are metres (#5)
* Add fixes for missing well depth, zero-width bottom lithology intervals, and well yields of zero which should be missing (#11, #14, #23)

## Exports
* Fix download buttons (#1)
* ArcHydro exports coordinates in BC Albers projection (#8)
* Add Aquifer ID, Artesian Conditions, and Artesian Pressure to leapfrog collars (#22)
* Add argument for exporting as zip archive (`zip = TRUE`)
* In `preview = TRUE` Strater data names now match the file names



# Version 0.0.3
* Fix ArcHydro exports
* Fix tiles erroring with missing areas
* Flag mismatches between yield and depth extractions
* Update internal data
* Allow removal of bcmaps/cded cache as well as bcaquiferdata cache
* Update Shiny app cache handling

# Version 0.0.1.9000
- Fix package dependencies
- Clarify error messages for tiles and for loading shapefiles
- Round elevation and well depths
- Switch to bslib for better displays
- Add Info tabs to better explain lithology and hydrostratigraphy
- Clean up lithology
- Clean up hydrostratigraphy
- Tweak lithology
- Tweak flags
- Add leapfrog and surfer exports
- Compile all Lidar tiles before hand for complete and quicker searching



# Version 0.0.0.9000
- Initial app
