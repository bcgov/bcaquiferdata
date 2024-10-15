# Version dev
* Fix download buttons (#1)
* Fix yield values where no 0 before the decimal and units are metres (#5)
* Add citation information (#4)
* Add option to merge Lidar and TRIM elevation data (#3)
* Use combined `lithology_raw_data`, `lithology_description_code`, `lithology_material_code`, 
  `lithology_colour_code`, `lithology_hardness_code`, and `lithology_observation` (#6)
* Update flags - Categorize, rename and add new 
  (`flag_int_overlap`, `flag_int_gap`, `flag_int_note`, `flag_int_bottom`, etc.) (#9, #11, #12, #14, #16)
* Add detections for duplicate lithologies/wells (#15)
* Add fixes for missing well depth and zero-width bottom lithology intervals (#11, #14)
* Add tooltips (#17)
* ArcHydro exports coordinates in BC Albers projection (#8)

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
