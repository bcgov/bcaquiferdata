#### Hydrostratigraphy

This section takes lithology descriptions and extracts more details regarding 
water flow and yield at different depths 
(see the 'Info' tab under "Explore Lithology" for more details regarding 
lithology descriptions).

Occasionally raw lithology descriptions for a single depth contain information 
for water flow for that depth, or for several different depths.

- "1 gpm at 40 feet" indicates a flow of 1 Gallon per Minute at a depth of 12.19m
- "1 gpm at 40 feet; 1.2 gpm at 60ft" indicates flow for two different depths

If there are multiple depths, these records will be split into two records

Adding the "Raw lithology" column adds the `lithology_raw_data` column so you can 
see for yourself what the original data looked like.

Adding "Flags" adds the `flag_extra_digits` column which flags raw lithology that
had extra digits which were not converted to a yield or depth. 
This generally indicates one of the following:

- An error/typo in the lithology e.g., "fine t0 0medium brown sand"
- A lack of units e.g., "1 gpm at 625" (we can assume feet, but we are not sure


