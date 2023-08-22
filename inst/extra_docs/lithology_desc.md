#### Lithology

Lithology categories (`lithology_category`) are created by cleaning and categorizing the 
lithology descriptions from GWELLs data (`lithology_raw_data`).

For more details on how lithology is categorized, please see the
documentation regarding <a href="http://steffilazerte.ca/bcaquiferdata/articles/lithology_categorization.html" target="_blank">Lithology Cleaning and Categorizing</a>.

Adding "Basic" columns means including relevant lithology measures such as
- `well_tag_number`
- `lithology_from_m` / `lithology_to_m` -> Start and end of the depth being described
- `lithology_raw_data` -> Original lithology description from GWELLS
- `lithology_clean` -> Cleaned lithology description (see below)
- `lithology_category` -> Categorized lithology (see below)

Adding "Extra" columns means including `lithology_extra` which captures 
extra potentially important descriptors (such as `wet`, `flow`, `seepage`).

Adding "All from GWELLS" includes all lithology related columns included in GWELLS
(e.g., `lithology_description_code` etc.)

Adding "Intermediate Categories" means including `lith_primary`, `lith_secondary`,
`lith_tertiary`, which were created from `lithology_clean` and used to create 
the `lithology_category` (see below)

Adding "Flags" means including columns indicating lithology observations flagged
as problematic or questionable (see the "Flags" section for a glossary).

**Categorizing lithology**

Since we use an algorithm and set of rules to do this transformation, it is 
entirely possible that some categories are odd or incorrect.

The first step is to clean the data (this results in `lith_clean`)
- Remove erroneous text
- Fix spelling mistakes
- Consolidate similar terms

The second step is to extract important terms to create the primary, secondary and tertiary categories
(`lith_primary`, `lith_secondary`, `lith_tertiary`). For example, 
- `sand` is a primary category
- `sand with silt` becomes a primary category of sand and secondary of silt
- `silty sand` becomes a primary category of sand and tertiary of silt.

See <a href="http://steffilazerte.ca/bcaquiferdata/articles/lithology_categorization.html#categorization-rules" target="_blank">Categorization Rules</a> for more details on this type of categorization. 

The final step is to convert the primary, secondary and tertiary categories into
one overall category (`lithology_category`). 

