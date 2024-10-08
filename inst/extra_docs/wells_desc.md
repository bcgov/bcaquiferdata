#### Wells

In this step we filter GWELLs to watershed area and use Lidar and/or TRIM digital
elevation models to calculate well elevation.

The spatial files to defined the watershed area can be either a collection of
spatial shape files (to select multiple files hold the Ctrl key while clicking 
on several files) or they can be a zipped set of shape files.

The elevation data source can be Lidar, TRIM or a combination of the two.

**Use caution when combining elevation sources**

Combining elevation data from different sources, measured with different
techniques may introduce artifacts into your data. Generally speaking it is best
to stick with a single data source.

However, if you find yourself in the position of having incomplete data in both cases,
it can be useful to use both. It is recommended that you explore the 
elevation columns in the Wells Data tab to confirm that this makes sense in your
situation. The column `elev1` refers to the primary source, `elev2` to the secondary,
and `elev` to the combined (primary, supplemented with secondary where data is missing). 

In the map, the *primary* source of data is outlined in black to make visual
comparisons simpler.

**Zero-width bottom lithology intervals**

Occasionally, the lithology record for a well may have a zero-width bottom interval
which occurs when the drillers hit bedrock, noted what they hit and stopped drilling,
marking the start and end of the intervals as the same depth 
(intervals with this problem are identified by `flag_int_bottom = TRUE`).

This can cause problems when trying to use lithology in other software (such as LeapFrog).
We can fix this by adding 1m to the end depth where this occurs 
(intervals fixed this way have `fix_int_bottom = TRUE`). 

Note that when exporting to LeapFrog this fix occurs automatically (with a message) as it is a requirement of the software.
