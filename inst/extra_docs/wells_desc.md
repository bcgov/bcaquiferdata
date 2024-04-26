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
