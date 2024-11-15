
Flag definitions for lithology and hydrostratigraphy. 
You can access this list in R at any time through the `flags` dataset.

Many of these flags indicate potential problems in the original data record on GWELLS.
However, if you  believe that the record is correct, but that the *flag* is incorrect, please
[open an issue](https://github.com/bcgov/bcaquiferdata/issues) and let us
know what the problem is (which flag is incorrect and why) 
and with which Well (`well_tag_number`).

`from` and `to` fields refer to `lithology_from_ft_bgl`/`lithology_to_ft_bgl` and `lithology_from_m`/`lithology_to_m` fields

The middle letters (e.g., `int` in `flag_int_missing`) reflect the kind
of data being flagged.

- `int` refers to a specific lithologic depth interval
- `lith` refers to a well's entire lithologic record
- `cat` refers to the lithology categorization
- `pos` refers to the specific position of a category
- `yield` refers to the yield extracted from a lithologic record
- `depth` refers to well depths
