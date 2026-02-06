# Lithology Cleaning and Categorizing

### Overview

When GWELLS data is downloaded, the raw lithology descriptions are
cleaned and categories into lithology categories for use by other
programs.

This results in ultimately transforming the original starting
description from GWELLS into a new lithology category defined by a set
of rules.

For example, an original lithology record of “**gravel with some sandy
seams**” would be categorized as “**Sand and Gravel (Clean)**”.

However this process happens over several steps and for transparency,
the outputs of intermediate steps are retained in the final data set.

Here is a full example of lithology data.

| lithology_raw_data            | lithology_clean  | lith_primary  | lith_secondary | lith_tertiary | lithology_extra | lithology_category      | flag_bedrock | flag_boulders | flag_missing_cats |
|-------------------------------|------------------|---------------|----------------|---------------|-----------------|-------------------------|--------------|---------------|-------------------|
| gravl w/ sands                | gravel with sand | gravel        | sand           |               |                 | Sand and Gravel (Clean) | FALSE        | FALSE         | FALSE             |
| bentonite                     | bedrock          | bedrock       |                |               |                 | Bedrock                 | FALSE        | FALSE         | FALSE             |
| sand and roots                | sand & organic   | sand, organic |                |               | organic         | Organics                | FALSE        | FALSE         | FALSE             |
| muddy sand                    | silty sand       | sand          |                | silt          |                 | Sand and Fines          | FALSE        | FALSE         | FALSE             |
| reddish sand with pink gravel | sand with gravel | sand          | gravel         |               |                 | Sand and Gravel (Clean) | FALSE        | FALSE         | FALSE             |

> In this article we will explain how this data is created.

## Categorization Steps

Categorizing lithology happens over three steps:

1.  Cleaning
2.  Initial categorizing
3.  Final categorizing

The lithology data contains columns reflecting these steps.

| Column                                         | Description                                                                       | Step                     |
|------------------------------------------------|-----------------------------------------------------------------------------------|--------------------------|
| lithology_raw_data                             | Original lithology description from GWELLS                                        | Original Data            |
| lithology_clean                                | Cleaned lithology description                                                     | 1\. Cleaning             |
| lith_primary, lith_secondary, lith_tertiary    | Intermediate categories created from `lithology_clean`                            | 2\. Initial categorizing |
| lithology_extra                                | Extra, potentially important descriptors extracted from the lithology description | 3\. Final categorizing   |
| lithology_category                             | Final categorized lithology                                                       | 3\. Final categorizing   |
| flag_bedrock, flag_boulders, flag_missing_cats | Columns flagging a particular observation as problematic                          | 3\. Final categorizing   |

#### 1. Cleaning

- Remove erroneous text (unnecessary qualifiers)
- Fix spelling mistakes
- Consolidate/standardize similar terms

For example…

[TABLE]

#### 2. Initial Categorizing

Create *primary*, *secondary* and *tertiary* categories from important
terms

**Primary categories**

- **‘Standalone’ terms**, possibly qualified by other categories
- e.g., sand, silt, clay, till, boulders, bedrock

**Secondary categories**

- **‘With’ terms**
- e.g., with sand, with silt, with clay, with boulders, with bedrock,
  with till

**Tertiary categories**

- **Terms ending in ‘y’/‘ey’**
- sandy, silty, clayey, tilly, bouldery

[TABLE]

#### 3. Final Categorizing

These categories are then used to define a single final category,
according to a a set of rules

For example…

[TABLE]

The “Categorization” section explains in more detail how this final
category is decided upon.

#### Flags and Extra

In addition to creating the lithology category, we flag specific
situations that may warrent extra investigation, as well as pull out and
note some terms in an ‘extra’ column (`lithology_extra`).

## Categorization Rules

Here are the rules used to define final lithology categories, by
examining the primary, secondary, and tertiary categories.

> **Note:** These rules are in order of importance. Therefore if a
> combination of terms matches more than one rule, the first rule takes
> presidence.

### Weathered, Fractured or Faulted Bedrock

> **Any** category is `fractured`, `weathered`, or `faulted`

[TABLE]

### Bedrock

> **Any** category is `bedrock`

[TABLE]

### Boulders

> **Any** category is `boulders`

[TABLE]

### Organics

> **Primary** is `organic`

[TABLE]

### Gravel, Sand, Clay, or Silt

> **Primary** is `gravel`, `sand`, `clay`, *or* `silt`  
> **No Secondary/Tertiary** (*Except `silty clay` and `clay with silt`*)

[TABLE]

### Sandy or Gravelly Silt

> **Primary** is `silt`  
> **Secondary/Tertiary** are `sand` or `gravel`

[TABLE]

### Sand and Gravel (Clean)

> **Both (and only)** `sand` and `gravel` are both present in **any**
> category

[TABLE]

### Sand and Gravel (Dirty)

> `gravel` or `sand` are both present in any category, at least one is
> **Primary** and **Secondary/Tertiary** is also `silt` or `clay`  
> **OR**  
> **Primary** is **all** `gravel`, `sand` and `silt`/`clay`

[TABLE]

### Sand and Fines

> **Primary** is `sand` and **Secondary/Tertiary** is `silt` or `clay`  
> **OR**  
> **Primary** is **both** `sand` and `silt` (*not* `clay`)

[TABLE]

### Gravel (Dirty)

> **Primary** is `gravel` and **Secondary/Tertiary** is `silt` or
> `clay`  
> **OR**  
> **Primary** is **both** `gravel` and `silt` (*not* `clay`)

[TABLE]

### Sand or Gravel Till or Diamicton

> **Any** category is `sgtill`  
> **OR**  
> **Primary** is `till` or `clay` and **any** category is `sand` or
> `gravel` (but both cannot be primary) **OR**  
> **Primary** is `sand` or `gravel` and **Secondary/Tertiary** is
> `till`  
> **OR**  
> **Primary** is `compact` and **any** category is `sand` or `gravel`

[TABLE]

### Medium to Clay Till or Diamicton

> **Primary** is `till`, `hardpan` or `hard earth`  
> **OR**  
> **Primary** is `silt` and **Secondary/Tertiary** is `till`  
> **OR**  
> **Primary** is `clay` and **any** category is `till`  
> **OR**  
> **Primary** is `compact` and **any** category is `silt` or `clay`  
> **OR**  
> **Any combination** of `silt` or `clay` not already categorized

**Note:** That `silty clay` is already categorized as “Clay” (see
[Gravel, Sand, Clay, or Silt](#gravel-sand-clay-or-silt))

[TABLE]

### Shells

> **Primary** is *only* `shells`  
> **No Secondary/Tertiary**

[TABLE]

### Overburden

> **Primary** is *only* `overburden`  
> **No Secondary/Tertiary**

[TABLE]

### No category

> **All** categories are empty

## Extra columns

- Organics, Boulders, and Shells are noted in the column
  `lithology_extra`

- As are:

  - `flow` (water, flowing, stream of water, etc.)
  - `seepage`, `wet`, `saturated`, `trickle`
  - `waterbearing` (water-bearing, wb, w.b. etc.)
  - `aquifer`, `reservoir`, `artesian`

## Ambiguous distinctions

### Rock vs. rocks. vs rocky

[TABLE]
