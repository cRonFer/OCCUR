**[OCCUR](https://ecoinformatic.shinyapps.io/OCCUR/)** app is a "step by step" guide that goes over 5 different modules to curate biodiversity data records. It was created to facilitate the process of filtering,cleaning and validating occurrence species records from data repositories.This interactive workflow will help the user in the selection of data records between all possibilities depending on their study case, considering their pros and cons.Each module will also display how data certainty and data coverage change when selecting different scenarios of the application of filtering and cleaning rules.
https://ecoinformatic.shinyapps.io/OCCUR/

**INSTRUCTIONS**
1. Choose a module of the 5 available in the left panel.
2. Select between filters / steps in left-upper box (there are no previous selections marked).
3. Check the "Trade-off" table that will display with each selection in the right-upper box (left panel).
4. Check the "Methods" table that will display with each selection in the right-upper box (right panel).
5. See the bibliography associated in the "References" panel.
6. Check how certainty and data coverage varies with each selection in the left-bottom panel to make your final selection. Values goes from 0 (minimum certainty or data coverage available) to 1 (maximum certainty or data coverage available).
7. Download the final guide to process data and write the methods section based on the selected steps by module in the "Final report" tab.'

```mermaid
graph LR
subgraph ide1 [<h1><strong>MODULES]
A(<h1>Basis Of Record<br> <h3><em>Nature of the record based on <br>the original method of collection) ==> B(<h1>Taxonomy<br><h2><em>Every aspect of the harmonization in <br>nomenclatural and taxonomical<br>standardization of occurrence records)
B ==> C(<h1>Geography<br> <h2><em>Spatial information of occurrence records)
C ==> D(<h1>Time<br> <h2><em>Presence of a date of collection <br>in occurrence records and <br>the definition of temporal ranges)
D ==> E(<h1>Duplicates<br> <h2><em>Same information in a combination of <br>fields to describe various occurrence records)   

style A fill:#1874CD,stroke:#333,color:#fff
style B fill:#FFA500,stroke:#333,color:#fff

style C fill:#B03060,stroke:#333,color:#fff

style D fill:#008B00,stroke:#333,color:#fff
style E fill:#605ca8,stroke:#333,color:#fff

style ide1 fill:transparent,stroke:#333,color:#333
  end
```
