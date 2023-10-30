**[OCCUR](https://ecoinformatic.shinyapps.io/OCCUR/)** app is a step by step guide that was created to facilitate the process of filtering, cleaning and validating occurrence species records from data repositories. This interactive workflow will help the user in the selection of data records between all possibilities depending on their study case, considering their pros and cons. Each module will also display how data certainty and data coverage change when selecting different scenarios of the application of filtering and cleaning rules.
https://ecoinformatic.shinyapps.io/OCCUR/

![image](https://github.com/cRonFer/OCCUR/assets/76005368/d055a963-6af5-4853-b731-94046207de03)

OCCUR app goes over 5 different modules to curate biodiversity data records. 

```mermaid
graph LR
subgraph ide1 [<h1><strong>MODULES]
A(<h1>Basis Of Record<br> <h3><em>The specific nature of the data record <br>based on the original method of collection) ==> B(<h1>Taxonomy<br> <h2><em>Every aspect of the harmonisation in <br>nomenclatural and taxonomical<br>standardisation of occurrence records)
B ==> C(<h1>Geography<br> <h2><em>Spatial information of occurrence records)
C ==> D(<h1>Time<br> <h2><em>Presence of a date of collection <br>in occurrence records and <br>the definition of temporal ranges)
D ==> E(<h1>Duplicates<br> <h2><em>Identical combination of information <br>in fields associated with various <br>occurrence records)   

style A fill:#1874CD,stroke:#333,color:#fff
style B fill:#FFA500,stroke:#333,color:#fff

style C fill:#B03060,stroke:#333,color:#fff

style D fill:#008B00,stroke:#333,color:#fff
style E fill:#605ca8,stroke:#333,color:#fff

style ide1 fill:transparent,stroke:#333,color:#333
  end
```


**INSTRUCTIONS**
1. Choose a module of the 5 available in the left panel.
![image](https://github.com/cRonFer/OCCUR/assets/76005368/aeab8f44-b5c1-4831-93db-41db6cbd776a)
2. Select between filters / steps in left-upper box (there are no previous selections marked).
![image](https://github.com/cRonFer/OCCUR/assets/76005368/7f17117f-316e-4b49-8c93-bef278831c7d)
3. Check the "Trade-off" table that will display with each selection in the right-upper box (left panel).
![image](https://github.com/cRonFer/OCCUR/assets/76005368/3660b67e-e76c-472e-8629-0913d34ab15d)
4. Check the "Methods" table that will display with each selection in the right-upper box (middle panel).
![image](https://github.com/cRonFer/OCCUR/assets/76005368/d8b94cde-a985-437c-8c77-9a7e7ac2147d)
5. See the bibliography associated in the "References" panel and click in 'See ref' to open the link in your web browser.
![image](https://github.com/cRonFer/OCCUR/assets/76005368/04a96ed5-b87d-4585-a42b-c191c0c9fe23)
6. Check an example of the associated "R Code" in the table that will display with each selection in the right-upper box (right panel). Use the 'copy' button to add the lines into your R code.
![image](https://github.com/cRonFer/OCCUR/assets/76005368/19592e7c-9f16-4be3-b57a-f689a5ddf460)
7. Check how certainty and data coverage varies with each selection in the left-bottom panel to make your final selection. Values goes from 0 (minimum certainty or data coverage available) to 1 (maximum certainty or data coverage available).
![image](https://github.com/cRonFer/OCCUR/assets/76005368/0c65121f-81a6-47ab-8ae3-dc736e78a459)
8. Check the options marked in each module in the bottom-right box.
![image](https://github.com/cRonFer/OCCUR/assets/76005368/5eaa8684-5fbf-4d70-a056-72f207b68cc3)
9. Click in the Download button in the "Final report" tab to obtain your final guide to process data and write the methods section based on the selected steps by module. 
![image](https://github.com/cRonFer/OCCUR/assets/76005368/48416e43-b3c9-497e-a382-fc866600bddf)



