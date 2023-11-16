![OCCURicon30](https://github.com/cRonFer/OCCUR/assets/76005368/09be6bbd-20ec-4de0-a3a4-e75dfb782648)&nbsp;**[OCCUR](https://ecoinformatic.shinyapps.io/OCCUR/)** app is a step by step guide that was created to facilitate the process of filtering, cleaning and validating occurrence species records from data repositories. This interactive workflow will help the user in the selection of data records between all possibilities depending on their study case, considering their pros and cons. Each module will also display how data certainty and data coverage change when selecting different scenarios of the application of filtering and cleaning rules.
https://ecoinformatic.shinyapps.io/OCCUR/

![image](https://github.com/cRonFer/OCCUR/assets/76005368/2c307dda-3039-47be-907c-f074a514b27c)


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
![image](https://github.com/cRonFer/OCCUR/assets/76005368/f49f274a-1952-4e2e-9cbb-bc3513d89379)
2. Select between filters / steps in left-upper box (there are no previous selections marked).
![image](https://github.com/cRonFer/OCCUR/assets/76005368/c382b390-f913-4a59-ab0d-dc2c97b4ac68)
3. Check the "Trade-off" table that will display with each selection in the right-upper box (left panel).
![image](https://github.com/cRonFer/OCCUR/assets/76005368/998972bc-41ce-4fc9-be3c-893fa51de01c)
4. Check the "Methods" table that will display with each selection in the right-upper box (middle panel).
![image](https://github.com/cRonFer/OCCUR/assets/76005368/0e44145c-f276-461a-b969-45d557b8211f)
5. See the bibliography associated in the "References" panel and click in 'See ref' to open the link in your web browser.
![image](https://github.com/cRonFer/OCCUR/assets/76005368/15954d10-0b1c-4d0f-8f53-bb68cd6cb87f)
6. Check an example of the associated "R Code" in the table that will display with each selection in the right-upper box (right panel). Use the 'copy' button to add the lines into your R code.
![image](https://github.com/cRonFer/OCCUR/assets/76005368/c8349b74-53a1-4862-a974-4afb64f56a9e)
7. Check how certainty and data coverage varies with each selection in the left-bottom panel to make your final selection. Values goes from minimum certainty or data coverage available to  maximum certainty or data coverage available.
![image](https://github.com/cRonFer/OCCUR/assets/76005368/ce02c377-e83a-4dc0-a444-5cdbd33fe1e9)
8. Check the options marked in each module in the bottom-right box.
![image](https://github.com/cRonFer/OCCUR/assets/76005368/9752e3c0-c861-4ccc-bada-791880fca91d)
9. Click in the Download button in the "Final report" tab to obtain your final guide to process data and write the methods section based on the selected steps by module. 
![image](https://github.com/cRonFer/OCCUR/assets/76005368/e001db96-32a8-4217-af5d-84520731c7ae)



