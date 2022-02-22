# SNA toolbox

This repository is intended to provide the Physics Education Community (PER) with simple and intuitive `R` code to analyze and visualize social network data.

The network analysis of data and all networks visualizations presented in the `SNA_analysis.pdf` documents were done using the [R](https://cran.r-project.org/) statistical programming language. The document itself was generated with [RStudio](https://www.rstudio.com/products/rstudio/download/#download). While `RStudio` is not required for the analysis, it makes working with the `R` code easier, especially for novice users.

Presently the code allows for simple analysis of the basic network topology measures (e.g., diameter, density, average path length) as well as some simple network visualizations.

Any questions or concerns regarding the provided code and requests for additional features should be addressed to me at jpzwolak {at} gmail {.} com

Enjoy!

## File structure

`Data` - this folder contains a sample edge lists (in the `Edge lists` subfolder) and nodes data. These are fake data intended only for creating dummy example networks.

`Tools` - this folder contains tools that should make your life easier!

- `SNA Survey (example).pdf`: an example of an in-class, pen-and-paper social network survey that we designed to collect the network data for our study.

- `Data-coder.xlsx`: a Microsoft Excel spreadsheet that I designed to simplify the coding process. The sheets in this document are interdependent and therefore some of them are locked to prevent unintentional modification. Note that this document was designed for our survey - to be used with different surveys it needs to be modified or re-designed, whichever is easier.

	- `Names`: use this sheet to establish a dictionary that will be used in the coding process. For instance, when coding the pen-and-paper surveys I used two-digit identifiers instead of the 7-digit student IDs to avoid errors and to expedite the process.
	- `Summary`: this sheet is used as a "self-check". From the `Names` tab, the student IDs should be pasted to the `ID` column. 
	- `SNAt1`: this sheet should be used to code the data. Once the two-digit identifiers are in the respective columns (i.e., `Source` or `Target`), the names of students will be retrieved from the `Names` sheet (this can be used as a check to make sure the coding is done correctly). To make sure that the weights are coded correctly, the `Summary` sheet can be used. In particular, for a given student the number of peers listed under the "weight 1", "weight 2" and "weight 3" category needs to be put in from the survey. If all weights were coded correctly, the cell `M1` (`V1`, etc.) will have value `0`. If the value is non-zero, columns `N`- `R` (`W`-`AA`, etc.) will point to the student with miscoded edges.   
