## Local Bandwagoning and National Balancing: How Uninformed Voters Respond to the Partisan Environment and Why It Matters

* #### [Working Paper (4/9/2019)](papers/Kato2019loba_v11.pdf)
* #### [Presentation Slide (4/6/2019)](slides/Uninformed_Choice_slide_latest.pdf)

## Author
Gento Kato (gento.badgerATgmail.com)

## Abstract 
Scholarly debate on civic competence often assumes that political knowledge is the prerequisite for systematic and ``correct'' decision-making. Uninformed voters, then, are portrayed as unsystematic and misguided decision-makers. The current study challenges this assumption by arguing that uninformed voters may not rely on (potentially misguided) individual preferences but rather refer to the partisan environment, the partisan voting patterns in past elections, to guide their decisions. The analysis of Cooperative Congressional Election Study (CCES) and American National Election Studies (ANES) provides the supporting evidence to this claim. Uninformed voters respond to the partisan environment in two ways: First, they \textit{bandwagon} with the local partisan environment; second, they \textit{balance} against the national partisan environment. The consequences of this context-based uninformed voting are evaluated through agent-based simulation. The results provide the view of uninformed voters as more systematic and effective decision-makers than previously suggested.

## Original Datasets & Functions

1. [Cooperative Congressional Election Study (CCES)](https://cces.gov.harvard.edu): used in Study 1
   * CCES2008: Download Version 6.0 Dataset (Stata Binary)
   * CCES2016: Download Version 4.0 Dataset (Stata Binary) 
2. [CQ Voting and Elections Collection](http://library.cqpress.com/elections/): Check out my another repository [HERE](https://github.com/gentok/cqvec) for how you can compile the data obtained from the source website.
3. [American National Election Studies (ANES)](https://electionstudies.org): used in Study 2
4. [<code>cotextvoting</code>](https://gentok.github.io/contextvoting/) package: Custom R package used for the analysis in Study 3

<!-- ## Generate Datasets for the Analysis

1. **Set Data Path**: Modify <code>[import_data.R](codes_cces/import_data.R)</code>. Define paths to relevant original datasets and execute the file.
2. **Generate CCES08 Datasets**: Execute <code>[CCES08_data1.R](codes_cces/CCES08_data1.R)</code>, <code>[CCES08_data2.R](codes_cces/CCES08_data2.R)</code>, and <code>[CCES08_data3.R](codes_cces/CCES08_data3.R)</code> in the respective order.
3. **Generate CCES16 Datasets**: Execute <code>[CCES16_data1.R](codes/CCES16_data1.R)</code>, <code>[CCES16_data2.R](codes/CCES16_data2.R)</code>, and <code>[CCES16_data3.R](codes/CCES16_data3.R)</code> in the respective order.

## Analysis Codes

* <code>[CCES_analysis0_functions.R](codes/CCES_analysis0_functions.R)</code>: Containing functions and resources used in analysis.
* <code>[CCES08_analysis1.R](codes/CCES08_analysis1.R)</code>: Run logistic regression using CCES08 data.
* <code>[CCES08_analysis2.R](codes/CCES08_analysis2.R)</code>: Make prediction using CCES08 data.
* <code>[CCES16_analysis1.R](codes/CCES16_analysis1.R)</code>: Run logistic regression using CCES16 data.
* <code>[CCES16_analysis2.R](codes/CCES16_analysis2.R)</code>: Make prediction using CCES16 data.
* <code>[CCES_analysis1.R](codes/CCES_analysis1.R)</code>: Combining estimation results from <code>[CCES08_analysis1.R](codes/CCES08_analysis1.R)</code> and <code>[CCES16_analysis1.R](codes/CCES16_analysis1.R)</code>.
* <code>[CCES_descriptives2.R](codes/CCES_descriptives2.R)</code>: Export descriptive statistics.
* <code>[CCES_pvi_mapping.R](codes/CCES_pvi_mapping.R)</code>: Map social information to the map of America. Used for slides.
* <code>[CCES_analysis_slide.R](codes/CCES_analysis_slide.R)</code>: Exporting figures for slides. -->

## Project Structure

 * *codes_cces*: R codes used for Study 1
 * *codes_anes*: R codes used for Study 2
 * *codes_abm*: R codes used in Study 3
 * *data*: Storage for datasets (Most of Datasets will be generated through R codes in *codes* folder)
 * *outputs*: Results (plots) generated for the presentation
 * *papers*: Storage for current versions of working papers

## License 

The analytical results in this project are licensed under the [Creative Commons Attribution 4.0 license](https://choosealicense.com/licenses/cc-by-4.0/), and the programming code used to generate the result is licensed under the [MIT license](https://choosealicense.com/licenses/mit/).