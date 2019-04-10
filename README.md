## Social Information and Uninformed Voting Behavior: Delegating or Bandwagoning?

* #### [Working Paper (4/9/2019)](papers/Kato2018deor_v8.pdf)
* #### [Presentation Slide (4/6/2019)](slides/Uninformed_Choice_slide_latest.pdf)

## Author
Gento Kato (gento.badgerATgmail.com)

## Abstract 
Scholarly debate on civic competence often considers political knowledge as the prerequisite for high-quality decision-making. Then, uninformed voters, those who are uncertain about one's political preference, are often considered to be incompetent and inconsistent decision makers. The current study argues that uninformed voters are making voting choices through consistent but different logic than informed voters. Focusing on the role of social information, knowledge about the preference of others in the society, two potential logics of uninformed voting, \textit{delegation} and \textit{bandwagon}, are put to the empirical test using election surveys. The empirical analysis suggests that uninformed voters use ideological preference and retrospective evaluation less, but utilize social information more than informed voters to guide their decisions. Further evidence provides strong support for bandwagon logic of utilizing social information. The result sheds new light on the studies of civic competence by exploring why informed and uninformed voters behave differently, rather than considering how incompetent uninformed voters are.

## Original Datasets

1. [Cooperative Congressional Election Study (CCES)](https://cces.gov.harvard.edu)
   * CCES2008: Download Version 6.0 Dataset (Stata Binary)
   * CCES2016: Download Version 4.0 Dataset (Stata Binary) 
2. [CQ Voting and Elections Collection](http://library.cqpress.com/elections/): Check out my another repository [HERE](https://github.com/gentok/cqvec) for how you can compile the data obtained from the source website.

## Generate Datasets for the Analysis

1. **Set Data Path**: Modify <code>[import_data.R](codes/import_data.R)</code>. Define paths to relevant original datasets and execute the file.
2. **Generate CCES08 Datasets**: Execute <code>[CCES08_data1.R](codes/CCES08_data1.R)</code>, <code>[CCES08_data2.R](codes/CCES08_data2.R)</code>, and <code>[CCES08_data3.R](codes/CCES08_data3.R)</code> in the respective order.
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
* <code>[CCES_analysis_slide.R](codes/CCES_analysis_slide.R)</code>: Exporting figures for slides.

## Project Structure

 * *codes*: R codes used for data construction and analysis
 * *data*: Storage for datasets (Most of Datasets will be generated through R codes in *codes* folder)
 * *outputs*: Results (plots) generated for the presentation
 * *papers*: Storage for current versions of working papers

## License 

The analytical results in this project are licensed under the [Creative Commons Attribution 4.0 license](https://choosealicense.com/licenses/cc-by-4.0/), and the programming code used to generate the result is licensed under the [MIT license](https://choosealicense.com/licenses/mit/).