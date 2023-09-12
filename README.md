# Dempster-Shafer Τheory Application in Recommender Systems and Comparison of Constraint Programming’s and Möbius Transform’s Implementation
In this repository you can find my Thesis for my Diploma (BSc) in Informatics and Telecommunications, from the National Kapodistrian University of Athens (https://pergamos.lib.uoa.gr/uoa/dl/frontend/el/browse/3228581).
The title of the thesis is "*Dempster-Shafer Τheory Application in Recommender Systems and Comparison of Constraint Programming’s and Möbius Transform’s Implementation*".

## Theory - Report
The thesis with the corresponding technical APPENDIX (ANNEX in this case) is found in [Thesis.pdf].

## Implementation
The implementation is in the folder [implementation].

### Prerequisites 
In order to execute the code *ECLiPSe 6.1*, *Python 2.7* & *3.10*, *rpy2*, *PyCLP* should be installed in the machine. If you find difficulties in installing the PyCLP please refer to the ANNEX in the Thesis-Report.

### Code structure 
- The folder [application_prolog] refers to the Application of DST and use of CLP.
- The folder [prolog_vs_ibelief] refers to the comparison of the two implementations.

In each folder you will find a *bash script (.sh)* that runs multiple executions of the code.

For the code structure and the functionality of each module, please refer to the Thesis in **Chapters 6** and **7** respectively.

### Dataset
The dataset won't be provided here, as it is large, but can be found in : 
- https://drive.google.com/drive/folders/1tqFFT6QBJaE7qzGn44tnt4Yf5ygiB9ch?usp=sharing   or
- https://www.kaggle.com/datasets/vodclickstream/netflix-audience-behaviour-uk-movies

The dataset should be placed inside [application_prolog] folder and saved with the name ***'vodclickstream_uk_movies_03.csv'*** in order for the code to work.


[//]: # (These are reference links used in the body of this note and get stripped out when the markdown processor does its job. There is no need to format nicely because it shouldn't be seen. Thanks SO - http://stackoverflow.com/questions/4823468/store-comments-in-markdown-syntax)

[Thesis.pdf]: 
<https://github.com/tatiana-boura/BSc-Thesis-Dempster-Shafer-Theory-Application-in-Recommender-Systems/blob/main/Report-Thesis.pdf>
[implementation]: 
<https://github.com/tatiana-boura/BSc-Thesis-Dempster-Shafer-Theory-Application-in-Recommender-Systems/tree/main/Boura_BSc_thesis_code>
[application_prolog]:
<https://github.com/tatiana-boura/BSc-Thesis-Dempster-Shafer-Theory-Application-in-Recommender-Systems/tree/main/Boura_BSc_thesis_code/application_prolog>
[prolog_vs_ibelief]:
<https://github.com/tatiana-boura/BSc-Thesis-Dempster-Shafer-Theory-Application-in-Recommender-Systems/tree/main/Boura_BSc_thesis_code/prolog_vs_ibelief>
