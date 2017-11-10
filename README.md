# R Package: Pasta
## PAckage for Speckle Tracking Analysis
- Version: 1.0
- Date: 2017-04-03
- Author: Guillaume Chaumet <guillaumechaumet@gmail.com> [aut, cre] and Alain Boussuges <alain.boussuges@gmail.com> [aut]
- Maintainer: Guillaume Chaumet <guillaumechaumet@gmail.com>
- Description: This package contains functions for extracting raw data from 2D Speckle tracking echocardiography
- License: MIT
- Imports: data.table, parallel


=======

This package should be installed on R with the following command

```
  install.packages('devtools')
  library(devtools)
  install_github('guillaumechaumet/pasta')
  library(pasta)

```

Locate the path where your data are and launch the following command:

```
  myspectrackingdata<-multi.import.xtrain(path='yourdatapath')

```
