@Library('jenkins-r-shared-library') _

rBuildPipeline(
  dockerImage: 'library/r-base:3.6.1-stat-latest',
  mailTo: 'stefan.fleck@statistik.gv.at',
  rPackages: 'knitr,testthat,roxygen2',
  githubPackages: '-',
  additionalScript:'-',
  ignoreCheck:'NO'
)
