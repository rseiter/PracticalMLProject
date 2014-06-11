# Download data file for this project.
# https://class.coursera.org/predmachlearn-002/human_grading/view/courses/972090/assessments/4/submissions
# The data comes from http://groupware.les.inf.puc-rio.br/har
# Credit them if used for any other purpose.

file1Url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
file2Url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
file1Path <- "./data/pml-training.csv"
file2Path <- "./data/pml-testing.csv"
dateDownloadedFile <- "./data/dateDownloaded.txt"

if (!file.exists("data")) {
  dir.create("data")
}

if (!file.exists(file1Path) || !file.exists(file2Path)) {
  download.file(file1Url, destfile = file1Path)
  download.file(file2Url, destfile = file2Path)
  dateDownloaded <- date()
  dateDownloaded
  cat(dateDownloaded, file=dateDownloadedFile)
} else if (file.exists(dateDownloadedFile)) {
  print(paste("Data files", file1Path, "and", file2path, "downloaded on"))
  cat(readChar(dateDownloadedFile, 1e5))
}
