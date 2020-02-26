#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)

#* @apiTitle Image Classification

#* Clasiffy and Image 
#* @param img Image To Classify
#* @get /images
function(img) {
  image <- as.vector(img)
  numbers <- getImageNumbers(image)
  numbers <- numbers[,-1]
  nbPred <- predict(naiveModel,newdata = numbers)
  Result <- as.vector(nbPred)
    list(img = paste0("Naive Bayes Model Classifeid This Image as: ", Result))
}
