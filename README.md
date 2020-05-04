# Fruit-Texture-Classification

A project on classification of fruit images based on texture as feature vectors in R language. 

At first feature vectors are generated first from fruit images. The algorithms used are following:
  * Grey Level Co-occurence Matrix (GLCM)
  * Local Binary Pattern (LBP)
  * Histogram of Gradients (HoG)

Next feature vectors are passed as input to following classification algorithms:
  * Implemented K-NN
  * Implemented naive_bayes
  * SVM (used library)
  
From this project we conclude that, SVM shows better performance when used with HoG extractor while k-NN performs better when used with LBP extractor.
