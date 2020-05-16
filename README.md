1. Open the Google Colab Notebook - (https://colab.research.google.com/drive/1Ewa13mT_wHUq0DUCB-wWTe-NI-M9mkGu)
2. Copy the code in your Notebook file
3. The icml_face_data.csv must be uploaded and mounted in your Google Drive.
4. Download the icml_face_data.csv from (https://personal.utdallas.edu/~axa190084/icml_face_data.csv) this link.
5. Mount the csv file dowloaded in your local machine to the Google Drive using the following commands :
  // Command to be run in Google Colab
  
  from google.colab import files
  
  uploaded = files.upload()

6.After running this command run the notebook code to train and test the model
