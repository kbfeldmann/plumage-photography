//This macro accomplishes separate tasks for each individual photographed:
//1: Open, measure and add trichromatic reflectance data from every .mspec file to a single .csv
//2: Open, measure and add tetrachromatic reflectance data from every .mspec file to a single .csv
//3: Open, measure and add area data from every .srw file to a single .csv

folderPath = getDirectory("Choose a Directory");
folderName = File.getName(folderPath);
//csvReflectPath = folderPath + "/" + folderName + "_reflectance" + ".csv";
csvReflectPath = folderPath + "/" + folderName + "_reflectance" + "EYEBROW" + ".csv";
//csvAreaPath = folderPath + "/" + folderName + "_area" + ".csv";
csvAreaPath = folderPath + "/" + folderName + "_area" + "EYEBROW" + ".csv";
//csvLuminancePath = folderPath + "/" + folderName + "_luminance" + ".csv";
csvLuminancePath = folderPath + "/" + folderName + "_luminance" + "EYEBROW" +".csv";
//patches = newArray("Back", "Belly", "LSide", "RSide");
patches = newArray("Back","RSide");

//OPEN .MSPEC FILE AND EXTRACT REFLECTANCE DATA
function measureReflectance(folder, name, patch, counter) {
     mspecPath = folder + "/" + patch + "/" + name + "_" + patch + ".mspec";
     run(" Load Multispectral Image", "select="+mspecPath+" image=[Linear Normalised Reflectance Stack]");
     run("Measure ROIs");
     if (counter == 0){
          saveAs("Results", csvReflectPath);
     } else {
                   String.copyResults;
                   data = String.paste;
                   data = replace(data, "\t", ",");
                   data = substring(data, 0, lengthOf(data)-1);
                   File.append(data, csvReflectPath);
              }
     close();
     selectWindow("Results");
     run("Close");
     selectWindow("ROI Manager");
     run("Close");
}

//CONVERT .MSPEC FILE TO CONE CATCH AND EXTRACT LUMINANCE AND COLOR
function measureLuminance(folder, name, patch, counter) {
     mspecPath = folder + "/" + patch + "/" + name + "_" + patch + ".mspec";
     run(" Load Multispectral Image", "select="+mspecPath+" image=[Linear Normalised Reflectance Stack]");
     run("Convert to Cone Catch", "model=[Samsung NX1000 Nikkor EL 80mm D65 to Bluetit D65] remove replace=0.001");
     run("Pattern Colour & Luminance Measurements", "image_label=[ ] luminance_channel=dbl bandpass_method=DoG start_size=0 end_size=0 step_size=1.41421356 step_multiplier=Multiply luminance_bands=0 lowest_luminance=0 highest_luminance=100 transform_luminance=Linear prefix=[]");     
     for (i=0; i<nResults; i++){
          result = getResultLabel(i);
          individualPatch = name + "_" + patch + result;
          individualPatch = replace(individualPatch, " ", "");
          setResult("Label", i, individualPatch);
     }
     updateResults();

     if (counter == 0){
          selectWindow("Results");
          saveAs("Results", csvLuminancePath);
     } else {
               String.copyResults;
               data = String.paste;
               data = replace(data, "\t", ",");
               data = substring(data, 0, lengthOf(data)-1);
               File.append(data, csvLuminancePath);
              }
     close();
     close();
     selectWindow("Results");
     run("Close");
     selectWindow("ROI Manager");
     run("Close");
     selectWindow("Luminance Results");
     run("Close");
     selectWindow("Pattern Results");
     run("Close");
}

//OPEN .SRW FILE AND EXTRACT AREA DATA
function measureArea(folder, name, patch, counter){
     areaMeasured = 0;
     fileListPath = folderPath + patch;
     //tifPath = folder + "/" + patch + "/" + name + "_" + patch + "_Vis1.tif";
     tifPath = folder + "/" + patch + "/" + name + "_" + patch + "_Eyebrow.tif";
     //zipPath = folder + "/" + patch + "/" + name + "_" + patch + "_Vis1.zip";
     zipPath = folder + "/" + patch + "/" + name + "_" + patch + "_Eyebrow.zip";
     //zipName = name + "_" + patch + "_Vis1.zip";
     zipName = name + "_" + patch + "_Eyebrow.zip";
     files = getFileList(fileListPath);
     for (i = 0; i < files.length; i++){
          if (files[i] == zipName){
               areaMeasured = 1;
          }
     }
     if (areaMeasured == 1){
          run("Set Measurements...", "area display redirect=None decimal=9");
          open(tifPath);
          run("ROI Manager...");
          roiManager("Open", zipPath);
          for (i = 0; i < roiManager("count"); i++){
               roiManager("Select", i);
               run("Measure");
          }
          if (counter == 0){
               saveAs("Results", csvAreaPath);
          } else {
                        String.copyResults;
                        data = String.paste;
                        data = replace(data, "\t", ",");
                        data = substring(data, 0, lengthOf(data)-1);
                        File.append(data, csvAreaPath);
                   }
          close();
          selectWindow("Results");
          run("Close");
          selectWindow("ROI Manager");
          run("Close");
     } else {
                       print("Error: Individual does not have area measurements.");
              }
}

//CREATE .CSV FILES
createFile = 0;
for (i = 0; i < patches.length; i++){
     measureReflectance(folderPath, folderName, patches[i], createFile);
     measureLuminance(folderPath, folderName, patches[i], createFile);
     measureArea(folderPath, folderName, patches[i], createFile);
     createFile++;
}

selectWindow("Log");
run("Close");
