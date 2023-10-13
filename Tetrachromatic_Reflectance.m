%% ANALYZE TETRACHROMATIC REFLECTANCE DATA - PHOTOS

% Combines data stored in .CSV files, ensures all expected regions have
% data associated with them, checks to see if the tests match their
% expected values and, assuming all previous steps were successful, runs
% TetraColorSpace.

cd /Users/katherinefeldmann/Desktop/CU_Boulder/Taylor_Lab/Honors Thesis/R Code;
startPath = uigetdir('./subfolder1/');
[filepath,name] = fileparts(startPath);
files = dir(startPath);
folders = files([files.isdir]);
%EXCELpath = strcat(startPath, "/", name, "_tetrachromatic.xls");
EXCELpath = strcat(startPath, "/", name, "_tetrachromaticEYEBROW.xls");

%COMBINE DATA INTO AN EXCEL DOCUMENT

% Uploads data stored in .CSV files (throughout numerous folders) and concatenates
% into one EXCEL document.

H = 2;
for i = 3:length({folders.name}) %Omit '.' and '..' by starting with the 3rd entry
    cd(fullfile(startPath,folders(i).name));
    %filename = strcat(startPath,'/',folders(i).name,'/',folders(i).name,'_luminance.csv');
    filename = strcat(startPath,'/',folders(i).name,'/',folders(i).name,'_luminanceEYEBROW.csv');
    %individualPath = strcat(startPath,'/',folders(i).name,'/',folders(i).name,"_luminance.xls");
    individualPath = strcat(startPath,'/',folders(i).name,'/',folders(i).name,"_luminanceEYEBROW.xls");
    
    if isfile(filename)
        %data = readtable(strcat(folders(i).name,"_luminance.csv"));
        data = readtable(strcat(folders(i).name,"_luminanceEYEBROW.csv"));
        lumData = data;
        data = data(1:end,{'Label','uvMean','swMean','mwMean','lwMean'});
        headersTetra = {'Label','uvMean','swMean','mwMean','lwMean'};
        writecell(headersTetra,EXCELpath,"Sheet",1,"Range","A1:E1");
        writetable(data,EXCELpath,'Sheet',1,'Range',['A' num2str(H)],'WriteVariableNames',0);
        H = H + height(data);
        j = 0;
        headersLum = {'uv','sw','mw','lw'};
        writecell(headersLum,individualPath,"Sheet",1,"Range","B1:E1");
        for k = 1:height(lumData)
           if(contains(table2array(lumData(k,"Label")),"99test") == 0 && contains(table2array(lumData(k,"Label")),"20test") == 0)
               j = j + 1;
               lw = table2array(lumData(k,"lwMean"))/(table2array(lumData(k,"lwMean")) + table2array(lumData(k,"mwMean")) + table2array(lumData(k,"swMean")) + table2array(lumData(k,"uvMean")));
               mw = table2array(lumData(k,"mwMean"))/(table2array(lumData(k,"lwMean")) + table2array(lumData(k,"mwMean")) + table2array(lumData(k,"swMean")) + table2array(lumData(k,"uvMean")));
               sw = table2array(lumData(k,"swMean"))/(table2array(lumData(k,"lwMean")) + table2array(lumData(k,"mwMean")) + table2array(lumData(k,"swMean")) + table2array(lumData(k,"uvMean")));
               uv = table2array(lumData(k,"uvMean"))/(table2array(lumData(k,"lwMean")) + table2array(lumData(k,"mwMean")) + table2array(lumData(k,"swMean")) + table2array(lumData(k,"uvMean")));
               values = [uv,sw,mw,lw];
               writematrix("Label",individualPath,"Sheet",1,"Range","A1");
               writecell(table2array(lumData(k,"Label")),individualPath,"Sheet",1,"Range",strcat("A",num2str(j + 1)));
               writematrix(values,individualPath,"Sheet",1,"Range",strcat("B",num2str(j + 1),":E",num2str(j + 1)));
           end
        end
    else
        disp(strcat(folders(i).name,' has not been analyzed'));
    end
end

%VERIFY ALL PATCH AND TEST ROIS EXIST

% Ensures that there is reflectance data for all patches and that each region has
% a 99test and a 20test.

% excelData = readtable(EXCELpath);
% individuals = unique(regexp(excelData.Label,'[BM][CO][CC][HH]\d*','once','match'));
% patches = {'_Back_head','_Belly_throat','_\wSide_cheek','_\wSide_\w*terior_c\w*al','_\w*_\d*test'};
% occurances = {1,1,2,8,8};
% for i = 1:length(individuals)
%     for j = 1:length(patches)
%         count = length(unique(regexp(excelData.Label,strcat(individuals(i),patches(j)),'once','match')));
%         if(count-1 ~= occurances{j})
%             disp(strcat("MISSING ROIs: ",individuals(i),patches(j)));
%             return
%         end
%     end
% end
% disp("All ROIs Present!");

%VERIFY ACCURACY OF TEST ROIS

% Analyzes 99test and 20test values to see how close they are to 99 and 20
% respectively. If values are not within the accepted range, a warning will
% appear and the photo of interest may need to be reanalyzed.

% excelData = readtable(EXCELpath);
% 
% lw99 = excelData(strcmp(excelData.Label,regexp(excelData.Label,'[BM][CO][CC][HH]\d*_\w*_99test','once','match')),["lwMean","Label"]);
% mw99 = excelData(strcmp(excelData.Label,regexp(excelData.Label,'[BM][CO][CC][HH]\d*_\w*_99test','once','match')),["mwMean","Label"]);
% sw99 = excelData(strcmp(excelData.Label,regexp(excelData.Label,'[BM][CO][CC][HH]\d*_\w*_99test','once','match')),["swMean","Label"]);
% uv99 = excelData(strcmp(excelData.Label,regexp(excelData.Label,'[BM][CO][CC][HH]\d*_\w*_99test','once','match')),["uvMean","Label"]);
% 
% lw20 = excelData(strcmp(excelData.Label,regexp(excelData.Label,'[BM][CO][CC][HH]\d*_\w*_20test','once','match')),["lwMean","Label"]);
% mw20 = excelData(strcmp(excelData.Label,regexp(excelData.Label,'[BM][CO][CC][HH]\d*_\w*_20test','once','match')),["mwMean","Label"]);
% sw20 = excelData(strcmp(excelData.Label,regexp(excelData.Label,'[BM][CO][CC][HH]\d*_\w*_20test','once','match')),["swMean","Label"]);
% uv20 = excelData(strcmp(excelData.Label,regexp(excelData.Label,'[BM][CO][CC][HH]\d*_\w*_20test','once','match')),["uvMean","Label"]);
% 
% standardValues = {lw99,mw99,sw99,uv99,lw20,mw20,sw20,uv20};
% 
% for i = 1:size(standardValues,2)
%     standardDev = std(table2array(standardValues{i}(:,1)));
%     maxValue = standardDev*5 + mean(table2array(standardValues{i}(:,1)));
%     minValue = mean(table2array(standardValues{i}(:,1))) - standardDev*5;
%     errorMax = table2array(standardValues{i}(:,1)) > maxValue;
%     errorMin = table2array(standardValues{i}(:,1)) < minValue;
%     disp(standardValues{i}(errorMax,:));
%     disp(standardValues{i}(errorMin,:));
% end

%CREATE EXCEL FOR TETRACOLORSPACE

excelData = readtable(EXCELpath);
%inputPath = strcat(startPath, "/", name, "_tetrainput.xls");
inputPath = strcat(startPath, "/", name, "_tetrainputEYEBROW.xls");

lwData = table2array(excelData(:,"lwMean"));
mwData = table2array(excelData(:,"mwMean"));
swData = table2array(excelData(:,"swMean"));
uvData = table2array(excelData(:,"uvMean"));

writetable(excelData,inputPath,"Sheet",1);

for i = 1:height(excelData)
    lw = lwData(i)/(lwData(i) + mwData(i) + swData(i) + uvData(i));
    mw = mwData(i)/(lwData(i) + mwData(i) + swData(i) + uvData(i));
    sw = swData(i)/(lwData(i) + mwData(i) + swData(i) + uvData(i));
    uv = uvData(i)/(lwData(i) + mwData(i) + swData(i) + uvData(i));
    values = [uv,sw,mw,lw];
    writematrix(values,inputPath,"Sheet",1,"Range",strcat("B",num2str(i + 1),":E",num2str(i + 1)));
end

excelColumns = {'Patch','u/v','s','m','l'};
writecell(excelColumns,inputPath,"Sheet",1,"Range","A1:E1");

disp("Finished combining data.");

%% ANALYZE TETRACHROMATIC REFLECTANCE DATA - SPECTROMETER

cd /Volumes/Feldmann_Research/Taylor_Lab_Honors_Thesis;
startPath = uigetdir('./subfolder1/');
[filepath,name] = fileparts(startPath);
files = dir(startPath);
folders = files([files.isdir]);
EXCELpath = strcat(startPath, "/", name, "_tetrachromatic.xls");

%COMBINE DATA INTO AN EXCEL DOCUMENT

% Uploads data stored in .CSV files (throughout numerous folders) and concatenates
% into one EXCEL document.

for i = 3:length({folders.name}) %Omit '.' and '..' by starting with the 3rd entry
    cd(fullfile(startPath,folders(i).name));
    individualPath = strcat(startPath,'/',folders(i).name,'/',folders(i).name,'.xls');
    
    patches = [];
    txtFiles = dir(strcat(startPath,'/',folders(i).name));
    for k = 3:length(txtFiles)
        patch = regexp(txtFiles(k).name,'[HCST][a-z]*[dket]','match');
        patches = [patches patch];
    end
    patches = string(unique(patches));
   
    for j = 1:length(patches)
        filename1 = strcat(startPath,'/',folders(i).name,'/',folders(i).name,'_',patches(j),'_Reflection_1.txt');
        filename2 = strcat(startPath,'/',folders(i).name,'/',folders(i).name,'_',patches(j),'_Reflection_2.txt');
        filename3 = strcat(startPath,'/',folders(i).name,'/',folders(i).name,'_',patches(j),'_Reflection_3.txt');
        data1 = readtable(filename1);
        data1.Properties.VariableNames = ["Wavelength","Reflectance1"];
        data2 = readtable(filename2);
        data2.Properties.VariableNames = ["Wavelength","Reflectance2"];
        data3 = readtable(filename3);
        data3.Properties.VariableNames = ["Wavelength","Reflectance3"];

        data = [data1(:,"Reflectance1") data2(:,"Reflectance2") data3(:,"Reflectance3")];
        reflectMean = mean(data{:,:},2);
        
        if j == 1
            inputData = data1(:,"Wavelength");
            inputData.(patches(j)) = reflectMean;
        else
            inputData.(patches(j)) = reflectMean;
        end
    end
    writetable(inputData,individualPath,"Sheet",1);
end

disp("Finished combining data.");

%% RUN TETRACOLORSPACE
    
cd /Users/katherinefeldmann/Desktop/CU_Boulder/MATLAB/;

diary tetraData.txt;

TetraColorSpace;

diary off;