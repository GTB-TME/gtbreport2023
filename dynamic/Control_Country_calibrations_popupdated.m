% Iterate over all countries to simulate disruptions

clear all;
ctrys = {'AFG','UZB'};

tic
% --- Make calibrations
for ictry = 1:length(ctrys)
    clearvars -except ctrys ictry;
    fprintf('%0.5g: ', ictry);
    ctry = ctrys{ictry};
    iso3 = ctrys{ictry};
    
    Setup_model_popupdated;
    Get_calibrations_popupdated;
    
    fprintf('\n');  close all;
end
toc