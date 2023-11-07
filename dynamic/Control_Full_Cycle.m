% Iterate over all countries to simulate disruptions

clear all;

iso3s_todo = {'AFG','AGO','AZE','BGD','BRA','COL','IDN','KAZ','KEN','KGZ','KHM','LSO',...
    'MEX','MMR','MNG','MYS','NPL','PAK','PER','PHL','RUS','THA','TLS','UZB','VNM','ZWE'};

% --- Perform full cycle for countries: calibration, finding raw disruption
% vector, and aligning with pre-pandemic estimates
tic
for iiso3 = 1:length(iso3s_todo)
    clearvars -except iso3s_todo iiso3;
    fprintf('%0.5g: ', iiso3);
    iso3 = iso3s_todo{iiso3};

    Setup_model;
    Get_calibrations;
    Disruptionvector_get_initial;
    Extrapolate_Countries;

    fprintf('\n');  close all;
end
toc