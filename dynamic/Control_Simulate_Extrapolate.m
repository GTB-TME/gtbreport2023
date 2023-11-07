% Iterate over all countries to simulate disruptions

clear all;

iso3s_todo = {'AFG','AGO','AZE','BGD','BRA','COL','IDN','KAZ','KEN','KGZ','KHM','LSO',...
    'MEX','MMR','MNG','MYS','NPL','PAK','PER','PHL','RUS','THA','TLS','UZB','VNM','ZWE'};

tic
% --- Extrapolate to fit WHO estimates and write results
for iiso3 = 1:length(iso3s_todo)
    clearvars -except iso3s_todo iiso3;
    fprintf('%0.5g: ', iiso3);
    iso3 = iso3s_todo{iiso3};

    tic
    Disruptionvector_get_initial;
    toc
    
    Extrapolate_Countries2;

    fprintf('\n');  close all;
end
toc