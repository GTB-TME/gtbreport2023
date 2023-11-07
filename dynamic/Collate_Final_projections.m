% Bring together all final model outputs, in format required for report

clear all;

opt = 'Collate countries';
% opt = 'Collate regions';


% -------------------------------------------------------------------------
% --- Pull together all modelled countries

if strcmp(opt,'Collate countries')
    iso3s_todo = {'AFG','AGO','AZE','BGD','BRA','COL','IDN','KAZ','KEN','KGZ','KHM','LSO',...
        'MEX','MMR','MNG','MYS','NPL','PAK','PER','PHL','RUS','THA','TLS','UZB','VNM','ZWE'};
    
    alltbl = {};
    % --- Get extrapolated estimates and concatenate
    for iiso3 = 1:length(iso3s_todo)
        clearvars -except iso3s_todo iiso3 alltbl;
        fprintf('%0.5g: ', iiso3);
        iso3 = iso3s_todo{iiso3};
        load([iso3, '/extrapolated_outputs.mat'],'tbl');
        alltbl = [alltbl; tbl];
        fprintf('\n');
    end
    
    % Formulate file name
    [yy,mm,dd] = ymd(datetime("today"));    
    if mm < 10
        smm = ['0',num2str(mm)];
    else
        smm = num2str(mm);
    end
    fname = [num2str(yy-2000), smm, num2str(dd), '_Modelled_Countries.csv'];
    cell2csv(fname,alltbl);

    
    
% -------------------------------------------------------------------------
% --- Pull together all extrapolated countries in modelled regions
        
elseif strcmp(opt,'Collate regions')
    regs_todo = {'region_AFR', 'region_AMR', 'region_EUR'};
    
    
    alltbl = {};
    % --- Get extrapolated estimates and concatenate
    for ireg = 1:length(regs_todo)
        clearvars -except regs_todo ireg alltbl;
        fprintf('%0.5g: ', ireg);
        reg = regs_todo{ireg};
        
        load([reg, '/extrapolated_outputs.mat'],'tbl');
        alltbl = [alltbl; tbl];
        fprintf('\n');
    end
    % Formulate file name
    [yy,mm,dd] = ymd(datetime("today"));    
    fname = [str2num(yy-2000), str2num(mm), str2num(dd), "_Extrapolated_Countries.csv"];
    cell2csv(fname,alltbl);
    
end
