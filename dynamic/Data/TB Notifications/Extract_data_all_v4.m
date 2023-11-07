% _all: Version to pull data from 2018 onwards, into one table
% v2:   Version to extrapolate time trends for each country, not just
% cumulative notifications (for purpose of plotting)
% v3:   Version to include Hazim's numbers for notifications

clear all; warning off;
C = readtable('TB_notifications_2023-04-24.csv');

colnames = C.Properties.VariableNames;

% --- Filter on years from 2015
rows    = find(C.year>=2015);
C1      = C(rows,:);

% --- Pull out the relevant notification data
notifcols  = [find(strcmp(colnames,'iso3')), find(strcmp(colnames,'country')), find(strcmp(colnames,'year')), find(strcmp(colnames,'g_whoregion')), find(strcmp(colnames,'ret_nrel')), find(strcmp(colnames,'c_newinc'))];
C2         = C1(:,notifcols);
% C2.allnoti = C2.ret_nrel + C2.c_newinc;
C2.allnoti = C2.c_newinc;

years = unique(C2.year);
for iy = 1:length(years)
    if iy == 1
        cols = [1:2, size(C2,2)];
        tmp  = C2(C2.year==years(iy),cols);
        mat  = renamevars(tmp,'allnoti',['noti ', num2str(years(iy))]);
    else
        cols = [1,size(C2,2)];
        tmp  = renamevars(C2(C2.year==years(iy),cols),'allnoti',['noti ', num2str(years(iy))]);
        mat  = outerjoin(mat, tmp, 'MergeKeys', true);
    end
end

% --- Now append 2022 data from Hazim
C2  = readtable('annual_2022_notifications_2022-05-01.csv');
C3  = renamevars(C2(:,[2,4]),"c_newinc","noti 2022");
mat = outerjoin(mat, C3, 'MergeKeys', true);

% Pad NaNs
avail2022 = ones(size(mat,1),1);                                           % Whether 2022 data is available yet (info lost in padding, but needed in Get_disruption_data5)
for ii = 1:size(mat,1)
    if isnan(mat(ii,:).("noti 2021"))
        mat(ii,:).("noti 2021") = mat(ii,:).("noti 2020");
    end
end
for ii = 1:size(mat,1)
    if isnan(mat(ii,:).("noti 2022"))
        mat(ii,:).("noti 2022") = mat(ii,:).("noti 2021");
        avail2022(ii) = 0;
    end
end

% return;
save preCOVID_notifs mat avail2022;



% -------------------------------------------------------------------------
% --- Analysis ------------------------------------------------------------

% See which countries have significant trends from 2015 onwards
nums = mat{:,3:end};
coeffs = [];
predic = [];
for ii = 1:size(nums,1)
    mdl = fitlm(2015:2019, nums(ii,1:5), 'linear');
    coeffs(ii,:) = [mdl.Coefficients.pValue(2), mdl.Coefficients.Estimate(2)];

    % Predicted timeseries for 2020 - 2022
    if coeffs(ii,1)<0.05
        pred = predict(mdl,[2015:2022]')';
        predic(ii,:) = max(pred,0);
    else
        predic(ii,:) = nums(ii,5)*ones(1,8);
%         predic(ii,:) = mean(nums(ii,1:5))*ones(1,8);
    end
end

% Now calculate the shortfall
actual = sum(nums(:,6:end-1),2);
shortf = sum(predic(:,6:end-1),2) - actual;
ranked = sortrows([shortf, (1:length(shortf))'],-1);
ord    = ranked(~isnan(ranked(:,1)),2);

matord = mat(ord,:);

% Get countries accounting for 95% of global drop
vec    = max(shortf(ord),0);
cumul_contrib = cumsum(vec)/sum(vec);
maxind = find(cumul_contrib>0.95,1,'first');

exclude  = {'DZA','PNG','ROU','SLE','UGA','UKR'};                           % Countries we decided to exclude based on discussion at WHO on 1 May 2023
new_ctrs = setdiff(mat.iso3(ord(1:maxind)), exclude);

% --- Get previously modelled countries
load ../../country_rankings_global_PREVIOUS ctrs_priority;
tmp1 = ctrs_priority;
tmp1(length(ctrs_priority)+1) = {'MOZ'};
% Adjust for countries that were removed for the report
removed = {'ZAF','ETH','PRK','TUR','USA','UKR','ROU'};
old_ctrs = setdiff(tmp1,removed);

% --- Only PNG is meant to be dropped - append any other countries absent
% in new list, that were previously present

missing  = setdiff(setdiff(old_ctrs, new_ctrs),'PNG');
new_ctrs = [new_ctrs; missing];

% --- Get rank of new countries in decreasing order of global contribution
tmp = [ord, (1:length(ord))'];
for ii = 1:length(ord)
    ctry = mat.iso3{ord(ii)};
    if ismember(ctry,new_ctrs)
        tmp(ii,3) = 1;
    end
end
selord = tmp(tmp(:,3)>0,1);

matsel    = mat(selord,:);
predicsel = predic(selord,:);


% Identify countries being ADDED compared to 2022 report
adding = setdiff(new_ctrs, old_ctrs);

% Identify countries being DROPPED compared to 2022 report
dropping = setdiff(old_ctrs, new_ctrs);


% --- Draw the notifications and baselines to visualise disruptions -------

xx = 2015:2022;

% Countries to be modelled
figure; ct = 1; 
tmp = sqrt(length(new_ctrs)); nr = floor(tmp); nc = ceil(tmp);
if nr*nc<length(new_ctrs)
    nr=nr+1;
end
for ii = 1:size(matsel,1)
    subplot(nr,nc,ct); hold on;
    ctry = matsel.iso3(ii); 

    data = matsel{ii,3:end}/1e4;
    pred = predicsel(ii,:)/1e4;
    
    plot(xx,data,'.','markersize',20);
    plot(xx,pred, 'linewidth', 1.5);
    title(ctry);
    if ismember(ctry,adding)
        title([ctry, '*']);
    end
    ylim([0 max(data)*1.3]);
    ct = ct+1;
end

% Countries to be dropped?
figure; ct = 1; 
tmp = sqrt(length(dropping)); nr = floor(tmp); nc = ceil(tmp);
for ii = 1:length(dropping)
    subplot(nr,nc,ct); hold on;
    ind = find(strcmp(mat.iso3,dropping{ii}));

    data = mat{ind,3:end}/1e4;
    pred = predic(ind,:)/1e4;
    
    plot(xx,data,'.','markersize',20);
    plot(xx,pred, 'linewidth', 1.5);
    title(mat.iso3(ind));
    ylim([0 max(data)*1.3]);
    ct = ct+1;
end




return;


figure; nr = 2; nc = 3; ct = 1;
% selisos = {'ARM','BOL','BLR','DOM','GEO','NAM','SVN'};
% selisos = {'ZAF','ETH','PRK','PNG','ROU','NPL','AZE','LSO','COL','TUR'};
selisos = {'PNG','NPL','AZE','LSO','COL'};
for ii = 1:length(selisos)
    subplot(nr,nc,ct); hold on;
    ind = find(strcmp(mat.iso3,selisos{ii}));
    plot(xx,mat{ind,3:end}/1e4,'.','markersize',20);
    plot(xx,predic(ind,:)/1e4, 'linewidth', 1.5);
    title(mat.iso3(ind));
    ylim([0 max(mat{ind,3:end}/1e4)*1.3]);
    ct = ct+1;
end


return;



% For those, predict what notifications should have been between 2020 -
% 2022
exp_notifs = [];
for ii = 1:size(nums,1)
    if ismember(ii,inds)
        

    else


    end
end



return;

% --- Prepare for saving
notifs_18 = C2(1:2:end,[1:3,end]);
notifs_19 = C2(2:2:end,[1:3,end]);



% -------------------------------------------------------------------------
% --- Also, make lookups that will be useful in later categorisations -----

% --- Get a list of countries in each region
regs = unique(notifs_19.g_whoregion);
for ir = 1:length(regs)
    rows = find(strcmp(notifs_19.g_whoregion,regs{ir}));
    reg2iso3s.(regs{ir}) = notifs_19.iso3(rows);
end

% --- Get a lookup of each iso3 code to country names, in agreement with Thembisa model
iso3s = unique(notifs_19.iso3);
for ic = 1:length(iso3s)
    ctr = iso3s{ic};
    row = find(strcmp(notifs_19.iso3,ctr));
    iso2ctry.(ctr) = notifs_19.country{row};
end
% Make adjustments according to labelling in Thembisa model
iso2ctry.COD = 'Democratic Republic of the Congo';
iso2ctry.GBR = 'United Kingdom';


save notif_data_1819 notifs_18 notifs_19;
save lookups reg2iso3s iso2ctry;



return;

















reg = {};

% --- Populate regions for country ----------------------------------------
iso3s = notifs_18.iso3;
for ic = 1:length(iso3s)
    ctr = iso3s{ic};
    reg{ic} = notifs_18.g_whoregion{find(strcmp(notifs_18.iso3,ctr))};
end

lookup = [iso3s, reg'];
% Reverse, to get a list of countries in each region
regs = unique(reg);
for ir = 1:length(regs)
    ctrlist.(regs{ir}) = lookup(find(strcmp(lookup(:,2),regs{ir})),1);
end

% --- Get a lookup of each iso3 code to country ---------------------------
for ic = 1:length(ctrs2_mat)
    ctr = ctrs2_mat{ic};
    row = find(strcmp(notifs_18.iso3,ctr));
    iso2ctry.(ctr) = notifs_18.country{row};
end
% Make adjustments according to labelling in Thembisa model
iso2ctry.COD = 'Congo';
iso2ctry.GBR = 'United Kingdom';

save notif_data_1819 notifs_18 notifs_19;



% % --- Also, get information for numbers diagnosed with HIV
% col = [find(strcmp(colnames,'iso3')), find(strcmp(colnames,'ret_nrel')), find(strcmp(colnames,'c_newinc')), find(strcmp(colnames,'hivtest_pos'))];
% C4 = C1(:,notifcols);