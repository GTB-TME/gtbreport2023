% v4: Now taking 2022 data as well
% v5: Compare against annual data and adjust as needed, to get agreement
% with annual totals
% v6: Including late adjustments to annual data from Brazil

clear all; 

% -------------------------------------------------------------------------
% --- Get the disruption data, and organise -------------------------------

[num,txt,raw] = xlsread('TB_provisional_notifications_2023-04-24.xlsx');

colnames = raw(1,:);
getcol = @(str) find(contains(colnames,str));

% Country codes
dat  = raw(2:end,[getcol('iso3'),getcol('year')]);
iso3_disrp = unique(dat(:,1));
yrs  = [2020 2021 2022];

% --- Get monthly and quarterly data --------------------------------------
mdata = nan(length(iso3_disrp), 12, length(yrs));  mcols = getcol('m_');
qdata = nan(length(iso3_disrp), 4,  length(yrs));  qcols = getcol('q_');

for ii = 1:length(iso3_disrp)
    rows = find(strcmp(dat(:,1),iso3_disrp{ii}));
    yrsa = cell2mat(dat(rows,2));
    for iy = 1:length(yrs)
        ir = find(yrsa==yrs(iy));
        if ~isempty(ir)
            mdata(ii,:,iy) = cell2mat(raw(rows(ir)+1,mcols));
            qdata(ii,:,iy) = cell2mat(raw(rows(ir)+1,qcols));
        end    
    end
end

% --- Get monthly/quarterly classification --------------------------------
col2  = getcol('report_frequency');
freq  = nan(length(iso3_disrp),length(yrs));
% years = [2020, 2021];
for ii = 1:length(iso3_disrp)
    rows = find(strcmp(dat(:,1),iso3_disrp{ii}));
    yrsa = cell2mat(dat(rows,2));
    for iy = 1:length(yrs)
        ir = find(yrsa==yrs(iy));
        if ~isempty(ir)
            freq(ii,iy)  = cell2mat(raw(rows(ir)+1,col2));
        end
    end
end


% --- Check data against annual totals, and adjust where necessary --------
m_annu = squeeze(sum(mdata,2,'omitnan'));
q_annu = squeeze(sum(qdata,2,'omitnan'));
annu   = m_annu + q_annu;

% Pull the annual totals for previous years, and align
load ../'TB Notifications'/preCOVID_notifs;
tmp  = [mat.('noti 2020'), mat.('noti 2021'), mat.('noti 2022')];
ctrs = mat.iso3;

% For the case of Brazil, pull the updated numbers that Hazim sent on 24
% July 2023
C = readtable('../TB Notifications/annual_notifs_2023-07-24.csv');
rows = find(strcmp(C.iso3,'BRA') & C.year>=2020);
vals = C.c_newinc(rows)';
tmp(find(strcmp(ctrs,'BRA')),:) = vals;



rows = [];
for ii = 1:length(iso3_disrp)
    rows(ii) = find(strcmp(ctrs,iso3_disrp{ii}));
end
extr = tmp(rows,:);
avai = avail2022(rows,:);

% Find the annual adjustment factors in order to agree with annual totals
mul = extr./annu; 
mul(annu==0) = 1;
% Set 2022 correction to 1 where there isn't 2022 data
mul(avai==0,3) = 1;
relmul = mul-1;

% Sometimes, there's an apparent large multiplier but just because of
% missing data in the disruption data - check where these are the case
[rows,cols] = find(relmul>0.1);
for ii = 1:length(rows)
    row = rows(ii); col = cols(ii);
    if freq(row,col) == 70
        tmp = mdata(row,:,col);
    else
        tmp = qdata(row,:,col);
    end
    
    % Set that year's correction to 1
    iscomplete = ~isnan(sum(tmp));
    if isnan(sum(tmp))
        mul(row,col) = 1;
    end
end
save multipliers mul iso3_disrp;

% Finally, adjust the monthly and quarterly data accordingly
tmp = permute(repmat(mul,[1 1 12]),[1,3,2]);
mdata = mdata.*tmp;

tmp = permute(repmat(mul,[1 1 4]),[1,3,2]);
qdata = qdata.*tmp;


% --- Finally, make monthly disruption data into quarterly, to ensure consistency
% across regions ----------------------------------------------------------
dims   = size(mdata);
tmp    = reshape(mdata,[dims(1), 3, 4, dims(end)]);
mqdata = squeeze(sum(tmp,2));
% Draw data from either source as necessary
disr_data = zeros(size(mqdata));
for ic = 1:length(iso3_disrp)
    tmp1 = [];
    for iy = 1:size(freq,2)
       if freq(ic,iy)==70
          tmp1 = [tmp1, mqdata(ic,:,iy)];
       else
          tmp1 = [tmp1, qdata(ic,:,iy)]; 
       end
   end
   allqdata(ic,:) = tmp1;
end
% Drop columns where no countries have data
del = find(sum(isnan(allqdata),1)==length(iso3_disrp));
allqdata(:,del) = [];
% For all remaining rows, pad out the remaining nans
for ic = 1:length(iso3_disrp)
    vec = allqdata(ic,:); mn = mean(vec,'omitnan');
    allqdata(ic,:) = fillmissing(allqdata(ic,:),'constant',mn); 
end

save disruption_data_2023_04_24 iso3_disrp mdata qdata freq allqdata;