% When calibrating to pre-pandemic conditions, modelled uncertainty will
% generally be less than uncertainty in the data, because of the
% mechanistic links that the model makes between the different data sources
% (for example, between incidence and mortality). To avoid discontinuities, 
% this code expands modelled uncertainty to match WHO estimates used for
% pre-pandemic calibration.

% As for 'Extrapolate_Countries' but working on Regions, and creating
% estimates for each of the countries within those Regions

clear all;

% reg = 'AFR'; dir = ['region_',reg,'/']; load([dir,'projections_raw3']);
% reg = 'AMR'; dir = ['region_',reg,'/']; load([dir,'projections_raw_URbnew01']);
reg = 'EUR'; dir = ['region_',reg,'/']; load([dir,'projections_raw_URbnew015']);


inct  = aux.inct;
mort  = aux.mort;
inct2 = aux.inct2;
mort2 = aux.mort2;

% -------------------------------------------------------------------------
% --- Get the incidence and mortality rate ratios -------------------------

% IRR for total incidence
IRR_all = (inct(:,:,1)./inct(1,:,1))';
% IRR for HIV+ve
tmp = squeeze(sum(inct2(:,[2,3],:,:),2));
IRR_hiv = (tmp(:,:,1)./tmp(1,:,1))';
% MRR for total mortality
MRR_all = (mort(:,:,1)./mort(1,:,1))';
% MRR for HIV+ve
tmp = squeeze(sum(mort2(:,2,:,:),2));
MRR_hiv = (tmp(:,:,1)./tmp(1,:,1))';
% Bring them all together
if opts.hiv
    RRs_bsline = cat(3, IRR_all, IRR_hiv, MRR_all, MRR_hiv);
else
    RRs_bsline = cat(3, IRR_all, MRR_all);
end

% IRR for total incidence
IRR_all = (inct(:,:,2)./inct(1,:,2))';
% IRR for HIV+ve
tmp = squeeze(sum(inct2(:,[2,3],:,:),2));
IRR_hiv = (tmp(:,:,2)./tmp(1,:,2))';
% MRR for total mortality
MRR_all = (mort(:,:,2)./mort(1,:,2))';
% MRR for HIV+ve
tmp = squeeze(sum(mort2(:,2,:,:),2));
MRR_hiv = (tmp(:,:,2)./tmp(1,:,2))';
% Bring them all together
if opts.hiv
    RRs_disrupt = cat(3, IRR_all, IRR_hiv, MRR_all, MRR_hiv);
else
    RRs_disrupt = cat(3, IRR_all, MRR_all);
end


load('Data/TB Estimates/estim_data_2b.mat');

if opts.hiv
    names = {'inc_2019','inc_h1','mort_all','mort_H1'};
else
    names = {'inc_2019','mort_all'};
end

tblinc = [];
tblmrt = [];


for ic = 1:length(ctrlist.(reg))
    iso3 = ctrlist.(reg){ic};
    
    % Get the data for incidence and mortality
    countryrow = estims(strcmp(estims.iso3,iso3),:);
    mat        = reshape(countryrow{1,3:end},[3,6])';
    
    % Make adjustments in case boundary estimates are same as central (e.g. for
    % MEX)
    inds = find(mat(:,1)==mat(:,2));
    mat(inds,1) = mat(inds,2)*0.95;
    
    inds = find(mat(:,2)==mat(:,3));
    mat(inds,3) = mat(inds,2)*1.05;
    
    data.inc_2019  = mat(1,:)/12;
    data.inc_h1    = mat(2,:)/12;
    data.mort_H1   = mat(4,:)/12;
    data.mort_all  = mat(5,:)/12;
    
    incmrt = [];
    
    for ni = 1:length(names)
        name = names{ni};
        
        [~, out] = get_distribution_fns(data.(name), 'lognorm', 0);
        cdif = 1; count = 0;
        while cdif>1e-3 && count<1e4   % <--- TEMPORARY: fix when finalising
            sam = lognrnd(out(1),out(2),size(RRs_bsline,1),1);
            vec = prctile(sam,[2.5,50,97.5])./data.(name)-1;
            cdif = sum(vec.^2); count = count+1;
            if count>1e4
                error(sprintf('Not providing good samples, iso3: %s',iso3));
            end
        end
        
        tmp1 = sam.*RRs_bsline(:,:,ni);
        tmp2 = sam.*RRs_disrupt(:,:,ni);
        tmp3 = cat(3,tmp1,tmp2);
        % Get annual aggregations
        dims = size(tmp3);
        tmp4 = reshape(tmp3,[dims(1), 12, dims(2)/12, dims(3)]);
        tmp5 = squeeze(sum(tmp4,2));
        % Get percentiles
        outmat_yr(:,:,:,ni,ic)  = prctile(tmp5,[2.5,50,97.5],1);           % 1.Lo/Md/Hi 2.Year 3.Baseline/COVID 4.Inc/Mrt by HIV 5.Country
        
    end
    
    % --- Save the projections under country names, to allow later
    % plotting: e.g. with Plot_projections_Regional
    incmrt = outmat_yr(:,:,:,:,ic);
    dimnames = {'1. Lo/Md/hi'; '2. Year'; '3. Baseline/COVID'; '4. Inc/Mort by HIV'};
    save([dir,'country projections/inc_mrt_',iso3,'.mat'],'incmrt','dimnames');
end


% -------------------------------------------------------------------------
% --- Print the results

if opts.hiv
    hivs  = {'a','pos'};
else
    hivs  = {'a'};
end
scenarios = {'Baseline','COVID'};
measures  = {'inc','mort'};
years     = [2019:2022];

% Reshape outmat_yr so that is has an additional dimension for HIV status
dims = size(outmat_yr);
tmp1 = reshape(outmat_yr,[dims(1:3),length(hivs),dims(4)/length(hivs),dims(5)]);                 % 1.Lo/Md/Hi 2.Year 3.Baseline/COVID 4.HIV 5.Inc/Mrt 6.Country
extr = tmp1(:,1:4,:,:,:,:);

printing = 1;
if printing
    tbl = {}; row = 1;
    for ic = 1:length(ctrlist.(reg))
        for iy = 1:length(years)
            for im = 1:length(measures)
                for is = 1:length(scenarios)
                    for ih = 1:length(hivs)
                        tbl{row,1} = ctrlist.(reg){ic};
                        tbl{row,2} = scenarios{is};
                        tbl{row,3} = hivs{ih};
                        tbl{row,4} = measures{im};
                        tbl{row,5} = years(iy);
                        
                        vals = extr(:,iy,is,ih,im,ic)';
                        tbl{row,6} = vals(2);
                        tbl{row,7} = vals(1);
                        tbl{row,8} = vals(3);
                        row = row+1;
                    end
                end
            end
        end
        %save([iso3, '/extrapolated_outputs.mat'], 'tbl', 'all_pct_yr');
    end
    save(['region_',reg,'/extrapolated_outputs.mat'],'tbl','outmat_yr');
end

