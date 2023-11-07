% When calibrating to pre-pandemic conditions, modelled uncertainty will
% generally be less than uncertainty in the data, because of the
% mechanistic links that the model makes between the different data sources
% (for example, between incidence and mortality). To avoid discontinuities, 
% this code expands modelled uncertainty to match WHO estimates used for
% pre-pandemic calibration.

looping  = 1;
if ~looping
    clearvars -except looping; 
    iso3 = 'PAK';
end

chck = [iso3,'/Model_setup_popupdated.mat'];
if isfile(chck)
    load(chck);
else
    load([iso3,'/Model_setup.mat']);
end
load([iso3,'/projections_smoothedvec']);
% load([iso3,'/projections_URaggregated']);
% load([ctry,'/projections_raw.mat']);

plotting = 0;
printing = 1;


% -- Get the data
alldat = [data.inc_2019; data.inc_h1; data.mort_all; data.mort_H1]/12;
% if ~isempty(find(isnan(alldat)))
%    error('Some data missing');
% end

% --- Get the model simulations
tmp1 = squeeze(sum(inct2(:,[2,3],:,:),2));
tmp2 = squeeze(mort2(:,2,:,:));

allsim = cat(4,inct,tmp1,mort,tmp2);

if ~opts.hiv
    alldat = alldat([1,3],:);
    allsim = allsim(:,:,:,[1,3]);
end
if strcmp(iso3,'PRK')
    alldat = [data.inc_2019; data.mort_H0]/12;
end

% -------------------------------------------------------------------------
% --- Now sample from the data to get initial conditions
for iz = 1:size(alldat,1)
    dat = alldat(iz,:);
    [~, out] = get_distribution_fns(dat, 'lognorm', 0);
    cdif = 1;
    while cdif>1e-2
        sam = lognrnd(out(1),out(2),size(xs,1),1);
        vec = prctile(sam,[2.5,50,97.5])./dat-1;
        cdif = sum(vec.^2);
    end
    if ~looping
        fprintf('Agreement with WHO estimates:\n'); prctile(sam,[2.5,50,97.5])./dat
    end
    sam_who(:,iz) = sam;
end


% -------------------------------------------------------------------------
% --- Estimate regression coefficients for the model-based outcomes, based
% only on incidence or mortality

for iz = 1:size(allsim,4)                                                  % Incidence and mortality
    for ii = 1:size(allsim,3)                                              % Baseline vs disruption scenario
        mat = allsim(:,:,ii,iz)'*1e5;
        X = [mat(:,1), xs];
        
        beta = [];
        for ic = 2:size(mat,2)
            Y = mat(:,ic);
            beta(:,ic) = mvregress(X,Y);
        end
        beta(1,1) = 1;
        
        % Make extrapolations
        X = [sam_who(:,iz), xs];
        
        mat1 = X*beta;
        timeser(:,:,ii,iz) = mat1;
        all_pct_mo(:,:,ii,iz) = prctile(mat1,[2.5,50,97.5],1);             % Dims: 1.Lo/Md/Hi, 2.Month, 3.Baseline/disruption, 4.Inc all/Inc HIV/Mort all/Mort HIV
    
        % Also get annualised rates
        [~,all_pct_yr(:,:,ii,iz)] = annualise(mat1,2,1,1);                 % Dims: 1.Lo/Md/Hi, 2.Year, 3.Baseline/disruption, 4.Inc all/Inc HIV/Mort all/Mort HIV
    end
    betasto(:,:,iz) = beta;
end



% -------------------------------------------------------------------------
% --- Plot the results

if plotting
    
    figure; hold on;
    tp = 0.1; fs = 14; lw = 1.5;
    cols = linspecer(3);
    xpts = (1:size(all_pct_mo,2))+1;
    
    if ~opts.hiv
        nr = 1; nc = 2; inds = [1,2];
    else
        nr = 2; nc = 2; inds = [1,3,2,4];
    end
    
    for iz = 1:size(all_pct_mo,4)
        subplot(nr,nc,inds(iz));
        for ii = 1:size(all_pct_mo,3)
            plt = all_pct_mo(:,:,ii,iz);
            lg(ii,:) = plot(xpts, plt(2,:),'Color',cols(ii,:),'linewidth',lw); hold on;
            jbfill(xpts,plt(3,:),plt(1,:),cols(ii,:),'None',1,tp); hold on;
        end
        dat = alldat(iz,:);
        difdat = diff(dat);
        lg(ii+1,:) = errorbar(1,dat(2),difdat(1),difdat(2),'linewidth',lw,'Color',cols(ii+1,:));
        plot(1,dat(2),'.','markersize',24,'Color',cols(ii+1,:))
        
        yl = ylim; yl(1) = 0; ylim(yl);
        xl = xlim; xl(2) = xpts(end)+10; xlim(xl);
        
        xlabel('Months from Jan 2019');
        ylabel('Monthly rate per 100k population');
        set(gca,'fontsize',fs);
        if iz == 1
            legend(lg,'Model posterior-based outputs','Statistically extrapolated','WHO uncertainty ranges, 2019','Location','SouthEast');
        end
    end
end


% -------------------------------------------------------------------------
% --- Save the results

if opts.hiv
    hivs  = {'a','pos'};
else
    hivs  = {'a'};
end
scenarios = {'Baseline','COVID'};
measures  = {'inc','mort'};
years     = [2019:2022];

% Reshape all_pct_yr so that it has an additional dimension for HIV status
dims = size(all_pct_yr);
extr = reshape(all_pct_yr, [dims(1:end-1) length(hivs) dims(end)/length(hivs)]);  % Dims: 1.Lo/Md/Hi 2.Year 3.Scenario 4.HIV status 5.Incd/Mort
extr = extr(:,1:4,:,:,:);

if printing
    tbl = {}; row = 1;
    for iy = 1:length(years)
        for im = 1:length(measures)
            for is = 1:length(scenarios)
                for ih = 1:length(hivs)
                    tbl{row,1} = iso3;
                    tbl{row,2} = scenarios{is};
                    tbl{row,3} = hivs{ih};
                    tbl{row,4} = measures{im};
                    tbl{row,5} = years(iy);
                    
                    vals = extr(:,iy,is,ih,im)';
                    tbl{row,6} = vals(2);
                    tbl{row,7} = vals(1);
                    tbl{row,8} = vals(3);
                    row = row+1;
                end
            end
        end
    end
    save([iso3, '/extrapolated_outputs.mat'], 'tbl', 'all_pct_yr');
end
