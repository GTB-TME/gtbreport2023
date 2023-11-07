% v2: Version to allow better control over whether to re-simulate over a
% new posterior density

looping = 0;
if ~looping
    clear all; iso3 = 'BGD';
end
chck = [iso3,'/projections_raw3.mat'];
if isfile(chck)
    load(chck);
else
    load([iso3,'/projections_raw2.mat']);
end
% load([iso3,'/projections_raw_URb203.mat']);


% -------------------------------------------------------------------------
% --- Load modified vector if available -----------------------------------
chck = [iso3,'/sm_vec.mat'];
% chck = [iso3,'/sm_vec_extended.mat'];

if isfile(chck)
    load(chck);
    vec1 = sm_vec;
else
    vec1 = vec0;
end

% Load old disruption vector
% load(['../../Phase 2/Coding/General2f/',[iso3],'/disruption_vector'],'vec0');
% vec1 = vec0;
% opts.const = 0;



% -------------------------------------------------------------------------
% --- Simulate over new posterior samples if needed -----------------------
new_posterior = 1;
if new_posterior
    nsam = 200;
    ix0 = size(xsto,1)/2;
    dx = round(ix0/nsam);
    xs = xsto(ix0:dx:end,:);
    % Replicate betared
    nrepl = ceil(size(xs,1)/length(betared));
    tmp1 = repmat(betared,nrepl,1)';
    tmp2 = tmp1(:)';
    betared = tmp2(1:size(xs,1));
    
    opts.const = 0;
    [pdif, aux] = simulate_disruptions_fn_v3(xs, vec1, datafreq, notif_dat, betared, prm, ref, agg, sel, tend, lhd, gps, opts);
end

% Create new outputs if needed
if ~exist('inct','var')
    [pdif, aux] = simulate_disruptions_fn_v3(xs, vec1, datafreq, notif_dat, betared, prm, ref, agg, sel, tend, lhd, gps, opts);
end

inct  = aux.inct;
mort  = aux.mort;
inct2 = aux.inct2;
mort2 = aux.mort2;
noti  = aux.noti;

mat = cat(4,inct,mort)*1e5;
[~, out_pct] = annualise(mat,1,true,2);
inca_pct = out_pct(:,:,:,1);
mrta_pct = out_pct(:,:,:,2);
noti_pct = permute(prctile(noti,[2.5,50,97.5],3),[3,1,2,4])*1e5;




% -------------------------------------------------------------------------
% --- Set up x-axis labels ------------------------------------------------

xlbl = {};
yrs = [2019:2024]; count = 1;

if strcmp(datafreq,'monthly')
    mos = {'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'};
    for iy = 1:length(yrs)
        for im = 1:length(mos)
            xlbl{count} = [mos{im}, ' ', num2str(yrs(iy))]; count = count+1;
        end
    end
elseif strcmp(datafreq,'quarterly')
    qts = {'Q1','Q2','Q3','Q4'};
    for iy = 1:length(yrs)
        for iq = 1:length(qts)
            xlbl{count} = [qts{iq}, ' ', num2str(yrs(iy))]; count = count+1;
        end
    end
elseif strcmp(datafreq,'annual')
    for iy = 1:length(yrs)
        xlbl{count} = [num2str(yrs(iy))]; count = count+1;
    end
end


% ---------------------------------------------------------------------
% --- Plot the fit with notifications ---------------------------------

% notif_dat = notif_rate;

ff=figure; set(ff,'Position',[515 259 1135 691]);
subplot(2,2,1);
hold on;
lw = 1.5; fs = 14; ts = 0.1; ms = 24;
cols = linspecer(2);

if strcmp(datafreq,'monthly')
    mat = squeeze(noti_pct(:,:,1,:));
    dt = 1/12;
    ylbl = 'Monthly notifications per 100k';
    skip = 4;
    %xlbl(1:12) = [];
elseif strcmp(datafreq,'quarterly')
    mat = squeeze(notq_pct(:,:,1,:));
    dt = 1/4;
    ylbl = 'Quarterly notifications per 100k';
    skip = 2;
    %xlbl(1:4) = [];
elseif strcmp(datafreq,'annual')
    dims = size(noti);
    tmp1 = reshape(noti,[12,dims(1)/12,dims(2:end)])*1e5;
    tmp2 = squeeze(sum(tmp1,1));
    mat  = permute(prctile(tmp2,[2.5,50,97.5],2),[2,1,3]);
    dt = 1;
    ylbl = 'Annual notifications per 100k';
    skip = 1;
    %xlbl(1:4) = [];
end

for ii = 1:2
    plt = mat(:,:,ii);
    pl2(ii,:) = plot(plt(2,:),'Color',cols(ii,:),'linewidth',lw); hold on;
    jbfill(1:size(plt,2),plt(3,:),plt(1,:),cols(ii,:),'None',1,ts); hold on;
end
pl2(3,:) = plot(1/dt + [1:length(notif_dat)], notif_dat,'.-','Color','g','linewidth',lw,'markersize',ms);
ylabel(ylbl);
xinds = 1:skip:1/dt+length(notif_dat);

xhi = find(~isnan(notif_dat),1,'last');
xlim([1, 1/dt+xhi]);
% ym = [0,max(notif_dat)*1.2]; ylim(ym);
yl = ylim; yl(1) = 0; ylim(yl);
%title([ctry, ' Notifications']);

if contains(iso3,'region')
    title([erase(iso3,'region_'), ' Region Notifications']);
else
    title([iso3, ' Notifications']);
end
legend(pl2([3,1,2],:),'Data','Modelled baseline','Modelled disruption','location','SouthWest','autoupdate','off');

inds = 1:skip:size(plt,2);
set(gca,'fontsize',fs,'XTick',inds,'XTickLabel',xlbl(inds));

subplot(2,2,2);
vecplt = [ones(1,1/dt), vec1];
plot(vecplt,'linewidth',lw);
title('Disruption vector');
xlim([1 length(vecplt)]);
yl = ylim; yl(1) = 0; ylim(yl);
set(gca,'fontsize',fs,'XTick',inds,'XTickLabel',xlbl(inds));


% -------------------------------------------------------------------------
% --- Plot the incidence and mortality projections ------------------------

inct0 = inct; mort0 = mort;

cols = linspecer(3); fs = 14; lw = 1.5;
years = 2019:2024;
tis = {'Incidence','Mortality'};
allmat = cat(4, inca_pct, mrta_pct);
if max(allmat(:))<1
    allmat = allmat*1e5;
end

% Also, get the old estimates for comparison
fname = ['../../Phase 2/Coding/General2f/',iso3, '/projections_raw.mat'];
if isfile(fname)
    load(fname,'inct','mort');
    tmp1 = cat(4, inct, mort)*1e5;
    [~,tmp4] = annualise(tmp1,1,true,2);
    allmat = cat(3,allmat,tmp4(:,:,2,:));
    allmat = allmat(:,:,[3,2],:);
    oldestim = 1;
else
    allmat = allmat(:,:,2,:);
    oldestim = 0;
end
%allmat(:,:,1,:) = [];

if oldestim
    plinds = {1:3, 1:4};
    
    % Plot them
    for ii = 1:2
        subplot(2,2,ii+2); hold on;
        for jj = 1:2
            plt = allmat(:,:,jj,ii)';
            lgg(jj,:) = plot(years(plinds{jj}), plt(2,plinds{jj}), 'linewidth', lw, 'Color', cols(jj,:)); hold on;
            jbfill(years(plinds{jj}), plt(3,plinds{jj}), plt(1,plinds{jj}), cols(jj,:), 'None', 1, 0.1); hold on;
        end
        yl = ylim; yl(1) = 0; ylim(yl);
        set(gca,'fontsize',fs,'XTick',years(plinds{jj}));
        xlim([years(1), 2022]);
        title(tis{ii});
    end
    subplot(2,2,3); ylabel('Rate per 100k population');
    legend(lgg, '2022 report', 'Draft for 2023 report','Location','SouthEast');
else
    plinds = {1:4};
    
    % Plot them
    for ii = 1:2
        subplot(2,2,ii+2); hold on;
        for jj = 1:1
            plt = allmat(:,:,jj,ii)';
            lgg(jj,:) = plot(years(plinds{jj}), plt(2,plinds{jj}), 'linewidth', lw, 'Color', cols(jj,:)); hold on;
            jbfill(years(plinds{jj}), plt(3,plinds{jj}), plt(1,plinds{jj}), cols(jj,:), 'None', 1, 0.1); hold on;
        end
        yl = ylim; yl(1) = 0; ylim(yl);
        set(gca,'fontsize',fs,'XTick',years(plinds{jj}));
        xlim([years(1), 2022]);
        title(tis{ii});
    end
    subplot(2,2,3); ylabel('Rate per 100k population');
    legend(lgg, 'Draft for 2023 report','Location','SouthEast');
end
% load chirp; sound(y,Fs); pause;

% saving = input("\n Save updated disruption vector? 0/1    ");
saving = 1;
if saving
    % Also get annualised incidence, mortality estimates
    sm_vec = vec0;
    inct = inct0; mort = mort0;
    save([iso3,'/projections_smoothedvec'], 'sm_vec', 'noti', 'inct', 'mort', 'inct2', 'mort2', 'xs');
    close;
end

