% 1819: Version to get data from both 2018 and 2019, for purpose of
% calculating trends

clear all; 

C = readtable('tb_2022-06-27.csv');

colnames = C.Properties.VariableNames;

% --- Filter on >2019 year
rows    = find(C.year>=2019);
C1      = C(rows,:);

% --- Pull out the relevant notification data
notifcols  = [find(strcmp(colnames,'iso3')), find(strcmp(colnames,'ret_nrel')), find(strcmp(colnames,'c_newinc'))];
C2         = C1(:,notifcols);
C2.allnoti = C2.ret_nrel + C2.c_newinc;

% --- Prepare for saving
notifs_18 = C2(1:2:end,[1,end]);
notifs_19 = C2(2:2:end,[1,end]);

% save notif_data_1819 notifs_18 notifs_19;



% --- Also, get information for numbers diagnosed with HIV
col = [find(strcmp(colnames,'iso3')), find(strcmp(colnames,'ret_nrel')), find(strcmp(colnames,'c_newinc')), find(strcmp(colnames,'newrel_hivpos'))];
C4 = C1(:,col);
C4.allnoti = C4.ret_nrel + C4.c_newinc;

iso3s   = C4.iso3(1:3:end);
allnoti = reshape(C4.allnoti,3,length(iso3s));
hivpos  = reshape(C4.newrel_hivpos,3,length(iso3s));

% Filter on those with high HIV burden as of 2019
rat = hivpos(1,:)./allnoti(1,:);
inds = find(rat)>0.1;

iso3s = iso3s(inds);
allnoti = allnoti(:,inds);
hivpos  = hivpos(:,inds);

ratnoti = allnoti([2,3],:)./allnoti(1,:);
rathivp = hivpos([2,3],:)./hivpos(1,:);



figure; fs = 14;

hold on;
pl(1,:) = plot(ratnoti(1,:),rathivp(1,:),'.','markersize',14); 
pl(2,:) = plot(ratnoti(2,:),rathivp(2,:),'.','markersize',14); 
yl = ylim; yl(1) = 0; ylim(yl); xl = xlim; xl(1) = 0; xlim(xl);

xlabel('All notifications (HIV+ve and HIV-ve), relative to 2019');    
ylabel('HIV+ve notifications, relative to 2019'); 
set(gca,'fontsize',fs);

legend(pl,'2020 vs 2019','2021 vs 2019','autoupdate','off');
line([0, min(yl(2), xl(2))], [0, min(yl(2), xl(2))], 'linestyle', '--');

inds = find(ratnoti(2,:)>1.4);
iso3s(inds)

% gr = @(iso3) find(strcmp(iso3s,iso3));
% rows = [gr('BRA'), gr('UGA'), gr('KEN'), gr('ZAF')];
% figure; plot(ratnoti(1,rows),rathivp(1,rows),'.'); 
% yl = ylim; yl(1) = 0; ylim(yl); xl = xlim; xl(1) = 0; xlim(xl);