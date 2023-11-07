clear all; 

ctrys = {'AGO','AZE','BGD','BRA','CHN','COL','IDN','KAZ','KEN','KGZ','KHM','LSO',...
    'MEX','MMR','MYS','NPL','PAK','PER','PHL','RUS','THA','TLS','VNM','ZWE'};

load multipliers;
for ii = 1:length(ctrys)
    row = find(strcmp(iso3_disrp,ctrys{ii}));
    ydat(ii,:) = 100*(mul(row,:)-1);
end

figure; fs = 14;
barh(ydat);
set(gca,'YTick',1:length(ctrys),'YTickLabel',ctrys,'fontsize',fs);
xlabel('Percent increase, total subannual to annual data');
legend('2020','2021','2022','Location','East');


return;

tmp = max(abs(ydat),[],2)
inds = find(tmp>5)
ctrys(inds)