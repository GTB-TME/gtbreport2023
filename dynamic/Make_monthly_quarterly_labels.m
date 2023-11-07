clear all; 

yrs = [2019:2024]; count = 1;

mos = {'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'};
for iy = 1:length(yrs)
    for im = 1:length(mos)
        xlbl_mo{count} = [mos{im}, ' ', num2str(yrs(iy))]; count = count+1;
    end
end
    
qts = {'Q1','Q2','Q3','Q4'};
for iy = 1:length(yrs)
    for iq = 1:length(qts)
        xlbl_qt{count} = [qts{iq}, ' ', num2str(yrs(iy))]; count = count+1;
    end
end
    
for iy = 1:length(yrs)
    xlbl_annu{count} = [num2str(yrs(iy))]; count = count+1;
end

save xlabels xlbl_mo xlbl_qt xlbl_annu;