% Perform country calibrations in batches

tic
clear all; close all;
iso3 = 'THA'
Setup_model;
Get_calibrations;
close all;
toc

tic
clear all; 
iso3 = 'PRK'
Setup_model;
Get_calibrations;
close all;
toc

