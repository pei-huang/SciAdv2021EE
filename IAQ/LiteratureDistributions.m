function [homeprojec,std_up,std_low,std_C0,pd_C0,pd_P,home_volume,rel_a_inf,k_dep] = LiteratureDistributions()

    homeprojec = readtable('Output\OldVsNew.csv');
    
    % For outdoor conc from EPA, approximate as normal distribution
    std_up = (9.615-7.617)/1.28;
    std_low = (7.617-5.196)/1.28;
    std_C0 = (std_up+std_low)/2;
    
    %pd_C0 = makedist('Normal','mu',7.62,'sigma',std_C0); % Use this for panels a and b
    pd_C0 = makedist('Uniform','lower',1,'upper',35); % Use this for panels c and d
    % For penetration factor
    pd_P = makedist('Uniform','lower',0.8,'upper',0.9);

    load('home_vol.mat')

    load('a_rel_scen.mat');
    rel_a_inf = a_rel_scen;
    clear a_rel_scen
    
    k_dep = 0.3; % 1/hr 
end