%% Load in literature distributions.
[homeprojec,std_up,std_low,std_C0,pd_C0,pd_P,home_volume,k_dep] = LiteratureDistributions();

%% Choose model years, EE scenarios, and simulation runs.
model_years = [4 38]; % 1 is 2013, 4 is 2016, 38 is 2050
model_scen = [1 2 3]; % 1 is reference, 2 is intermediate, 3 is optimisitic
model_runs = 10000000; % Number of homes to simulate

%% Code structure for running the Monte Carlo simulation.
%model_vars = {'C_ss','year','scen','home_type','a_inf','a_nat','house_vol','household_size','E','P','C0','f_hvac','a_recirc','eff_filt','efold'};
model_vars = {'C_ss','year','scen','home_type','a_inf','a_nat','house_vol','household_size','E','P','C0','f_hvac','a_recirc','eff_filt'};

p = 1; % Loop progress for putting in command window.

for year = model_years 
    for scen = model_scen 
        sim_mat = zeros(model_runs,length(model_vars));
        
        if year == 4 && (scen == 2 || scen == 3)
           continue % skip the int and opt scenario for 2016 as they are the same as reference.
        end
            for i = 1:model_runs
                % Randomly generate the values required to run the model
                % Decide whether it will be an old or new home
                [home_type] = DetermineHome(homeprojec,year);
                % Then determine the the infiltration rate, square footage, natural
                % ventilation, household size etc. 
                [a_inf,a_nat] = Ventilation2(year,home_type,rel_a_inf,scen);
                % Determine house size
                [house_vol] = home_volume(year);
                % Determine household size
                [household_size] = HouseholdSize();    
                % Then determine the indoor emission rate
                [E] = Emissions(household_size,year);                   
                % Then determine the penetration factor
                [P] = random(pd_P);
                % Then determine the outdoor ambient concentration 
                [C0] = random(pd_C0);
                if C0 < 1
                    C0 = 1; % Lower limit
                end
                % Determine HVAC recirculation terms
                [f_hvac,a_recirc,eff_filt] = HVAC(home_type,year,scen);

                % Then calculate the steady state concentration
                C_ss = ((a_inf*P+a_nat)*C0+E/house_vol)/(a_inf+a_nat+k_dep+f_hvac*a_recirc*eff_filt);

                % Finally calculate the efolding time
                efold = EFoldTime(C_ss,a_inf,a_nat,P,C0,house_vol,k_dep,a_recirc,eff_filt,E);
                
                % Store the data in a matrix
                sim_mat(i,:) = [C_ss,year,scen,home_type,a_inf,a_nat,house_vol,household_size,E,P,C0,f_hvac,a_recirc,eff_filt,efold];
            end
        sim_data = array2table(sim_mat,'VariableNames',model_vars);
        
        % Save data in parts to keep file sizes down, later concatenate
        % them into a single unified file
        filename_i = strcat(num2str(p),'.mat');
        save(filename_i,'sim_data','-v7.3')   
        % Print model progress into command window (-2 in formula below only if running year 4)
        fprintf('Progress: %.2f \n',p/(length(model_years)*length(model_scen)-2)*100);
        p = p + 1;    
        clear sim_data sim_mat
    end
end

%% Use the pregenerated data files to construct the figure panels from the main text.
% Some differences between these figure panels and those in the main text
% may exist as final graphing was done in Igor Pro, not Matlab. 
% These do not require you to run the model above and instead uses
% pregenerated files from the model output already.
CreatePanel3A() % Panels A and B have pregenerated files to help with the plotting process
CreatePanel3B()

sim_data_CD = LoadPanelCDdata();
CreatePanel3C(sim_data_CD)
CreatePanel3D(sim_data_CD)

%% Create SI figure panels. 
CreateFigS19()
CreateFigS20()
CreateFigS21()
CreateFigS22()
CreateFigS23()
CreateFigS25()