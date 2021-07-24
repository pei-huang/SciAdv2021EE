function CreateFigS22()

    load('OutputData\InfiltrationBoxPlot.mat')
    
    a_2016 = sim_data{sim_data.year==4&sim_data.scen==1,4};
    a_2050_ref = sim_data{sim_data.year==38&sim_data.scen==1,4};
    a_2050_int = sim_data{sim_data.year==38&sim_data.scen==2,4};
    a_2050_opt = sim_data{sim_data.year==38&sim_data.scen==3,4};
    
    a_plot = [a_2016,a_2050_ref,a_2050_int,a_2050_opt];
    label = {'2016','2050 Ref','2050 Int','2050 Opt'};
    
    figure
    boxplot(a_plot,label,'symbol', '')
    xlabel('Scenarios')
    ylabel('Infiltration Rate (1/hr)')
    

end

