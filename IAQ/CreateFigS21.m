function CreateFigS21()

    load('OutputData\EmissionsBoxPlot.mat')
    
    e_2016 = sim_data{sim_data.year==4,3};
    e_2050 = sim_data{sim_data.year==38,3};
    e_plots = [e_2016, e_2050];
    label = {'2016','2050'};
    
    figure
    boxplot(e_plots,label,'symbol', '')
    ylim([0 3500])
    
    ylabel('Average Indoor Emission Rate (\mug/hr)')
    xlabel('Year')
 
    
end

