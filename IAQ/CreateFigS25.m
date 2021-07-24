function CreateFigS25()
    load('OutputData\FinalRun1.mat')
    sim_all = sim_data;
    load('OutputData\FinalRun2.mat')
    sim_all = [sim_all;sim_data];
    load('OutputData\FinalRun3.mat')
    sim_all = [sim_all;sim_data];

    sim_2016 = sim_all(1:100000,:);
    sim_2050_ref = sim_all(1215654:1215654+100000-1,:);
    sim_2050_int = sim_all(sim_all.year==38&sim_all.scen==2,:);
    sim_2050_opt = sim_all(sim_all.year==38&sim_all.scen==3,:);
    sim_data = [sim_2016;sim_2050_ref;sim_2050_int;sim_2050_opt];

    scale_to_run = [110 100 90 80 70 60 50 40];
    load('InputData\home_vol.mat');
    load('InputData\relach.mat')
    scale_str = [1.1 1.0 0.9 0.8 0.7 0.6 0.5 0.4];
    
    for i = 1:length(scale_to_run)
        scale_i = scale_to_run(i);
        sim_2050_ref_new = calcNewC(sim_2050_ref,scale_i,V(:,38,1));
        sim_2050_int_new = calcNewC(sim_2050_int,scale_i,V(:,38,2));
        sim_2050_opt_new = calcNewC(sim_2050_opt,scale_i,V(:,38,2));

        ratio_int_Css(i) = mean(sim_2050_int_new.C_ss(sim_2050_int_new.k>=5&sim_2050_int_new.a_recirc==0))/mean(sim_2050_ref_new.C_ss(sim_2050_ref_new.k>=5&sim_2050_ref_new.a_recirc==0));
        ratio_opt_Css(i) = mean(sim_2050_opt_new.C_ss(sim_2050_opt_new.k>=5&sim_2050_opt_new.a_recirc==0))/mean(sim_2050_ref_new.C_ss(sim_2050_ref_new.k>=5&sim_2050_ref_new.a_recirc==0));

        ratio_int_ainf(i) = mean(sim_2050_int_new.a_inf)/mean(sim_2050_ref_new.a_inf);
        ratio_opt_ainf(i) = mean(sim_2050_opt_new.a_inf)/mean(sim_2050_ref_new.a_inf);

        mean_ainf(1,i) = mean(sim_2050_ref_new.a_inf);
        std_ainf(1,i) = std(sim_2050_ref_new.a_inf);

        mean_ainf(2,i) = mean(sim_2050_int_new.a_inf);
        std_ainf(2,i) = std(sim_2050_int_new.a_inf);

        mean_ainf(3,i) = mean(sim_2050_opt_new.a_inf);
        std_ainf(3,i) = std(sim_2050_opt_new.a_inf);

        sim_data = [sim_2050_ref_new;sim_2050_int_new;sim_2050_opt_new];
        year = 38;

        output_C = [];
        output_IO = [];
        output_AINF = [];
        output_Beta = [];

        for scen = 1:3
            scen_k = [];
            scenIO_k = [];
            scenAINF_k = [];
            scen_Beta = [];
            for kk = 1:10
                row_k = [];
                rowIO_k = [];
                row_Beta = [];

                index_i_recric = sim_data.year==year&sim_data.scen==scen&sim_data.a_recirc~=0&sim_data.k==kk;
                index_i_norec = sim_data.year==year&sim_data.scen==scen&sim_data.a_recirc==0&sim_data.k==kk;

                % Regular stuff
                C_ss_recirc_avg = mean(sim_data.C_ss(index_i_recric));
                std_recirc = std(sim_data.C_ss(index_i_recric));
                C_ss_rorec_avg = mean(sim_data.C_ss(index_i_norec));
                std_norec = std(sim_data.C_ss(index_i_norec));


                % Average infiltration rate
                a_inf_recirc_avg = mean(sim_data.a_inf(index_i_recric));
                a_inf_norec_avg = mean(sim_data.a_inf(index_i_norec));


                % For fraction of indoor vs outdoors
                vol = home_volume(year);
                sim_in_out_recirc = sim_data(index_i_recric,:);
                sim_in_out_norecirc = sim_data(index_i_norec,:);

                indoor_conc_recirc = mean(sim_in_out_recirc.E)/vol;
                indoor_conc_norecirc = mean(sim_in_out_norecirc.E)/vol;

                outdoor_conc_recirc = mean((sim_in_out_recirc.a_inf+sim_in_out_recirc.P+sim_in_out_recirc.a_nat).*sim_in_out_recirc.C0);
                outdoor_conc_norecirc = mean((sim_in_out_norecirc.a_inf+sim_in_out_norecirc.P+sim_in_out_norecirc.a_nat).*sim_in_out_norecirc.C0);

                f_recirc_indoor = indoor_conc_recirc/(indoor_conc_recirc+outdoor_conc_recirc);
                f_norecirc_indoor = indoor_conc_norecirc/(indoor_conc_norecirc+outdoor_conc_norecirc);

                f_recirc_outdoor = 1 - f_recirc_indoor;
                f_norecirc_outdoor = 1 - f_norecirc_indoor;


                % For beta value
                F_i_recirc = (C_ss_recirc_avg - indoor_conc_recirc)/mean(sim_in_out_recirc.C0);
                F_i_norecirc = (C_ss_rorec_avg - indoor_conc_norecirc)/mean(sim_in_out_norecirc.C0);            

                row_k = [C_ss_recirc_avg,std_recirc,C_ss_rorec_avg,std_norec];
                rowIO_k = [f_recirc_indoor,f_recirc_outdoor,f_norecirc_indoor,f_norecirc_outdoor];
                rowAINF_k = [a_inf_recirc_avg,a_inf_norec_avg];
                row_Beta = [F_i_recirc,F_i_norecirc];

                scen_k = [scen_k;row_k];
                scenIO_k = [scenIO_k;rowIO_k];
                scenAINF_k = [scenAINF_k;rowAINF_k];
                scen_Beta = [scen_Beta;row_Beta];
            end
            output_C = [output_C,scen_k];
            output_IO = [output_IO,scenIO_k];
            output_AINF = [output_AINF,scenAINF_k];
            output_Beta = [output_Beta,scen_Beta];
        end
% Uncomment this section if you want to create the files used for health
% effect estimates 
%         emission_dec = (5:10:95)';
%         conc_table = array2table([emission_dec,output_C],'VariableNames',{'EmissionDecile','RefWithRecircMean','RefWithRecircStd','RefNoRecircMean','RefNoRecircStd','IntEEWithRecircMean','IntEEWithRecircStd','IntEENoRecircMean','IntEENoRecircStd','OptEEWithRecircMean','OptEEWithRecircStd','OptEENoRecircMean','OptEENoRecircStd'});
%         io_table = array2table([emission_dec,output_IO],'VariableNames',{'EmissionDecile','RefWithRecircIndoorFrac','RefWithRecircOutdoorFrac','RefNoRecircIndoorFrac','RefNoRecircOutdoorFrac','IntEEWithRecircIndoorFrac','IntEEWithRecircOutdoorFrac','IntEENoRecircIndoorFrac','IntEENoRecircOutdoorFrac','OptEEWithRecircIndoorFrac','OptEEWithRecircOutdoorFrac','OptEENoRecircIndoorFrac','OptEENoRecircOutdoorFrac'});
%         ainf_table = array2table([emission_dec,output_AINF],'VariableNames',{'EmissionDecile','RefWithRecirc','RefWithoutRecirc','IntEEWithRecirc','IntEEWithoutRecirc','OptEEWithRecirc','OptEEWithoutRecirc'});
%         
%         % Write Excel file before going to the next year
%         %filename = strcat('SensAnalysis\Year',num2str(year),'Scale',num2str(scale_to_run(i)),'PM25Conc.csv');
%         %writetable(conc_table,filename)  
%             
%         % Write Excel file before going to the next year
%         %filename = strcat('SensAnalysis\Year',num2str(year),'Scale',num2str(scale_to_run(i)),'IOpct.csv');
%         %writetable(io_table,filename)   
%         
%         % Write Excel file
%         filename = strcat('SensAnalysis\Year',num2str(year),'Scale',num2str(scale_to_run(i)),'Ainf.csv');
%         writetable(ainf_table,filename)  
% 
%         beta_table = array2table([emission_dec,output_Beta],'VariableNames',{'EmissionDecile','RefWithRecirc','RefWithoutRecirc','IntEEWithRecirc','IntEEWithoutRecirc','OptEEWithRecirc','OptEEWithoutRecirc'});
% 
%         % Write Excel file
%         filename = strcat('BetaCoefficientAnalysisSensAnalysis\Year',num2str(year),num2str(scale_to_run(i)),'Finf.csv');
%         writetable(beta_table,filename)    
    
    end

    ref = [202 108 95]/255;
    int = [69 183 205]/255;
    opt = [64 177 162]/255;
    color = {ref,int,opt};




    figure
    subplot(2,1,1)
    % summary of infiltration spread up top
    offset = 0.005;
    for i = 1:3
        errorbar(scale_to_run/100+offset,mean_ainf(i,:),std_ainf(i,:),'Color',color{i},'LineWidth',2)
        offset = offset-0.005;
        %xlim([0.55 1.15])
        set ( gca, 'xdir', 'reverse' )
        hold on
    end
    xlabel('E_{inf} vs. BSEI weighting factor (E_{inf} \propto X BSEI)')
    ylabel('Avg a_{inf} [hr^{-1}]')
    legend({'Ref','Int','Opt'},'Location','northwest')
    hold off

    subplot(2,1,2)

    yyaxis left
    plot(scale_to_run/100,ratio_int_Css,'Color',color{2},'LineStyle','-','LineWidth',2)
    hold on
    yyaxis right
    plot(scale_to_run/100,ratio_int_ainf,'Color',color{2},'LineStyle','--','LineWidth',2)
    hold on
    yyaxis left
    ylabel({'C_{ss, int/opt}/C_{ss, ref} for 2050';'for top 50% indoor emissions w/o recirc'})
    plot(scale_to_run/100,ratio_opt_Css,'Color',color{3},'LineStyle','-','LineWidth',2)
    hold on
    yyaxis right
    ylabel({'a_{inf, int/opt}/a_{inf, ref} for 2050';'for top 50% indoor emissions w/o recirc'})
    plot(scale_to_run/100,ratio_opt_ainf,'Color',color{3},'LineStyle','--','LineWidth',2)
    xlabel('E_{inf} vs. BSEI weighting factor (E_{inf} \propto X BSEI)')
    %xlim([0.55 1.15])
    yyaxis left
    %ylim([0.99 1.21])
    set ( gca, 'xdir', 'reverse' )

    legend({'Int/ref','Opt/ref'},'Location','northwest')
end

