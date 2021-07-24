function [outputArg1,outputArg2] = CreateFigS19()
    
    T_bsei = readtable('InputData\BSEIvalues.csv'); % This is taken from the Yale-NEMS data output files
    
    ref = [202 108 95]/255;
    int = [69 183 205]/255;
    opt = [64 177 162]/255;
    color = {ref,int,opt};
    
    figure 
    subplot(2,1,1)
    plot(T_bsei.Year,T_bsei.bsei_ref_cool_old,'-s','Color',color{1});
    hold on
    plot(T_bsei.Year,T_bsei.bsei_ref_cool_new,'-d','Color',color{1});
    hold on
    plot(T_bsei.Year,T_bsei.bsei_int_cool_old,'-s','Color',color{2});
    hold on
    plot(T_bsei.Year,T_bsei.bsei_int_cool_new,'-d','Color',color{2});
    hold on
    plot(T_bsei.Year,T_bsei.bsei_opt_cool_old,'-s','Color',color{3});
    hold on
    plot(T_bsei.Year,T_bsei.bsei_opt_cool_new,'-d','Color',color{3});
    hold off
    ylabel('BSEI_c')
    ylim([0 1.2])
    
    subplot(2,1,2)
    plot(T_bsei.Year,T_bsei.bsei_ref_heat_old,'-s','Color',color{1});
    hold on
    plot(T_bsei.Year,T_bsei.bsei_ref_heat_new,'-d','Color',color{1});
    hold on
    plot(T_bsei.Year,T_bsei.bsei_int_heat_old,'-s','Color',color{2});
    hold on
    plot(T_bsei.Year,T_bsei.bsei_int_heat_new,'-d','Color',color{2});
    hold on
    plot(T_bsei.Year,T_bsei.bsei_opt_heat_old,'-s','Color',color{3});
    hold on
    plot(T_bsei.Year,T_bsei.bsei_opt_heat_new,'-d','Color',color{3});
    hold off
    ylabel('BSEI_h')
    ylim([0 1.2])
    xlabel('Year')
    
end

