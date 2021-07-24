function [outputArg1,outputArg2] = CreatePanel3D(sim_data_CD)
    sim_data = sim_data_CD;

    index_ref = sim_data.scen==1&sim_data.year==38;
    index_opt = sim_data.scen==3&sim_data.year==38;

    E_ref = sim_data.E(index_ref);
    E_opt = sim_data.E(index_opt);
    C_ref = sim_data.C0(index_ref);
    C_opt = sim_data.C0(index_opt);
    C_ss_ref = sim_data.C_ss(index_ref);
    C_ss_opt = sim_data.C_ss(index_opt);
    V_opt = sim_data.house_vol(index_opt);
    a_inf_opt = sim_data.a_inf(index_opt);
    a_nat_opt = sim_data.a_nat(index_opt);
    P_opt = sim_data.P(index_opt);

    E_pct_ref = prctile(E_ref,0:100);
    E_pct_opt = prctile(E_opt,0:100);

    C_out_bins = linspace(1,35,101);

    x = zeros(1,100);
    y = zeros(1,100);

    for i = 1:100 % ambient concentration
        for j = 1:100 % emission deciles
            index_cell_ref = E_ref>=E_pct_ref(j)&E_ref<E_pct_ref(j+1)&C_ref>=C_out_bins(i)&C_ref<C_out_bins(i+1);
            index_cell_opt = E_opt>=E_pct_opt(j)&E_opt<E_pct_opt(j+1)&C_opt>=C_out_bins(i)&C_opt<C_out_bins(i+1);

            indoor_conc_cell_ref = mean(C_ss_ref(index_cell_ref));
            indoor_conc_cell_opt = mean(C_ss_opt(index_cell_opt));

            ratio_cell = indoor_conc_cell_opt/indoor_conc_cell_ref;

            grid_panel_c(i,j) = ratio_cell;

            indoor_contribution = mean(E_opt(index_cell_opt))/mean(V_opt(index_cell_opt));
            outdoor_contribution = (mean(a_inf_opt(index_cell_opt))*mean(P_opt(index_cell_opt))+mean(a_nat_opt(index_cell_opt)))*mean(C_out_bins(i)+C_out_bins(i+1));

            grid_panel_d(i,j) = indoor_contribution/outdoor_contribution;

            grid_panel_e(i,j) = indoor_conc_cell_opt/mean(C_out_bins(i)+C_out_bins(i+1));

            x(j) = (j+(j+1))/2-1;      
        end
        y(i) = (C_out_bins(i)+C_out_bins(i+1))/2;
    end

    % Create Z for ref

    [X,Y] = meshgrid(x,y) ;
    
    figure
    surf(X,Y,log10(grid_panel_e));
    view([0 90])
    xlim([0 100])
    ylim([0 35])
    ylabel('Ambient Outdoor PM_{2.5} Concentration (ug/m^{3})')
    xlabel('Indoor Emission Percentile')

    box on
    set(gca,'YMinorTick','off')
    set(gca,'TickDir','out');
    colormap();
    rwb = colormap(bluewhitered());
    %c = colorbar('Ticks',[-1 0 1 4],'TickLabels',{'0','1','2','5'});
    c = colorbar();
    c.Label.String = 'Indoor/Outdoor Ratio (Log Scale)';



end

