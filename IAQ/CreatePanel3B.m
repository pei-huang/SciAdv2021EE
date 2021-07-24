function [outputArg1,outputArg2] = CreatePanel3B(inputArg1,inputArg2)
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
    [rows, cols] = size(sim_data);

    for i = 1:rows
        sim_data.efold(i) = EFoldTime(sim_data.C_ss(i),sim_data.a_inf(i),sim_data.a_nat(i),sim_data.P(i),sim_data.C0(i),sim_data.house_vol(i),0.3,sim_data.a_recirc(i),sim_data.eff_filt(i),sim_data.E(i));
    end

    ref = [202 108 95]/255;
    int = [69 183 205]/255;
    opt = [64 177 162]/255;
    color = {ref,int,opt};

    y_2016_efold_recirc = sim_data.year==4&sim_data.scen==1&sim_data.a_recirc~=0;
    y_2016_efold_norec = sim_data.year==4&sim_data.scen==1&sim_data.a_recirc==0;


    i = 1;
    for year = [4 38]
        j = 1;
        for scen = [1 2 3]

            recirc_index = sim_data.year==year&sim_data.scen==scen&sim_data.a_recirc~=0;
            norec_index = sim_data.year==year&sim_data.scen==scen&sim_data.a_recirc==0;

            efold_recirc = sim_data.efold(recirc_index);
            efold_norec = sim_data.efold(norec_index);

            recirc_pct(j,1,i) = mean(efold_recirc);
            recirc_pct(j,2,i) = std(efold_recirc);

            if year==4 && (scen==2 || scen==3)
                recirc_pct(2:3,1,i) = recirc_pct(1,1,i);
                recirc_pct(2:3,2,i) = recirc_pct(1,2,i);
                norec_pct(2:3,1,i) = norec_pct(1,1,i);
                norec_pct(2:3,2,i) = norec_pct(1,2,i);
            else
                recirc_pct(j,1,i) = mean(efold_recirc);
                recirc_pct(j,2,i) = std(efold_recirc);
                norec_pct(j,1,i) = mean(efold_norec);
                norec_pct(j,2,i) = std(efold_recirc);
            end

            j = j + 1;
        end
       i = i + 1;
    end

    recirc_mean = recirc_pct(:,1,2)./recirc_pct(:,1,1); % for mean
    norec_mean = norec_pct(:,1,2)./norec_pct(:,1,1); % for mean

    recirc_std = sqrt((recirc_pct(:,2,2)./recirc_pct(:,1,2)).^2+(recirc_pct(:,2,1)./recirc_pct(:,1,1)).^2);
    norec_std = sqrt((norec_pct(:,2,2)./norec_pct(:,1,2)).^2+(norec_pct(:,2,1)./norec_pct(:,1,1)).^2);

    fig = figure;
    index = 1;

    for scen = [1 2 3]
        s1 = scatter(index-0.05,recirc_mean(scen),'o','filled');
        s1.CData = color{scen};
        hold on
        e1 = errorbar(index-0.05,recirc_mean(scen),recirc_std(scen),recirc_std(scen));
        e1.Color = color{scen};
        s2 = scatter(index+0.05,norec_mean(scen),'^','filled');
        s2.CData = color{scen};
        hold on
        e2 = errorbar(index+0.05,norec_mean(scen),norec_std(scen),norec_std(scen));
        e2.Color = color{scen};
        hold on
        index = index + .3;
    end

    plot([0 4],[1 1],'--','LineWidth',1,'Color',[191 191 191]/255)
    hold on
    h = zeros(2, 1);
    h(1) = scatter(NaN,NaN,'o','filled','MarkerFaceColor',[127 127 127]/255);
    h(2) = scatter(NaN,NaN,'^','filled','MarkerFaceColor',[127 127 127]/255);
    legend(h, 'Recirculation','No recirculation');
    legend boxoff
    hold off
    ylim([0.75 2])
    ylabel('Relative change in response time (t_{2050}/t_{2016})')
    set(gca,'YMinorTick','off')
    set(gca,'TickDir','out');
    xticks([1 1.3 1.6])
    xlim([0.9 1.7])
    yticks([1 1.5 2.0])
    xlabel('Scenarios')
    xticklabels({'Reference','Intermediate EE','Optimistic EE'})
    box on
    title('Panel 3b')
end

