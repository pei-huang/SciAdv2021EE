function CreatePanel3A()
% Makes Figure Panel 3A by reading data found in the "Output" folder
    data_2016 = csvread('OutputData\Year4.csv');
    data_2050 = csvread('OutputData\Year38.csv');
    ratios = data_2050./data_2016;
    X1 = 1:10;
    X2 = 12:21;
    ref = [202 108 95]/255;
    int = [69 183 205]/255;
    opt = [64 177 162]/255;
    color = {ref,int,opt};
    
    % Figures in paper were made in Igor Pro, this is an approximation
    % using Matlab.
    figure
    l = 1;
    for i = [1 5 9]
        plot(X1,ratios(:,i),'-o','Color',color{l},'MarkerFaceColor',color{l},'LineWidth',2.5)
        hold on 
        plot(X2,ratios(:,i+2),'-^','Color',color{l},'MarkerFaceColor',color{l},'LineWidth',2.5)
        l = l + 1;
        hold on
    end
    plot([1 10],[1 1],'--','LineWidth',1.5,'Color',[191 191 191]/255)
    hold on
    plot([12 21],[1 1],'--','LineWidth',1.5,'Color',[191 191 191]/255)
    ylabel('Relative change in indoor PM_{2.5} concentration (C_{2050}/C_{2016})')
    xlabel('Indoor Emission Percentile')
    h = zeros(3, 1);
    h(1) = plot(NaN,NaN,'-','Color',color{1},'LineWidth',2.5);
    h(2) = plot(NaN,NaN,'-','Color',color{2},'LineWidth',2.5);
    h(3) = plot(NaN,NaN,'-','Color',color{3},'LineWidth',2.5);
    legend(h, 'Reference','Intermediate EE','Optimistic EE','Orientation','horizontal')
    legend box off
    ylim([0.75 1.25])
    xlim([0 22])
    xticks([X1 X2])
    xticklabels({'5','15','25','35','45','55','65','75','85','95','5','15','25','35','45','55','65','75','85','95'})
    yticks([0.8 0.9 1.0 1.1 1.2])
    set(gca,'TickDir','out');
    txt1 = {'Recirculation'};
    text(5.5,1.15,txt1,'HorizontalAlignment','center')
    txt2 = {'No recirculation'};
    text(16.5,1.15,txt2,'HorizontalAlignment','center')
    title('Panel 3A')
end

