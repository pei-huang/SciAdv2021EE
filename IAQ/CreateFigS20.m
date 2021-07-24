function CreateFigS20()
    
    homeprojec = readtable('InputData\OldVsNew.csv');
    % This was calculated from the Yale-NEMS files, minor differences between the
    % scenarios.
    hvac_pct = [0.549222138	0.550877783	0.552486123	0.554676732	0.556989523	0.559390429	0.561800393	0.564214292	0.566625683	0.569033881	0.571420689	0.573772538	0.576079554	0.578329191	0.580509471	0.582617536	0.584670381	0.586680781	0.588635917	0.590523415	0.592368795	0.594192993	0.595994228	0.597762596	0.599493467	0.601191193	0.602853338	0.604481374	0.606074489	0.607628923	0.609151549	0.610646811	0.612113871	0.613551188	0.614955266	0.616325165	0.617661215	0.618967222];
      
    gray = [128,128,128]/255;
    
    figure
    plot(homeprojec.Year,homeprojec.Pct_Old,'-s','Color',gray)
    hold on
    plot(homeprojec.Year,homeprojec.Pct_New,'-d','Color',gray)
    hold on
    plot(homeprojec.Year,hvac_pct,'Color','red')
    
    xlabel('Year')
    ylabel('Fraction of housing stock')
    
    legend({'Exisiting homes','New homes','w/ HVAC'})
    
end

