function [data_t] = calcNewC(data_t,scale_factor,V_i)
%UNTITLED2 Summary of this function goes here
%   Detailed explanation goes here
    
    k_dep = 0.3;
    P = data_t.P;
    a_inf = zeros(length(P),1);
    for i = 1:length(P)
       home_i = data_t.home_type(i); 
       scale_old = V_i(home_i);
       scale_new = ((100-scale_factor)/100*(1-scale_old))+scale_old;

       if (data_t.a_inf(i)/scale_old)<0.1
           a_inf(i,1) = 0.1;
       else
           a_inf(i,1) = data_t.a_inf(i)*scale_new/scale_old;
       end
    end
    
    
    a_nat = data_t.a_nat;
    C0 = data_t.C0;
    E = data_t.E;
    house_vol = data_t.house_vol;
    f_hvac = data_t.f_hvac;
    a_recirc = data_t.a_recirc;
    eff_filt = data_t.eff_filt;

    C_ss_new = ((a_inf.*P+a_nat).*C0+E./house_vol)./(a_inf+a_nat+k_dep+f_hvac.*a_recirc.*eff_filt);

    data_t.C_ss = C_ss_new;
    data_t.a_inf = a_inf;

end

