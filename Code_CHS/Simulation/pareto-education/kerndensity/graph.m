clear all;
cd out;
load plog.out;
cd ../Figures;
path(path,'../');
set(0,'DefaultAxesFontName','time');

figure;
h = axes('Position',[0 0 1 1],'Visible','off');
axes('Position',[0.15 0.23 0.7 0.67]);
plot(plog(:,1),plog(:,2),plog(:,1),plog(:,3),'--','LineWidth',3);
str(1) = {'Figure'};
str(2) = {'Densities of Log Ratio of Early to Late Investments under Different Weighting'};
title(str,'FontSize',14);
clear str
legend('Education','Crime');
xlabel('Ratio Early to Late Investment');
print -dpsc fig
clear str

cd ..
