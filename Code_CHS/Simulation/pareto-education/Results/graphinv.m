clear all;
load jinv11.txt;
load jinv21.txt;
set(0,'DefaultAxesFontName','time');

figure;
subplot(1,2,1);
surf(jinv11(2:12,1),jinv11(1,2:12),jinv11(2:12,2:12));
str(1) = {'Figure'};
str(2) = {'Optimal Early Investments by Child Cognitive and Noncognitive Skills'};
title(str,'FontSize',14);
%axis([-1 1 -1 1 0.9 1.3]);
clear str
xlabel('Child Initial Noncognitive Skill')
ylabel('Child Initial Cognitive Skill')

subplot(1,2,2);
surf(jinv21(2:12,1),jinv21(1,2:12),jinv21(2:12,2:12));
str(1) = {'Figure'};
str(2) = {'Optimal Late Investments by Child Cognitive and Noncognitive Skills'};
title(str,'FontSize',14);
%axis([-1 1 -1 1 0.9 1.3]);
colormap('gray');
clear str;
xlabel('Child Initial Noncognitive Skill')
ylabel('Child Initial Cognitive Skill')
clear all;

load jinv12.txt;
load jinv22.txt;
set(0,'DefaultAxesFontName','time');

figure;
subplot(1,2,1);
surf(jinv12(2:12,1),jinv12(1,2:12),jinv12(2:12,2:12));
str(1) = {'Figure'};
str(2) = {'Optimal Early Investments by Parental Cognitive and Noncognitive Skills'};
title(str,'FontSize',14);
%axis([-1 1 -1 1 0.9 1.25]);
clear str
xlabel('Parental Noncognitive Skill')
ylabel('Parental Cognitive Skill')
subplot(1,2,2);
surf(jinv22(2:12,1),jinv22(1,2:12),jinv22(2:12,2:12));
str(1) = {'Figure'};
str(2) = {'Optimal Late Investments by Parental Cognitive and Noncognitive Skills'};
title(str,'FontSize',14);
%axis([-1 1 -1 1 0.9 1.25]);
colormap('gray');
clear str
xlabel('Parental Noncognitive Skill')
ylabel('Parental Cognitive Skill')
clear all;

clear all;