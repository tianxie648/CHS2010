clear all;
load jointeduc1.txt;
load jointcrime1.txt;
set(0,'DefaultAxesFontName','time');

figure;
subplot(1,2,1);
surf(jointeduc1(2:12,1),jointeduc1(1,2:12),jointeduc1(2:12,2:12));
str(1) = {'Figure'};
str(2) = {'Ratio of Early to Late Investments by Childs Initial Conditions of Cognitive and Noncognitive Skills Max Educ'};
title(str,'FontSize',14);
%axis([-1 1 -1 1 1.1 1.7]);
clear str
xlabel('Child Initial Noncognitive Skill')
ylabel('Child Initial Cognitive Skill')
subplot(1,2,2);
surf(jointcrime1(2:12,1),jointcrime1(1,2:12),jointcrime1(2:12,2:12));
str(1) = {'Figure'};
str(2) = {'Ratio of Early to Late Investments by Childs Initial Conditions of Cognitive and Noncognitive Skills Min Crime'};
title(str,'FontSize',14);
clear str
xlabel('Child Initial Noncognitive Skill')
ylabel('Child Initial Cognitive Skill')
colormap(gray);
%axis([-1 1 -1 1 1.1 1.7]);
%shading interp;
%whitebg([0 0 0]);
clear all;

load jointeduc2.txt;
load jointcrime2.txt;
set(0,'DefaultAxesFontName','time');

figure;
subplot(1,2,1);
surf(jointeduc2(2:12,1),jointeduc2(1,2:12),jointeduc2(2:12,2:12));
str(1) = {'Figure'};
str(2) = {'Ratio of Early to Late Investments by Mothers Cognitive and Noncognitive Skills Max Educ'};
title(str,'FontSize',14);
%axis([-1 1 -1 1 1.1 1.7]);
clear str
xlabel('Mother Noncognitive Skill')
ylabel('Mother Cognitive Skill')
subplot(1,2,2);
surf(jointcrime2(2:12,1),jointcrime2(1,2:12),jointcrime2(2:12,2:12));
str(1) = {'Figure'};
str(2) = {'Ratio of Early to Late Investments by Mothers Cognitive and Noncognitive Skills Min Crime'};
title(str,'FontSize',14);
colormap(gray);
%axis([-1 1 -1 1 1.1 1.7]);
clear str
xlabel('Mother Noncognitive Skill')
ylabel('Mother Cognitive Skill')

clear all;

