function[m]=kerndens(x,dom,h,plo)
%[m]=kerndens(x,dom,h,plo)
%x is continuous with density f(x)
%This function calculates the nonparametric density using different
%kernel functions (specified by user). 
%OPTIONS:
%	h:
%		defines the width of the weight function window around each grid 
%		point
%	m: 
%		is the name for the estimated density over the domain
%	dom:
%		is the name of the domain x
%   plo: 
%       1 if you want graph, any other number no graph    
n=size(x,1);
nn=size(dom,1);
fid=fopen('temp.txt','w');
for s=1:nn;
   for i=1:n;
      k(i,1)=exp((-((dom(s,1)-x(i,1))/h)^2)/2)/sqrt(2*pi);
   end;
   a=sum(k);   
   m(s,1)=a/(h*n);
end;
if plo==1l
    figure;
    plot(dom,m);
end;