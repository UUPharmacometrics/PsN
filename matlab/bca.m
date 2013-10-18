function [lower,upper]=bca(estimate,bootstat,jackstat,low,up)
% Beräknar BCA-konfidensintervall enligt Efron
% Anrop [lower,upper]=bca(estimate,bootstat,jackstat,low,up)
% Parametrar:
% estimate=originalestimat
% bootstat=matris av bootstrapestimat
% jackstat=matris av jackknifeestimat
% low=undre konfidensgrad i procent
% up=övre konfidensgrad i procent
% Output:
% lower=undre konfidensgräns
% upper=övre konfidensgräns

% Beräkning av hjälpstorheten zhat 
[a,b]=size(bootstat);
antal=sum(bootstat<=(ones(a,1)*estimate))
zhat=norminv(antal/a);
% Beräkning av hjälpstorheten ahat
%[nrow,ncol]=size(x);
%if nrow<ncol
%	x=x';
%	nrow=ncol ;
%end
%% Jackknife estimates in s (Lasse)
%for j=1:nrow
%	if j==1
%		values=[2:nrow];
%	elseif j==nrow
%		values=[1:nrow-1];
%	else	
%		values=[1:j-1, j+1:nrow];
%	end
%	s(j,:)=feval(fun,x(values,:));
%end
s=jackstat;
mn=mean(s); 				% theta_hat_dot (Lasse)
[b c]=size(s);
d=s-ones(b,1)*mn;
sum2=sum(d.^2);				% - täljare
sum3=sum(d.^3);				% del av nämnare
ahat=-sum3./(6*(sum2.^(1.5)));
% Beräkning av korrigerade konfidensgrader enligt Efron 
n=norminv(low/100);
alfa1=100*normcdf(zhat+(zhat+n)./(1-ahat.*(zhat+n)));
n=norminv(up/100);
alfa2=100*normcdf(zhat+(zhat+n)./(1-ahat.*(zhat+n)));
% Beräkning av konfidensgränser med percentilmetoden
lower=diag(prctile(bootstat,alfa1))';
upper=diag(prctile(bootstat,alfa2))';
