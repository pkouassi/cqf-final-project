1;

%Returns fair rate of CDS 
clear all;
global alpha;
global T;
global currentobligor;
global target_value;
global defTime;

alpha=0.01:0.01:20; %hazard rate coefficients corresponding to indexing by T
T=0:0.1:200; %Time coefficients corresponding to alpha
R=[.51 .21 .1;.21 .65 .1;.1 .1 .85]; %covariance matrix

global N;
N=3; %No of obligors
DoF=6; %Degree of freedom for t-copula
scenarios=20; %no of MC simulations
alpha=alpha';
alpha=[alpha alpha*2 alpha*1.5];


T=T';

function [ret]=ProcessGammaFn(t)
	global alpha;
	global T;
	global currentobligor;
	global target_value;
	k=min(find(T>t));
	tmpsum=0;
	for j=1:k,
		tmpsum=tmpsum+alpha(j,currentobligor)*(T(j+1)-T(j));
	end
	target_value;
	ret=abs(-tmpsum-target_value);
endfunction


defTime=zeros(scenarios,N);
umat=zeros(scenarios,N);
A=chol(R); %R=A'*A here
A=A'; %R should be = A*A'
for i=1:scenarios,
	z=randn(N,1);
	s=chisquare_rnd(DoF);
	y=z'*A;
	x=(sqrt(DoF)/sqrt(s))*y;
	u=t_cdf(x,DoF);
	umat(i,:)=log(u);
	for j=1:N,
		currentobligor=j;
		target_value=log(u(j));
		defTime(i,j)=fmins('ProcessGammaFn',.21);
	end
end



if scenarios < 10,
	load -force -text deftime.txt;
	defTime=deftimemat;
endif


function [ret]=SwapSpreadEqn(x)
	x
	global defTime;
	tn = 3; %maturity of default swap
	scenarios=size(defTime,1);

	n=6; %time steps for indexing preminum payments
	N=size(defTime,2); %no of obligors
	r=0.05; %risk free rate 
	k=2; %seniority level eg 2nd to default swap
	s=x; %basis point quote for fair rate of CDS
	s=s/1000;
	Recovery_rate=ones(N,1)*0.5;
	M=1000; %notional amount
	delta = 0.5; %semi-annual fixing
	t=tn/n;
	PL=0; %Premium leg
	for i=1:n,
		%find probability of survival of kth default until each indexing period
		SurvivalProb=1-sum(sum(defTime<t,2)>=k,1)/size(defTime,1);
		B=exp(-r*t);
		PL=PL+s*M*delta*B*SurvivalProb;
		t=t+(tn/n);
	end
	PL

	kdefaultTime=sort(defTime,2);
	kdefaultTime=kdefaultTime(:,k); %default time for each scenario
	ntimesteps=6;
	dt=tn/ntimesteps;
	
	
	ntimesteps=10;
	dt=tn/ntimesteps;
	DP=0; %Default leg
	
	for j=1:N,
	        j;
		tmpsum=0;
		t=dt;
		for ti=1:ntimesteps,
			no_of_events=0;
			%find probability that k-th default occured within this time interval
			% and jth obligor is responsible for the k-th default
			for i=1:size(kdefaultTime,1),
				if  ((kdefaultTime(i)>(t-dt)) && (kdefaultTime(i)<=t) ),
					%kth default event was triggered during this time interval
					%disp('here');
					if defTime(i,j)==kdefaultTime(i),
						%jth obligor caused the k-th default
						no_of_events=no_of_events+1;
					end
				end
			end
			prob_tau_k_jth=no_of_events/size(kdefaultTime,1);
			B=exp(-r*t);
			tmpsum=tmpsum+B*prob_tau_k_jth;
			t=t+dt;
		end
		DP=DP+M*(1-Recovery_rate(j))*tmpsum;
	end
	DP
	
	AP=0; %Accrued premium
	ntimesubsteps=10;
	t=tn/n;
	dt=t/ntimesubsteps;
	t=0;
	for i=1:n,
		curt=t;
		for ti=1:ntimesubsteps,
			%find probability that k th default happened 
			%between ti and ti+1
			no_of_events=0;
			for i=1:size(kdefaultTime,1),
				if  ((kdefaultTime(i)>(curt)) && (kdefaultTime(i)<=(curt+dt)) ),
					no_of_events=no_of_events+1;
				endif
			end
			ProbOfDefault=no_of_events/scenarios;
			curt=curt+dt;
			B=exp(-r*curt);
			AP=AP+s*M*(curt-t)*(1/(tn/n))*delta*B*ProbOfDefault;
		end
		t=t+tn/n;
	end
	AP
	ret=PL-(DP-AP)
endfunction

fair_cds_rate=fsolve('SwapSpreadEqn',250)