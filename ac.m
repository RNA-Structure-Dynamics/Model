%x(1):mu_1
%x(2):mu_2
%x(3):sigma_1^2
%x(4):sigma_2^2
function y=ac(x)
y(1)=(x(1)+x(2))/2-0.031;
y(2)=(x(1).^2+x(2).^2+x(3)+x(4))/2-0.508;
y(3)=(x(1).^3+3*x(1).*x(3)+x(2).^3+3*x(2).*x(4))/2+0.207;
y(4)=(x(1).^4+6*x(1).^2.*x(3)+3*x(3).^2+x(2).^4+6*x(2).^2.*x(4)+3*x(4).^2)/2-0.779;
end

%run fsolve(@ac,[-0.4,0.4,0.5,0.12])