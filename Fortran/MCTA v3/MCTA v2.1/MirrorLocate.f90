module mirrortype                           !!���嶨�վ���Ϣ����
    implicit none
    type mirrort
        integer :: i !���վ����
        integer :: j
        real :: x    !���վ����ĵ�����
        real :: y
        real :: z
        real :: m    !���վ����淨����
        real :: n
        real :: l
        real :: x1   !���վ���Ӱ�ǵ�1����
        real :: y1
        real :: z1
        real :: x2   !���վ���Ӱ�ǵ�2����
        real :: y2
        real :: z2
        real :: x3   !���վ���Ӱ�ǵ�3����
        real :: y3
        real :: z3
        logical :: inter   !���վ��Ƿ�����Ӱ
    end type
    end module
    
module cptmirrorN                           !!���㶨�վ����߷���������ģ��
    implicit none
    
    contains
    real function GmirrorNm(x,y,z,m,n,l)    !!���㶨�վ���������m
        real x,y,z  !���վ����ĵ�����
        real m,n,l  !������߷������� 
        
        real Htower
        common /Htowe/ Htower
        
        real t
        
        t=sqrt(m**2+n**2+l**2)/sqrt(x**2+y**2+(Htower-z)**2)
        
        GmirrorNm=(m-x*t)/2
        
    end function
    
    real function GmirrorNn(x,y,z,m,n,l)   !!���㶨�վ���������n
        real x,y,z  !���վ����ĵ�����
        real m,n,l  !������߷������� 
        
        real Htower
        common /Htowe/ Htower
        
        real t
        
        t=sqrt(m**2+n**2+l**2)/sqrt(x**2+y**2+(Htower-z)**2)
        
        GmirrorNn=(n-y*t)/2
        
    end function
    
    real function GmirrorNl(x,y,z,m,n,l)   !!���㶨�վ���������l
        real x,y,z  !���վ����ĵ�����
        real m,n,l  !������߷������� 
        
        real Htower
        common /Htowe/ Htower
        
        real t
        
        t=sqrt(m**2+n**2+l**2)/sqrt(x**2+y**2+(Htower-z)**2)
        
        GmirrorNl=(l-(z-Htower)*t)/2
        
    end function
    
    end module
    