module generateran                          !!���������ģ��
    implicit none
    
    contains
        real function randomfunc(lbound,ubound) !������lbound��uboundΪ���±߽�������
        implicit none
    
        real :: lbound,ubound
        real :: t
        call random_number(t)
    
        randomfunc=lbound+(ubound-lbound)*t
    
        end function
    end module
    

module incidencetype                        !!�������������Ϣ����
    implicit none
    type incident
        real :: x      !������������
        real :: y
        real :: z
        real :: m      !�������ʸ��
        real :: n
        real :: l
        integer :: ref !�Ƿ���
    end type
    end module
    
module reflectiontype                       !!���巴�������Ϣ����
    implicit none
    type reflect
        real :: x  !������������
        real :: y
        real :: z
        real :: m  !�������ʸ��
        real :: n
        real :: l
        real :: fy !���������۽��潻������
        real :: fz 
    end type
    end module
    
module cptincidence                         !!�������������Ϣ��ģ��
    implicit none
    
    contains
        real function Gincidentx(m,n,l,x,e1,e2)  !!���㶨�վ�����λ������x
            implicit none
            real e1
            real e2
            real x      !���վ����ĵ�����
            real m,n,l  !���վ���������
            
            real sina   !���վ����߸߶Ƚ�����
            real tanh   !���վ����ߵ���ͶӰ��y������н�����
            
            real xx,yy  !���վ����������ڶ��վ����ĵ��������꣨���վ�ƽ���ڵ����ϵ�ͶӰ��
            real angleb !���վ����ߵ���ͶӰ��y������н�
            
            tanh=m/n
            sina=l/sqrt(m**2+n**2+l**2)
            
            xx=sina*e2
            yy=e1
            angleb=atan(tanh)
            
            Gincidentx=x+ xx*sin(angleb) - yy*cos(angleb)
            
        end function
        
        real function Gincidenty(m,n,l,y,e1,e2) !!���㶨�վ�����λ������y
            implicit none
            real e1
            real e2
            real m,n,l
            real y
            
            real sina
            real tanh    
            
            real xx,yy
            real angleb
            
            tanh=m/n
            sina=l/sqrt(m**2+n**2+l**2)
            
            xx=sina*e2
            yy=e1
            angleb=atan(tanh)
            
            Gincidenty=y + yy*sin(angleb) + xx*cos(angleb)
            
        end function
        
        real function Gincidentz(m,n,l,z,e1,e2)  !!���㶨�վ�����λ������z
            implicit none
            real e1
            real e2
            real z      !���վ����ĵ�߶�
            real m,n,l  !���վ���������
            
            real sina1  !���վ��߶Ƚ�
            real sina   !���վ����߸߶Ƚ�
            
            sina=l/sqrt(m**2+n**2+l**2)
            sina1=sqrt(1-sina**2)
            Gincidentz=z + e2*sina1
            
        end function    

        real function Gincidentm(h,a)            !!����̫��������߷�������m
            implicit none
            real h,a    !̫���߶Ƚǣ���λ��
            Gincidentm=-(1/tan(h))/sqrt(1+(tan(a)**2))
        end function
        
        real function Gincidentn(h,a)            !!����̫��������߷�������n
            implicit none
            real h,a
            Gincidentn=tan(a)*(1/tan(h))/sqrt(1+(tan(a)**2))
        end function
        
        real function Gincidentl(h,a)            !!����̫��������߷�������l
            implicit none
            real h,a
            Gincidentl=1
        end function
        
        integer function Refornot(Erefl,e3)      !!�ж���������Ƿ���
            implicit none
            
            real e3
            real Erefl         
            
            if (e3>Erefl) then
                Refornot = 0
            else
                Refornot = 1
            end if
            
        end function
        
    end module
    
module cptreflection                        !!���㷴�������Ϣ��ģ��
    implicit none
    
    contains
        real function Greflectm(x1,y1,z1,m,n,l) !!���㷴����߷�������m
            implicit none
            real m,n,l    !������߷�������
            real x1,y1,z1 !���վ���������
            real t
            
            t=(m*x1+n*y1+l*z1)/(x1**2+y1**2+z1**2)
            
            Greflectm=2*x1*t-m
            
        end function
        
        real function Greflectn(x1,y1,z1,m,n,l) !!���㷴����߷�������n
            implicit none
            real m,n,l
            real x1,y1,z1 !���վ���������
            real t
            

            t=(m*x1+n*y1+l*z1)/(x1**2+y1**2+z1**2)
            
            Greflectn=2*y1*t-n
            
        end function
        
        real function Greflectl(x1,y1,z1,m,n,l) !!���㷴����߷�������l
            implicit none
            real m,n,l
            real x1,y1,z1 !���վ���������
            real t

            t=(m*x1+n*y1+l*z1)/(x1**2+y1**2+z1**2)
            
            Greflectl=2*z1*t-l
            
        end function
        
    end module
        
module cptintersection                      !!���㷴�������۽��潻���ģ��
    implicit none
    
    contains
        real function Gintersecy(x,y,z,m,n,l)   !!���㷴�������۽��潻������y
            real x,y,z,m,n,l !��������꼰������߷�������
            real t
        
            t=-x/m
            Gintersecy=y+n*t
        
        end function
        
        real function Gintersecz(x,y,z,m,n,l)   !!���㷴�������۽��潻������z
            real x,y,z,m,n,l
            real t
        
            t=-x/m
            Gintersecz=z+l*t
        
        end function
        
    end module
    