module generateran                          !!随机数生成模块
    implicit none
    
    contains
        real function randomfunc(lbound,ubound) !生成以lbound、ubound为上下边界的随机数
        implicit none
    
        real :: lbound,ubound
        real :: t
        call random_number(t)
    
        randomfunc=lbound+(ubound-lbound)*t
    
        end function
    end module
    

module incidencetype                        !!定义入射光线信息数组
    implicit none
    type incident
        real :: x      !入射点绝对坐标
        real :: y
        real :: z
        real :: m      !入射光线矢量
        real :: n
        real :: l
        integer :: ref !是否反射
    end type
    end module
    
module reflectiontype                       !!定义反射光线信息数组
    implicit none
    type reflect
        real :: x  !反射点绝对坐标
        real :: y
        real :: z
        real :: m  !反射光线矢量
        real :: n
        real :: l
        real :: fy !反射光线与聚焦面交点坐标
        real :: fz 
    end type
    end module
    
module cptincidence                         !!计算入射光线信息的模块
    implicit none
    
    contains
        real function Gincidentx(m,n,l,x,e1,e2)  !!计算定日镜入射位置坐标x
            implicit none
            real e1
            real e2
            real x      !定日镜中心点坐标
            real m,n,l  !定日镜法线向量
            
            real sina   !定日镜法线高度角正弦
            real tanh   !定日镜法线地面投影与y正方向夹角正切
            
            real xx,yy  !定日镜上入射点对于定日镜中心点的相对坐标（定日镜平面在地面上的投影）
            real angleb !定日镜法线地面投影与y正方向夹角
            
            tanh=m/n
            sina=l/sqrt(m**2+n**2+l**2)
            
            xx=sina*e2
            yy=e1
            angleb=atan(tanh)
            
            Gincidentx=x+ xx*sin(angleb) - yy*cos(angleb)
            
        end function
        
        real function Gincidenty(m,n,l,y,e1,e2) !!计算定日镜入射位置坐标y
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
        
        real function Gincidentz(m,n,l,z,e1,e2)  !!计算定日镜入射位置坐标z
            implicit none
            real e1
            real e2
            real z      !定日镜中心点高度
            real m,n,l  !定日镜法线向量
            
            real sina1  !定日镜高度角
            real sina   !定日镜法线高度角
            
            sina=l/sqrt(m**2+n**2+l**2)
            sina1=sqrt(1-sina**2)
            Gincidentz=z + e2*sina1
            
        end function    

        real function Gincidentm(h,a)            !!计算太阳入射光线方向向量m
            implicit none
            real h,a    !太阳高度角，方位角
            Gincidentm=-(1/tan(h))/sqrt(1+(tan(a)**2))
        end function
        
        real function Gincidentn(h,a)            !!计算太阳入射光线方向向量n
            implicit none
            real h,a
            Gincidentn=tan(a)*(1/tan(h))/sqrt(1+(tan(a)**2))
        end function
        
        real function Gincidentl(h,a)            !!计算太阳入射光线方向向量l
            implicit none
            real h,a
            Gincidentl=1
        end function
        
        integer function Refornot(Erefl,e3)      !!判断入射光线是否反射
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
    
module cptreflection                        !!计算反射光线信息的模块
    implicit none
    
    contains
        real function Greflectm(x1,y1,z1,m,n,l) !!计算反射光线方向向量m
            implicit none
            real m,n,l    !入射光线方向向量
            real x1,y1,z1 !定日镜法线向量
            real t
            
            t=(m*x1+n*y1+l*z1)/(x1**2+y1**2+z1**2)
            
            Greflectm=2*x1*t-m
            
        end function
        
        real function Greflectn(x1,y1,z1,m,n,l) !!计算反射光线方向向量n
            implicit none
            real m,n,l
            real x1,y1,z1 !定日镜法线向量
            real t
            

            t=(m*x1+n*y1+l*z1)/(x1**2+y1**2+z1**2)
            
            Greflectn=2*y1*t-n
            
        end function
        
        real function Greflectl(x1,y1,z1,m,n,l) !!计算反射光线方向向量l
            implicit none
            real m,n,l
            real x1,y1,z1 !定日镜法线向量
            real t

            t=(m*x1+n*y1+l*z1)/(x1**2+y1**2+z1**2)
            
            Greflectl=2*z1*t-l
            
        end function
        
    end module
        
module cptintersection                      !!计算反射光线与聚焦面交点的模块
    implicit none
    
    contains
        real function Gintersecy(x,y,z,m,n,l)   !!计算反射光线与聚焦面交点坐标y
            real x,y,z,m,n,l !反射点坐标及反射光线方向向量
            real t
        
            t=-x/m
            Gintersecy=y+n*t
        
        end function
        
        real function Gintersecz(x,y,z,m,n,l)   !!计算反射光线与聚焦面交点坐标z
            real x,y,z,m,n,l
            real t
        
            t=-x/m
            Gintersecz=z+l*t
        
        end function
        
    end module
    