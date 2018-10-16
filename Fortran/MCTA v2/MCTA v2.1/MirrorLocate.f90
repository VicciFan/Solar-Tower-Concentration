module mirrortype                           !!定义定日镜信息数组
    implicit none
    type mirrort
        integer :: i !定日镜编号
        integer :: j
        real :: x    !定日镜中心点坐标
        real :: y
        real :: z
        real :: m    !定日镜镜面法向量
        real :: n
        real :: l
    end type
    end module
    
module cptmirrorN                           !!计算定日镜法线方向向量的模块
    implicit none
    
    contains
    real function GmirrorNm(x,y,z,m,n,l)    !!计算定日镜法线向量m
        real x,y,z  !定日镜中心点坐标
        real m,n,l  !入射光线方向向量 
        
        real Htower
        common /Htowe/ Htower
        
        real t
        
        t=sqrt(m**2+n**2+l**2)/sqrt(x**2+y**2+(Htower-z)**2)
        
        GmirrorNm=(m-x*t)/2
        
    end function
    
    real function GmirrorNn(x,y,z,m,n,l)   !!计算定日镜法线向量n
        real x,y,z  !定日镜中心点坐标
        real m,n,l  !入射光线方向向量 
        
        real Htower
        common /Htowe/ Htower
        
        real t
        
        t=sqrt(m**2+n**2+l**2)/sqrt(x**2+y**2+(Htower-z)**2)
        
        GmirrorNn=(n-y*t)/2
        
    end function
    
    real function GmirrorNl(x,y,z,m,n,l)   !!计算定日镜法线向量l
        real x,y,z  !定日镜中心点坐标
        real m,n,l  !入射光线方向向量 
        
        real Htower
        common /Htowe/ Htower
        
        real t
        
        t=sqrt(m**2+n**2+l**2)/sqrt(x**2+y**2+(Htower-z)**2)
        
        GmirrorNl=(l-(z-Htower)*t)/2
        
    end function
    
    end module
    