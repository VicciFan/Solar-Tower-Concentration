module shadow
    implicit none
    
    contains
    
    real function Ganglepointx(number,m,n,l,x,y,z)   !!求定日镜顶角点位置坐标x
        integer number !定日镜角代号，1为东南角，2为西南角，3为西北角，4为东北角
        real m,n,l     !定日镜法线向量
        real x,y,z     !定日镜中心点位置坐标
        
        real tanh
        real sina
        real xx,yy
        real angleb
        
        real e1,e2
        
        tanh=m/n
        sina=l/sqrt(m**2+n**2+l**2)
        
        if (number==1) then
            e1=1.05
            e2=-1.35
        else if(number==2) then
            e1=-1.05
            e2=-1.35
        else if (number==3) then
            e1=-1.05
            e2=1.35
        else if (number==4) then
            e1=1.05
            e2=1.35
        end if
            
        xx=sina*e2
        yy=e1
        angleb=atan(tanh)
            
        Ganglepointx=x+ xx*sin(angleb) - yy*cos(angleb)
        
    end function
    
    
    real function Ganglepointy(number,m,n,l,x,y,z)   !!求定日镜顶角点位置坐标y
        integer number
        real m,n,l
        real x,y,z
        
        real tanh
        real sina
        real xx,yy
        real angleb
        
        real e1,e2
        
        tanh=m/n
        sina=l/sqrt(m**2+n**2+l**2)
        
        if (number==1) then
            e1=1.05
            e2=-1.35
        else if(number==2) then
            e1=-1.05
            e2=-1.35
        else if (number==3) then
            e1=-1.05
            e2=1.35
        else if (number==4) then
            e1=1.05
            e2=1.35
        end if
            
        xx=sina*e2
        yy=e1
        angleb=atan(tanh)
        
        Ganglepointy=y + yy*sin(angleb) + xx*cos(angleb)
        
    end function
    
    
    real function Ganglepointz(number,m,n,l,x,y,z)    !!求定日镜顶角点位置坐标z
        integer number
        real m,n,l
        real x,y,z
        
        real sina1  !定日镜高度角
        real sina   !定日镜法线高度角
        
        real e1,e2
        
        if (number==1) then
            e1=1.05
            e2=-1.35
        else if(number==2) then
            e1=-1.05
            e2=-1.35
        else if (number==3) then
            e1=-1.05
            e2=1.35
        else if (number==4) then
            e1=1.05
            e2=1.35
        end if
        
        sina=l/sqrt(m**2+n**2+l**2)
        sina1=sqrt(1-sina**2)
        Ganglepointz=z + e2*sina1    
        
    end function     
    
    
    real function Ginterpointx(x,y,z,m,n,l,fm,fn,fl,fx,fy,fz) !!求邻近定日镜顶角点在入射光线方向上于定日镜面上投影点位置坐标x
        real x,y,z     !邻近定日镜顶角点坐标
        real m,n,l     !入射方向向量
        real fm,fn,fl  !定日镜法线向量      
        real fx,fy,fz  !定日镜中心点坐标
        
        real t
        
        t = ((fx-x)*fm+(fy-y)*fn+(fz-z)*fl)/(fm*m+fn*n+fl*l)
        
        Ginterpointx= x+m*t
        
    end function
    
    
    real function Ginterpointy(x,y,z,m,n,l,fm,fn,fl,fx,fy,fz)  !!求邻近定日镜顶角点在入射光线方向上于定日镜面上投影点位置坐标y
        real x,y,z
        real m,n,l
        real fm,fn,fl        
        real fx,fy,fz
        
        real t
        
        t = ((fx-x)*fm+(fy-y)*fn+(fz-z)*fl)/(fm*m+fn*n+fl*l)
        
        Ginterpointy= y+n*t
        
    end function    
    
    
    real function Ginterpointz(x,y,z,m,n,l,fm,fn,fl,fx,fy,fz)  !!求邻近定日镜顶角点在入射光线方向上于定日镜面上投影点位置坐标z
        real x,y,z
        real m,n,l
        real fm,fn,fl
        real fx,fy,fz
        
        real t
        
        t = ((fx-x)*fm+(fy-y)*fn+(fz-z)*fl)/(fm*m+fn*n+fl*l)
        
        Ginterpointz= z+l*t
        
    end function    

    
    logical function Ionsurface(x,y,z,fm,fn,fl,fx,fy,fz)  !!判断邻近定日镜顶角点投影是否在定日镜上
        real x,y,z     !顶角点投影位置坐标
        real fm,fn,fl  !定日镜法线向量
        real fx,fy,fz  !定日镜中心点坐标
        
        real e1,e2
        
        real sina   !定日镜法线高度角正弦
        real tanh   !定日镜法线地面投影与y正方向夹角正切
            
        real xx,yy  !顶角点对于定日镜中心点的相对坐标（定日镜平面在地面上的投影）
        real angleb !定日镜法线地面投影与y正方向夹角
            
        tanh=fm/fn
        sina=fl/sqrt(fm**2+fn**2+fl**2)

        angleb=atan(tanh)    
        
        xx=sin(angleb)*(x-fx)+cos(angleb)*(y-fy)
        yy=sin(angleb)*(y-fy)-cos(angleb)*(x-fx)

        e1=yy
        e2=xx/sina
        
        if (e1<1.05 .and.  e1>-1.05 .and. e2<1.35 .and. e2>-1.35) then
            Ionsurface= .true.
        else
            Ionsurface= .false.
        end if
        
        
    end function
    
    logical function Iinshadow(x,y,z,x1,y1,z1,x2,y2,z2,x3,y3,z3,fx,fy,fz,fm,fn,fl)  !!判断入射点是否在邻近定日镜遮挡范围中
        real x,y,z     !入射点位置坐标
        real x1,y1,z1  !顶角点1投影位置坐标，x向点
        real x2,y2,z2  !顶角点2投影位置坐标，折角点
        real x3,y3,z3  !顶角点3投影位置坐标，y向点
        real fx,fy,fz  !定日镜中心坐标
        real fm,fn,fl  !定日镜法线向量
        
        real e1,e2
        real e11,e12
        real e21,e22
        real e31,e32
        
        real sina   !定日镜法线高度角正弦
        real tanh   !定日镜法线地面投影与y正方向夹角正切
        real angleb !定日镜法线地面投影与y正方向夹角
            
        tanh=fm/fn
        sina=fl/sqrt(fm**2+fn**2+fl**2)
        angleb=atan(tanh)    
        
        e12=(sin(angleb)*(x1-fx)+cos(angleb)*(y1-fy))/sina
        e11=sin(angleb)*(y1-fy)-cos(angleb)*(x1-fx)
        
        e22=(sin(angleb)*(x2-fx)+cos(angleb)*(y2-fy))/sina
        e21=sin(angleb)*(y2-fy)-cos(angleb)*(x2-fx)        
        
        e32=(sin(angleb)*(x3-fx)+cos(angleb)*(y3-fy))/sina
        e31=sin(angleb)*(y3-fy)-cos(angleb)*(x3-fx)
        
        e2=(sin(angleb)*(x-fx)+cos(angleb)*(y-fy))/sina
        e1=sin(angleb)*(y-fy)-cos(angleb)*(x-fx)
        
        if(e31 > (e21-e11)/(e22-e12)*(e32-e12)+e11 .and. e12> (e32-e22)/(e31-e21)*(e11-e21)+e22 ) then
            if(e1>(e21-e11)/(e22-e12)*(e2-e12)+e11 .and. e2> (e32-e22)/(e31-e21)*(e1-e21)+e22 ) then
                Iinshadow= .true.
            else
                Iinshadow= .false.
            end if
        else if(e31 > (e21-e11)/(e22-e12)*(e32-e12)+e11 .and. e12< (e32-e22)/(e31-e21)*(e11-e21)+e22 ) then
            if(e1>(e21-e11)/(e22-e12)*(e2-e12)+e11 .and. e2< (e32-e22)/(e31-e21)*(e1-e21)+e22 ) then
                Iinshadow= .true.
            else
                Iinshadow= .false.
            end if
        else if(e31 < (e21-e11)/(e22-e12)*(e32-e12)+e11 .and. e12< (e32-e22)/(e31-e21)*(e11-e21)+e22 ) then
            if(e1<(e21-e11)/(e22-e12)*(e2-e12)+e11 .and. e2< (e32-e22)/(e31-e21)*(e1-e21)+e22 ) then
                Iinshadow= .true.
            else
                Iinshadow= .false.
            end if
        else if(e31 < (e21-e11)/(e22-e12)*(e32-e12)+e11 .and. e12> (e32-e22)/(e31-e21)*(e11-e21)+e22 ) then
            if(e1<(e21-e11)/(e22-e12)*(e2-e12)+e11 .and. e2>(e32-e22)/(e31-e21)*(e1-e21)+e22 ) then
                Iinshadow= .true.
            else
                Iinshadow= .false.
            end if
        end if
        
    end function
    
    
    end module 
    
module shadowtype
    implicit none
    type shadowt
        integer :: mirrorA !定日镜编号
        integer :: number  !顶角点编号
    end type
    end module