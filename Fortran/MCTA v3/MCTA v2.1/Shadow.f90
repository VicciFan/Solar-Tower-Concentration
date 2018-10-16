module shadow
    implicit none
    
    contains
    
    real function Ganglepointx(number,m,n,l,x,y,z)   !!���վ����ǵ�λ������x
        integer number !���վ��Ǵ��ţ�1Ϊ���Ͻǣ�2Ϊ���Ͻǣ�3Ϊ�����ǣ�4Ϊ������
        real m,n,l     !���վ���������
        real x,y,z     !���վ����ĵ�λ������
        
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
    
    
    real function Ganglepointy(number,m,n,l,x,y,z)   !!���վ����ǵ�λ������y
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
    
    
    real function Ganglepointz(number,m,n,l,x,y,z)    !!���վ����ǵ�λ������z
        integer number
        real m,n,l
        real x,y,z
        
        real sina1  !���վ��߶Ƚ�
        real sina   !���վ����߸߶Ƚ�
        
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
    
    
    real function Ginterpointx(x,y,z,m,n,l,fm,fn,fl,fx,fy,fz) !!���ڽ����վ����ǵ���������߷������ڶ��վ�����ͶӰ��λ������x
        real x,y,z     !�ڽ����վ����ǵ�����
        real m,n,l     !���䷽������
        real fm,fn,fl  !���վ���������      
        real fx,fy,fz  !���վ����ĵ�����
        
        real t
        
        t = ((fx-x)*fm+(fy-y)*fn+(fz-z)*fl)/(fm*m+fn*n+fl*l)
        
        Ginterpointx= x+m*t
        
    end function
    
    
    real function Ginterpointy(x,y,z,m,n,l,fm,fn,fl,fx,fy,fz)  !!���ڽ����վ����ǵ���������߷������ڶ��վ�����ͶӰ��λ������y
        real x,y,z
        real m,n,l
        real fm,fn,fl        
        real fx,fy,fz
        
        real t
        
        t = ((fx-x)*fm+(fy-y)*fn+(fz-z)*fl)/(fm*m+fn*n+fl*l)
        
        Ginterpointy= y+n*t
        
    end function    
    
    
    real function Ginterpointz(x,y,z,m,n,l,fm,fn,fl,fx,fy,fz)  !!���ڽ����վ����ǵ���������߷������ڶ��վ�����ͶӰ��λ������z
        real x,y,z
        real m,n,l
        real fm,fn,fl
        real fx,fy,fz
        
        real t
        
        t = ((fx-x)*fm+(fy-y)*fn+(fz-z)*fl)/(fm*m+fn*n+fl*l)
        
        Ginterpointz= z+l*t
        
    end function    

    
    logical function Ionsurface(x,y,z,fm,fn,fl,fx,fy,fz)  !!�ж��ڽ����վ����ǵ�ͶӰ�Ƿ��ڶ��վ���
        real x,y,z     !���ǵ�ͶӰλ������
        real fm,fn,fl  !���վ���������
        real fx,fy,fz  !���վ����ĵ�����
        
        real e1,e2
        
        real sina   !���վ����߸߶Ƚ�����
        real tanh   !���վ����ߵ���ͶӰ��y������н�����
            
        real xx,yy  !���ǵ���ڶ��վ����ĵ��������꣨���վ�ƽ���ڵ����ϵ�ͶӰ��
        real angleb !���վ����ߵ���ͶӰ��y������н�
            
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
    
    logical function Iinshadow(x,y,z,x1,y1,z1,x2,y2,z2,x3,y3,z3,fx,fy,fz,fm,fn,fl)  !!�ж�������Ƿ����ڽ����վ��ڵ���Χ��
        real x,y,z     !�����λ������
        real x1,y1,z1  !���ǵ�1ͶӰλ�����꣬x���
        real x2,y2,z2  !���ǵ�2ͶӰλ�����꣬�۽ǵ�
        real x3,y3,z3  !���ǵ�3ͶӰλ�����꣬y���
        real fx,fy,fz  !���վ���������
        real fm,fn,fl  !���վ���������
        
        real e1,e2
        real e11,e12
        real e21,e22
        real e31,e32
        
        real sina   !���վ����߸߶Ƚ�����
        real tanh   !���վ����ߵ���ͶӰ��y������н�����
        real angleb !���վ����ߵ���ͶӰ��y������н�
            
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
        integer :: mirrorA !���վ����
        integer :: number  !���ǵ���
    end type
    end module