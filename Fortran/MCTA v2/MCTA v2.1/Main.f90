program main
    
    use constant
    use netdef    
    
    use mirrortype
    use incidencetype
    use reflectiontype
    
    use cptsolarangle
    use cptmirrorN
    use cptincidence
    use cptreflection
    use cptintersection
    
    use generateran
    
    use msflib
    use ifqwin
    use dflib
    
    implicit none
    
    !以下为程序变量声明部分………………………………………!
    integer :: sampleNN=1
    integer :: mirrorNN=1
    real e1,e2,e3
    
    real :: Htower  
    common /Htowe/ Htower
    
    integer :: ftype=0
    real :: farea=0
    
    real :: FDmin
    integer :: MirrorNox,MirrorNoy
    real :: MirrorDx=0,MirrorDy=0
    integer :: MirrorDelta=0   
    real :: MirrorL=2.7,MirrorB=2.1,MirrorH=2.25
    
    integer :: sampleN
    common /sample/ sampleN
    data sampleN /1000/
    
    integer mirrors 
    common /mirror/ mirrors
   
    type(mirrort),allocatable :: mirror(:) 
    type(incident),allocatable :: IMS(:,:)
    type(reflect),allocatable :: RMS(:,:)
    
    integer mirrorN
    integer mirrorI,mirrorJ
    
    integer,parameter :: fileid1=10
    integer oti,otj
    
    real :: sunelevation = 0
    real :: sunazimuth = 0
    real :: dayL=0
    
    real :: soltime=0     
    integer :: dayn=0        
    real :: lati=0       
    
    real  RayEff
    real  FieldEff
    
    integer :: figuren=0
    integer :: figurem=0
    integer ofi
    integer ofj
    
    integer :: result
    !……………………………………………………………………!
   

    !以下为输入太阳参数的部分……………………………………!
    write(*,*) "Please type in calendar day:"
    read(*,*) dayn
    do while ( dayn < 1  .or.  dayn > 365)
        write(*,*) "Out of range! Please type again!"
        read(*,*) dayn
        end do 
        
    write(*,*) "Please type in latitude (degree,north as positive):"
    read(*,*) lati
    do while ( lati<(-90) .or. lati>90)
        write(*,*) "Out of range! Please type again!"
        read(*,*) lati
    end do 
        
    dayL= Gdaylength(lati,dayn)
    
    write(*,*) "Please type in solar time:"
    read(*,*) soltime
    do while ( soltime<(12-dayL/2) .or. soltime>(12+dayL/2))
        write(*,*) "Not in daytime! Please type in another number!"
        read(*,*) soltime
    end do 
    !……………………………………………………………………!
    
    
    !以下为计算太阳角度的部分……………………………………!
    sunelevation = Gelevation(lati,soltime,dayn)
        if (sunelevation <0) then
           sunelevation=-sunelevation
        end if
        
    sunazimuth = Gazimuth(lati,soltime,dayn)
    !……………………………………………………………………!
    
    
    !以下为输入吸收塔高的部分……………………………………!
    write(*,*) "Please type in tower height:"
    read(*,*) Htower
    !……………………………………………………………………!
    
    
    !以下为输入定日镜结构参数的部分……………………………!   
    write(*,*) "Please type in mirror field type(1 or 2 or 3 or 4 or 5 or 6):"
    read(*,*) ftype    
    do while ( ftype /= 1 .and. ftype /= 2 .and. ftype /= 3 .and. ftype /= 4 .and. ftype /= 5 .and. ftype /= 6)
        write(*,*) "Out of range! Please type again!"
        read(*,*) ftype
        end do 
        
    write(*,*) "Please type in minimum tower-field distance:"
    read(*,*) FDmin
    
    write(*,*) "Please type in Mirror number in x and y direction:"
    read(*,*) MirrorNox,MirrorNoy
    
    write(*,*) "Please type in Mirror distance in x and y direction:"
    read(*,*) MirrorDx,MirrorDy

    write(*,*) "Please type in Mirror increasing number in x direction:"
    read(*,*) MirrorDelta
    !……………………………………………………………………!
    
    
    !以下为输入单个定日镜取样光线数目的部分…………………!
    write(*,*) "Please type in sample number (Larger than 1000):"
    read(*,*) sampleN
    do while ( sampleN<1000)
        write(*,*) "Not big enough! Please type again:"
        read(*,*) sampleN
    end do 
    !……………………………………………………………………!
    
    
    !以下为定日镜编号部分…………………………………………! 
    mirrors = MirrorNox*(2*MirrorNoy+MirrorDelta*(MirrorNox-1))/2
    
    allocate(mirror(mirrors))
    allocate(IMS(mirrors,sampleN))
    allocate(RMS(mirrors,sampleN))
    
    mirrorN=0
    mirrorI=1
    mirrorJ=1
    
    do mirrorJ=1,MirrorNox
        do mirrorI=1,MirrorNoy+(mirrorJ-1)*MirrorDelta
            mirrorN=mirrorN+1
            mirror(mirrorN)%i=mirrorI
            mirror(mirrorN)%j=mirrorJ
        end do
    end do
    !……………………………………………………………………!
    
    
    !以下为计算定日镜位置坐标部分………………………………!
    if (ftype == 1) then
        do mirrorN=1,mirrors
            mirror(mirrorN)%x=((mirror(mirrorN)%j)-1)*MirrorDx+FDmin
            mirror(mirrorN)%y=((mirror(mirrorN)%i)-1)*MirrorDy
            mirror(mirrorN)%z=MirrorH
        end do
        farea=FDmin*0.5*MirrorDy*(MirrorNoy-1)+0.5*MirrorDx*(MirrorNox-1)*MirrorDy*(MirrorNoy-1+(MirrorNox-1)*MirrorDelta-1)
        
    else if (ftype == 2) then
         do mirrorN=1,mirrors
            mirror(mirrorN)%x=((mirror(mirrorN)%j)-1)*MirrorDx+FDmin
            if ((-mirror(mirrorN)%i)**2 >=0 ) then
                mirror(mirrorN)%y=((mirror(mirrorN)%i)-1)*MirrorDy + 0.5*MirrorDy
            else
                mirror(mirrorN)%y=((mirror(mirrorN)%i)-1)*MirrorDy
            end if
            mirror(mirrorN)%z=MirrorH
         end do
         farea=FDmin*0.5*MirrorDy*(MirrorNoy-1)+0.5*MirrorDx*(MirrorNox-1)*MirrorDy*(MirrorNoy-1+(MirrorNox-1)*MirrorDelta-1)
         
    else if (ftype == 3) then
        do while (MirrorDy/FDmin*(MirrorNoy+(MirrorNox-1)*MirrorDelta-1) > pi/2)
            write(*,*) "Unreasonable field design!Please type in minimum tower-field distance:"
            read(*,*) FDmin
            write(*,*) "Please type in Mirror number in x and y direction:"
            read(*,*) MirrorNox,MirrorNoy
            write(*,*) "Please type in Mirror distance in x and y direction:"
            read(*,*) MirrorDx,MirrorDy
            write(*,*) "Please type in Mirror increasing number in x direction:"
            read(*,*) MirrorDelta
        end do
        do mirrorN=1,mirrors
            mirror(mirrorN)%x=(((mirror(mirrorN)%j)-1)*MirrorDx+FDmin)*cos(((mirror(mirrorN)%i)-1)*MirrorDy/FDmin)
            mirror(mirrorN)%y=(((mirror(mirrorN)%j)-1)*MirrorDx+FDmin)*sin(((mirror(mirrorN)%i)-1)*MirrorDy/FDmin)
            mirror(mirrorN)%z=MirrorH
        end do
        farea=0.5*(FDmin+(MirrorNox-1)*MirrorDx)**2 * (MirrorNoy-1+(MirrorNox-1)*MirrorDelta)*MirrorDy/FDmin    
    
    else if (ftype == 4) then
        if ( (MirrorDelta*MirrorDy/MirrorDx) > (MirrorDy*(MirrorNoy-1)/FDmin) ) then
            do while (MirrorDy/(FDmin+(MirrorNox-1)*MirrorDx)*(MirrorNoy+(MirrorNox-1)*MirrorDelta-1) > pi/2)
                write(*,*) "Unreasonable field design!Please type in minimum tower-field distance:"
                read(*,*) FDmin
                write(*,*) "Please type in Mirror number in x and y direction:"
                read(*,*) MirrorNox,MirrorNoy
                write(*,*) "Please type in Mirror distance in x and y direction:"
                read(*,*) MirrorDx,MirrorDy
                write(*,*) "Please type in Mirror increasing number in x direction:"
                read(*,*) MirrorDelta
            end do
            farea=0.5*(FDmin+(MirrorNox-1)*MirrorDx)**2 * (MirrorNoy-1+(MirrorNox-1)*MirrorDelta)*MirrorDy/(FDmin+(MirrorNox-1)*MirrorDx)  
        else
            do while (MirrorDy/FDmin*(MirrorNoy+(MirrorNox-1)*MirrorDelta-1) > pi/2)
                write(*,*) "Unreasonable field design!Please type in minimum tower-field distance:"
                read(*,*) FDmin
                write(*,*) "Please type in Mirror number in x and y direction:"
                read(*,*) MirrorNox,MirrorNoy
                write(*,*) "Please type in Mirror distance in x and y direction:"
                read(*,*) MirrorDx,MirrorDy
                write(*,*) "Please type in Mirror increasing number in x direction:"
                read(*,*) MirrorDelta
            end do 
            farea=0.5*(FDmin+(MirrorNox-1)*MirrorDx)**2 * (MirrorNoy-1)*MirrorDy/FDmin             
        end if
        do mirrorN=1,mirrors
            mirror(mirrorN)%x=(((mirror(mirrorN)%j)-1)*MirrorDx+FDmin)*cos(((mirror(mirrorN)%i)-1)*MirrorDy/(FDmin+(mirror(mirrorN)%j-1)*MirrorDx))
            mirror(mirrorN)%y=(((mirror(mirrorN)%j)-1)*MirrorDx+FDmin)*sin(((mirror(mirrorN)%i)-1)*MirrorDy/(FDmin+(mirror(mirrorN)%j-1)*MirrorDx))
            mirror(mirrorN)%z=MirrorH
        end do
    
    else if (ftype == 5) then
         do mirrorN=1,mirrors
            mirror(mirrorN)%x=0
            mirror(mirrorN)%y=0
            mirror(mirrorN)%z=MirrorH
         end do
         farea=0
         
    else if (ftype == 6) then
         do mirrorN=1,mirrors
            mirror(mirrorN)%x=0
            mirror(mirrorN)%y=0
            mirror(mirrorN)%z=MirrorH
         end do
         farea=0
         
    end if
    
    !……………………………………………………………………!
    
    
    !以下为计算入射光线方向向量部分……………………………!
    do mirrorNN=1,mirrors
        do sampleNN=1,sampleN
            
            IMS(mirrorNN,sampleNN)%m = Gincidentm(sunelevation,sunazimuth)
            IMS(mirrorNN,sampleNN)%n = Gincidentn(sunelevation,sunazimuth)
            IMS(mirrorNN,sampleNN)%l = Gincidentl(sunelevation,sunazimuth)
            
        end do
    end do
    !……………………………………………………………………!
    
     
    !以下为计算定日镜表面法线向量部分…………………………!
    do mirrorN=1,mirrors
        
        mirror(mirrorN)%n=GmirrorNn(mirror(mirrorN)%x,mirror(mirrorN)%y,mirror(mirrorN)%z,IMS(mirrorN,1)%m,IMS(mirrorN,1)%n,IMS(mirrorN,1)%l)
        mirror(mirrorN)%m=GmirrorNm(mirror(mirrorN)%x,mirror(mirrorN)%y,mirror(mirrorN)%z,IMS(mirrorN,1)%m,IMS(mirrorN,1)%n,IMS(mirrorN,1)%l)
        mirror(mirrorN)%l=GmirrorNl(mirror(mirrorN)%x,mirror(mirrorN)%y,mirror(mirrorN)%z,IMS(mirrorN,1)%m,IMS(mirrorN,1)%n,IMS(mirrorN,1)%l)
        
    end do
    !……………………………………………………………………!

    
    
    !主程序循环，蒙特卡洛过程……………………………………!
    do mirrorNN=1,mirrors
        do sampleNN=1,sampleN
            
            call random_seed()
            e1=randomfunc(-1.05,1.05)
            e2=randomfunc(-1.35,1.35)
            e3=randomfunc(0.0,1.0)
            
            IMS(mirrorNN,sampleNN)%x = Gincidentx(mirror(mirrorNN)%m,mirror(mirrorNN)%n,mirror(mirrorNN)%l,mirror(mirrorNN)%x,e1,e2)
            IMS(mirrorNN,sampleNN)%y = Gincidenty(mirror(mirrorNN)%m,mirror(mirrorNN)%n,mirror(mirrorNN)%l,mirror(mirrorNN)%y,e1,e2)
            IMS(mirrorNN,sampleNN)%z = Gincidentz(mirror(mirrorNN)%m,mirror(mirrorNN)%n,mirror(mirrorNN)%l,mirror(mirrorNN)%z,e1,e2)
            
            IMS(mirrorNN,sampleNN)%ref = Refornot(Erefl,e3)
            
            if (IMS(mirrorNN,sampleNN)%ref == 1) then
                
                RMS(mirrorNN,sampleNN)%x = IMS(mirrorNN,sampleNN)%x
                RMS(mirrorNN,sampleNN)%y = IMS(mirrorNN,sampleNN)%y
                RMS(mirrorNN,sampleNN)%z = IMS(mirrorNN,sampleNN)%z
                
                RMS(mirrorNN,sampleNN)%m = Greflectm(mirror(mirrorNN)%m,mirror(mirrorNN)%n,mirror(mirrorNN)%l,IMS(mirrorNN,sampleNN)%m,IMS(mirrorNN,sampleNN)%n,IMS(mirrorNN,sampleNN)%l)
                RMS(mirrorNN,sampleNN)%n = Greflectn(mirror(mirrorNN)%m,mirror(mirrorNN)%n,mirror(mirrorNN)%l,IMS(mirrorNN,sampleNN)%m,IMS(mirrorNN,sampleNN)%n,IMS(mirrorNN,sampleNN)%l)
                RMS(mirrorNN,sampleNN)%l = Greflectl(mirror(mirrorNN)%m,mirror(mirrorNN)%n,mirror(mirrorNN)%l,IMS(mirrorNN,sampleNN)%m,IMS(mirrorNN,sampleNN)%n,IMS(mirrorNN,sampleNN)%l)
                RMS(mirrorNN,sampleNN)%fy = Gintersecy(RMS(mirrorNN,sampleNN)%x,RMS(mirrorNN,sampleNN)%y,RMS(mirrorNN,sampleNN)%z,RMS(mirrorNN,sampleNN)%m,RMS(mirrorNN,sampleNN)%n,RMS(mirrorNN,sampleNN)%l)
                RMS(mirrorNN,sampleNN)%fz = Gintersecz(RMS(mirrorNN,sampleNN)%x,RMS(mirrorNN,sampleNN)%y,RMS(mirrorNN,sampleNN)%z,RMS(mirrorNN,sampleNN)%m,RMS(mirrorNN,sampleNN)%n,RMS(mirrorNN,sampleNN)%l)
                
            else
                RMS(mirrorNN,sampleNN)%x = 0
                RMS(mirrorNN,sampleNN)%y = 0
                RMS(mirrorNN,sampleNN)%z = 0
                RMS(mirrorNN,sampleNN)%m = 0
                RMS(mirrorNN,sampleNN)%n = 0
                RMS(mirrorNN,sampleNN)%l = 0
                RMS(mirrorNN,sampleNN)%fy = 0
                RMS(mirrorNN,sampleNN)%fz = 0
                
            end if
            
            if ( RMS(mirrorNN,sampleNN)%x == 0 ) then
                unreflect=unreflect+1
                
            else
                call WhichNet(RMS(mirrorNN,sampleNN)%fy,RMS(mirrorNN,sampleNN)%fz)
                
            end if
            
        end do
    end do
    
    RayEff=(mirrors*sampleN-unfocus-unreflect)/(mirrors*sampleN)
    FieldEff=(mirrors*sampleN-unfocus-unreflect)/farea
    
    !……………………………………………………………………!
    
    
    
    !以下为输出结果到文件的部分…………………………………!
    open(fileid1,file="Output.txt")
    
    write(fileid1,"('Mirror Number:'I9';Sample Number:'I15/ )") mirrors,sampleN
    
    write(fileid1,"('Ray Efficiency:'I9';Field Efficiency:'I15/ )") RayEff,FieldEff
    
    write(fileid1,"('Unfocus Number:'I9';Unreflected Number:'I15/ )") unfocus,unreflect
    
    do oti=-9,10
        do otj=-9,10
            write(fileid1,"(I5,I5,I9/)") oti,otj, nets(oti,otj)
        end do
    end do
    !……………………………………………………………………!
    
    
    !以下为输出结果到图像的部分…………………………………!
    do ofi=-9,10                      !计算单个聚焦网格中最大光线数目
        do ofj=-9,10
            if (figurem < nets(ofi,ofj)) then
                figurem =  nets(ofi,ofj)
            end if
        end do
    end do
    
    result=SetWindow(.true.,-100,100,100,-100)
    
    result=Setbkcolorrgb(#FFFFFF)
    
    call ClearScreen($GClearScreen)
        
        do ofi=-9,10                   !对各个聚焦面网格进行不同亮度的实心矩形绘图
            do ofj=-9,10
                figuren =  nets(ofi,ofj)
                
                if (figuren <= (0.02*figurem)) then
                    result=Setcolorrgb(#FFFFFF)
                else if (figuren <= (0.12*figurem)) then
                    result=Setcolorrgb(#EEEEEE)
                else if (figuren <= (0.24*figurem)) then
                    result=Setcolorrgb(#CCCCCC)
                else if (figuren <= (0.36*figurem)) then
                    result=Setcolorrgb(#AAAAAA)
                else if (figuren <= (0.48*figurem)) then
                    result=Setcolorrgb(#888888)
                else if (figuren <= (0.6*figurem)) then
                    result=Setcolorrgb(#666666)
                else if (figuren <= (0.72*figurem)) then
                    result=Setcolorrgb(#444444)
                else if (figuren <= (0.84*figurem)) then
                    result=Setcolorrgb(#222222)
                else if (figuren <= figurem) then
                    result=Setcolorrgb(#000000)
                end if
                
                result=Rectangle_w($GFillinterior,ofi,ofj,ofi-1,ofj-1)
                
            end do
        end do
    
    stop
    !……………………………………………………………………!
    
end program
    
