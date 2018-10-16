module figure
    use sgl
    implicit none
    
    integer :: figuren=0
    integer :: figurem=0
    integer ofi
    integer ofj
    
    integer :: nets(-9:10,-9:10)      !聚焦面网格数组，对应数值为聚焦到该网格中的光线数目
    common /net/ nets
    
    do ofi=-9,10                      !计算单个聚焦网格中最大光线数目
        do ofj=-9,10
            if (figurem < nets(ofi,ofj)) then
                figurem =  nets(ofi,ofj)
        end do
    end do
    

    call sglClearBuffer()          !清除屏幕
        
        do ofi=-9,10                   !对各个聚焦面网格进行不同亮度的实心矩形绘图
            do ofj=-9,10
                figuren =  nets(ofi,ofj)
                
                if (figuren <= 0.02*figurem) then
                    call sglColor3f(0.02,0.02,0)
                else if (figuren <= 0.1*figurem) then
                    call sglColor3f(0.1,0.1,0)
                else if (figuren <= 0.2*figurem) then
                    call sglColor3f(0.2,0.2,0)
                else if (figuren <= 0.3*figurem) then
                    call sglColor3f(0.3,0.3,0) 
                else if (figuren <= 0.4*figurem) then
                    call sglColor3f(0.4,0.4,0)
                else if (figuren <= 0.5*figurem) then
                    call sglColor3f(0.5,0.5,0)
                else if (figuren <= 0.6*figurem) then
                    call sglColor3f(0.6,0.6,0)
                else if (figuren <= 0.7*figurem) then
                    call sglColor3f(0.7,0.7,0)
                else if (figuren <= 0.8*figurem) then
                    call sglColor3f(0.8,0.8,0)
                else if (figuren <= 0.9*figurem) then
                    call sglColor3f(0.9,0.9,0)
                else if (figuren <= figurem) then
                    call sglColor3f(1.0,1.0,0)
                end if
                
                call sglFilledRect(ofi,ofj,ofi-1,ofj-1)
                
            end do
        end do
        
    call sglSetVisual(-1.0,1.0,1.0,-1.0)    !建立虚拟坐标
    call sglCreateWindow(100,100,400,400,1)
    call sglMainLoop()
  
    stop
    
    end module
    
    