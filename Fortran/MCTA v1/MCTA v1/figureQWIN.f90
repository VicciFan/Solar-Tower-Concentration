
    !以下为输出结果到图像的部分…………………………………!
    do ofi=-9,10                      !计算单个聚焦网格中最大光线数目
        do ofj=-9,10
            if (figurem < nets(ofi,ofj)) then
                figurem =  nets(ofi,ofj)
            end if
        end do
    end do
    
    call Setviewport(-100,100,100,-100)
    Result=Setwindow(.true.,-100,-100,100,100)
    
    call Clearscreen($gclearscreen)          !清除屏幕
    
    Result=Setbkcolorrgb(#FFFFFF)
        
        do ofi=-9,10                   !对各个聚焦面网格进行不同亮度的实心矩形绘图
            do ofj=-9,10
                figuren =  nets(ofi,ofj)
                
                if (figuren <= 0.02*figurem) then
                    Result=Setcolorrgb(#FFFFFF)
                else if (figuren <= 0.12*figurem) then
                    Result=Setcolorrgb(#EEEEEE)
                else if (figuren <= 0.24*figurem) then
                    Result=Setcolorrgb(#CCCCCC)
                else if (figuren <= 0.36*figurem) then
                    Result=Setcolorrgb(#AAAAAA)
                else if (figuren <= 0.48*figurem) then
                    Result=Setcolorrgb(#888888)
                else if (figuren <= 0.6*figurem) then
                    Result=Setcolorrgb(#666666)
                else if (figuren <= 0.72*figurem) then
                    Result=Setcolorrgb(#444444)
                else if (figuren <= 0.84*figurem) then
                    Result=Setcolorrgb(#222222)
                else if (figuren <= figurem) then
                    Result=Setcolorrgb(#000000)
                end if
                
                Result=Rectangle(1,ofi,-ofj,ofi-1,-ofj+1)_w(1,ofi,ofj,ofi-1,ofj-1)
                
            end do
        end do
    
    stop
    !……………………………………………………………………!