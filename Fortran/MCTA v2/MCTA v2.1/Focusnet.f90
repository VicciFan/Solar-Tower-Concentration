module netdef                               !!定义聚焦面网格及参数
    implicit none
    
    integer :: nets(-9:10,-9:10)      !聚焦面网格数组，对应数值为聚焦到该网格中的光线数目
    common /net/ nets
    data nets /400*0/
    
    integer :: unfocus                  !未聚焦到聚焦网格的光线数目
    common /unfocu/ unfocus
    data unfocus /0/
    
    integer :: unreflect                !未发生发射的光线数目
    common /unreflec/ unreflect
    data unreflect /0/
    
    end module

subroutine WhichNet(y,z)                    !!判断反射光线的聚焦点所在的网格编号，并记录光线数
    implicit none
    real :: y,z                      !反射光线聚焦点坐标
    integer :: neti = 0
    integer :: netj = 0

    integer :: nets(-9:10,-9:10)
    common /net/ nets
    
    integer :: unfocus
    common /unfocu/ unfocus
    
    real :: Htower
    common /Htowe/ Htower
    
        neti=1 + floor(y/0.1)
        netj=1 + floor((z-Htower)/0.1)
    
    if (neti<=10 .and. neti>=(-9) .and. netj<=10 .and. netj>=(-9)) then
        nets(neti,netj)=nets(neti,netj)+1
    else
        unfocus=unfocus+1
    end if
    
    return
    end