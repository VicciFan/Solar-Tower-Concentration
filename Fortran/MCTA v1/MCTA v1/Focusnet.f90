module netdef                               !!����۽������񼰲���
    implicit none
    
    integer :: nets(-9:10,-9:10)      !�۽����������飬��Ӧ��ֵΪ�۽����������еĹ�����Ŀ
    common /net/ nets
    data nets /400*0/
    
    integer :: unfocus                  !δ�۽����۽�����Ĺ�����Ŀ
    common /unfocu/ unfocus
    data unfocus /0/
    
    integer :: unreflect                !δ��������Ĺ�����Ŀ
    common /unreflec/ unreflect
    data unreflect /0/
    
    end module

subroutine WhichNet(y,z)                    !!�жϷ�����ߵľ۽������ڵ������ţ�����¼������
    implicit none
    real :: y,z                      !������߾۽�������
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