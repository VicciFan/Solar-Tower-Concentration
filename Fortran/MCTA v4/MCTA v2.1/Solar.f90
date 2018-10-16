module cptsolarangle
    implicit none 
    
    contains
    
    real function Gazimuth(lati,soltime,dayn)
        real lati,soltime
        integer dayn
        real zenith
        real hour
        real declination
        real sgn
        
        declination=asin(0.39795*cos(2*3.14*(dayn-173)/365))
        hour=3.14/12*(soltime-12)
        zenith=acos(cos(lati)*cos(declination)*cos(hour)+sin(lati)*sin(declination))
        
        if (hour > 0) then
            sgn=1
        else if (hour == 0) then
            sgn=0
        else
            sgn=-1
        end if
        
        Gazimuth=sgn*sqrt((acos((cos(zenith)*sin(lati)-sin(declination))/(sin(zenith)*cos(lati))))**2)
        
    end function
    
    real function Gelevation(lati,soltime,dayn)
        real lati,soltime
        real zenith
        integer dayn
        real hour
        real declination
        
        declination=asin(0.39795*cos(2*3.14*(dayn-173)/365))
        hour=3.14/12*(soltime-12)
        zenith=acos(cos(lati)*cos(declination)*cos(hour)+sin(lati)*sin(declination))
        Gelevation=3.1415926/2-zenith
        
    end function
    
    real function Gdaylength(lati,dayn)
        real lati
        real declination
        integer dayn
        
        declination=asin(0.39795*cos(2*3.14*(dayn-173)/365))
        Gdaylength=24/3.14*acos(-tan(3.14*lati/180)*tan(declination))
        
    end function
    
    end module
    