      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      ! This subroutine determines the rotation speed of the dish from
      ! a linear fit to encoder readings during one rotation.
	! First modified on 01/06/99 to deal with double-dangling in 1999
	! encoder.
	! 16/08/03	: note 2 from azimuth.for included.
      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      
      SUBROUTINE degperfr(tmean,zmean,ttim,aazi,sigg,slop,dslop,azi,n,m)
      
          REAL*8  tmean,zmean,ttim(200),aazi(200),sigg(200),zer,dzer,
     &            slop,dslop,sigm,azi,aux
      
              IF (m.EQ.0) THEN
                    
                  tmean   =   tmean/n
                  zmean   =   zmean/n
                  
                  READ(1,*) jframe, x, x, x, azi
                  
                  aazi(n+2-m)    =   azi
                  
              ELSE
              
                  tmean   =   (tmean - ttim(n))/(n-1)
                  zmean   =   (zmean - aazi(n))/(n-1)
                  
                  aazi(n+2-m)    =   azi
                  
              END IF
                          
              DO i = 1,n-m
                          
                  ttim(i) =   ttim(i) - tmean
                  aazi(i) =   aazi(i) - zmean
                              
              END DO
              
			IF ((n-m).GT.2) THEN
                  
				mwt = 0                      
                      
				CALL fitlin(ttim,aazi,(n-m),sigg,mwt,zer,slop,dzer,
     &						dslop,sigm,ch2,q)

			ELSE

				slop	=	0.D0

				dslop	=	0.D0

			END IF
     
              DO i = 1,n-m
                          
                  aazi(i) =   aazi(i) + zmean
                              
              END DO 
                          
			aux	= azi
			
			READ(1,*) jframe, x, x, x, azi

			IF ((aazi(n+2-m).LE.azi).OR.
     &			(ABS(aazi(n+2-m)-azi).LT.ABS(slop*.56002D0/8*7))) THEN
			
				dstep	=	(360.D0 + aazi(n-m) - azi)/3.D0	
					
				DO i = 1,3 
				
					aazi(n+i-m)    =   aazi(n-m) - i*dstep

					IF (aazi(n+i-m).LT.0.D0) aazi(n+i-m) = aazi(n+i-m)
     &														+ 360.D0
				END DO

			ELSE

				aazi(n+1-m) =  aazi(n-m) - (360.D0 + aazi(n-m) 
     &											- aazi(n+2-m))/2.D0
				IF(aazi(n+1-m).LT.0.D0) aazi(n+1-m) = aazi(n+1-m)
     &												          + 360.D0
				aazi(n+3-m) =  azi
				
			END IF

			n	=	n	+	1
               
      RETURN
      
      END