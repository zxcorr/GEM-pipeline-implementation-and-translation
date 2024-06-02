      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      ! this subroutine obtains the kinematic status of the GEM dish 
      ! when displaying an expected number of numframes per rotation. 
      ! In particular, it passes the value of dpf (degrees per frame) 
      ! to the calling program in order to predict the azimuth of the 
      ! NS firings.  
      !&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
         
      SUBROUTINE sakbih(gat,fpd,numframes) 
      
          INTEGER*4   jcorr(200)
         
          REAL*8      time,azimut,ttime(200),fpd,speed(500),dspeed(500),
     &                aazmean,aazimut(200),slopemean,timemean,sigma,
     &                sigman,sigint,compaz,slope,dslope,
     &                sig(200),firstazi,midazi,lastazi
      
          LOGICAL     first,dangling
      
          CHARACTER   gat*16													! use 17-bit names for yyyy > 1999 and 16-bit for < 2000
 

          
      !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      
          open(1, file  = '/home/nsoares/GEM/2300mhz/dat/'//gat//'.dat',
     &            status  = 'old', err = 200)                          
     
          j   =   0
          k   =   0 
          l   =   0
          ic  =   0  
          j105    =   0
          j106    =   0
          timemean    =   0.
          aazmean     =   0.
          first       =   .TRUE.
          dangling    =   .FALSE.
      
          DO WHILE (.NOT.EOF(1))
      
              READ(1,*) jframe, time, x, x, azimut 
          
              IF (first) THEN
              
                  firsttime   = time      ! Get a reference in time and
                  compaz      = azimut    ! direction.
                  first       = .FALSE.
                  
              END IF 
              
              IF ((ABS(azimut-compaz).LT.176.)) THEN
              
                  l = l + 1                           
                  
                  IF ((l.GT.1).AND.((time - ttime(l-1)).LT.0)) THEN
                  
                      ttime(l)   = time + 86400
                      
                  ELSE
                      
                      ttime(l)   = time
                      
                  END IF
                   
                  aazimut(l)  =   azimut
                  timemean    =   timemean + ttime(l)
                  aazmean     =   aazmean + azimut
                  sig(l)      =   1.
                  
                  IF ((azimut-compaz).GT.0D0) THEN
                  
                      IF (l.GE.(numframes-1)) THEN
                  
                          dangling    =   .TRUE.  
                          
                      ELSE
                      
                          ic  =   ic  +   1      
                          
                          jcorr(ic)   =   l      
                          
                      END IF
                      
                  END IF
                  
              ELSE
                  
                  IF (l.GE.(numframes-1)) THEN
                  
                      IF (l.EQ.(numframes-1)) j105 = j105 + 1
                      
                      IF (l.EQ.numframes) j106 = j106 + 1
              
                      j = j + 1  
                      
                      IF (dangling) THEN
                          
                          mm  =   1
                          
                          CALL degperfr(timemean,aazmean,ttime,aazimut,
     &                                 sig,slope,dslope,azimut,l,mm)
     
                          lastazi     =   aazimut(l-2) 
													 
						midazi      =   aazimut(l-1) 
													  
						firstazi    =   aazimut(l)    

                          l2  =   1
                          
                      ELSE
                      
                          lastazi     =   aazimut(l)
                      
                          mm  =   0
                          
                          CALL degperfr(timemean,aazmean,ttime,aazimut,
     &                                 sig,slope,dslope,azimut,l,mm)

                          midazi      =   aazimut(l)

                          firstazi    =   aazimut(l+1)                         

                          l2  =   2 
                          
                      END IF
                      
                      speed(j)    = slope
                      dspeed(j)   = dslope
                      
                      IF (ic.GT.0) THEN
                      
                          DO i = 1,ic
                          
                              aazimut(jcorr(i))   =  aazimut(jcorr(i)-1) 
              
     &                                                + slope*0.56002D0

							IF (aazimut(jcorr(i)).LT.0.D0) 
     &							aazimut(jcorr(i)) =	aazimut(jcorr(i)) +	360.D0
     							
                              
                          END DO                     
                          
                      END IF
                      
                  ELSE
                  
                      PRINT *,' incomplete rotation of ',l,' frames'
					PRINT *,' '
					                      
                      IF (ic.GT.0) THEN  
                      
                          mm  =   1
                          
                          CALL degperfr(timemean,aazmean,ttime,aazimut,
     &                                 sig,slope,dslope,azimut,l,mm)

                          l2  =   1
                          
                      ELSE
                      
                          mm  =   0
                          
                          CALL degperfr(timemean,aazmean,ttime,aazimut,
     &                                 sig,slope,dslope,azimut,l,mm) 
                      
                          l2  =   2
                          
                      END IF
                      
                  END IF                                    
                  
                  fpd =   slope*.56002D0
                  
                  DO i = 1,l+l2 
                  
                      IF ((i.GT.1).AND.
     &				((ABS(aazimut(i)-aazimut(i-1)).LT.ABS(fpd/2)).OR.
     &				 (ABS(aazimut(i)-aazimut(i-1)).GT.ABS(3*fpd/2)))) 
     &				THEN
                      
                          aazimut(i)  =   aazimut(i-1) + fpd
                            
                      END IF
                      
                  END DO
                                
                  l   =   0
                  ic  =   0
                  timemean    = 0.
                  aazmean     = 0.
                  dangling    = .FALSE.
              
              END IF
              
              compaz = azimut
           
          END DO 

          DO i = 1,l				! last incomplete rotation  
                  
			IF ((i.GT.1).AND.
     &		((ABS(aazimut(i)-aazimut(i-1)).LT.ABS(fpd/2)).OR.
     &		 (ABS(aazimut(i)-aazimut(i-1)).GT.ABS(3*fpd/2)))) THEN
                      
				aazimut(i)  =   aazimut(i-1) + fpd
                            
			END IF
                              
          END DO
          
          wt          = 0.                    
          slopemean   = 0.
            
          DO i = 1,j                          
              w           = 1./dspeed(i)**2
              slopemean   = slopemean + speed(i)*w
              wt          = wt + w
          END DO 
      
          slopemean   = slopemean/wt

		sigma	=	0.D0
      
          DO i = 1,j
              w       = 1./dspeed(i)**2
              sigma   = sigma + w*(speed(i) - slopemean)**2    
          END DO 
      
          sigma   = SQRT(sigma/wt*j/(j-1))
          sigman  = sigma/SQRT(j*1.)
          sigint  = SQRT(1./wt)                    
          Z       = sigman/sigint
          dZ      = 1./SQRT(2.*(j-1))
          
          fpd     = slopemean*.56002D0
      
          
  200 CLOSE(1)
      
      RETURN
      
      END 