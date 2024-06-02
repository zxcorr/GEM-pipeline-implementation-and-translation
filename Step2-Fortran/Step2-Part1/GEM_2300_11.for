      !*****************************************************************
      ! this program has been developed from sakbiha.for in order to fix
      ! the Vmaxaz parameter in du2phy2.for and further to correct the 
      ! presence of dangling azimuth values (a kind of retrograde motion
      ! apparent when the encoder scale has to reset from Vmin to Vmax,
      ! supposedly when it goes from 0 V to 10 V, or sometimes for no 
      ! apparent reason at all). This implementation forces kukulkan.for 
      ! to abandon the call to subroutine sakbih.for, but moves the 
      ! reference for absolute calibration (parameter azoff) from 
      ! du2phy2.for to it. Besides, it will have to read the azimuth as
      ! a separate 1-column input. In addition, for the 1998 encoder, 
	! this program corrects for a faulty azimuth appearing around 240 
	! degrees, which would make the dpf value too small.
	! Note 1: when the 1999 unit was installed, a double dangling case
	! began to appear at regular intervals. For this matter, this 
	! version of azimuth.for allows this case to be handled according
	! to a slight modification in the dpf subroutine together with
	! the apropriate labeling of last-mid-and-firstazi.	The data
	! parameter numframes also goes from 106 to 105 for that reason.
	! Note 2: a faulty dpfazi assignment was corrected on 30/12/99, 
	! along with a comparison check for making dpfazi within 7/8 of 
	! fpd inside dpf subroutine.
	! 16/08/2003	: 9-bit names are used for 2000+ filenames.
	!				: numframes = 104 for no-dangling encoder readings
	!				: The default is 105 for 1999 data.
	! 16/06/2005	numframes has to be set to the highest number possible
	!				without creating incomplete rotations except at the  
	!				end of the file.
	! 28/09/2005	missing frames in Colombian data required that the
	!				azimuth allocation would not be considered bad readings
      !*****************************************************************	 
      !$DEBUG
	INTEGER*4   numfile(400),jcorr(200)      
         
      REAL*8      time,azimut,ttime(200),fpd,speed(500),dspeed(500),
     &			aazmean,aazimut(200),slopemean,timemean,sigma,
     &            sigman,sigint,compaz,slope,dslope,dpfmean,sigdpf,slp,
     &            sig(200),firstazi,midazi,lastazi,dpfazi(200),Vslp,Vmax
	LOGICAL     first,dangling
      
      CHARACTER   filenum*8,datetag*8,dayfile(400)*8,tag*16,filelist*9		! use 9-bit names for yyyy > 1999 and 8-bit for < 2000
C      CHARACTER   filenum*8,datetag*9,dayfile(400)*9,tag*17,filelist*9		! use 9-bit names for yyyy > 1999 and 8-bit for < 2000
 
      DATA        numframes /105/												! use 106 for 1998 data; use 105 for 1999 data; 
      
      PRINT *,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%'
      PRINT *,'                  GEM 1 : ENCODER AZIMUTH CORRECTION'
      PRINT *,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%%%%%%'
      PRINT *,' '                           

	PRINT *,' Single file    : [ -> 1]'
	PRINT *,' Multiple files : [ -> 2]'
	READ(*,*) jlist

	IF (jlist.EQ.1) THEN
	
		PRINT *,' Enter the date of the year   [yy_MM_dd] : '
		READ(*,*) datetag
      
		PRINT *,' Enter the UT time of the day [hhmmssml] : '
		READ(*,*) jfilenum

		jj	=	1

	ELSE

		PRINT *,' ENTER the file list name >'
		READ(*,*) filelist

		OPEN(1, file    = '/home/nsoares/GEM/2300mhz/txt/'
     &                    //filelist//'.txt', 
     &			status  = 'old',
     &			err     = 200)

          OPEN(4, file   = '/home/nsoares/GEM/2300mhz/rots/dpf'
     &     //filelist//'.dat', 
     &            status  = 'unknown',
     &            err     = 200)

		jj	=	0

		DO WHILE (.NOT.EOF(1))

			jj	=	jj	+	1

			READ(1,*) dayfile(jj),numfile(jj)

		END DO

		CLOSE(1)

	END IF

	PRINT *,' SET Vmax?'
	PRINT *,'    1 -> YES'
     	PRINT *,'    2 ->  NO'
	READ(*,*) jset

	IF (jset.EQ.1) THEN

		PRINT *,' Vmax:'
		READ(*,*) Vmax

	ELSE

C		Vmax	=	10.0035			! 1995 in Colombia
C
C		Vmax	=	9.89485			! 1998 Vmax
C
		Vmax	=	9.8993			! 1999 Vmax +/- 0.0024
C
C		Vmax	=	9.7423			! 2003 Vmax

	END IF

	PRINT *,' SET slp rate?'
	PRINT *,'    1 -> YES'
     	PRINT *,'    2 ->  NO'
	READ(*,*) jset

	IF (jset.EQ.1) THEN

		PRINT *,' slp:'
		READ(*,*) slp

	ELSE

C		slp	=	18.20457			! iterated 1998 Vmax

		slp	=	17.0357				! iterated 1999 Vmax +/- 0.3918

	END IF

	PRINT *,' ENTER the expected number of frames/scan sequence :'
	PRINT *,' [1994 > 106; 1999 > 105; 2005 > 103,104]'
	READ(*,*) numframes
	PRINT *,' '


	DO ll = 1,jj

		IF (jlist.EQ.2) THEN

			jfilenum	=	numfile(ll)

			datetag		=	dayfile(ll)

		END IF

		CALL tagging(jfilenum,filenum)

		tag	=	datetag//filenum

	PRINT *,' '
      PRINT *, ' Doing file : '//tag//'.txt'                      
                                  
      open(1, file    = '/home/nsoares/GEM/2300mhz/dat/'
     &    //tag//'.dat', 
     &        status  = 'old',
     &        err     = 200)
      open(2, file    = '/home/nsoares/GEM/2300mhz/rots/'
     &    //tag//'rot.dat',		
     &		status  = 'unknown', 
     &		err     = 200) 
      open(3, file    = '/home/nsoares/GEM/2300mhz/azimuth/'
     &    //tag//'azi.dat',
     &		status  = 'unknown',
     &        err     = 200)

      !:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      
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
                  
                  ! this is the expected behavior, except for the very 
                  ! last azimut which may differ from the previous one
                  ! by < 176 and yet show reversal in sense of 
                  ! direction. The usual is a continuous decrease in 
                  ! azimut. An unexpected sign reversal can also happen.
                  
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
                  
                          dangling    =   .TRUE.  ! sign reversal in
                                                  ! last azimut detected
                          
                      ELSE
                      
                          ic  =   ic  +   1       ! unexpected sign 
                          
                          jcorr(ic)   =   l       ! reversal detected
                          
                      END IF
                      
                  END IF
                  
              ELSE
                  
                  IF (l.GE.(numframes-1)) THEN
                  
                      IF (l.EQ.(numframes-1)) j105 = j105 + 1
                      
                      IF (l.EQ.numframes) j106 = j106 + 1
              
                      j = j + 1  
                      
C                      PRINT *,' ',j,' ',l

                      ! In case the sign reversal had already occured
                      ! in the previous frame but the magnitude fell 
                      ! short of 176, we delete it from the mean (mm=1)
					! double-dangling forces l to increase by 1. Even
					! when lastazi is really only the l-1 element, mid
					! azi serves just as well as firstazi to set dpfazi
                      
                      IF (dangling) THEN
                          
                          mm  =   1
                          
                          CALL dpf(timemean,aazmean,ttime,aazimut,sig,
     &                             slope,dslope,azimut,l,mm)
     
                          lastazi     =   aazimut(l-2) 
													 
						midazi      =   aazimut(l-1) 
													  
						firstazi    =   aazimut(l)    
													 
						l2  =   1                    

                      ELSE
                      
                          lastazi     =   aazimut(l)
                                                
                          mm  =   0
                          
                          CALL dpf(timemean,aazmean,ttime,aazimut,
     &                                sig,slope,dslope,azimut,l,mm)

                          midazi      =   aazimut(l)

                          firstazi    =   aazimut(l+1)                         

                          l2  =   2 
                          
                      END IF
                      
                      speed(j)    = slope
                      dspeed(j)   = dslope
                      
                      ! If the Vmax used is correct, then midazi will be
                      ! spaced evenly from lastazi to firstazi according
                      ! to the prevailing slope. So we check and correct
                      
                      dpfazi(j)   =   (360.D0 + lastazi - firstazi)/2.D0    
                      
                      IF (ic.GT.0) THEN
                      
                          DO i = 1,ic
                          
                              aazimut(jcorr(i))   =  aazimut(jcorr(i)-1) 
              
     &                                                + slope*0.56002D0

							IF (aazimut(jcorr(i)).LT.0.D0) 
     &							aazimut(jcorr(i))	=	
     &							aazimut(jcorr(i))	+	360.D0	
                              
                          END DO                     
                          
                      END IF
                      
      WRITE(2,50) j,slope,dslope,lastazi,midazi,firstazi,dpfazi(j)
   
                  ELSE
                  
                      PRINT *,' incomplete rotation of ',l,' frames'

                      IF (ic.GT.0) THEN  
                      
                          mm  =   1
                          
                          CALL dpf(timemean,aazmean,ttime,aazimut,
     &                                sig,slope,dslope,azimut,l,mm)                

                          l2  =   1
                          
                      ELSE
                      
                          mm  =   0
                          
                          CALL dpf(timemean,aazmean,ttime,aazimut,
     &                                sig,slope,dslope,azimut,l,mm) 
                      
                          l2  =   2
                          
                      END IF
                      
                  END IF  
				
                  fpd =   slope*.56002D0
                  
                  DO i = 1,l+l2 

                      IF (i.GT.1) THEN
					
						IF ( ( ( ABS(aazimut(i) - aazimut(i-1))
     &							.LT.ABS(fpd/2) )
     &						.OR.( ABS(aazimut(i) - aazimut(i-1))
     &							.GT.ABS(3*fpd/2) ) )
     &					.AND.((ttime(i)-ttime(i-1)).LT.(3*.56002/2)) )		! there are  real holes in the Colombian data
     &					THEN
                      
							aazimut(i)  =   aazimut(i-1) + fpd

						END IF

					END IF
                      
      WRITE(3,75) aazimut(i)
                              
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

			IF (i.GT.1) THEN
                  
				IF ( ( ( ABS(aazimut(i) - aazimut(i-1))
     &					.LT.ABS(fpd/2) )
     &				.OR.( ABS(aazimut(i) - aazimut(i-1))
     &					.GT.ABS(3*fpd/2) ) )
     &			.AND.((ttime(i)-ttime(i-1)).LT.(3*.56002/2)) )				! there are  real holes in the Colombian data
     &			THEN
                      
					aazimut(i)  =   aazimut(i-1) + fpd
                            
				END IF

			END IF

      WRITE(3,75) aazimut(i)      ! assumes same dpf as previous one.
                              
          END DO

      CLOSE(2)
      CLOSE(3)
      
          wt          = 0.                    
          slopemean   = 0.
          dpfmean     = 0.
            
          DO i = 1,j                          
              w           = 1./dspeed(i)**2
              slopemean   = slopemean + speed(i)*w
              wt          = wt + w 
              dpfmean     = dpfmean + dpfazi(i)
          END DO 
      
          slopemean   = slopemean/wt 
          dpfmean     = dpfmean/j 
          
          sigma   =   0.
          sigdpf  =   0.
      
          DO i = 1,j
              w       = 1./dspeed(i)**2
              sigma   = sigma + w*(speed(i) - slopemean)**2    
              sigdpf  = sigdpf + (dpfazi(i) - dpfmean)**2
          END DO 
      
          sigma   = SQRT(sigma/wt*j/(j-1))
          sigman  = sigma/SQRT(j*1.)
          sigint  = SQRT(1./wt)                    
          Z       = sigman/sigint
          dZ      = 1./SQRT(2.*(j-1))
          
          sigdpf  = SQRT(sigdpf/(j-1))/SQRT(j*1.D0)    !  standard error
          
   50 FORMAT(I5,6(F12.6))
   
   75 FORMAT(F12.6)
   
      PRINT *,' '           
      PRINT *,':::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     &::::::::::::::::::::'
      PRINT *,'              Rotation Statistics for File : '//filenum//
     &'.dat'
      PRINT *,' '        
      
          WRITE(*,101) slopemean
  101 FORMAT(' MEAN dish rotation speed   : ',F9.6,' deg/sec')
          WRITE(*,102) sigman
  102 FORMAT('      external error        : ',F9.6,' deg/sec
     &rotations')            
          WRITE(*,103) sigma
  103 FORMAT('      1-sigma dispersion    : ',F9.6,' deg/sec            
     &   of') 
          WRITE(*,104) sigint
  104 FORMAT('      internal error        : ',F9.6,' deg/sec           1 
     &05 frames')
          WRITE(*,105) Z,dZ,j105
  105 FORMAT('      Z-statistic           : ',F6.3,' +/- ',F6.3,'       
     &      => ',I3,'     (*)')
          PRINT *,' '     
      
          fpd     = slopemean*.56002D0
      
          WRITE(*,106) fpd,dpfmean,sigdpf
  106 FORMAT(' Average dpf speed          : ',F9.6,' deg/frame [',F8.6,'
     & +/- ',F8.6,']')
          WRITE(*,107) sigman*.56002D0
  107 FORMAT('         external error     : ',F9.6,' deg/frame        ro
     &tations of')
          WRITE(*,108) sigma*.56002D0
  108 FORMAT('         1-sigma dispersion : ',F9.6,' deg/frame         1
     &06 frames')
          WRITE(*,109) sigint*.56002D0,j106
  109 FORMAT('         internal error     : ',F9.6,' deg/frame         
     & => ',I3)
          PRINT *,' '  
          
          WRITE(*,110) 360.D0/slopemean
  110 FORMAT(' Average rotation period    : ',F10.5,' seconds')
          WRITE(*,111) 360.D0*sigman/slopemean**2
  111 FORMAT('         external error     : ',F10.5,' seconds      (*) F
     &irst frame')
          WRITE(*,112) 360.D0*sigma/slopemean**2
  112 FORMAT('         1-sigma dispersion : ',F10.5,' seconds           
     &   NOT ')
          WRITE(*,113) 360.D0*sigint/slopemean**2
  113 FORMAT('         internal error     : ',F10.5,' seconds
     & INCLUDED')
     
          PRINT *,' '    
          
          Vslp    =   Vmax + (ABS(fpd) - dpfmean)/slp
      
		IF (jlist.EQ.2) WRITE(4,120)   j,fpd,(sigma*.56002D0),dpfmean,
     &									sigdpf,(ABS(fpd)-dpfmean),Vslp
     
  120 FORMAT(I4,6F12.6)
          
          CLOSE(1)              
  
      END DO
      
      CLOSE(4)
      
      
  200 STOP '%%%%%%%%%%%%%%%%%%%%%%%%%% GEM 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%
     &%%%%%%%%%%%%%%%'
     
	END

      SUBROUTINE dpf(tmean,zmean,ttim,aazi,sigg,slop,dslop,azi,n,m)
      
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

	INCLUDE 'tagging.for'

      INCLUDE 'fitlin.for'
      
      INCLUDE 'gammq.for'
      
      INCLUDE 'gser.for'

      INCLUDE 'gcf.for'

      INCLUDE 'gammln.for' 
