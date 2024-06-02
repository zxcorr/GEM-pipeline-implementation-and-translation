      SUBROUTINE tagging(jfil,fil) 
      
          CHARACTER   fil*8,lett(8)*1,lettnum(10)*1 
          
          DATA        (lettnum(i), i=1,10) /'1','2','3','4','5','6','7',
     &                                  '8','9','0'/
      
          
          jaux    =   jfil

          DO i = 1,7

              jlett   =   jaux/10**(8 - i)

              IF (jlett.EQ.0) jlett   =   10

              DO k = 1,10

                  IF (jlett.EQ.k) lett(i) =   lettnum(k)

              END DO

              IF (jlett.NE.10) jaux   =   jaux    -   jlett*10**(8 - i)

          END DO

          DO k = 1,10

              IF (jaux.EQ.0) jaux =   10

              IF (jaux.EQ.k) lett(8)    =  lettnum(k)

          END DO

          fil     =   lett(1)

		DO i = 2,8
			
			fil	=	TRIM(fil)//lett(i)
			
		END DO

      RETURN
      
      END