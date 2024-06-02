      SUBROUTINE hpsort(N,VECTOR)
                     
      REAL*8 VECTOR(N), X
      
      IF (N.LT.2) RETURN
      
      L=N/2+1
      K=N
                     
      DO WHILE (.TRUE.)
      
         IF (L.GT.1) THEN
              L=L-1
              X=VECTOR(L)
         ELSE 
              X=VECTOR(K)
              VECTOR(K)=VECTOR(1)
              K=K-1
              
              IF (K.EQ.1) THEN
                  VECTOR(1)=X
                  RETURN
              END IF
              
          END IF
          
          I=L 
          J=L+L
          DO WHILE (J.LE.K)
          
              IF (J.LT.K) THEN 
                  IF (VECTOR(J).LT.VECTOR(J+1)) J=J+1
              END IF
              
              IF (X.LT.VECTOR(J)) THEN
                  VECTOR(I)=VECTOR(J)
                  I=J 
                  J=J+J
              ELSE 
                  J=K+1
              END IF 
           
          END DO 
          VECTOR(I)=X
              
      END DO

      END