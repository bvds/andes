*     DGESVD Example Program Text
*     NAG Copyright 2005.
      IMPLICIT NONE
*     .. Parameters ..
      INTEGER          NIN, NOUT, NIND
      PARAMETER        (NIN=5,NOUT=6,NIND=11)
      INTEGER          MMAX, NB, NMAX
*               NB is just a size for the work array
      PARAMETER        (MMAX=10000,NB=64,NMAX=5000)
      INTEGER          LDA, LDVT, LWORK
      PARAMETER        (LDA=MMAX,LDVT=NMAX,
     +                 LWORK=MMAX+4*NMAX+NB*(MMAX+NMAX))
      DOUBLE PRECISION ONE, ZERO
      PARAMETER        (ONE=1.0,ZERO=0.0)
*     .. Local Scalars ..
      DOUBLE PRECISION EPS, SERRBD, ELE, MIND, CUTOFF, ESTVAR,
     +                 YMAG
      INTEGER          I, IFAIL, INFO, J, LWKOPT, M, N, NONZERO, K
*     .. Local Arrays ..
      DOUBLE PRECISION A(LDA,NMAX), DUMMY(1,1), RCONDU(NMAX), 
     +                 X(NMAX), SIGMAX(NMAX), Y(MMAX), YRES(MMAX),
     +                 RCONDV(NMAX), S(NMAX), UERRBD(NMAX),
     +                 VERRBD(NMAX), VT(LDVT,NMAX), WORK(LWORK)
*     .. External Functions ..
      DOUBLE PRECISION DLAMCH, DDOT
      EXTERNAL         DLAMCH, DDOT
*     .. External Subroutines ..
      EXTERNAL         DDISNA, DGESVD
*                                    , X04CAF
*     .. Executable Statements ..
*
      WRITE (NOUT,*) 'DGESVD Example Program Results'
      WRITE (NOUT,*)
*     dimensions given first
      READ (NIN,*) M, N, NONZERO
      WRITE (NOUT,*) 'matrix size ',M,'x',N,' with ',NONZERO,' nonzero'

      IF (M.LE.MMAX .AND. N.LE.NMAX) THEN
*
*        Read the m by n matrix A from data file
*
         DO I=1,M
            DO j=1,N
               A(I,J)=0.0
            END DO
         END DO
         DO K=1,NONZERO
            READ (NIN,*) I,J,ELE
            A(I,J)=ELE
         END DO
*
*   Get independent variable
*
         OPEN (UNIT=NIND,FILE='eachtime.out',STATUS='old')
         READ (NIND,*) MIND
         If (M.NE.MIND) THEN
            WRITE (NOUT,*) 'M and MIND do not match'
            STOP
         END IF
         READ (NIND,*) (Y(I),I=1,MIND)
         CLOSE (NIND)
*
*        Compute the singular values and left and right singular vectors
*        of A (A = U*S*(V**T), m.ge.n)
*
         CALL DGESVD('Overwrite A by U','Singular vectors (V)',M,N,A,
     +               LDA,S,DUMMY,1,VT,LDVT,WORK,LWORK,INFO)
         LWKOPT = WORK(1)
*
         IF (INFO.EQ.0) THEN
*
*           Print solution
*
            WRITE (NOUT,*) 'Singular values'
            WRITE (NOUT,99999) (S(J),J=1,N)
*
            IFAIL = 0
*            CALL X04CAF('General',' ',M,N,A,LDA,
*     +                  'Left singular vectors (first n columns of U)',
*     +                  IFAIL)
*            WRITE (NOUT,*)
*            CALL X04CAF('General',' ',N,N,VT,LDVT,
*     +                  'Right singular vectors by row (V**T)',IFAIL)
*
*           Get the machine precision, EPS and compute the approximate
*           error bound for the computed singular values.  Note that for
*           the 2-norm, S(1) = norm(A)
*
            EPS = DLAMCH('Eps')
            SERRBD = EPS*S(1)
*
*           Call DDISNA (F08FLF) to estimate reciprocal condition
*           numbers for the singular vectors
*
            CALL DDISNA('Left',M,N,S,RCONDU,INFO)
            CALL DDISNA('Right',M,N,S,RCONDV,INFO)
*
*           Compute the error estimates for the singular vectors
*
            DO 20 I = 1, N
               UERRBD(I) = SERRBD/RCONDU(I)
               VERRBD(I) = SERRBD/RCONDV(I)
 20                   CONTINUE
*
*           Print the approximate error bounds for the singular values
*           and vectors
*
C            WRITE (NOUT,*)
C            WRITE (NOUT,*) 'Error estimate for the singular values'
C            WRITE (NOUT,99998) SERRBD
C            WRITE (NOUT,*)
C            WRITE (NOUT,*)
C     +        'Error estimates for the left singular vectors'
C            WRITE (NOUT,99998) (UERRBD(I),I=1,N)
C            WRITE (NOUT,*)
C            WRITE (NOUT,*)
C     +        'Error estimates for the right singular vectors'
C            WRITE (NOUT,99998) (VERRBD(I),I=1,N)
         ELSE
            WRITE (NOUT,99997) 'Failure in DGESVD. INFO =', INFO
         END IF
*
*        Print workspace information
*
         IF (LWORK.LT.LWKOPT) THEN
            WRITE (NOUT,*)
            WRITE (NOUT,99996) 'Optimum workspace required = ', LWKOPT,
     +        'Workspace provided         = ', LWORK
         END IF
*
*         Calculate the dependent variables
*
*     put result in WORK
      CALL DGEMV ('Transpose', M, N, ONE, A, LDA, Y, 1,
     $                   ZERO, WORK, 1)
      CUTOFF = S(1)*EPS*M
      WRITE (NOUT,*)'Cutoff for null space',CUTOFF
      DO I=1,N
         IF(S(I).GT.CUTOFF) THEN
            WORK(I) = WORK(I)/S(I)
         ELSE
            WORK(I) = 0.0
         END IF
      END DO
      CALL DGEMV ('Transpose', N, N, ONE, VT, LDVT, WORK, 1,
     $                   ZERO, X, 1)
C        Find fit residuals and estimated variance
C        This uses the fact that work, as calculated above, now contains 
C        the projection of Y onto the non-singular part of the space 
C        (divided by S(I)).
      DO I=1,N
         WORK(I)=WORK(I)*S(I)
      END DO
      DO I=1,M
         YRES(I)=Y(I)
      END DO
      CALL DGEMV ('Normal', M, N, ONE, A, LDA, WORK, 1,
     $     -1.0D0, YRES, 1)
      ESTVAR = DDOT(M,YRES,1,YRES,1)/(M-N)
      YMAG = DDOT(M,Y,1,Y,1)
C      WRITE (NOUT,*) 'Fit residuals',(YRES(I),I=1,M)
      WRITE (NOUT,*) 'Estimated variance',ESTVAR
      WRITE (NOUT,*) 'Coefficient of determination R^2',
     +               ONE-ESTVAR*(M-N)/YMAG
         
C        Calculate errors
      DO I=1,N
         IF (S(I).GT.CUTOFF) THEN
            WORK(I) = ONE/(S(I)*S(I))
         ELSE
            WORK(I)=ZERO
         END IF
         END DO
      DO I=1,N
         SIGMAX(I)=ZERO
         DO J=1,N
            SIGMAX(I) = SIGMAX(I) + VT(J,I)*VT(J,I)*WORK(J)
         END DO
         SIGMAX(I)=SQRT(ESTVAR*SIGMAX(I))
      END DO
C
      WRITE (NOUT,*) 'Dependent variables'
      WRITE (NOUT,99995) (X(J),SIGMAX(J),J=1,N)
         
      ELSE
         WRITE (NOUT,*) 'MMAX and/or NMAX too small'
      END IF
      STOP
*
99999  FORMAT (3X,(8F8.4))
C99998   FORMAT (4X,1P,6E11.1)
99997    FORMAT (1X,A,I4)
99996     FORMAT (1X,A,I5,/1X,A,I5)
99995   FORMAT (1(F15.6,F14.6))
      END
