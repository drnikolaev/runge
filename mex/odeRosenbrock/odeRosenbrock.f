      SUBROUTINE mexFunction (nlhs, plhs, nrhs, prhs)
CDEC$ IF DEFINED (FTN_EXPORTS)
CDEC$ ATTRIBUTES DLLEXPORT :: mexFunction
CDEC$ ENDIF
      ! Linux Matlab (x64) wants them to be 4 bytes long precisely.
      ! Windows Matlab doesn't seem to care though.
      INTEGER*4 nlhs, nrhs
      INTEGER plhs(*), prhs(*)

      INTEGER mxCreateDoubleMatrix, mxIsChar, mxIsNumeric
      INTEGER mxGetString, mxGetM, mxGetN
      INTEGER M
      DOUBLE PRECISION T0, T, H, Hmin, Hmax, EPS, P, H1, HS
      INTEGER IERR
      LOGICAL BUL
      DOUBLE PRECISION, ALLOCATABLE :: X(:)
      DOUBLE PRECISION, ALLOCATABLE :: R(:)
      INTEGER lF(1), rF(2), lFJ(1), rFJ(2), lFT(1), rFT(2)
      CHARACTER*256 sF, sFJ, sFT
      CHARACTER*(2) TOSTR, TOSTR4
      COMMON /odeRosBLOCK/ lF, rF, lFJ, rFJ, lFT, rFT, sF, sFJ, sFT
      INTEGER PC
      EXTERNAL F, FJ, FT

      PC = 0    ! reserved (not used here)

C     Check for proper number of arguments. 
      IF (nrhs .ne. 11) THEN
         CALL mexErrMsgTxt('11 inputs required but ' // 
     1    TRIM(TOSTR4(nrhs)) //
     2    ' provided. Run "help odeRosenbrock" to see help')
      ELSEIF (nlhs .ne. 1 .and. nlhs .ne. 2) THEN
         CALL mexErrMsgTxt(
     1     'One or two outputs required but ' // TRIM(TOSTR4(nlhs)) //
     2     ' provided. Run "help odeRosenbrock" to see help')
      ENDIF

C     Check inputs 
      IF (mxIsChar(prhs(1)) .ne. 1) THEN
         CALL mexErrMsgTxt('Input #1 (''F'') is not a string.')        !'F'
      ELSEIF (mxIsChar(prhs(2)) .ne. 1) THEN
         CALL mexErrMsgTxt('Input #2 (''FJ'') is not a string.')       !'FJ'
      ELSEIF (mxIsChar(prhs(3)) .ne. 1) THEN
         CALL mexErrMsgTxt('Input #3 (''FT'') is not a string.')       !'FT'
      ELSEIF (mxIsNumeric(prhs(4)) .ne. 1 .or. 
     1        mxGetM(prhs(4)) .ne. 1 .or. mxGetN(prhs(4)) .ne. 1) THEN
         CALL mexErrMsgTxt('Input #4 (T0) is not a number.')           !T0
      ELSEIF (mxIsNumeric(prhs(5)) .ne. 1.or. 
     1        mxGetM(prhs(5)) .ne. 1 .or. mxGetN(prhs(5)) .ne. 1) THEN
         CALL mexErrMsgTxt('Input #5 (T) is not a number.')            !T
      ELSEIF (mxIsNumeric(prhs(6)) .ne. 1 .or. 
     1        mxGetM(prhs(6)) .le. 0 .or. mxGetN(prhs(6)) .ne. 1) THEN
         CALL mexErrMsgTxt('Input #6 (X) is not a column vector.')     !X
      ELSEIF (mxIsNumeric(prhs(7)) .ne. 1 .or. 
     1        mxGetM(prhs(7)) .ne. 1 .or. mxGetN(prhs(7)) .ne. 1) THEN
         CALL mexErrMsgTxt('Input #7 (H) is not a number.')            !H
      ELSEIF (mxIsNumeric(prhs(8)) .ne. 1 .or. 
     1        mxGetM(prhs(8)) .ne. 1 .or. mxGetN(prhs(8)) .ne. 1) THEN
         CALL mexErrMsgTxt('Input #8 (Hmin) is not a number.')         !Hmin
      ELSEIF (mxIsNumeric(prhs(9)) .ne. 1 .or. 
     1        mxGetM(prhs(9)) .ne. 1 .or. mxGetN(prhs(9)) .ne. 1) THEN
         CALL mexErrMsgTxt('Input #9 (Hmax) is not a number.')         !Hmax
      ELSEIF (mxIsNumeric(prhs(10)) .ne. 1 .or. 
     1        mxGetM(prhs(10)) .ne. 1 .or. mxGetN(prhs(10)) .ne. 1) THEN
         CALL mexErrMsgTxt('Input #10 (eps) is not a number.')          !eps
      ELSEIF (mxIsNumeric(prhs(11)) .ne. 1 .or. 
     1        mxGetM(prhs(11)) .ne. 1 .or. mxGetN(prhs(11)) .ne. 1) THEN
         CALL mexErrMsgTxt('Input #11 (P) is not a number.')           !P
      ENDIF
      
      IF (mxGetString(prhs(1), sF, 256) .ne. 0) THEN
         CALL mexErrMsgTxt(
     1      'Input #1 (''F'') length must be less than 256.')
      ENDIF
      IF (mxGetString(prhs(2), sFJ, 256) .ne. 0) THEN
         CALL mexErrMsgTxt(
     1      'Input #2 (''FJ'') length must be less than 256.')
      ENDIF
      IF (mxGetString(prhs(3), sFT, 256) .ne. 0) THEN
         CALL mexErrMsgTxt(
     1      'Input #3 (''FT'') length must be less than 256.')
      ENDIF

      M = mxGetM(prhs(6))        !dimension
      plhs(1) = mxCreateDoubleMatrix(M, 1, 0)
      IF (nlhs .eq. 2) THEN
          plhs(2) = mxCreateDoubleMatrix(1, 1, 0)
      ENDIF

      rF(1)   = mxCreateDoubleMatrix(1, 1, 0)   ! t
      rF(2)   = mxCreateDoubleMatrix(M, 1, 0)   ! X
      rFJ(1)  = mxCreateDoubleMatrix(1, 1, 0)   ! t
      rFJ(2)  = mxCreateDoubleMatrix(M, 1, 0)   ! X
      rFT(1)  = mxCreateDoubleMatrix(1, 1, 0)   ! t
      rFT(2)  = mxCreateDoubleMatrix(M, 1, 0)   ! X

      ALLOCATE (X(M))
      ALLOCATE (R(6*M*M + 6*M))
      R = 0.D0

      CALL mxCopyPtrToReal8(mxGetPr(prhs(4)),  T0,   1)
      CALL mxCopyPtrToReal8(mxGetPr(prhs(5)),  T,    1)
      CALL mxCopyPtrToReal8(mxGetPr(prhs(6)),  X,    M)
      CALL mxCopyPtrToReal8(mxGetPr(prhs(7)),  H,    1)
      CALL mxCopyPtrToReal8(mxGetPr(prhs(8)),  Hmin, 1)
      CALL mxCopyPtrToReal8(mxGetPr(prhs(9)),  Hmax, 1)
      CALL mxCopyPtrToReal8(mxGetPr(prhs(10)),  EPS,  1)
      CALL mxCopyPtrToReal8(mxGetPr(prhs(11)), P,    1)

      HS = 0.D0;
      H = DSIGN(H, T - T0)
      BUL = .FALSE.
20    IF (.NOT. BUL) GOTO 25
      H = HS
      GOTO 40
25    H1 = T - T0
      IF (DABS(H1) .GT. DABS(H)) GOTO 30
      HS = H
      H = H1
      BUL = .TRUE.
30    CALL SROSN (PC, F, FJ, FT,
     1           M, Hmin, Hmax, EPS, P, X, T0, H, R, IERR)
      IF (BUL .AND. DABS(H1) .GT. DABS(H)) BUL = .FALSE.
      IF (IERR .EQ. 0) THEN
          GOTO 20
      ELSE
          CALL mexErrMsgTxt('odeRosenbrock failed to converge')
      ENDIF
40    CONTINUE

      CALL mxCopyReal8ToPtr(X, mxGetPr(plhs(1)), M)
      IF (nlhs .eq. 2) THEN
          CALL mxCopyReal8ToPtr(H, mxGetPr(plhs(2)), 1)
      ENDIF

      DEALLOCATE (R)
      DEALLOCATE (X)
      CALL mxDestroyArray(rFT(2))
      CALL mxDestroyArray(rFT(1))
      CALL mxDestroyArray(rFJ(2))
      CALL mxDestroyArray(rFJ(1))
      CALL mxDestroyArray(rF(2))
      CALL mxDestroyArray(rF(1))
      RETURN
      END SUBROUTINE

      SUBROUTINE F (PC, M, T, X, U)
      INTEGER PC, M
      DOUBLE PRECISION T, X(*), U(*)
      INTEGER lF(1), rF(2), lFJ(1), rFJ(2), lFT(1), rFT(2)
      CHARACTER*256 sF, sFJ, sFT
      CHARACTER*(2) TOSTR      
      COMMON /odeRosBLOCK/ lF, rF, lFJ, rFJ, lFT, rFT, sF, sFJ, sFT

      CALL mxCopyReal8ToPtr(T, mxGetPr(rF(1)), 1)
      CALL mxCopyReal8ToPtr(X, mxGetPr(rF(2)), M)

      CALL mexCallMATLAB(1, lF, 2, rF, sF)

      IF (mxIsNumeric(lF(1)) .ne. 1 .or. 
     1    mxGetM(lF(1)) .ne. M .or. mxGetN(lF(1)) .ne. 1) THEN
          CALL mexErrMsgTxt('Function ''' // TRIM(sF) //
     1        ''' must return column vector of size ' // 
     2        TRIM(TOSTR(M)))
      ENDIF

      CALL mxCopyPtrToReal8(mxGetPr(lF(1)), U, M)
      CALL mxDestroyArray(lF(1))
      RETURN
      END SUBROUTINE


      SUBROUTINE FJ (PC, M, T, X, U)
      INTEGER PC, M
      DOUBLE PRECISION T, X(*), U(*)
      INTEGER lF(1), rF(2), lFJ(1), rFJ(2), lFT(1), rFT(2)
      CHARACTER*256 sF, sFJ, sFT
      CHARACTER*(2) TOSTR      
      COMMON /odeRosBLOCK/ lF, rF, lFJ, rFJ, lFT, rFT, sF, sFJ, sFT

      CALL mxCopyReal8ToPtr(T, mxGetPr(rFJ(1)), 1)
      CALL mxCopyReal8ToPtr(X, mxGetPr(rFJ(2)), M)

      CALL mexCallMATLAB (1, lFJ, 2, rFJ, sFJ)

      IF (mxIsNumeric(lFJ(1)) .ne. 1 .or. 
     1    mxGetM(lFJ(1)) .ne. M .or. mxGetN(lFJ(1)) .ne. M) THEN
          CALL mexErrMsgTxt('Function ''' // TRIM(sFJ) //
     1      ''' must return matrix of size ' // 
     2      TRIM(TOSTR(M)) // ' by ' // TRIM(TOSTR(M)))
      ENDIF

      CALL mxCopyPtrToReal8(mxGetPr(lFJ(1)), U, M*M)
      CALL mxDestroyArray(lFJ(1))
      RETURN
      END SUBROUTINE


      SUBROUTINE FT (PC, M, T, X, U)
      INTEGER PC, M
      DOUBLE PRECISION T, X(*), U(*)
      INTEGER lF(1), rF(2), lFJ(1), rFJ(2), lFT(1), rFT(2)
      CHARACTER*256 sF, sFJ, sFT
      CHARACTER*(2) TOSTR      
      COMMON /odeRosBLOCK/ lF, rF, lFJ, rFJ, lFT, rFT, sF, sFJ, sFT

      CALL mxCopyReal8ToPtr(T, mxGetPr(rFT(1)), 1)
      CALL mxCopyReal8ToPtr(X, mxGetPr(rFT(2)), M)

      CALL mexCallMATLAB(1, lFT, 2, rFT, sFT)

      IF (mxIsNumeric(lFT(1)) .ne. 1 .or. 
     1    mxGetM(lFT(1)) .ne. M .or. mxGetN(lFT(1)) .ne. 1) THEN
          CALL mexErrMsgTxt('Function ''' // TRIM(sFT) //
     1        ''' must return column vector of size ' // 
     2        TRIM(TOSTR(M)))
      ENDIF

      CALL mxCopyPtrToReal8(mxGetPr(lFT(1)), U, M)
      CALL mxDestroyArray(lFT(1))
      RETURN
      END SUBROUTINE


