      CHARACTER*(2) FUNCTION TOSTR (n)
      INTEGER n
      INTEGER nM
      CHARACTER*10 sDigits /'0123456789'/

      IF (n .ge. 0 .and. n .le. 99) THEN
        IF (n .ge. 10) THEN
            nM = MOD (n, 10)
            TOSTR(1:1) = sDigits((n - nM) / 10 + 1:(n - nM) / 10 + 1)
            TOSTR(2:2) = sDigits(nM + 1:nM + 1)
        ELSE
            TOSTR(1:1) = sDigits(n + 1:n + 1)
            TOSTR(2:2) = ' '
        ENDIF
      ELSE
        TOSTR(1:2) = '**'
      ENDIF
      RETURN
      END FUNCTION

      CHARACTER*(2) FUNCTION TOSTR4 (n4)
      INTEGER*4 n4
      INTEGER n
      CHARACTER*2 TOSTR
      n = n4
      TOSTR4 = TOSTR(n)
      RETURN
      END FUNCTION



C      SUBROUTINE ISPRF1 (BUF, LEN, FMT, N)
C      CHARACTER*512 BUF
C      CHARACTER(*) FMT
C      INTEGER*4 N, LEN
C      INTEGER L(1), R(2)
C
C      R(1) = mxCreateString(FMT)
C      CALL mxSetN(R(1), 5)
C
C      R(2) = mxCreateNumericMatrix(1, 1, 
C     1    mxClassIDFromClassName('int64'), 0)
C      CALL mxCopyInteger4ToPtr(N, mxGetPr(R(2)), 1)
C      CALL mexCallMATLAB (1, L, 2, R, 'fprintf')
C      CALL mxCopyPtrToCharacter(mxGetPr(L(1)), BUF, LEN)
C      CALL mxDestroyArray(L(1))
C      CALL mxDestroyArray(R(2))
C      CALL mxDestroyArray(R(1))
C
C      RETURN
C      END SUBROUTINE
      
      
      
      
