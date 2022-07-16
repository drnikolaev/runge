#include <cvm.h>

//      SUBROUTINE MEXP (M, A, EA, TOL)

extern "C" {
    void
#if defined (_MSC_VER)
    __stdcall MEXP
#else
    mexp_
#endif
    (const tint* M, const double* A, double* EA, const double* TOL, tint* IERR) {
        const cvm::srmatrix a(const_cast<double*>(A), *M);
        cvm::srmatrix ea(EA, *M);
        *IERR = 0;
        try {
            ea.exp(a, *TOL);
        }
        catch (const cvm::cvmexception&) {
            *IERR = 65;
        }
    }
}
