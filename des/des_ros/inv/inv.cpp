#include <cvm.h>

//  SUBROUTINE INV (M, A, AI, IERR) - matrix inversion routine (external CVM)

extern "C" {
    void
#if defined (_MSC_VER)
    __stdcall INV
#else
    inv_
#endif
    (const tint* M, const double* A, double* AI, tint* IERR) {
        const cvm::srmatrix a(const_cast<double*>(A), *M);
        cvm::srmatrix ai(AI, *M);
        *IERR = 0;
        try {
            ai.inv(a);
        }
        catch (const cvm::cvmexception&) {
            *IERR = 65;
        }
    }
}

