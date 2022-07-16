/* Differential Equation Solver */
/* Copyright (C), Sergei Nikolaev, 2008-2012, http://cvmlib.com */
#ifndef _RUNGE_SOLVER_
#define _RUNGE_SOLVER_

#include "cvm.h"
#include "cfun.h"
#include "loader.h"

#if defined(WIN32) && defined(_DEBUG)
     #define DEBUG_NEW new( _NORMAL_BLOCK, __FILE__, __LINE__ )
     #define new DEBUG_NEW
#endif

extern "C" {

typedef void (__stdcall *F3)(tint* PC, const tint* M, const double* X, double* RET);
typedef void (__stdcall *F4)(tint* PC, const tint* M, const double* T, const double* X, double* RET);

//x' = F(t,x)
typedef void (__stdcall *solver_FTX_F)     (tint* PC, F4, tint* M, const double* HMIN, const double* HMAX,
                                            const double* EPS, const double* P, double* X, double* T, 
                                            double* H, double* R, tint* IERR);
//x' = A(t)x + fi(t) 
typedef void (__stdcall *solver_AFI_FAFI)  (tint* PC, F3, F3, tint* M, const double* HMIN, const double* HMAX,
                                            const double* EPS, const double* P, double* X, double* T, 
                                            double* H, double* R, tint* IERR);
//x' = F(t,x)
typedef void (__stdcall *solver_FTX_FFJ)   (tint* PC, F4, F4, tint* M, const double* HMIN, const double* HMAX,
                                            const double* EPS, const double* P, double* X, double* T, 
                                            double* H, double* R, tint* IERR);
//x' = Bx + u(t,x)
typedef void (__stdcall *solver_BUTX_BFU)  (tint* PC, const double* B, F4, tint* M, const double* HMIN, const double* HMAX,
                                            const double* EPS, const double* P, double* X, double* T, 
                                            double* H, double* R, tint* IERR);
//x'=f(x)
typedef void (__stdcall *solver_FX_FFJ)    (tint* PC, F3, F3, tint* M, const double* HMIN, const double* HMAX,
                                            const double* EPS, const double* P, double* X, double* T, 
                                            double* H, double* R, tint* IERR);
//x'=f(t,x)
typedef void (__stdcall *solver_FTX_FFJFT) (tint* PC, F4, F4, F4, tint* M, const double* HMIN, const double* HMAX,
                                            const double* EPS, const double* P, double* X, double* T, 
                                            double* H, double* R, tint* IERR);
}



namespace runge {

enum SystemType {
    FTX,
    FX,
    AF,
    BF
};

enum SolverType {
    UNDEFINED,
//x' = F(t,x)
//F(M,T,X,Z)
    FTX_F,
//x' = A(t)x + fi(t) 
//FA(M,T,A)
//FI(M,T,F)
    AFI_FAFI,
//x' = F(t,x)
//F(M,T,X,FX)
//FJ(M,T,X,XJ)
    FTX_FFJ,
//x' = Bx + u(t,x)
//B
//FU(M,T,X,U)
    BUTX_BFU,
//x'=f(x)
//F(M,X,FX)
//FJ(M,X,XJ)
    FX_FFJ,
//x'=f(t,x)
//F(M,T,X,FX)
//FJ(M,T,X,XJ)
//FT(M,T,X,XJ)
    FTX_FFJFT
};

SolverType solverTypeByName (const std::string& solverName);
bool isSolverEnabled (SystemType systemType, SolverType solverType);
SystemType systemTypeByName (const std::string& systemTypeName);
std::string systemTypeName (SystemType systemType);

class SolverFTX_F;
class SolverAFI_FAFI;
class SolverFTX_FFJ;
class SolverBUTX_BFU;
class SolverFX_FFJ;
class SolverFTX_FFJFT;


class SolverRegistry {
public:
    class SolverProperties {
        friend class SolverRegistry;

        SolverType mSolverType;
        DES_PROC_PTR mRoutinePtr;
        int mRSize2, mRSize1, mRSize0;    // real working array size is a*M^2+b*M+c
    public:
        SolverProperties()
          : mSolverType(UNDEFINED), mRoutinePtr(NULL), mRSize2(0), mRSize1(0), mRSize0(0) {}
        SolverProperties(SolverType solverType, DES_PROC_PTR ptr, int a, int b, int c)
          : mSolverType(solverType), mRoutinePtr(ptr), mRSize2(a), mRSize1(b), mRSize0(c) {}
        SolverProperties(const SolverProperties& that)
          : mSolverType(that.mSolverType), mRoutinePtr(that.mRoutinePtr), 
            mRSize2(that.mRSize2), mRSize1(that.mRSize1), mRSize0(that.mRSize0) {}
        SolverProperties& operator = (const SolverProperties& that) {
            mSolverType = that.mSolverType;
            mRoutinePtr = that.mRoutinePtr;
            mRSize2 = that.mRSize2;
            mRSize1 = that.mRSize1;
            mRSize0 = that.mRSize0;
            return *this;
        }

        SolverType getSolverType() const {
            return mSolverType;
        }
    };

private:
    static std::map<std::string, SolverProperties> sRegistry;
	static std::list<std::string> sSolverNames;

    static int getWorkingSize(const SolverProperties& props, int dim) throw(std::exception) {
        return props.mRSize2 * dim * dim + props.mRSize1 * dim + props.mRSize0;
    }

public:
    static void register_solver_routine (const std::string& solverName,
        SolverType solverType, DES_PROC_PTR mRoutinePtr, 
        int mRSize2, int mRSize1, int mRSize0) {
        sRegistry[solverName] = SolverProperties(solverType, mRoutinePtr, mRSize2, mRSize1, mRSize0);
		sSolverNames.push_back(solverName);
    }

    static const SolverProperties& getSolverProperties(const std::string& solverName) throw(std::exception);

    static const std::list<std::string>& getNames() {
        return sSolverNames;
	}

    static void unregister_all () {
		sRegistry.clear();
		sSolverNames.clear();
    }

    static SolverFTX_F*     createSolverFTX_F(const std::string& solverName, 
        const cvm::rfvector& fvectorFtx) throw(std::exception);
    static SolverAFI_FAFI*  createSolverSolverAFI_FAFI(const std::string& solverName, 
        const cvm::rfmatrix& fmatrixAt, const cvm::rfvector& fvectorFi) throw(std::exception);
    static SolverFTX_FFJ*   createSolverFTX_FFJ(const std::string& solverName, 
        const cvm::rfvector& fvectorFtx) throw(std::exception);
    static SolverBUTX_BFU*  createSolverBUTX_BFU(const std::string& solverName, 
        const cvm::rmatrix& rmatrixB, const cvm::rfvector& fvectorUtx) throw(std::exception);
    static SolverFX_FFJ*    createSolverFX_FFJ(const std::string& solverName, 
        const cvm::rfvector& fvectorFx) throw(std::exception);
    static SolverFTX_FFJFT* createSolverFTX_FFJFT(const std::string& solverName, 
        const cvm::rfvector& fvectorFx) throw(std::exception);
};


class Solver {
protected:
    tint mDim;                       // problem dimension
    cvm::rvector mtx;               // just a holder for point T and vector X
    double mHMIN, mHMAX, mEPS, mP;  // minimum step, maximum step, epsilon, absolute measure
    tint mIERR;                      // last step state
    cvm::rvector mR;                // working array for routines

    Solver (tint dim, int workingSize)
      : mDim(dim),
        mtx(mDim + 1),
        mHMIN(1.e-10), mHMAX(1.), mEPS(1.e-10), mP(1.e1),    // some defaults
        mIERR(0),
        mR(workingSize)
    {
    }

public:
    virtual ~Solver()
    {
    }

    tint* store_pointer()
    {
        return (tint*) this;
    }

    double get_hmin()
    {
        return mHMIN;
    }
    double get_hmax()
    {
        return mHMAX;
    }
    double get_eps()
    {
        return mEPS;
    }
    double get_p()
    {
        return mP;
    }
    void set_hmin(double HMIN)
    {
        mHMIN = HMIN;
    }
    void set_hmax(double HMAX)
    {
        mHMAX = HMAX;
    }
    void set_eps(double EPS)
    {
        mEPS = EPS;
    }
    void set_p(double P)
    {
        mP = P;
    }
    tint get_last_err()
    {
        return mIERR;
    }

    virtual bool step (double& t, cvm::rvector& x, double& h) = 0;

};


class SolverFTX_F : public Solver {
    friend class SolverRegistry;

    solver_FTX_F mRoutinePtr;
    cvm::rfvector mFVectorFtx;

    SolverFTX_F(solver_FTX_F routinePtr, tint dim, int workingSize, const cvm::rfvector& fvectorFtx)
      : Solver(dim, workingSize),
        mRoutinePtr(routinePtr),
        mFVectorFtx(fvectorFtx)
    {
    }

public:
    virtual bool step (double& t, cvm::rvector& x, double& h);
    void f (tint dim, const double& t, const double* px, double* pftx) throw(std::exception);
};


class SolverAFI_FAFI : public Solver {
    friend class SolverRegistry;

    solver_AFI_FAFI mRoutinePtr;
    cvm::rfmatrix mFMatrixAt;
    cvm::rfvector mFVectorFi;

    SolverAFI_FAFI(solver_AFI_FAFI routinePtr, tint dim, int workingSize, 
        const cvm::rfmatrix& fmatrixAt, const cvm::rfvector& fvectorFi)
      : Solver(dim, workingSize),
        mRoutinePtr(routinePtr),
        mFMatrixAt(fmatrixAt),
        mFVectorFi(fvectorFi)
    {
    }

public:
    virtual bool step (double& t, cvm::rvector& x, double& h);
    void fa (tint dim, const double& t, double* pat) throw(std::exception);
    void fi (tint dim, const double& t, double* pfi) throw(std::exception);
};


class SolverFTX_FFJ : public Solver {
    friend class SolverRegistry;

    solver_FTX_FFJ mRoutinePtr;
    cvm::rfvector mFVectorFtx;
    cvm::rfmatrix mFMatrixJtx;

    SolverFTX_FFJ(solver_FTX_FFJ routinePtr, tint dim, int workingSize, 
        const cvm::rfvector& fvectorFtx)
      : Solver(dim, workingSize),
        mRoutinePtr(routinePtr),
        mFVectorFtx(fvectorFtx),
        mFMatrixJtx(mFVectorFtx.jacobian(1))
    {
    }

public:
    virtual bool step (double& t, cvm::rvector& x, double& h);
    void ftx (tint dim, const double& t, const double* px, double* pf) throw(std::exception);
    void jtx (tint dim, const double& t, const double* px, double* pj) throw(std::exception);
};

class SolverBUTX_BFU : public Solver {
    friend class SolverRegistry;

    solver_BUTX_BFU mRoutinePtr;
    cvm::rmatrix mRMatrixB;
    cvm::rfvector mFVectorUtx;

    SolverBUTX_BFU(solver_BUTX_BFU routinePtr, tint dim, int workingSize, 
        const cvm::rmatrix& rmatrixB, const cvm::rfvector& fvectorUtx)
      : Solver(dim, workingSize),
        mRoutinePtr(routinePtr),
        mRMatrixB(rmatrixB),
        mFVectorUtx(fvectorUtx)
    {
    }

public:
    virtual bool step (double& t, cvm::rvector& x, double& h);
    void utx (tint dim, const double& t, const double* px, double* pu) throw(std::exception);
};


class SolverFX_FFJ : public Solver {
    friend class SolverRegistry;

    solver_FX_FFJ mRoutinePtr;
    cvm::rfvector mFVectorFx;
    cvm::rfmatrix mFMatrixFJx;

    SolverFX_FFJ(solver_FX_FFJ routinePtr, tint dim, int workingSize, 
        const cvm::rfvector& fvectorFx)
      : Solver(dim, workingSize),
        mRoutinePtr(routinePtr),
        mFVectorFx(fvectorFx),
        mFMatrixFJx(mFVectorFx.jacobian())
    {
    }

public:
    virtual bool step (double& t, cvm::rvector& x, double& h);
    void fx (tint dim, const double* px, double* pf) throw(std::exception);
    void jx (tint dim, const double* px, double* pj) throw(std::exception);
};

class SolverFTX_FFJFT : public Solver {
    friend class SolverRegistry;

    solver_FTX_FFJFT mRoutinePtr;
    cvm::rfvector mFVectorFtx;
    cvm::rfvector mFVectorJt;
    cvm::rfmatrix mFMatrixJx;

    SolverFTX_FFJFT(solver_FTX_FFJFT routinePtr, tint dim, int workingSize, 
        const cvm::rfvector& fvectorFx)
      : Solver(dim, workingSize),
        mRoutinePtr(routinePtr),
        mFVectorFtx(fvectorFx),
        mFVectorJt(mFVectorFtx.drv(1)),     // df(t,x)/dt
        mFMatrixJx(mFVectorFtx.jacobian(1))   // df(t,x)/dx
    {
    }

public:
    virtual bool step (double& t, cvm::rvector& x, double& h);
    void ftx (tint dim, const double& t, const double* px, double* pf) throw(std::exception);
    void jt (tint dim, const double& t, const double* px, double* pj) throw(std::exception);
    void jx (tint dim, const double& t, const double* px, double* pj) throw(std::exception);
};


}

#endif  // _RUNGE_SOLVER_
