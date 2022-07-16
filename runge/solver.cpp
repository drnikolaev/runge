/* Differential Equation Solver */
/* Copyright (C), Sergei Nikolaev, 2008-2011, http://cvmlib.com */

#include "solver.h"
#include <stdexcept>
#include <sstream>

// right part routines
extern "C" {      
    void __stdcall _FTX_F (tint* PC, const tint* M, const double* T, const double* X, double* Z)
    {
        runge::SolverFTX_F* psolver = (runge::SolverFTX_F*) PC;
        psolver -> f(*M, *T, X, Z);
    }

    void __stdcall _AFI_FA (tint* PC, const tint* M, const double* T, double* A)
    {
        runge::SolverAFI_FAFI* psolver = (runge::SolverAFI_FAFI*) PC;
        psolver -> fa(*M, *T, A);
    }

    void __stdcall _AFI_FI (tint* PC, const tint* M, const double* T, double* F)
    {
        runge::SolverAFI_FAFI* psolver = (runge::SolverAFI_FAFI*) PC;
        psolver -> fi(*M, *T, F);
    }

    void __stdcall _FTX_FFJ_F (tint* PC, const tint* M, const double* T, const double* X, double* Z)
    {
        runge::SolverFTX_FFJ* psolver = (runge::SolverFTX_FFJ*) PC;
        psolver -> ftx(*M, *T, X, Z);
    }

    void __stdcall _FTX_FFJ_J (tint* PC, const tint* M, const double* T, const double* X, double* Z)
    {
        runge::SolverFTX_FFJ* psolver = (runge::SolverFTX_FFJ*) PC;
        psolver -> jtx(*M, *T, X, Z);
    }

    void __stdcall _BUTX_BFU_U (tint* PC, const tint* M, const double* T, const double* X, double* U)
    {
        runge::SolverBUTX_BFU* psolver = (runge::SolverBUTX_BFU*) PC;
        psolver -> utx(*M, *T, X, U);
    }

    void __stdcall _FX_FFJ_F (tint* PC, const tint* M, const double* X, double* F)
    {
        runge::SolverFX_FFJ* psolver = (runge::SolverFX_FFJ*) PC;
        psolver -> fx(*M, X, F);
    }

    void __stdcall _FX_FFJ_J (tint* PC, const tint* M, const double* X, double* F)
    {
        runge::SolverFX_FFJ* psolver = (runge::SolverFX_FFJ*) PC;
        psolver -> jx(*M, X, F);
    }

    void __stdcall _FTX_FFJFT_F (tint* PC, const tint* M, const double* T, const double* X, double* Z)
    {
        runge::SolverFTX_FFJFT* psolver = (runge::SolverFTX_FFJFT*) PC;
        psolver -> ftx(*M, *T, X, Z);
    }

    void __stdcall _FTX_FFJFT_JX (tint* PC, const tint* M, const double* T, const double* X, double* Z)
    {
        runge::SolverFTX_FFJFT* psolver = (runge::SolverFTX_FFJFT*) PC;
        psolver -> jx(*M, *T, X, Z);
    }

    void __stdcall _FTX_FFJFT_JT (tint* PC, const tint* M, const double* T, const double* X, double* Z)
    {
        runge::SolverFTX_FFJFT* psolver = (runge::SolverFTX_FFJFT*) PC;
        psolver -> jt(*M, *T, X, Z);
    }
}


namespace runge {

std::map<std::string, SolverRegistry::SolverProperties> SolverRegistry::sRegistry;
std::list<std::string> SolverRegistry::sSolverNames;

bool isSolverEnabled (SystemType systemType, SolverType solverType)
{
    bool solverEnabled = false;
    switch (systemType) {
    case FTX:
        solverEnabled = solverType == FTX_F || solverType == FTX_FFJ || solverType == FTX_FFJFT;
        break;
    case FX:
        solverEnabled = solverType == FX_FFJ;
        break;
    case AF:
        solverEnabled = solverType == AFI_FAFI;
        break;
    case BF:
        solverEnabled = solverType == BUTX_BFU;
        break;
    }
    return solverEnabled;
}

SolverType solverTypeByName (const std::string& solverType)
{
	SolverType type = UNDEFINED;
	if (solverType.compare("FTX_F") == 0) {
		type = FTX_F;
	} else if (solverType.compare("AFI_FAFI") == 0) {
		type = AFI_FAFI;
	} else if (solverType.compare("FTX_FFJ") == 0) {
		type = FTX_FFJ;
	} else if (solverType.compare("BUTX_BFU") == 0) {
		type = BUTX_BFU;
	} else if (solverType.compare("FX_FFJ") == 0) {
		type = FX_FFJ;
	} else if (solverType.compare("FTX_FFJFT") == 0) {
		type = FTX_FFJFT;
	} else {
        std::ostringstream os;
        os << "Unknown solver type: " << solverType << std::ends;
        throw std::runtime_error(os.str());
	}
	return type;
}

std::string systemTypeName (SystemType systemType)
{
	switch (systemType) {
	case FTX:
		return "FTX";
	case FX:
		return "FX";
	case AF:
		return "AF";
	case BF:
		return "BF";
	}
	return "??";
}

SystemType systemTypeByName (const std::string& systemTypeName)
{
	SystemType type = FTX;
	if (systemTypeName.compare("FTX") == 0) {
	} else if (systemTypeName.compare("FX") == 0) {
		type = FX;
	} else if (systemTypeName.compare("AF") == 0) {
		type = AF;
	} else if (systemTypeName.compare("BF") == 0) {
		type = BF;
	}
	return type;
}

// internal checker
void verifySizes (const char* function_name, tint sz1, tint sz2) throw(std::exception)
{
    if (sz1 != sz2) {
        std::ostringstream os;
        os << function_name << ": sizes mismatch: " << sz1 << " vs. " << sz2 << std::ends;
        throw std::runtime_error(os.str());
    }
}

// internal checker
void verifySolverTypes (const char* function_name, runge::SolverType type1, runge::SolverType type2) throw(std::exception)
{
    if (type1 != type2) {
        std::ostringstream os;
        os << function_name << ": solver types mismatch: " << type1 << " vs. " << type2 << std::ends;
        throw std::runtime_error(os.str());
    }
}


const SolverRegistry::SolverProperties& SolverRegistry::getSolverProperties(const std::string& solverName) throw(std::exception)
{
    std::map<std::string, SolverRegistry::SolverProperties>::const_iterator it = sRegistry.find(solverName);
    if (it == sRegistry.end()) {
        std::ostringstream os;
        os << "Solver \'" << solverName << "\' has not been registered" << std::ends;
        throw std::runtime_error(os.str());
    }
    return it -> second;
}


SolverFTX_F* SolverRegistry::createSolverFTX_F(const std::string& solverName, 
        const cvm::rfvector& fvectorFtx) throw(std::exception)
{
    static const char functionName[] = "SolverRegistry::createSolverFTX_F";
    const SolverProperties& solverProperties = getSolverProperties(solverName);
    verifySolverTypes (functionName, solverProperties.mSolverType, runge::FTX_F);
    int dim = (int) fvectorFtx.size();
    return new SolverFTX_F((solver_FTX_F) solverProperties.mRoutinePtr, dim, getWorkingSize(solverProperties, dim), fvectorFtx);
}

SolverAFI_FAFI* SolverRegistry::createSolverSolverAFI_FAFI(const std::string& solverName, 
        const cvm::rfmatrix& fmatrixAt, const cvm::rfvector& fvectorFi) throw(std::exception)
{
    static const char functionName[] = "SolverRegistry::createSolverSolverAFI_FAFI";
    const SolverProperties& solverProperties = getSolverProperties(solverName);
    verifySolverTypes (functionName, solverProperties.mSolverType, runge::AFI_FAFI);
    int dim = (int) fvectorFi.size();
    return new SolverAFI_FAFI((solver_AFI_FAFI) solverProperties.mRoutinePtr, dim, getWorkingSize(solverProperties, dim), fmatrixAt, fvectorFi);
}

SolverFTX_FFJ* SolverRegistry::createSolverFTX_FFJ(const std::string& solverName, 
        const cvm::rfvector& fvectorFtx) throw(std::exception)
{
    static const char functionName[] = "SolverRegistry::createSolverFTX_FFJ";
    const SolverProperties& solverProperties = getSolverProperties(solverName);
    verifySolverTypes (functionName, solverProperties.mSolverType, runge::FTX_FFJ);
    int dim = (int) fvectorFtx.size();
    return new SolverFTX_FFJ((solver_FTX_FFJ) solverProperties.mRoutinePtr, dim, getWorkingSize(solverProperties, dim), fvectorFtx);
}

SolverBUTX_BFU* SolverRegistry::createSolverBUTX_BFU(const std::string& solverName, 
        const cvm::rmatrix& rmatrixB, const cvm::rfvector& fvectorUtx) throw(std::exception)
{
    static const char functionName[] = "SolverRegistry::createSolverBUTX_BFU";
    const SolverProperties& solverProperties = getSolverProperties(solverName);
    verifySolverTypes (functionName, solverProperties.mSolverType, runge::BUTX_BFU);
    int dim = (int) fvectorUtx.size();
    return new SolverBUTX_BFU((solver_BUTX_BFU) solverProperties.mRoutinePtr, dim, getWorkingSize(solverProperties, dim), rmatrixB, fvectorUtx);
}

SolverFX_FFJ* SolverRegistry::createSolverFX_FFJ(const std::string& solverName, 
        const cvm::rfvector& fvectorFx) throw(std::exception)
{
    static const char functionName[] = "SolverRegistry::createSolverFX_FFJ";
    const SolverProperties& solverProperties = getSolverProperties(solverName);
    verifySolverTypes (functionName, solverProperties.mSolverType, runge::FX_FFJ);
    int dim = (int) fvectorFx.size();
    return new SolverFX_FFJ((solver_FX_FFJ) solverProperties.mRoutinePtr, dim, getWorkingSize(solverProperties, dim), fvectorFx);
}

SolverFTX_FFJFT* SolverRegistry::createSolverFTX_FFJFT(const std::string& solverName, 
        const cvm::rfvector& fvectorFx) throw(std::exception)
{
    static const char functionName[] = "SolverRegistry::createSolverFTX_FFJFT";
    const SolverProperties& solverProperties = getSolverProperties(solverName);
    verifySolverTypes (functionName, solverProperties.mSolverType, runge::FTX_FFJFT);
    int dim = (int) fvectorFx.size();
    return new SolverFTX_FFJFT((solver_FTX_FFJFT) solverProperties.mRoutinePtr, dim, getWorkingSize(solverProperties, dim), fvectorFx);
}


// right part for FTX_F
void SolverFTX_F::f (tint dim, const double& t, const double* px, double* pftx) throw(std::exception)
{
    static const char functionName[] = "SolverFTX_F::f";
    verifySizes (functionName, dim, (tint) mFVectorFtx.size());
    mtx(1) = t;
    mtx.assign (2, px);
    mFVectorFtx.value (mtx, pftx);
}

// matrix A(t) for AFI_FAFI
void SolverAFI_FAFI::fa (tint dim, const double& t, double* pat) throw(std::exception)
{
    static const char functionName[] = "SolverAFI_FAFI::fa";
    verifySizes (functionName, dim, (tint) mFMatrixAt.msize());
    verifySizes (functionName, dim, (tint) mFMatrixAt.nsize());
    mtx(1) = t;
    mFMatrixAt.value (mtx, pat);
}

// vector fi(t) for AFI_FAFI
void SolverAFI_FAFI::fi (tint dim, const double& t, double* pfi) throw(std::exception)
{
    static const char functionName[] = "SolverAFI_FAFI::fi";
    verifySizes (functionName, dim, (tint) mFVectorFi.size());
    mtx(1) = t;
    mFVectorFi.value (mtx, pfi);
}


// right part for FTX_FFJ
void SolverFTX_FFJ::ftx (tint dim, const double& t, const double* px, double* pftx) throw(std::exception)
{
    static const char functionName[] = "SolverFTX_FFJ::ftx";
    verifySizes (functionName, dim, (tint) mFVectorFtx.size());
    mtx(1) = t;
    mtx.assign (2, px);
    mFVectorFtx.value (mtx, pftx);
}

// right part for FTX_FFJ
void SolverFTX_FFJ::jtx (tint dim, const double& t, const double* px, double* jtx) throw(std::exception)
{
    static const char functionName[] = "SolverFTX_FFJ::jtx";
    verifySizes (functionName, dim, (tint) mFMatrixJtx.msize());
    verifySizes (functionName, dim, (tint) mFMatrixJtx.nsize());
    mtx(1) = t;
    mtx.assign (2, px);
    mFMatrixJtx.value (mtx, jtx);
}

// u(t,x) for BUTX_BFU
void SolverBUTX_BFU::utx (tint dim, const double& t, const double* px, double* pu) throw(std::exception)
{
    static const char functionName[] = "SolverBUTX_BFU::utx";
    verifySizes (functionName, dim, (tint) mRMatrixB.msize());
    verifySizes (functionName, dim, (tint) mRMatrixB.nsize());
    verifySizes (functionName, dim, (tint) mFVectorUtx.size());
    mtx(1) = t;
    mtx.assign (2, px);
    mFVectorUtx.value (mtx, pu);
}

// f(x) for FX_FFJ
void SolverFX_FFJ::fx (tint dim, const double* px, double* pf) throw(std::exception)
{
    static const char functionName[] = "SolverFX_FFJ::fx";
    verifySizes (functionName, dim, (tint) mFVectorFx.size());
    mFVectorFx.value (px, pf);
}

// fj(x) for FX_FFJ
void SolverFX_FFJ::jx (tint dim, const double* px, double* pj) throw(std::exception)
{
    static const char functionName[] = "SolverFX_FFJ::jx";
    verifySizes (functionName, dim, (tint) mFMatrixFJx.msize());
    verifySizes (functionName, dim, (tint) mFMatrixFJx.nsize());
    mFMatrixFJx.value (px, pj);
}

// right part for FTX_FFJFT
void SolverFTX_FFJFT::ftx (tint dim, const double& t, const double* px, double* pftx) throw(std::exception)
{
    static const char functionName[] = "SolverFTX_FFJFT::ftx";
    verifySizes (functionName, dim, (tint) mFVectorFtx.size());
    mtx(1) = t;
    mtx.assign (2, px);
    mFVectorFtx.value (mtx, pftx);
}

// df(t,x)/dt for FTX_FFJFT
void SolverFTX_FFJFT::jt (tint dim, const double& t, const double* px, double* pftx) throw(std::exception)
{
    static const char functionName[] = "SolverFTX_FFJFT::jt";
    verifySizes (functionName, dim, (tint) mFVectorJt.size());
    mtx(1) = t;
    mtx.assign (2, px);
    mFVectorJt.value (mtx, pftx);
}

// df(t,x)/dx for FTX_FFJFT
void SolverFTX_FFJFT::jx (tint dim, const double& t, const double* px, double* pftx) throw(std::exception)
{
    static const char functionName[] = "SolverFTX_FFJFT::jx";
    verifySizes (functionName, dim, (tint) mFMatrixJx.msize());
    verifySizes (functionName, dim, (tint) mFMatrixJx.nsize());
    mtx(1) = t;
    mtx.assign (2, px);
    mFMatrixJx.value (mtx, pftx);
}


// step implementations
bool SolverFTX_F::step (double& t, cvm::rvector& x, double& h)
{
    static const char functionName[] = "SolverFTX_F::step";
    verifySizes (functionName, mDim, x.size());
    mRoutinePtr (store_pointer(), _FTX_F, &mDim, &mHMIN, &mHMAX, &mEPS, &mP, x, &t, &h, mR, &mIERR);
    return mIERR == 0;   // 0 - success
}

bool SolverAFI_FAFI::step (double& t, cvm::rvector& x, double& h)
{
    static const char functionName[] = "SolverAFI_FAFI::step";
    verifySizes (functionName, mDim, x.size());
    mRoutinePtr (store_pointer(), _AFI_FA, _AFI_FI, &mDim, &mHMIN, &mHMAX, &mEPS, &mP, x, &t, &h, mR, &mIERR);
    return mIERR == 0;   // 0 - success
}

bool SolverFTX_FFJ::step (double& t, cvm::rvector& x, double& h)
{
    static const char functionName[] = "SolverFTX_FFJ::step";
    verifySizes (functionName, mDim, x.size());
    mRoutinePtr (store_pointer(), _FTX_FFJ_F, _FTX_FFJ_J, &mDim, &mHMIN, &mHMAX, &mEPS, &mP, x, &t, &h, mR, &mIERR);
    return mIERR == 0;   // 0 - success
}

bool SolverBUTX_BFU::step (double& t, cvm::rvector& x, double& h)
{
    static const char functionName[] = "SolverBUTX_BFU::step";
    verifySizes (functionName, mDim, x.size());
    mRoutinePtr (store_pointer(), mRMatrixB, _BUTX_BFU_U, &mDim, &mHMIN, &mHMAX, &mEPS, &mP, x, &t, &h, mR, &mIERR);
    return mIERR == 0;   // 0 - success
}

bool SolverFX_FFJ::step (double& t, cvm::rvector& x, double& h)
{
    static const char functionName[] = "SolverFX_FFJ::step";
    verifySizes (functionName, mDim, x.size());
    mRoutinePtr (store_pointer(), _FX_FFJ_F, _FX_FFJ_J, &mDim, &mHMIN, &mHMAX, &mEPS, &mP, x, &t, &h, mR, &mIERR);
    return mIERR == 0;   // 0 - success
}

bool SolverFTX_FFJFT::step (double& t, cvm::rvector& x, double& h)
{
    static const char functionName[] = "SolverFTX_FFJFT::step";
    verifySizes (functionName, mDim, x.size());
    mRoutinePtr (store_pointer(), _FTX_FFJFT_F, _FTX_FFJFT_JX, _FTX_FFJFT_JT, &mDim, &mHMIN, &mHMAX, &mEPS, &mP, x, &t, &h, mR, &mIERR);
    return mIERR == 0;   // 0 - success
}

}   // namespace runge
