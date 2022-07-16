//#include <stdio.h>
//#include <string.h>

#include "stdafx.h"

#include <math.h>
#include <string.h>
#include <iostream>
#include <ostream>
#include <iomanip>
#include <stdexcept>
#include <sstream>

#include <cfun.h>


#if defined (_WIN32) || defined (__CYGWIN__)
#   define DES_USES_WIN
#endif

#if defined (DES_USES_WIN)
#   ifdef _WIN64
#      ifdef CVM_ILP64
#          ifdef _DEBUG
#              define ENGL_SO    "des_engl_em64t_ilp64_debug.dll"
#              define ROS_SO     "des_ros_em64t_ilp64_debug.dll"
#              define LOU_SO     "des_lou_em64t_ilp64_debug.dll"
#          else
#              define ENGL_SO    "des_engl_em64t_ilp64.dll"
#              define ROS_SO     "des_ros_em64t_ilp64.dll"
#              define LOU_SO     "des_lou_em64t_ilp64.dll"
#          endif
#      else
#          ifdef _DEBUG
#              define ENGL_SO    "des_engl_em64t_debug.dll"
#              define ROS_SO     "des_ros_em64t_debug.dll"
#              define LOU_SO     "des_lou_em64t_debug.dll"
#          else
#              define ENGL_SO    "des_engl_em64t.dll"
#              define ROS_SO     "des_ros_em64t.dll"
#              define LOU_SO     "des_lou_em64t.dll"
#          endif
#      endif
#   else
#      ifdef _DEBUG
#          define ENGL_SO    "des_engl_ia32_debug.dll"
#          define ROS_SO     "des_ros_ia32_debug.dll"
#          define LOU_SO     "des_lou_ia32_debug.dll"
#      else
#          define ENGL_SO    "des_engl_ia32.dll"
#          define ROS_SO     "des_ros_ia32.dll"
#          define LOU_SO     "des_lou_ia32.dll"
#      endif
#   endif
#   define ENGL_PROC  "SENGL"
#   define ROS_A_PROC "SROSA"
#   define ROS_N_PROC "SROSN"
#   define LOU_I_PROC "SLOUI"
#   define LOU_N_PROC "SLOUN"
#   define LOU_U_PROC "SLOUU"
#else
#   ifdef __APPLE__
#       define DES_SO_EXT "dylib"
#   else
#       define DES_SO_EXT "so"
#   endif
#   if defined(EM64T) || defined(__amd64__) || defined(__x86_64__)
#      ifdef CVM_ILP64
#          ifdef _DEBUG
#              define ENGL_SO    "libdes_engl_em64t_ilp64_debug." DES_SO_EXT
#              define ROS_SO     "libdes_ros_em64t_ilp64_debug." DES_SO_EXT
#              define LOU_SO     "libdes_lou_em64t_ilp64_debug." DES_SO_EXT
#          else
#              define ENGL_SO    "libdes_engl_em64t_ilp64." DES_SO_EXT
#              define ROS_SO     "libdes_ros_em64t_ilp64." DES_SO_EXT
#              define LOU_SO     "libdes_lou_em64t_ilp64." DES_SO_EXT
#          endif
#      else
#          ifdef _DEBUG
#              define ENGL_SO    "libdes_engl_em64t_debug." DES_SO_EXT
#              define ROS_SO     "libdes_ros_em64t_debug." DES_SO_EXT
#              define LOU_SO     "libdes_lou_em64t_debug." DES_SO_EXT
#          else
#              define ENGL_SO    "libdes_engl_em64t." DES_SO_EXT
#              define ROS_SO     "libdes_ros_em64t." DES_SO_EXT
#              define LOU_SO     "libdes_lou_em64t." DES_SO_EXT
#          endif
#      endif
#   else
#      ifdef _DEBUG
#          define ENGL_SO    "libdes_engl_ia32_debug." DES_SO_EXT
#          define ROS_SO     "libdes_ros_ia32_debug." DES_SO_EXT
#          define LOU_SO     "libdes_lou_ia32_debug." DES_SO_EXT
#      else
#          define ENGL_SO    "libdes_engl_ia32." DES_SO_EXT
#          define ROS_SO     "libdes_ros_ia32." DES_SO_EXT
#          define LOU_SO     "libdes_lou_ia32." DES_SO_EXT
#      endif
#   endif

#   define ENGL_PROC  "sengl_"
#   define ROS_A_PROC "srosa_"
#   define ROS_N_PROC "srosn_"
#   define LOU_I_PROC "sloui_"
#   define LOU_N_PROC "sloun_"
#   define LOU_U_PROC "slouu_"
#endif


#if defined (__GNUC__)
#   include <sys/time.h>
#endif

#include "../loader.h"
#include "../solver.h"

using namespace cvm;

long long get_microseconds()
{
#if defined (__GNUC__) && !defined(__MINGW32__)
	static timeval tv;
	gettimeofday(&tv, NULL);
	return tv.tv_sec * 1000000LL + tv.tv_usec; 
#else
	static FILETIME tv;
	GetSystemTimeAsFileTime(&tv);
	return ((static_cast<long long>(tv.dwHighDateTime) << 32) + tv.dwLowDateTime) / 10; // 100 nanosecs intervals
#endif
}

long long get_microseconds_since(bool since_last = true)
{
	static long long first_microseconds = get_microseconds();
	static long long last_microseconds = first_microseconds;
	long long current_microseconds = get_microseconds();
	if (since_last) {
		long long ret = current_microseconds - last_microseconds;
		last_microseconds = current_microseconds;
		return ret;
	}
	return current_microseconds - first_microseconds;
}


namespace test_des {

	// multithreaded synchronizer
class CriticalSection {
private:
    bool mbOK;

#if defined (CVM_MT)
    #if defined (WIN32) || defined (_WIN32)
        ::CRITICAL_SECTION mCriticalSection; 
    #else                                                                       // POSIX Threads library assumed
        pthread_mutex_t mMutex;
        pthread_mutexattr_t mMutexAttr;
    #endif
#endif

public:
    CriticalSection () : mbOK (false)
    {
#if defined (CVM_MT)
    #if defined (WIN32) || defined (_WIN32)
        if (::InitializeCriticalSectionAndSpinCount (&mCriticalSection, 0x80000400)) {
            mbOK = true;
        }
        else {
            ::InitializeCriticalSection (&mCriticalSection);
            mbOK = true;
        }
    #else
        if (pthread_mutexattr_init (&mMutexAttr) != 0) {
            std::cout << "FAILED TO pthread_mutexattr_init" << std::endl;
        }
        if (pthread_mutexattr_setpshared (&mMutexAttr, PTHREAD_PROCESS_PRIVATE) != 0) {
            std::cout << "FAILED TO pthread_mutexattr_setpshared" << std::endl;
        }
        if (pthread_mutex_init (&mMutex, &mMutexAttr) != 0) {
            std::cout << "FAILED TO pthread_mutex_init" << std::endl;
        }
    #endif
#endif
    }

    ~CriticalSection ()
    {
#if defined (CVM_MT)
    #if defined (WIN32) || defined (_WIN32)
        if (mbOK) {
            ::DeleteCriticalSection (&mCriticalSection);
        }
    #else
        if (pthread_mutexattr_destroy (&mMutexAttr) != 0) {
            std::cout << "FAILED TO pthread_mutexattr_destroy" << std::endl;
        }
        if (pthread_mutex_destroy (&mMutex) != 0) {
            std::cout << "FAILED TO pthread_mutex_destroy" << std::endl;
        }

    #endif
#endif
        mbOK = false;
    }

    void enter ()
    {
#if defined (CVM_MT)
    #if defined (WIN32) || defined (_WIN32)
        if (mbOK) {
            ::EnterCriticalSection (&mCriticalSection); 
        }
    #else
        if (pthread_mutex_lock (&mMutex) != 0) {
            std::cout << "FAILED TO pthread_mutex_lock" << std::endl;
        }
    #endif
#endif
    }

    void leave ()
    {
#if defined (CVM_MT)
    #if defined (WIN32) || defined (_WIN32)
        if (mbOK) {
            ::LeaveCriticalSection (&mCriticalSection);
        }
    #else
        if (pthread_mutex_unlock (&mMutex) != 0) {
            std::cout << "FAILED TO pthread_mutex_unlock" << std::endl;
        }
    #endif
#endif
    }
};

}


test_des::CriticalSection gCS;

class LockIt
{
public:
    LockIt ();
    ~LockIt ();
};

LockIt::LockIt ()
{
    gCS.enter();
}

LockIt::~LockIt ()
{
    gCS.leave();
}


class test_exception : public std::exception
{
    std::string m_what;
public:
    explicit test_exception(const std::string& what) : m_what(what) {}
    virtual ~test_exception() throw () {}
    virtual const char* what() const throw () {
        return m_what.c_str();
    }
};


void report (const char* szMsg, int line) throw (test_exception)
{
    std::ostringstream oss;
    oss << "TEST \'" << szMsg << "\' FAILED ON LINE " << line << std::endl;
    throw test_exception (oss.str());
}

template <typename E, typename R>
void report (const E& expected, const R& returned, const char* szMsg, int line) throw (test_exception)
{
    std::ostringstream oss;
    oss.precision (16);
    oss.setf (std::ios::scientific | std::ios::showpoint | std::ios::left);
    oss << "TEST \'" << szMsg << "\' FAILED ON LINE " << line << std::endl;
    oss << "Expected: " << expected << std::endl;
    oss << "Returned: " << returned << std::endl << std::ends;
    throw test_exception (oss.str());
}

void report_time (std::ostream& os) throw (std::exception)
{
//    os << "*clock* " << get_microseconds_since() << "ms " << std::endl;
    os << get_microseconds_since() << "us ";
}

void CheckBool (bool bPattern, bool b, const char* szMsg, std::ostream&, int line) throw (test_exception)
{
    LockIt l;
    if (b != bPattern) {
    	report (bPattern, b, szMsg, line);
    }
}

void CheckBoolNoLock (bool bPattern, bool b, const char* szMsg, std::ostream&, int line) throw (test_exception)
{
    if (b != bPattern) {
    	report (bPattern, b, szMsg, line);
    }
}

void CheckInt (tint vPattern, tint v, const char* szMsg, std::ostream&, int line) throw (test_exception)
{
    LockIt l;
    if (v != vPattern) {
        report (vPattern, v, szMsg, line);
    }
}

void CheckReal (treal vPattern, treal v, const char* szMsg, std::ostream&, int line, treal rSp = cvmMachSp ()) throw (test_exception)
{
    LockIt l;
    const treal mp = (treal) fabs ((treal) vPattern);
    treal vn = v - vPattern;
    if (mp > (treal) 1.) vn /= mp;
    if (fabs (vn) > rSp) {
        report (vPattern, v, szMsg, line);
    }
}

void Fail (const char* szMsg, int line) throw (test_exception)
{
    LockIt l;
    report (szMsg, line);
}


extern "C" {

    //    SUBROUTINE F(M,T,X,FX) computes vector f(t,x)
    void __stdcall Fengl (tint* PC, const tint* M, const double* T, const double* X, double* Z) {
        Z[0] = -X[0] - 5. * X[1];
        Z[1] = X[0] + X[1];
    }
    //    SUBROUTINE F(M,T,X,FX) computes vector f(t,x)
    void __stdcall Fengl2 (tint* PC, const tint* M, const double* T, const double* X, double* Z) {
        Z[0] =  3. * X[0] + 4. * X[1];
        Z[1] = -4. * X[0] + 3. * X[1];
    }
    
    //    SUBROUTINE F(M,X,FX) computes vector f(x)
    void __stdcall FRosA (tint* PC, const tint* M, const double* X, double* F) {
        F[0] = -1.e4 * X[0] + 1.e2 * X[1] - 10. * X[2] + X[3];
        F[1] = -1.e3 * X[1] + 10. * X[2] - 10. * X[3];
        F[2] = -X[2] + 10. * X[3];
        F[3] = -0.1 * X[3];
    }
    //    SUBROUTINE FJ(M,X,XJ) computes Jacobi matrix FJ(x)
    void __stdcall FRosAj (tint* PC, const tint* M, const double* X, double* DF) {
    	DF[0] = -1.e4; DF[4] =  1.e2; DF[8]  = -10.; DF[12] =  1.;
    	DF[1] =  0.;   DF[5] = -1.e3; DF[9]  =  10.; DF[13] = -10.;
    	DF[2] =  0.;   DF[6] =  0.;   DF[10] = -1.;  DF[14] =  10.;
    	DF[3] =  0.;   DF[7] =  0.;   DF[11] =  0.;  DF[15] = -0.1;
    }

    //    SUBROUTINE F(M,T,X,FX) computes vector f(t,x)
    void __stdcall FRosN (tint* PC, const tint* M, const double* T, const double* X, double* F) {
    	F[0] = -1.e2 * X[0];
    	double T1 = 20. * exp (-1.e2 * *T) + 2. * exp(- *T) * cos(*T);
    	F[1] = F[0] - 2. * X[1] + T1;
    	F[2] = F[0] + 9998. * X[1] - 9990. * X[2] - 10. * X[3] + T1;
    	F[3] = F[0] + 9988. * X[1] + 20. * X[2] - 10010. * X[3] + T1;
    }

    //    SUBROUTINE FJ(M,T,X,XJ) computes Jacobi matrix FJ(t,x)
    void __stdcall FRosNj (tint* PC, const tint* M, const double* T, const double* X, double* DF) {
    	DF[0] = -1.e2; DF[4] =    0.; DF[8]  =     0.; DF[12] =      0.;
    	DF[1] = -1.e2; DF[5] =   -2.; DF[9]  =     0.; DF[13] =      0.;
    	DF[2] = -1.e2; DF[6] = 9998.; DF[10] = -9990.; DF[14] =   -100.;
    	DF[3] = -1.e2; DF[7] = 9998.; DF[11] =    20.; DF[15] = -10010.;
    }

    //    SUBROUTINE FT(M,T,X,XJ) computes vector of derivatives df(t,x)/dt
    void __stdcall FRosNt (tint* PC, const tint* M, const double* T, const double* X, double* FT) {
    	FT[0] = 0.;
    	FT[1] = -2.e3 * exp (-1.e2 * *T) - 2. * exp (- *T) * (cos (*T) + sin (*T));
    	FT[2] = FT[1];
    	FT[3] = FT[1];
    }

    //    SUBROUTINE F(M,T,X,FX) computes vector F(t,x)
    void __stdcall FLouN (tint* PC, const tint* M, const double* T, const double* X, double* F) {
        F[0] = X[1] - 5. * cos (*T);
        F[1] = 2. * X[0] + X[1];
    }

    //    SUBROUTINE FJ(M,T,X,XJ) computes Jacobi matrix FJ(t,x)
    void __stdcall FLouNj (tint* PC, const tint* M, const double* T, const double* X, double* DF) {
    	DF[0] =  0.;   DF[2] =  1.;
    	DF[1] =  2.;   DF[3] =  1.;
    }

    //    FU   - SUBROUTINE FU(M,T,X,U) computes vactor u(t,x)
    void __stdcall FLouUu (tint* PC, const tint* M, const double* T, const double* X, double* U) {
    	U[0] =  X[0] / *T;
    	U[1] =  - X[1] / *T;
    }

    //     FA   - SUBROUTINE FA(M,T,A) computes matrix A(t)
    void __stdcall FLouIA (tint* PC, const tint* M, const double* T, double* A) {
    	A[0] =  1.;        A[2] =  1. / *T;
    	A[1] =  1. / *T;   A[3] =  1.;
    }

    //     FI   - SUBROUTINE FI(M,T,F) computes vector FI(t)
    void __stdcall FLouIfi (tint* PC, const tint* M, const double* T, double* FI) {
    	FI[0] = *T;
    	FI[1] = - *T;
    }
}


#define TEST_DES_TWO  2
#define TEST_DES_FOUR 4


int main (int argc, char* argv[])
{
    std::ostream& os = std::cout;
    os.precision (16);
    os.setf (std::ios::scientific | std::ios::showpoint | std::ios::left);

/*
#ifdef _WIN32
	os << "_WIN32" << std::endl;
#else
	os << "not _WIN32" << std::endl;
#endif
*/
    
    get_microseconds_since(false);	// set clock
    
	try {

	    {
            // VS2008 & VS2005 mix crashes here :(
            try {
			    string_array sa;
			    sa.push_back("{x1 x2} -x1-5*x2");
			    sa.push_back("{x1 x2} x1+x2");
		        rfvector fv (sa);
                Fail("No exception about parsing error", __LINE__);
            } catch (cvmexception& ex) {
                CheckInt (ex.cause(), CFUN_PARSEERROR, "Wrong exception cause", os, __LINE__);
            }
	    }		
        
        // Registering solvers:
        {
            runge::SolverRegistry::register_solver_routine ("engl", 
                runge::FTX_F,
                get_loader().get_solver (ENGL_SO, ENGL_PROC),
                0, 7, 0);

            runge::SolverRegistry::register_solver_routine ("lfafi", 
                runge::AFI_FAFI,
                get_loader().get_solver (LOU_SO, LOU_I_PROC),
                7, 5, 0);

            runge::SolverRegistry::register_solver_routine ("lftx", 
                runge::FTX_FFJ,
                get_loader().get_solver (LOU_SO, LOU_N_PROC),
                6, 6, 0);

            runge::SolverRegistry::register_solver_routine ("lbu",
                runge::BUTX_BFU,
                get_loader().get_solver (LOU_SO, LOU_U_PROC),
                5, 6, 0);

            runge::SolverRegistry::register_solver_routine ("rosa",
                runge::FX_FFJ,
                get_loader().get_solver (ROS_SO, ROS_A_PROC),
                6, 4, 0);

            runge::SolverRegistry::register_solver_routine ("rosn",
                runge::FTX_FFJFT,
                get_loader().get_solver (ROS_SO, ROS_N_PROC),
                6, 6, 0);

            os  << "Solvers registered in ";
	        report_time (os);
	        os  << std::endl;
        }


		// CFUN England
		{
	        double H, T, X1, X2;
            cvm::rvector X(TEST_DES_TWO);
	        H = 5.e-1;
	        T = 0.;
	        X[1] = 1.;
	        X[2] = 1.;
	
            string_array saVars, saBodies, saParameters, saMeanings;
            saVars.push_back("t");
            saVars.push_back("x1");
            saVars.push_back("x2");
            saBodies.push_back("-x1-5*x2");
            saBodies.push_back("x1+x2");

            rfvector fv (saVars, saBodies, saParameters, saMeanings);
            std::auto_ptr<runge::SolverFTX_F> pSolver(runge::SolverRegistry::createSolverFTX_F("engl", fv));

	        os  << "CFUN England Test Initialized in ";
	        report_time (os);
	        os  << std::endl;
	        
	        os  << "CFUN England:" << std::endl;
	        for (int i = 0; i < 10; ++i) {
	
                if (!pSolver->step(T, X, H)) {
                    Fail ("engl solver IERR", __LINE__);
                }

		        X1 = cos (2. * T) - 3. * sin (2. * T);
		        X2 = cos (2. * T) + sin (2. * T);
	
		    	report_time (os);
                CheckReal(X1, X[1], "engl solver X1", os, __LINE__, pSolver->get_eps());
	            CheckReal(X2, X[2], "engl solver X2", os, __LINE__, pSolver->get_eps());
	        }
	        os << std::endl;
		}

        // CFUN Lawson (A(t)x + fi(t))
        {
	        double H, T, X1, X2, EPS_MULTISTEP;
            cvm::rvector X(TEST_DES_TWO);
	        H = 1.e-2;
	        T = 1.;
	        X[1] = -1.;
	        X[2] = 1.;
            
            string_array saVars, saBodiesFi, saBodiesAt, saParameters, saMeanings;
            saVars.push_back("t");
            saBodiesFi.push_back("t");
            saBodiesFi.push_back("-t");
            rfvector fi (saVars, saBodiesFi, saParameters, saMeanings);
            saBodiesAt.push_back("1");
            saBodiesAt.push_back("1/t");
            saBodiesAt.push_back("1/t");
            saBodiesAt.push_back("1");
            rfmatrix fa (TEST_DES_TWO, TEST_DES_TWO, saVars, saBodiesAt, saParameters, saMeanings);

            std::auto_ptr<runge::SolverAFI_FAFI> pSolver(runge::SolverRegistry::createSolverSolverAFI_FAFI("lfafi", fa, fi));
            pSolver->set_eps(1.e-15);
	        EPS_MULTISTEP = pSolver->get_eps() * 10.;

            os  << "CFUN Lawson (A(t)x + fi(t)) Test Initialized in ";
            report_time (os);
            os  << std::endl;

            os  << "CFUN Lawson (A(t)x + fi(t)):" << std::endl;
            for (int i = 0; i < 10; ++i) {

                if (!pSolver->step(T, X, H)) {
                    Fail ("lfafi solver IERR", __LINE__);
                }

	            X1 = -2. +  (4.*exp(T)/exp(1.) - 2. - T*T) / T;
	            X2 =  2. + (-4.*exp(T)/exp(1.) + 2. + T*T) / T;

            	report_time (os);
                CheckReal(X1, X[1], "lfafi solver X1", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X2, X[2], "lfafi solver X2", os, __LINE__, EPS_MULTISTEP);
            }
            os << std::endl;
        }

        // CFUN Lawson (non-auto)
        {
	        double H, T, X1, X2, EPS_MULTISTEP;
            cvm::rvector X(TEST_DES_TWO);
	        H = 1.e-2;
	        T = 0.;
	        X[1] = 1.;
	        X[2] = 1.;

            string_array saVars, saBodies, saParameters, saMeanings;
            saVars.push_back("t");
            saVars.push_back("x1");
            saVars.push_back("x2");
            saBodies.push_back("x2-5*cos(t)");
            saBodies.push_back("2*x1+x2");

            rfvector fv (saVars, saBodies, saParameters, saMeanings);
            std::auto_ptr<runge::SolverFTX_FFJ> pSolver(runge::SolverRegistry::createSolverFTX_FFJ("lftx", fv));
            pSolver->set_eps(1.e-15);
	        EPS_MULTISTEP = pSolver->get_eps() * 10.;

            os  << "CFUN Lawson (non-auto) Test Initialized in ";
            report_time (os);
            os  << std::endl;

            os  << "CFUN Lawson (non-auto):" << std::endl;
            for (int i = 0; i < 10; ++i) {

                if (!pSolver->step(T, X, H)) {
                    Fail ("lftx solver IERR", __LINE__);
                }

	            X1 =  2. * exp (-T) - 2. * sin (T) -      cos (T);
	            X2 = -2. * exp (-T) +      sin (T) + 3. * cos (T);

            	report_time (os);
                CheckReal(X1, X[1], "lftx solver X1", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X2, X[2], "lftx solver X2", os, __LINE__, EPS_MULTISTEP);
            }
            os << std::endl;
        }

        // CFUN Lawson (Bx+u(t,x))
        {
	        double H, T, X1, X2, EPS_MULTISTEP;
            double Ba[] = {1., 2., 2., 1.};
            cvm::rvector X(TEST_DES_TWO);
            const cvm::srmatrix B(Ba, TEST_DES_TWO);
	        H = 1.e-2;
	        T = 1.;
	        X[1] = -1.;
	        X[2] = 1.;

            string_array saVars, saBodies, saParameters, saMeanings;
            saVars.push_back("t");
            saVars.push_back("x1");
            saVars.push_back("x2");
            saBodies.push_back("x1/t");
            saBodies.push_back("-x2/t");

            rfvector fvu (saVars, saBodies, saParameters, saMeanings);
            std::auto_ptr<runge::SolverBUTX_BFU> pSolver(runge::SolverRegistry::createSolverBUTX_BFU("lbu", B, fvu));
            pSolver->set_eps(1.e-15);
	        EPS_MULTISTEP = pSolver->get_eps() * 10.;

            const double a = -0.75 / exp (-1.) ;
            const double b = -0.25 / exp(3.);
            const double c = -0.125 / exp(3.);
            const double d = 0.375 / exp(-1.);

            os  << "CFUN Lawson (Bx+u(t,x)) Test Initialized in ";
            report_time (os);
            os  << std::endl;

            os  << "CFUN Lawson (Bx+u(t,x)):" << std::endl;
            for (int i = 0; i < 10; ++i) {

                if (!pSolver->step(T, X, H)) {
                    Fail ("lbu solver IERR", __LINE__);
                }

	            X1 =  a * exp(-T) + b * exp(3.*T);
	            X2 =  c * (2.-1./T) * exp(3.*T) + d * (2.+1./T) * exp(-T);

            	report_time (os);
                CheckReal(X1, X[1], "lbu solver X1", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X2, X[2], "lbu solver X2", os, __LINE__, EPS_MULTISTEP);
            }
            os << std::endl;
        }

        // CFUN Rosenbrock (auto)
        {
	        double H, T, EPS_MULTISTEP;
            double X1, X2, X3, X4, T1, T2, T3, AA, BB, C, DD, FF, GG, QQ;
            cvm::rvector X(TEST_DES_FOUR);
	        H = 1.e-2;
	        T = 0.;
	        X[1] = 1.; X[2] = 1.; X[3] = 1.; X[4] = 1.;

            string_array saVars, saBodies, saParameters, saMeanings;
            saVars.push_back("X1");
            saVars.push_back("X2");
            saVars.push_back("X3");
            saVars.push_back("X4");
            saBodies.push_back("-1.e4 * X1 + 1.e2 * X2 - 10. * X3 + X4");
            saBodies.push_back("-1.e3 * X2 + 10. * X3 - 10. * X4");
            saBodies.push_back("-X3 + 10. * X4");
            saBodies.push_back("-0.1 * X4");

            rfvector fv (saVars, saBodies, saParameters, saMeanings);
            std::auto_ptr<runge::SolverFX_FFJ> pSolver(runge::SolverRegistry::createSolverFX_FFJ("rosa", fv));
	        EPS_MULTISTEP = pSolver->get_eps() * 10.;

            os  << "CFUN Rosenbrock (auto) Test Initialized in ";
            report_time (os);
            os  << std::endl;

            os  << "CFUN Rosenbrock (auto):" << std::endl;
            for (int i = 0; i < 10; ++i) {

                if (!pSolver->step(T, X, H)) {
                    Fail ("rosa solver IERR", __LINE__);
                }

                X4 = exp (-0.1 * T);
                T1 = exp (-T);
                X3 = -(9.1 / 0.9) * T1 + (10. / 0.9) * X4;
                T2 = exp (-1.e3 * T);
                AA = -91. / (0.9 * 999.);
                BB = 91. / (0.9 * 999.9);
                C  = 1. - AA - BB;
                X2 = C * T2 + AA * T1 + BB * X4;
                T3 = exp (-1.e4 * T);
                DD = C / 90.;
                FF = (100. * AA + 91. / 0.9) / 9999.;
                GG = (100. * BB - 100. / 0.9 + 1.) / 9999.9;
                QQ = 1. - DD - FF - GG;
                X1 = QQ * T3 + DD * T2 + FF * T1 + GG * X4;

            	report_time (os);
                CheckReal(X1, X[1], "rosa solver X1", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X2, X[2], "rosa solver X2", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X3, X[3], "rosa solver X3", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X4, X[4], "rosa solver X4", os, __LINE__, EPS_MULTISTEP);
            }
            os << std::endl;
        }

        // CFUN Rosenbrock (non-auto)
        {
	        double H, T, EPS_MULTISTEP;
	        double X1, X2, X3, X4;
            cvm::rvector X(TEST_DES_FOUR);
	        H = 1.e-2;
	        T = 0.;
	        X[1] = 10.;
	        X[2] = 11.;
	        X[3] = 111.;
	        X[4] = 111.;

            string_array saVars, saBodies, saParameters, saMeanings;
            saParameters.push_back("T1");
            saMeanings.push_back("20. * exp (-1.e2 * T) + 2. * exp(- T) * cos(T)");
            saVars.push_back("T");
            saVars.push_back("X1");
            saVars.push_back("X2");
            saVars.push_back("X3");
            saVars.push_back("X4");
            saBodies.push_back("-1.e2 * X1");
            saBodies.push_back("-1.e2 * X1 - 2. * X2 + T1");
            saBodies.push_back("-1.e2 * X1 + 9998. * X2 - 9990. * X3 - 10. * X4 + T1");
            saBodies.push_back("-1.e2 * X1 + 9988. * X2 + 20. * X3 - 10010. * X4 + T1");

            rfvector fv (saVars, saBodies, saParameters, saMeanings);
            std::auto_ptr<runge::SolverFTX_FFJFT> pSolver(runge::SolverRegistry::createSolverFTX_FFJFT("rosn", fv));
            pSolver->set_eps(1.e-10);
            pSolver->set_p(1.e3);
	        EPS_MULTISTEP = pSolver->get_eps() * 10.;

            os  << "CFUN Rosenbrock (non-auto) Test Initialized in ";
            report_time (os);
            os  << std::endl;

            os  << "CFUN Rosenbrock (non-auto):" << std::endl;
            for (int i = 0; i < 10; ++i) {

                if (!pSolver->step(T, X, H)) {
                    Fail ("rosn solver IERR", __LINE__);
                }

	            X1 = 10. * exp (-1.e2 * T);
	            X2 = X1 + exp (-T) * (cos (T) + sin (T));
	            X3 = X2 + 1.e2 * exp (-1.e4 * T) * cos (10. * T);
	            X4 = X3 + 1.e2 * exp (-1.e4 * T) * sin (10. * T);

            	report_time (os);
                CheckReal(X1, X[1], "rosn solver X1", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X2, X[2], "rosn solver X2", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X3, X[3], "rosn solver X3", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X4, X[4], "rosn solver X4", os, __LINE__, EPS_MULTISTEP);
            }
            os << std::endl;
        }


///////////// native calls:

		// Testing England
		{
	        double P, HMIN, HMAX, H, EPS, T, X1, X2;
	        double X[TEST_DES_TWO];
	        double R[7*TEST_DES_TWO];
	        tint M, IERR;
	
	        M = 2;
	        P = 100.;
	        HMIN = 1.e-10;
	        HMAX = 1.e-2;
	        H = 5.e-1;
	        EPS = 1.e-11;
	        T = 0.;
	        X[0] = 1.;
	        X[1] = 1.;
	
            os  << "Dll \'" << ENGL_SO << "\', Proc \'" << ENGL_PROC << "\'" << std::endl;
	        solver_FTX_F proc1 = (solver_FTX_F) get_loader().get_solver (ENGL_SO, ENGL_PROC);
	        solver_FTX_F proc2 = (solver_FTX_F) get_loader().get_solver (ENGL_SO, ENGL_PROC); // comes fron cache

	        os  << "England Test Initialized in ";
	        report_time (os);
	        os  << std::endl;
	        
	        os  << "England 1:" << std::endl;
	        for (int i = 0; i < 10; ++i) {
	
	        	proc1 (NULL, Fengl, &M, &HMIN, &HMAX, &EPS, &P, X, &T, &H, R, &IERR);
	
		        X1 = cos (2. * T) - 3. * sin (2. * T);
		        X2 = cos (2. * T) + sin (2. * T);
	
		    	report_time (os);
	            CheckReal(X1, X[0], "sengl proc1", os, __LINE__, EPS);
	            CheckReal(X2, X[1], "sengl proc1", os, __LINE__, EPS);
	            CheckInt (0, IERR, "sengl proc1 IERR", os, __LINE__);
	        }
	        os << std::endl;
	        
	        H = 1.e-1;
	        T = 0.;
	        X[0] = 0.;
	        X[1] = 1.;
	        os << "England 2:" << std::endl;
	        for (int i = 0; i < 10; ++i) {
	
	        	proc2 (NULL, Fengl2, &M, &HMIN, &HMAX, &EPS, &P, X, &T, &H, R, &IERR);
	
		        X1 = exp (3. * T) * sin (4. * T);
		        X2 = exp (3. * T) * cos (4. * T);
	
		    	report_time (os);
	            CheckReal(X1, X[0], "sengl proc2", os, __LINE__, EPS);
	            CheckReal(X2, X[1], "sengl proc2", os, __LINE__, EPS);
	            CheckInt (IERR, 0,  "sengl proc2 IERR", os, __LINE__);
	        }
	        os << std::endl;
		}

        // Lawson (non-auto)
        {
	        double P, HMIN, HMAX, H, EPS, T, EPS_MULTISTEP;
	        double X[TEST_DES_TWO];
	        double R[6*TEST_DES_TWO*TEST_DES_TWO+6*TEST_DES_TWO];
	        tint M, IERR;
            double X1, X2;
	
	        M = 2;
	        P = 1000.;
	        HMIN = 1.e-10;
	        HMAX = 1.e-1;
	        H = 1.e-2;
	        EPS = 1.e-15;
	        EPS_MULTISTEP = EPS * 10.;
	        T = 0.;
	        X[0] = 1.;
	        X[1] = 1.;
	        
            os  << "Dll \'" << LOU_SO << "\', Proc \'" << LOU_N_PROC << "\'" << std::endl;
	        solver_FTX_FFJ proc_ln = (solver_FTX_FFJ) get_loader().get_solver (LOU_SO, LOU_N_PROC);

            os  << "Lawson (non-auto) Test Initialized in ";
            report_time (os);
            os  << std::endl;

            os  << "Lawson (non-auto):" << std::endl;
            for (int i = 0; i < 10; ++i) {

	            proc_ln (NULL, FLouN, FLouNj, &M, &HMIN, &HMAX, &EPS, &P, X, &T, &H, R, &IERR);

	            X1 =  2. * exp (-T) - 2. * sin (T) -      cos (T);
	            X2 = -2. * exp (-T) +      sin (T) + 3. * cos (T);

            	report_time (os);
                CheckInt (0, IERR, "sloun IERR", os, __LINE__);
                CheckReal(X1, X[0], "sloun X1", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X2, X[1], "sloun X2", os, __LINE__, EPS_MULTISTEP);
            }
            os << std::endl;
        }


        // Lawson (Bx+u(t,x))
        {
	        double P, HMIN, HMAX, H, EPS, T, EPS_MULTISTEP;
	        double X[TEST_DES_TWO];
	        double B[2*TEST_DES_TWO];
	        double R[5*TEST_DES_TWO*TEST_DES_TWO+6*TEST_DES_TWO];
	        tint M, IERR;
            double X1, X2;
	
	        M = 2;
	        P = 1000.;
	        HMIN = 1.e-10;
	        HMAX = 1.e-1;
	        H = 1.e-2;
	        EPS = 1.e-15;
	        EPS_MULTISTEP = EPS * 10.;
	        T = 1.;
	        X[0] = -1.;
	        X[1] = 1.;
	        B[0] = 1.; B[2] = 2.;
	        B[1] = 2.; B[3] = 1.;

            const double a = -0.75 / exp (-1.) ;
            const double b = -0.25 / exp(3.);
            const double c = -0.125 / exp(3.);
            const double d = 0.375 / exp(-1.);

            os  << "Dll \'" << LOU_SO << "\', Proc \'" << LOU_U_PROC << "\'" << std::endl;
	        solver_BUTX_BFU proc_lu = (solver_BUTX_BFU) get_loader().get_solver (LOU_SO, LOU_U_PROC);

            os  << "Lawson (Bx+u(t,x)) Test Initialized in ";
            report_time (os);
            os  << std::endl;

            os  << "Lawson (Bx+u(t,x)):" << std::endl;
            for (int i = 0; i < 10; ++i) {

	            proc_lu (NULL, B, FLouUu, &M, &HMIN, &HMAX, &EPS, &P, X, &T, &H, R, &IERR);

	            X1 =  a * exp(-T) + b * exp(3.*T);
	            X2 =  c * (2.-1./T) * exp(3.*T) + d * (2.+1./T) * exp(-T);

            	report_time (os);
                CheckInt (0, IERR, "slouu IERR", os, __LINE__);
                CheckReal(X1, X[0], "slouu X1", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X2, X[1], "slouu X2", os, __LINE__, EPS_MULTISTEP);
            }
            os << std::endl;
        }

        // Lawson (A(t)x + fi(t))
        {
	        double P, HMIN, HMAX, H, EPS, T, EPS_MULTISTEP;
	        double X[TEST_DES_TWO];
	        double R[7*TEST_DES_TWO*TEST_DES_TWO+5*TEST_DES_TWO];
	        tint M, IERR;
            double X1, X2;
	
	        M = 2;
	        P = 1000.;
	        HMIN = 1.e-10;
	        HMAX = 1.e-1;
	        H = 1.e-2;
	        EPS = 1.e-15;
	        EPS_MULTISTEP = EPS * 10.;
	        T = 1.;
	        X[0] = -1.;
	        X[1] = 1.;

            os  << "Dll \'" << LOU_SO << "\', Proc \'" << LOU_I_PROC << "\'" << std::endl;
	        solver_AFI_FAFI proc_li = (solver_AFI_FAFI) get_loader().get_solver (LOU_SO, LOU_I_PROC);

            os  << "Lawson (A(t)x + fi(t)) Test Initialized in ";
            report_time (os);
            os  << std::endl;

            os  << "Lawson (A(t)x + fi(t)):" << std::endl;
            for (int i = 0; i < 10; ++i) {

	            proc_li (NULL, FLouIA, FLouIfi, &M, &HMIN, &HMAX, &EPS, &P, X, &T, &H, R, &IERR);

	            X1 = -2. +  (4.*exp(T)/exp(1.) - 2. - T*T) / T;
	            X2 =  2. + (-4.*exp(T)/exp(1.) + 2. + T*T) / T;

            	report_time (os);
                CheckInt (0, IERR, "sloui IERR", os, __LINE__);
                CheckReal(X1, X[0], "sloui X1", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X2, X[1], "sloui X2", os, __LINE__, EPS_MULTISTEP);
            }
            os << std::endl;
        }

        // Rosenbrock (auto)
        {
	        double P, HMIN, HMAX, H, EPS, T, EPS_MULTISTEP;
	        double X[4];
	        double R[6*TEST_DES_FOUR*TEST_DES_FOUR+4*TEST_DES_FOUR];
	        tint M, IERR;
            double X1, X2, X3, X4, T1, T2, T3, AA, BB, C, DD, FF, GG, QQ;
	
	        M = 4;
	        P = 1000.;
	        HMIN = 1.e-8;
	        HMAX = 1.e-1;
	        H = 1.e-2;
	        EPS = 1.e-10;
	        EPS_MULTISTEP = EPS * 10.;
	        T = 0.;
	        X[0] = 1.;
	        X[1] = 1.;
	        X[2] = 1.;
	        X[3] = 1.;
	        
            os  << "Dll \'" << ROS_SO << "\', Proc \'" << ROS_A_PROC << "\'" << std::endl;
	        solver_FX_FFJ proc_a = (solver_FX_FFJ) get_loader().get_solver (ROS_SO, ROS_A_PROC);

            os  << "Rosenbrock (auto) Test Initialized in ";
            report_time (os);
            os  << std::endl;

            os  << "Rosenbrock (auto):" << std::endl;
            for (int i = 0; i < 10; ++i) {

	            proc_a (NULL, FRosA, FRosAj, &M, &HMIN, &HMAX, &EPS, &P, X, &T, &H, R, &IERR);

                X4 = exp (-0.1 * T);
                T1 = exp (-T);
                X3 = -(9.1 / 0.9) * T1 + (10. / 0.9) * X4;
                T2 = exp (-1.e3 * T);
                AA = -91. / (0.9 * 999.);
                BB = 91. / (0.9 * 999.9);
                C  = 1. - AA - BB;
                X2 = C * T2 + AA * T1 + BB * X4;
                T3 = exp (-1.e4 * T);
                DD = C / 90.;
                FF = (100. * AA + 91. / 0.9) / 9999.;
                GG = (100. * BB - 100. / 0.9 + 1.) / 9999.9;
                QQ = 1. - DD - FF - GG;
                X1 = QQ * T3 + DD * T2 + FF * T1 + GG * X4;

            	report_time (os);
                CheckInt (0, IERR, "srosa IERR", os, __LINE__);
                CheckReal(X1, X[0], "srosa X1", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X2, X[1], "srosa X2", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X3, X[2], "srosa X3", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X4, X[3], "srosa X4", os, __LINE__, EPS_MULTISTEP);
            }
            os << std::endl;
        }

        // Rosenbrock (non-auto)
        {
	        double P, HMIN, HMAX, H, EPS, T, EPS_MULTISTEP;
	        double X[TEST_DES_FOUR];
	        double R[6*TEST_DES_FOUR*TEST_DES_FOUR+6*TEST_DES_FOUR];
	        tint M, IERR;
            double X1, X2, X3, X4;
	
	        M = 4;
	        P = 1000.;
	        HMIN = 1.e-10;
	        HMAX = 1.e-1;
	        H = 1.e-2;
	        EPS = 1.e-10;
	        EPS_MULTISTEP = EPS * 10.;
	        T = 0.;
	        X[0] = 10.;
	        X[1] = 11.;
	        X[2] = 111.;
	        X[3] = 111.;
	        
            os  << "Dll \'" << ROS_SO << "\', Proc \'" << ROS_N_PROC << "\'" << std::endl;
	        solver_FTX_FFJFT proc_n = (solver_FTX_FFJFT) get_loader().get_solver (ROS_SO, ROS_N_PROC);

            os  << "Rosenbrock (non-auto) Test Initialized in ";
            report_time (os);
            os  << std::endl;

            os  << "Rosenbrock (non-auto):" << std::endl;
            for (int i = 0; i < 10; ++i) {

	            proc_n (NULL, FRosN, FRosNj, FRosNt, &M, &HMIN, &HMAX, &EPS, &P, X, &T, &H, R, &IERR);

	            X1 = 10. * exp (-1.e2 * T);
	            X2 = X1 + exp (-T) * (cos (T) + sin (T));
	            X3 = X2 + 1.e2 * exp (-1.e4 * T) * cos (10. * T);
	            X4 = X3 + 1.e2 * exp (-1.e4 * T) * sin (10. * T);

            	report_time (os);
                CheckInt (0, IERR, "srosn IERR", os, __LINE__);
                CheckReal(X1, X[0], "srosn X1", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X2, X[1], "srosn X2", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X3, X[2], "srosn X3", os, __LINE__, EPS_MULTISTEP);
                CheckReal(X4, X[3], "srosn X4", os, __LINE__, EPS_MULTISTEP);
            }
            os << std::endl;
        }

    } catch (cvmexception& ex) {
    	os << "cvmexception: " << ex.what() << std::endl;
    } catch (std::exception& ex) {
    	os << "std::exception: " << ex.what() << std::endl;
    }
    os << "Total execution time: " << get_microseconds_since(false) << "us" << std::endl;
    return 0;
}

