#include <list>
#include <vector>

#include <R.h>
#include <Rinternals.h>

#include "segFit.hpp"

using namespace std;

extern "C" SEXP segfitc(SEXP s, SEXP smp, SEXP btype, SEXP lb, SEXP ub, SEXP m,
			SEXP maxiter, SEXP factr, SEXP pgtol) {
    
    // compute
    SegFit segFit(*REAL(smp), static_cast<int>(*REAL(btype)),
		  *REAL(lb), *REAL(ub), static_cast<int>(*REAL(m)),
		  static_cast<int>(*REAL(maxiter)), *REAL(factr), *REAL(pgtol));

    const vector<Segment>* result;
  
    segFit.setString(REAL(s), length(s));
    segFit.run();

    result = segFit.getResult();  

    // allocate return value
    SEXP ret;
    PROTECT(ret = Rf_allocMatrix(REALSXP, 6, result->size()));

    int i = 0;
    for ( vector<Segment>::const_iterator iter = result->begin();
	  iter != result->end(); iter++ )
    {
	REAL(ret)[i]   = static_cast<double>(iter->headIndex) + 1;
	REAL(ret)[i+1] = static_cast<double>(iter->tailIndex) + 1;
	REAL(ret)[i+2] = iter->a;
	REAL(ret)[i+3] = iter->b;
	REAL(ret)[i+4] = iter->c;
	REAL(ret)[i+5] = static_cast<double>(iter->order);

	i += 6;
    }

    UNPROTECT(1);
    
    return ret;
}
