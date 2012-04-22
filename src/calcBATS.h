#ifndef _forecast_CALCBATS
#define _forecast_CALCBATS

#include <cmath>
#include <RcppArmadillo.h>


RcppExport SEXP calcBATS(SEXP ys, SEXP yHats, SEXP wTransposes, SEXP Fs, SEXP xs, SEXP gs, SEXP es ) ;

RcppExport SEXP calcBATSFaster(SEXP ys, SEXP yHats, SEXP wTransposes, SEXP Fs, SEXP xs, SEXP gs, SEXP es, SEXP xNought_s, SEXP sPeriods_s, SEXP betaV, SEXP tau_s, SEXP p_s, SEXP q_s )  ;

RcppExport SEXP calcWTilda(SEXP wTildaTransposes, SEXP Ds) ;

RcppExport SEXP makeBATSWMatrix(SEXP smallPhi_s, SEXP sPeriods_s, SEXP arCoefs_s, SEXP maCoefs_s) ;

RcppExport SEXP makeBATSGMatrix(SEXP alpha_s, SEXP beta_s, SEXP gammaVector_s, SEXP seasonalPeriods_s, SEXP p_s, SEXP q_s) ;

RcppExport SEXP updateFMatrix(SEXP F_s, SEXP smallPhi_s, SEXP alpha_s, SEXP beta_s, SEXP gammaBold_s, SEXP ar_s, SEXP ma_s, SEXP tau_s) ;

RcppExport SEXP updateWtransposeMatrix(SEXP wTranspose_s, SEXP smallPhi_s, SEXP tau_s, SEXP arCoefs_s, SEXP maCoefs_s, SEXP p_s, SEXP q_s) ;

RcppExport SEXP updateGMatrix(SEXP g_s, SEXP gammaBold_s, SEXP alpha_s, SEXP beta_s, SEXP gammaVector_s, SEXP seasonalPeriods_s) ;

//TBATS Functions
RcppExport SEXP makeTBATSWMatrix(SEXP smallPhi_s, SEXP kVector_s, SEXP arCoefs_s, SEXP maCoefs_s, SEXP tau_s) ;

RcppExport SEXP makeCIMatrix(SEXP k_s, SEXP m_s) ;

RcppExport SEXP makeSIMatrix(SEXP k_s, SEXP m_s) ;

RcppExport SEXP makeAIMatrix(SEXP C_s, SEXP S_s, SEXP k_s) ;

RcppExport SEXP updateTBATSGammaBold(SEXP gammaBold_s, SEXP kVector_s, SEXP gammaOne_s, SEXP gammaTwo_s) ;

RcppExport SEXP updateTBATSGMatrix(SEXP g_s, SEXP gammaBold_s, SEXP alpha_s, SEXP beta_s) ;

RcppExport SEXP calcTBATSFaster(SEXP ys, SEXP yHats, SEXP wTransposes, SEXP Fs, SEXP xs, SEXP gs, SEXP es, SEXP xNought_s) ;

#endif
