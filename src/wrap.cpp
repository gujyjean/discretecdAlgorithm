#include <Rcpp.h>
#include "dBasic.h"
#include "dCD.h"
#include "type.h"
#include <iostream>
using namespace Rcpp;


// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

#include <RcppEigen.h>

// [[Rcpp::depends(RcppEigen)]]
using namespace Eigen;
// using Rcpp::as;
typedef Map<MatrixXd> MapMatd;
typedef Map<MatrixXi> MapMati;
typedef Map<VectorXd> MapVecd;
typedef Map<VectorXi> MapVeci;


// [[Rcpp::export]]
List CD( int node,
                    int dataSize,
                    Eigen::Map<Eigen::MatrixXi> data,
                    Eigen::Map<Eigen::VectorXi> nlevels,
                    List obsIndex_R,
                    int eor_nr,
                    Eigen::Map<Eigen::MatrixXi> eor,
                    Eigen::Map<Eigen::VectorXd> lambda_seq,
                    int nlam,
                    double eps,
                    double convLb,
                    double qtol,
                    Eigen::Map<Eigen::MatrixXd> weights,
                    double gamma,
                    double upperbound
) {

  // convert Rcpp type to C++ type
  // construct obsIndex
 VectorXVXi obsIndex(node);
 for (int i=0; i<node; i++) {
   obsIndex(i) = obsIndex_R[i];
 }

  // construct levelIndex
  MatrixXVXi levelIndex(node, node);
  for (int i=0; i<node; i++) {
    for (int j=0; j<node; j++) {
      if (j != i) {
        levelIndex(j, i).resize(nlevels[j] - 1);
        for (int k=0; k<(nlevels[j]-1); k++) {
          levelIndex(j, i)(k) = k;
        }
      }
      else {
        levelIndex(j, i).resize(nlevels[j]);
        for (int k=0; k<nlevels[j]; k++) {
          levelIndex(j, i)(k) = k;
        }
      }
    }
  }

  MatrixXMXd betaM(node+1, node);
  MatrixXMXd betaN(nlam*(node+1), node);
  Eigen::MatrixXi estimateG = Eigen::MatrixXi::Zero(node*nlam, node);
  Eigen::VectorXd log_like(nlam), dur(nlam);
  for (int i = 0; i < nlam; i++) {
    log_like(i) = 0.0;
    dur(i) = 0.0;
  }

  // Eigen::MatrixXi t_data(Rcpp::as< MapMati >(data));
  Eigen::MatrixXi t_data(dataSize, node);
  for (int i=0; i<dataSize; i++) {
    for (int j=0; j<node; j++) {
      t_data(i, j) = data(i, j);
    }
  }
  Eigen::MatrixXd t_weights(node, node);
  for (int i=0; i<node; i++) {
    for (int j=0; j<node; j++) {
      t_weights(i, j) = weights(i, j);
    }
  }

  Eigen::MatrixXi t_eor(eor_nr, 2);
  for (int i=0; i<eor_nr; i++) {
    for (int j=0; j <2; j++) {
      t_eor(i, j) = eor(i, j);
    }
  }

  Eigen::VectorXi t_nlevels(node);
  for (int i=0; i<node; i++) {
    t_nlevels(i) = nlevels(i);
  }

  Eigen::VectorXd lambdaSeq(nlam);
  for (int i=0; i<nlam; i++) {
    lambdaSeq(i) = lambda_seq(i);
  }

  // Run CDAlgorithm

  CDAlgo(node, dataSize, t_data, t_nlevels, obsIndex, levelIndex, eor_nr, t_eor,
         nlam, eps, convLb, qtol, lambdaSeq, log_like, dur, betaM, betaN,
         estimateG, t_weights, gamma, upperbound);

  NumericMatrix adaptWeights(node, node);
  for (int i=0; i<node; i++) {
    for (int j=0; j<node; j++) {
      adaptWeights(i, j) = betaM(i, j).norm();
    }
  }

  // IntegerMatrix outputG(node*nlam, node);
  // for (int i=0; i<(node*nlam); i++) {
  //   for (int j=0; j<node; j++) {
  //     outputG(i, j) = estimateG(i, j);
  //   }
  // }
  // should return lambdaSeq, time.
  // return estimateG, adaptive weight matrix, and timing, ;
  return List::create(_["estimateG"] = wrap(estimateG), _["time"] = wrap(dur), _["adaptive_weights"] = wrap(adaptWeights));
}

// [[Rcpp::export]]
double lambdaMax( int node,
         int dataSize,
         Eigen::Map<Eigen::MatrixXi> data,
         Eigen::Map<Eigen::VectorXi> nlevels,
         List obsIndex_R,
         Eigen::Map<Eigen::MatrixXd> weights,
         double gamma,
         double upperbound
) {

  // convert Rcpp type to C++ type
  // construct obsIndex
  VectorXVXi obsIndex(node);
  for (int i=0; i<node; i++) {
    obsIndex(i) = obsIndex_R[i];
  }

  // construct levelIndex
  MatrixXVXi levelIndex(node, node);
  for (int i=0; i<node; i++) {
    for (int j=0; j<node; j++) {
      if (j != i) {
        levelIndex(j, i).resize(nlevels[j] - 1);
        for (int k=0; k<(nlevels[j]-1); k++) {
          levelIndex(j, i)(k) = k;
        }
      }
      else {
        levelIndex(j, i).resize(nlevels[j]);
        for (int k=0; k<nlevels[j]; k++) {
          levelIndex(j, i)(k) = k;
        }
      }
    }
  }

  MatrixXMXd betaM(node+1, node);
  // Eigen::MatrixXi t_data(Rcpp::as< MapMati >(data));
  Eigen::MatrixXi t_data(dataSize, node);
  for (int i=0; i<dataSize; i++) {
    for (int j=0; j<node; j++) {
      t_data(i, j) = data(i, j);
    }
  }
  Eigen::MatrixXd t_weights(node, node);
  for (int i=0; i<node; i++) {
    for (int j=0; j<node; j++) {
      t_weights(i, j) = weights(i, j);
    }
  }

  Eigen::VectorXi t_nlevels(node);
  for (int i=0; i<node; i++) {
    t_nlevels(i) = nlevels(i);
  }

  double lambda = 0.0;

  // Run CDAlgorithm

  maxLambda(node, dataSize, t_data, t_nlevels, obsIndex, levelIndex, betaM, t_weights, lambda, gamma, upperbound);

  // should return lambdaSeq, time.
  // return estimateG;
  return lambda;
}

// [[Rcpp::export]]
List CD_learning( int node,
         int dataSize,
         Eigen::Map<Eigen::MatrixXi> data,
         Eigen::Map<Eigen::VectorXi> nlevels,
         List obsIndex_R,
         int eor_nr,
         Eigen::Map<Eigen::MatrixXi> eor,
         double eps,
         double convLb,
         double qtol
) {

  // convert Rcpp type to C++ type
  // construct obsIndex
  VectorXVXi obsIndex(node);
  for (int i=0; i<node; i++) {
    obsIndex(i) = obsIndex_R[i];
  }

  // construct levelIndex
  MatrixXVXi levelIndex(node, node);
  for (int i=0; i<node; i++) {
    for (int j=0; j<node; j++) {
      if (j != i) {
        levelIndex(j, i).resize(nlevels[j] - 1);
        for (int k=0; k<(nlevels[j]-1); k++) {
          levelIndex(j, i)(k) = k;
        }
      }
      else {
        levelIndex(j, i).resize(nlevels[j]);
        for (int k=0; k<nlevels[j]; k++) {
          levelIndex(j, i)(k) = k;
        }
      }
    }
  }

  MatrixXMXd betaM(node+1, node);
  double log_like = 0.0;

  Eigen::MatrixXi t_data(dataSize, node);
  for (int i=0; i<dataSize; i++) {
    for (int j=0; j<node; j++) {
      t_data(i, j) = data(i, j);
    }
  }

  Eigen::MatrixXi t_eor(eor_nr, 2);
  for (int i=0; i<eor_nr; i++) {
    for (int j=0; j <2; j++) {
      t_eor(i, j) = eor(i, j);
    }
  }

  Eigen::VectorXi t_nlevels(node);
  for (int i=0; i<node; i++) {
    t_nlevels(i) = nlevels(i);
  }

  Eigen::MatrixXd adaptWeights = Eigen::MatrixXd::Zero(node, node);

  // Run CDAlgorithm

  CD_learning(node, dataSize, t_data, t_nlevels, obsIndex, levelIndex, eor_nr, t_eor,
         eps, convLb, qtol, log_like, betaM, adaptWeights);

  NumericMatrix adapt_weights(node, node);
  for (int row = 0; row < node; row++) {
    for (int col = 0; col < node; col++) {
      adapt_weights(row, col) = adaptWeights(row, col);
    }
  }

  // should return betaM, adaptWeights, log_like.
  // return estimateG;
  return List::create(_["adapt_weights"] = wrap(adapt_weights), _["log_likelihood"] = wrap(log_like));
}
