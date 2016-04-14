#ifndef _DISCRETE_BASIC_H_
#define _DISCRETE_BASIC_H_

#include "type.h"
#include <vector>

struct gStruct {
	Eigen::MatrixXi ordex;
	Eigen::MatrixXi trueG;
	std::vector<int> ts;
};

const int Cycle(const int& node, const int *G, const int& a, const int& b);
bool check(const int& node, const int* G);
void revsort(double *a, int *ib, int n);
template <class T> std::vector<T> ProbSampleNoReplace(const std::vector<T>& vec, int nans, std::vector<double>& prob);
template <class T> std::vector<T> SampleNoReplace(const std::vector<T>& value, int nans);
std::vector<int> seq(const int& from, const int& to);
std::vector<int> degreeG(const int& maxdeg, const int& node, const int& nedge);
gStruct GGen(const int& maxdeg, const int& node, const int& nedge);
gStruct MCGen(const int& node);
void DatGen(const Eigen::MatrixXi& ordex, const std::vector<int>& ts, int noi, int nobs, Eigen::MatrixXi& data, VectorXVXi& obsIndex, MatrixXVXi& levelIndex, double coef);
void DatGen_obs(const Eigen::MatrixXi& ordex, const std::vector<int>& ts, int nobs, Eigen::MatrixXi& data, MatrixXVXi& levelIndex, double coef);

#endif
