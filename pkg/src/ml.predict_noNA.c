#include <math.h>
#include <stdlib.h>

void ml_predict_noNA(double* X, double* Y, int* n_, int* K_, double* alpha_, int* grad_, double* weight)
{
	int K = *K_;
	int n = *n_;
	double alpha = *alpha_;
	int grad = *grad_;

	//at least two experts to combine: various inits
	double initWeight = 1. / K;
	for (int i=0; i<K; i++)
		weight[i] = initWeight;
	double* error = (double*)malloc(K*sizeof(double));
	double* cumDeltaError = (double*)calloc(K, sizeof(double));
	double* regret = (double*)calloc(K, sizeof(double));

	//start main loop
	for (int t=0; t<n; t++ < n)
	{
		if (grad)
		{
			double hatY = 0.;
			for (int i=0; i<K; i++)
				hatY += X[t*K+i] * weight[i];
			for (int i=0; i<K; i++)
				error[i] = 2. * (hatY - Y[t]) * X[t*K+i];
		}
		else
		{
			for (int i=0; i<K; i++)
			{
				double delta = X[t*K+i] - Y[t];
				error[i] = delta * delta;
			}
		}

		double hatError = 0.;
		for (int i=0; i<K; i++)
			hatError += error[i] * weight[i];
		for (int i=0; i<K; i++)
		{
			double deltaError = hatError - error[i];
			cumDeltaError[i] += deltaError * deltaError;
			regret[i] += deltaError;
			double eta = 1. / (1. + cumDeltaError[i]);
			weight[i] = regret[i] > 0. ? eta * regret[i] : 0.;
		}

		double sumWeight = 0.0;
		for (int i=0; i<K; i++)
			sumWeight += weight[i];
		for (int i=0; i<K; i++)
			weight[i] /= sumWeight;
		//redistribute weights if alpha > 0 (all weights are 0 or more, sum > 0)
		for (int i=0; i<K; i++)
			weight[i] = (1. - alpha) * weight[i] + alpha/K;
	}

	free(error);
	free(cumDeltaError);
	free(regret);
}
