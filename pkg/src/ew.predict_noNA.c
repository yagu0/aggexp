#include <math.h>
#include <stdlib.h>

void ew_predict_noNA(double* X, double* Y, int* n_, int* K_, double* alpha_, int* grad_, double* weight)
{
	int K = *K_;
	int n = *n_;
	double alpha = *alpha_;
	int grad = *grad_;

	//at least two experts to combine: various inits
	double invMaxError = 1. / 50; //TODO: magic number
	double logK = log(K);
	double initWeight = 1. / K;
	for (int i=0; i<K; i++)
		weight[i] = initWeight;
	double* error = (double*)malloc(K*sizeof(double));
	double* cumError = (double*)calloc(K, sizeof(double));

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
/*				if ((X[t*K+i] <= 30 && Y[t] > 30) || (X[t*K+i] > 30 && Y[t] <= 30))
					error[i] = 1.0;
				else
					error[i] = 0.0;
*/
			}
		}
		for (int i=0; i<K; i++)
			cumError[i] += error[i];

		if (t < n-1 && !grad)
		{
			//weight update is useless
			continue;
		}

		//double eta = invMaxError * sqrt(8*logK/(t+1)); //TODO: good formula ?
		double eta = invMaxError * 1. / (t+1); //TODO: good formula ?
		for (int i=0; i<K; i++)
			weight[i] = exp(-eta * cumError[i]);
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
	free(cumError);
}
