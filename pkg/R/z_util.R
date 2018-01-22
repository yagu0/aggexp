#Maximum size of stored data to predict next PM10
MAX_HISTORY = 10000

#Default lambda value (when too few data)
LAMBDA = 2.

#Maximum error to keep a line in (incremental) data
MAX_ERROR = 20.

#Turn a "vector" into 1D matrix if needed (because R auto cast 1D matrices)
matricize = function(x)
{
	if (!is.null(dim(x)))
		return (as.matrix(x))
	return (t(as.matrix(x)))
}

#Moore-Penrose pseudo inverse
mpPsInv = function(M)
{
	epsilon = 1e-10
    s = svd(M)
    sd = s$d ; sd[sd < epsilon] = Inf
    sd = diag(1.0 / sd, min(nrow(M),ncol(M)))
    return (s$v %*% sd %*% t(s$u))
}

#Heuristic for k in knn algorithms
getKnn = function(n)
{
	return ( max(1, min(50, ceiling(n^(2./3.)))) )
}

#Minimize lambda*||u||^2 + ||Xu - Y||^2
ridgeSolve = function(X, Y, lambda)
{
	s = svd(X)
	deltaDiag = s$d / (s$d^2 + lambda)
	deltaDiag[!is.finite(deltaDiag)] = 0.0
	if (length(deltaDiag) > 1)
		deltaDiag = diag(deltaDiag)
	return (s$v %*% deltaDiag %*% t(s$u) %*% Y)
}

#Return the indices (of rows, by default) without any NA
getNoNAindices = function(M, margin=1)
{
	return (apply(M, margin, function(z)(!any(is.na(z)))))
}
